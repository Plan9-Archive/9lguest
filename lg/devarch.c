#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "../port/error.h"
#include "lguest.h"

typedef struct IOMap IOMap;
struct IOMap
{
	IOMap	*next;
	int	reserved;
	char	tag[13];
	ulong	start;
	ulong	end;
};

static struct
{
	Lock;
	IOMap	*m;
	IOMap	*free;
	IOMap	maps[32];		// some initial free maps

	QLock	ql;			// lock for reading map
} iomap;

enum {
	Qdir = 0,
	Qioalloc = 1,
	Qiob,
	Qiow,
	Qiol,
	Qbase,

	Qmax = 16,
};

typedef long Rdwrfn(Chan*, void*, long, vlong);

static Rdwrfn *readfn[Qmax];
static Rdwrfn *writefn[Qmax];

static Dirtab archdir[Qmax] = {
	".",		{ Qdir, 0, QTDIR },	0,	0555,
	"ioalloc",	{ Qioalloc, 0 },	0,	0444,
	"iob",		{ Qiob, 0 },		0,	0660,
	"iow",		{ Qiow, 0 },		0,	0660,
	"iol",		{ Qiol, 0 },		0,	0660,
};
Lock archwlock;	/* the lock is only for changing archdir */
int narchdir = Qbase;
int (*_pcmspecial)(char*, ISAConf*);
void (*_pcmspecialclose)(int);

static int doi8253set = 0;

/* arguably this should be in here, not in main.c */
extern struct lguest_data lguest_data;

/*
 * Add a file to the #P listing.  Once added, you can't delete it.
 * You can't add a file with the same name as one already there,
 * and you get a pointer to the Dirtab entry so you can do things
 * like change the Qid version.  Changing the Qid path is disallowed.
 */
Dirtab*
addarchfile(char *name, int perm, Rdwrfn *rdfn, Rdwrfn *wrfn)
{
	int i;
	Dirtab d;
	Dirtab *dp;

	memset(&d, 0, sizeof d);
	strcpy(d.name, name);
	d.perm = perm;

	lock(&archwlock);
	if(narchdir >= Qmax){
		unlock(&archwlock);
		return nil;
	}

	for(i=0; i<narchdir; i++)
		if(strcmp(archdir[i].name, name) == 0){
			unlock(&archwlock);
			return nil;
		}

	d.qid.path = narchdir;
	archdir[narchdir] = d;
	readfn[narchdir] = rdfn;
	writefn[narchdir] = wrfn;
	dp = &archdir[narchdir++];
	unlock(&archwlock);

	return dp;
}

void
ioinit(void)
{
	int i;

	for(i = 0; i < nelem(iomap.maps)-1; i++)
		iomap.maps[i].next = &iomap.maps[i+1];
	iomap.maps[i].next = nil;
	iomap.free = iomap.maps;

}

// Reserve a range to be ioalloced later. 
// This is in particular useful for exchangable cards, such
// as pcmcia and cardbus cards.
int
ioreserve(int, int size, int align, char *tag)
{
	IOMap *m, **l;
	int i, port;

	lock(&iomap);
	// find a free port above 0x400 and below 0x1000
	port = 0x400;
	for(l = &iomap.m; *l; l = &(*l)->next){
		m = *l;
		if (m->start < 0x400) continue;
		i = m->start - port;
		if(i > size)
			break;
		if(align > 0)
			port = ((port+align-1)/align)*align;
		else
			port = m->end;
	}
	if(*l == nil){
		unlock(&iomap);
		return -1;
	}
	m = iomap.free;
	if(m == nil){
		print("ioalloc: out of maps");
		unlock(&iomap);
		return port;
	}
	iomap.free = m->next;
	m->next = *l;
	m->start = port;
	m->end = port + size;
	m->reserved = 1;
	strncpy(m->tag, tag, sizeof(m->tag));
	m->tag[sizeof(m->tag)-1] = 0;
	*l = m;

	archdir[0].qid.vers++;

	unlock(&iomap);
	return m->start;
}

//
//	alloc some io port space and remember who it was
//	alloced to.  if port < 0, find a free region.
//
int
ioalloc(int port, int size, int align, char *tag)
{
	IOMap *m, **l;
	int i;

	lock(&iomap);
	if(port < 0){
		// find a free port above 0x400 and below 0x1000
		port = 0x400;
		for(l = &iomap.m; *l; l = &(*l)->next){
			m = *l;
			if (m->start < 0x400) continue;
			i = m->start - port;
			if(i > size)
				break;
			if(align > 0)
				port = ((port+align-1)/align)*align;
			else
				port = m->end;
		}
		if(*l == nil){
			unlock(&iomap);
			return -1;
		}
	} else {
		// Only 64KB I/O space on the x86.
		if((port+size) > 0x10000){
			unlock(&iomap);
			return -1;
		}
		// see if the space clashes with previously allocated ports
		for(l = &iomap.m; *l; l = &(*l)->next){
			m = *l;
			if(m->end <= port)
				continue;
			if(m->reserved && m->start == port && m->end == port + size) {
				m->reserved = 0;
				unlock(&iomap);
				return m->start;
			}
			if(m->start >= port+size)
				break;
			unlock(&iomap);
			return -1;
		}
	}
	m = iomap.free;
	if(m == nil){
		print("ioalloc: out of maps");
		unlock(&iomap);
		return port;
	}
	iomap.free = m->next;
	m->next = *l;
	m->start = port;
	m->end = port + size;
	strncpy(m->tag, tag, sizeof(m->tag));
	m->tag[sizeof(m->tag)-1] = 0;
	*l = m;

	archdir[0].qid.vers++;

	unlock(&iomap);
	return m->start;
}

void
iofree(int port)
{
	IOMap *m, **l;

	lock(&iomap);
	for(l = &iomap.m; *l; l = &(*l)->next){
		if((*l)->start == port){
			m = *l;
			*l = m->next;
			m->next = iomap.free;
			iomap.free = m;
			break;
		}
		if((*l)->start > port)
			break;
	}
	archdir[0].qid.vers++;
	unlock(&iomap);
}

int
iounused(int start, int end)
{
	IOMap *m;

	for(m = iomap.m; m; m = m->next){
		if(start >= m->start && start < m->end
		|| start <= m->start && end > m->start)
			return 0; 
	}
	return 1;
}

static void
checkport(int start, int end)
{
	/* standard vga regs are OK */
	if(start >= 0x2b0 && end <= 0x2df+1)
		return;
	if(start >= 0x3c0 && end <= 0x3da+1)
		return;

	if(iounused(start, end))
		return;
	error(Eperm);
}

static Chan*
archattach(char* spec)
{
	return devattach('P', spec);
}

Walkqid*
archwalk(Chan* c, Chan *nc, char** name, int nname)
{
	return devwalk(c, nc, name, nname, archdir, narchdir, devgen);
}

static int
archstat(Chan* c, uchar* dp, int n)
{
	return devstat(c, dp, n, archdir, narchdir, devgen);
}

static Chan*
archopen(Chan* c, int omode)
{
	return devopen(c, omode, archdir, narchdir, devgen);
}

static void
archclose(Chan*)
{
}

enum
{
	Linelen= 31,
};

static long
archread(Chan *c, void *a, long n, vlong offset)
{
	char *buf, *p;
	int port;
	ushort *sp;
	ulong *lp;
	IOMap *m;
	Rdwrfn *fn;

	switch((ulong)c->qid.path){

	case Qdir:
		return devdirread(c, a, n, archdir, narchdir, devgen);

	case Qiob:
		port = offset;
		checkport(offset, offset+n);
		for(p = a; port < offset+n; port++)
			*p++ = inb(port);
		return n;

	case Qiow:
		if(n & 1)
			error(Ebadarg);
		checkport(offset, offset+n);
		sp = a;
		for(port = offset; port < offset+n; port += 2)
			*sp++ = ins(port);
		return n;

	case Qiol:
		if(n & 3)
			error(Ebadarg);
		checkport(offset, offset+n);
		lp = a;
		for(port = offset; port < offset+n; port += 4)
			*lp++ = inl(port);
		return n;

	case Qioalloc:
		break;

	default:
		if(c->qid.path < narchdir && (fn = readfn[c->qid.path]))
			return fn(c, a, n, offset);
		error(Eperm);
		break;
	}

	if((buf = malloc(n)) == nil)
		error(Enomem);
	p = buf;
	n = n/Linelen;
	offset = offset/Linelen;

	lock(&iomap);
	for(m = iomap.m; n > 0 && m != nil; m = m->next){
		if(offset-- > 0)
			continue;
		sprint(p, "%8lux %8lux %-12.12s\n", m->start, m->end-1, m->tag);
		p += Linelen;
		n--;
	}
	unlock(&iomap);

	n = p - buf;
	memmove(a, buf, n);
	free(buf);

	return n;
}

static long
archwrite(Chan *c, void *a, long n, vlong offset)
{
	char *p;
	int port;
	ushort *sp;
	ulong *lp;
	Rdwrfn *fn;

	switch((ulong)c->qid.path){

	case Qiob:
		p = a;
		checkport(offset, offset+n);
		for(port = offset; port < offset+n; port++)
			outb(port, *p++);
		return n;

	case Qiow:
		if(n & 1)
			error(Ebadarg);
		checkport(offset, offset+n);
		sp = a;
		for(port = offset; port < offset+n; port += 2)
			outs(port, *sp++);
		return n;

	case Qiol:
		if(n & 3)
			error(Ebadarg);
		checkport(offset, offset+n);
		lp = a;
		for(port = offset; port < offset+n; port += 4)
			outl(port, *lp++);
		return n;

	default:
		if(c->qid.path < narchdir && (fn = writefn[c->qid.path]))
			return fn(c, a, n, offset);
		error(Eperm);
		break;
	}
	return 0;
}

Dev archdevtab = {
	'P',
	"arch",

	devreset,
	devinit,
	devshutdown,
	archattach,
	archwalk,
	archstat,
	archopen,
	devcreate,
	archclose,
	archread,
	devbread,
	archwrite,
	devbwrite,
	devremove,
	devwstat,
};

/*
 *  the following is a generic version of the
 *  architecture specific stuff
 */

static int
unimplemented(int)
{
	return 0;
}

static void
nop(void)
{
}

/*
 * 386 has no compare-and-swap instruction.
 * Run it with interrupts turned off instead.
 */
static int
cmpswap386(long *addr, long old, long new)
{
	int r, s;
	
	s = splhi();
	if(r = (*addr == old))
		*addr = new;
	splx(s);
	return r;
}

/*
 * On a uniprocessor, you'd think that coherence could be nop,
 * but it can't.  We still need a barrier when using coherence() in
 * device drivers.
 *
 * On VMware, it's safe (and a huge win) to set this to nop.
 * Aux/vmware does this via the #P/archctl file.
 */
void (*coherence)(void) = nop;

int (*cmpswap)(long*, long, long) = cmpswap386;

uvlong lguestfastticks(uvlong *hz){
	uvlong x;
	if (hz)
		*hz = 1024*1024*1024*4ULL;
//	cycles(&x);
	x = lguest_get_ns();
	return x;
}

Lock lgintrlock;

void
lguesttimerset(uvlong){
}

void
lgintrinit(void)
{

}

int
lgintrisr(int)
{
	return 0;
}

int
lgintrenable(Vctl* v)
{
	int irq, irqbit;

	/*
	 * Given an IRQ, enable the corresponding interrupt in the lgintr
	 * and return the vector to be used. The lgintr is set to use a fixed
	 * range of vectors starting at VectorPIC.
	 */
	irq = v->irq;
	if(irq < 0 || irq > 63){
		print("lgintrenable: irq %d out of range\n", irq);
		return -1;
	}
	/* drop 32 from the irq */
	irq -= 32;
	irqbit = 1<<irq;

	ilock(&lgintrlock);
	/* sense changes. Here,  1 is DISABLED, not ENABLED
	  * it's BLOCKED 
	  */
	lguest_data.blocked_interrupts[0] &= ~irqbit;
	iunlock(&lgintrlock);

	return irq + 32;
}

int
lgintrvecno(int irq)
{
	return irq;
}

int
lgintrdisable(int irq)
{
	int irqbit;

	/*
	 * Given an IRQ, enable the corresponding interrupt in the lgintr
	 * and return the vector to be used. The lgintr is set to use a fixed
	 * range of vectors starting at VectorPIC.
	 */
	if(irq < 0 || irq > 63){
		print("lgintrenable: irq %d out of range\n", irq);
		return -1;
	}
	/* drop 32 from the irq */
	irq -= 32;
	irqbit = 1<<irq;

	ilock(&lgintrlock);
	/* sense changes. Here,  1 is DISABLED, not ENABLED
	  * it's BLOCKED 
	  */
	lguest_data.blocked_interrupts[0] |= irqbit;
	iunlock(&lgintrlock);

	return irq;
}

void
lgintron(void)
{
}

void
lgintroff(void)
{
}

static void lgrestart(void){
//iprint("HACK SETTING RIDIC TIME\n");
	lguest_interval_timer(100000000);
}

static void
lgclock(Ureg* ureg, void*)
{
//lgconswrite("L",1);
	timerintr(ureg, 0);
	lgrestart();
}


void
lgclockenable(void)
{
//	iprint("WARNING: CLOCK INTR DISABLED===========>\n");
	intrenable(IrqCLOCK, lgclock, 0, BUSUNKNOWN, "clock");
	/* now kick off the timer */
	lgrestart();
}


PCArch* arch;
extern PCArch* knownarch[];
PCArch archlguest = {
.id=		"archlguest",
.ident=		0,
//.reset=		panic,
.serialpower=	unimplemented,
.modempower=	unimplemented,
.fastclock=	lguestfastticks,
.timerset=	lguesttimerset,
.intrinit=	lgintrinit,
.intrenable=	lgintrenable,
.intrvecno=	lgintrvecno,
.intrdisable=	lgintrdisable,
.intron=		lgintron,
.introff=		lgintroff,
.clockenable=	lgclockenable,

/*
.intrinit=	unimplemented,
.intrenable=	unimplemented,
.intrvecno=	unimplemented,
.intrdisable=	unimplemented,
.intron=		unimplemented,
.introff=		unimplemented,

*/
};


typedef struct X86type X86type;
struct X86type {
	int	family;
	int	model;
	int	aalcycles;
	char*	name;
};

static X86type x86intel[] =
{
	{ 4,	0,	22,	"486DX", },	/* known chips */
	{ 4,	1,	22,	"486DX50", },
	{ 4,	2,	22,	"486SX", },
	{ 4,	3,	22,	"486DX2", },
	{ 4,	4,	22,	"486SL", },
	{ 4,	5,	22,	"486SX2", },
	{ 4,	7,	22,	"DX2WB", },	/* P24D */
	{ 4,	8,	22,	"DX4", },	/* P24C */
	{ 4,	9,	22,	"DX4WB", },	/* P24CT */
	{ 5,	0,	23,	"P5", },
	{ 5,	1,	23,	"P5", },
	{ 5,	2,	23,	"P54C", },
	{ 5,	3,	23,	"P24T", },
	{ 5,	4,	23,	"P55C MMX", },
	{ 5,	7,	23,	"P54C VRT", },
	{ 6,	1,	16,	"PentiumPro", },/* trial and error */
	{ 6,	3,	16,	"PentiumII", },
	{ 6,	5,	16,	"PentiumII/Xeon", },
	{ 6,	6,	16,	"Celeron", },
	{ 6,	7,	16,	"PentiumIII/Xeon", },
	{ 6,	8,	16,	"PentiumIII/Xeon", },
	{ 6,	0xB,	16,	"PentiumIII/Xeon", },
	{ 0xF,	1,	16,	"P4", },	/* P4 */
	{ 0xF,	2,	16,	"PentiumIV/Xeon", },

	{ 3,	-1,	32,	"386", },	/* family defaults */
	{ 4,	-1,	22,	"486", },
	{ 5,	-1,	23,	"P5", },
	{ 6,	-1,	16,	"P6", },
	{ 0xF,	-1,	16,	"P4", },	/* P4 */

	{ -1,	-1,	16,	"unknown", },	/* total default */
};

/*
 * The AMD processors all implement the CPUID instruction.
 * The later ones also return the processor name via functions
 * 0x80000002, 0x80000003 and 0x80000004 in registers AX, BX, CX
 * and DX:
 *	K5	"AMD-K5(tm) Processor"
 *	K6	"AMD-K6tm w/ multimedia extensions"
 *	K6 3D	"AMD-K6(tm) 3D processor"
 *	K6 3D+	?
 */
static X86type x86amd[] =
{
	{ 5,	0,	23,	"AMD-K5", },	/* guesswork */
	{ 5,	1,	23,	"AMD-K5", },	/* guesswork */
	{ 5,	2,	23,	"AMD-K5", },	/* guesswork */
	{ 5,	3,	23,	"AMD-K5", },	/* guesswork */
	{ 5,	6,	11,	"AMD-K6", },	/* trial and error */
	{ 5,	7,	11,	"AMD-K6", },	/* trial and error */
	{ 5,	8,	11,	"AMD-K6-2", },	/* trial and error */
	{ 5,	9,	11,	"AMD-K6-III", },/* trial and error */

	{ 6,	1,	11,	"AMD-Athlon", },/* trial and error */
	{ 6,	2,	11,	"AMD-Athlon", },/* trial and error */

	{ 4,	-1,	22,	"Am486", },	/* guesswork */
	{ 5,	-1,	23,	"AMD-K5/K6", },	/* guesswork */
	{ 6,	-1,	11,	"AMD-Athlon", },/* guesswork */
	{ 0xF,	-1,	11,	"AMD64", },	/* guesswork */

	{ -1,	-1,	11,	"unknown", },	/* total default */
};

/*
 * WinChip 240MHz
 */
static X86type x86winchip[] =
{
	{5,	4,	23,	"Winchip",},	/* guesswork */
	{6,	7,	23,	"Via C3 Samuel 2 or Ezra",},
	{6,	8,	23,	"Via C3 Ezra-T",},
	{6,	9,	23,	"Via C3 Eden-N",},
	{ -1,	-1,	23,	"unknown", },	/* total default */
};

/*
 * SiS 55x
 */
static X86type x86sis[] =
{
	{5,	0,	23,	"SiS 55x",},	/* guesswork */
	{ -1,	-1,	23,	"unknown", },	/* total default */
};

static X86type *cputype;

static void	simplecycles(uvlong*);
void	(*cycles)(uvlong*) = simplecycles;
void	_cycles(uvlong*);	/* in l.s */

void
delay(int millisecs)
{
	millisecs *= m->loopconst;
	if(millisecs <= 0)
		millisecs = 1;
	aamloop(millisecs);
}

void
microdelay(int microsecs)
{
	microsecs *= m->loopconst;
	microsecs /= 1000;
	if(microsecs <= 0)
		microsecs = 1;
	aamloop(microsecs);
}

/*  
 *  performance measurement ticks.  must be low overhead.
 *  doesn't have to count over a second.
 */
ulong
perfticks(void)
{
	uvlong x;

	if(m->havetsc)
		cycles(&x);
	else
		x = lguest_get_ns();
	return x;
}


static void
simplecycles(uvlong*x)
{
	*x = m->ticks;
}

void
cpuidprint(void)
{
	int i;
	char buf[128];

	i = sprint(buf, "cpu%d: %dMHz ", m->machno, m->cpumhz);
	if(m->cpuidid[0])
		i += sprint(buf+i, "%12.12s ", m->cpuidid);
	sprint(buf+i, "%s (cpuid: AX 0x%4.4uX DX 0x%4.4uX)\n",
		m->cpuidtype, m->cpuidax, m->cpuiddx);
	print(buf);
}

/*
 *  figure out:
 *	- cpu type
 *	- whether or not we have a TSC (cycle counter)
 *	- whether or not it supports page size extensions
 *		(if so turn it on)
 *	- whether or not it supports machine check exceptions
 *		(if so turn it on)
 *	- whether or not it supports the page global flag
 *		(if so turn it on)
 */
int
cpuidentify(void)
{
	char *p;
	int family, model, nomce;
	X86type *t, *tab;
	ulong cr4;
	vlong mca, mct;

	cpuid(m->cpuidid, &m->cpuidax, &m->cpuiddx);
	iprint("cpu id is %s\n", m->cpuidid);
	if(strncmp(m->cpuidid, "AuthenticAMD", 12) == 0)
		tab = x86amd;
	else if(strncmp(m->cpuidid, "CentaurHauls", 12) == 0)
		tab = x86winchip;
	else if(strncmp(m->cpuidid, "SiS SiS SiS ", 12) == 0)
		tab = x86sis;
	else
		tab = x86intel;
	
	family = X86FAMILY(m->cpuidax);
	model = X86MODEL(m->cpuidax);
	for(t=tab; t->name; t++)
		if((t->family == family && t->model == model)
		|| (t->family == family && t->model == -1)
		|| (t->family == -1))
			break;

	iprint("t->name is %p\n", t->name);
	m->cpuidtype = t->name;

	/*
	 *  if there is one, set tsc to a known value
	 */
	if(0 && m->cpuiddx & 0x10){
		m->havetsc = 1;
		cycles = _cycles;
		if(m->cpuiddx & 0x20)
			wrmsr(0x10, 0);
	}

	/*
 	 *  use i8253 to guess our cpu speed
	 */
//	guesscpuhz(t->aalcycles);
	m->cpumhz = 1000;
	m->havetsc = 0;
	cycles = _cycles;

	/*
	 * If machine check exception, page size extensions or page global bit
	 * are supported enable them in CR4 and clear any other set extensions.
	 * If machine check was enabled clear out any lingering status.
	 */
	if(0 && m->cpuiddx & 0x2088){
		cr4 = 0;
		if(m->cpuiddx & 0x08)
			cr4 |= 0x10;		/* page size extensions */
		if(p = getconf("*nomce"))
			nomce = strtoul(p, 0, 0);
		else
			nomce = 0;
		if((m->cpuiddx & 0x80) && !nomce){
			cr4 |= 0x40;		/* machine check enable */
			if(family == 5){
				rdmsr(0x00, &mca);
				rdmsr(0x01, &mct);
			}
		}
	
		/*
		 * Detect whether the chip supports the global bit
		 * in page directory and page table entries.  When set
		 * in a particular entry, it means ``don't bother removing
		 * this from the TLB when CR3 changes.''  
		 * 
		 * We flag all kernel pages with this bit.  Doing so lessens the
		 * overhead of switching processes on bare hardware,
		 * even more so on VMware.  See mmu.c:/^memglobal.
		 *
		 * For future reference, should we ever need to do a
		 * full TLB flush, it can be accomplished by clearing
		 * the PGE bit in CR4, writing to CR3, and then
		 * restoring the PGE bit.
		 */
		if(m->cpuiddx & 0x2000){
			cr4 |= 0x80;		/* page global enable bit */
			m->havepge = 1;
		}

		putcr4(cr4);
		if(m->cpuiddx & 0x80)
			rdmsr(0x01, &mct);
	}
	iprint("cpu type is %p, family %#x\n", t, t->family);
	cputype = t;
	return t->family;
}

static long
cputyperead(Chan*, void *a, long n, vlong offset)
{
	char str[32];
	ulong mhz;

	mhz = (m->cpuhz+999999)/1000000;

	snprint(str, sizeof(str), "%s %lud\n", cputype->name, mhz);
	return readstr(offset, a, n, str);
}

static long 
lgread(Chan*, void *a, long nn, vlong offset){
	struct lguest_data *lg = &lguest_data;
	char buf[256];
	int n;
	n = snprint(buf, sizeof buf, "blocked %#ulx walltime %#8.8ulx %#8.8ulx\n",lg->blocked_interrupts, lg->time.seconds, lg->time.nanoseconds);
	n += snprint(buf+n, sizeof buf-n, "reserve_mem %#8.8ulx tsc_khz %lud noirq_start %p noirq_end %p syscall_vec %d\n", 
	lg->reserve_mem, lg->tsc_khz,
	(void *)lg->noirq_start, (void *)lg->noirq_end, 
	lg->syscall_vec);
	buf[n] = 0;
	return readstr(offset, a, nn, buf);	
}
static long
archctlread(Chan*, void *a, long nn, vlong offset)
{
	char buf[256];
	int n;
	extern ulong fcallcount[], intrcount[];
	
	n = snprint(buf, sizeof buf, "cpu %s %lud%s\n",
		cputype->name, (ulong)(m->cpuhz+999999)/1000000,
		m->havepge ? " pge" : "");
	n += snprint(buf+n, sizeof buf-n, "pge %s\n", getcr4()&0x80 ? "on" : "off");
	n += snprint(buf+n, sizeof buf-n, "coherence ");
	if(coherence == mb386)
		n += snprint(buf+n, sizeof buf-n, "mb386\n");
	else if(coherence == mb586)
		n += snprint(buf+n, sizeof buf-n, "mb586\n");
	else if(coherence == nop)
		n += snprint(buf+n, sizeof buf-n, "nop\n");
	else
		n += snprint(buf+n, sizeof buf-n, "0x%p\n", coherence);
	n += snprint(buf+n, sizeof buf-n, "cmpswap ");
	if(cmpswap == cmpswap386)
		n += snprint(buf+n, sizeof buf-n, "cmpswap386\n");
	else if(cmpswap == cmpswap486)
		n += snprint(buf+n, sizeof buf-n, "cmpswap486\n");
	else
		n += snprint(buf+n, sizeof buf-n, "0x%p\n", cmpswap);
	n += snprint(buf+n, sizeof buf-n, "i8253set %s\n", doi8253set ? "on" : "off");
	n += snprint(buf+n, sizeof buf-n, "fcallcount %ld irqcount %ld\n", 
		fcallcount[0], intrcount[0]);
	buf[n] = 0;
	return readstr(offset, a, nn, buf);
}

enum
{
	CMpge,
	CMcoherence,
	CMi8253set,
};

static Cmdtab archctlmsg[] =
{
	CMpge,		"pge",		2,
	CMcoherence,	"coherence",	2,
	CMi8253set,	"i8253set",	2,
};

static long
archctlwrite(Chan*, void *a, long n, vlong)
{
	Cmdbuf *cb;
	Cmdtab *ct;

	cb = parsecmd(a, n);
	if(waserror()){
		free(cb);
		nexterror();
	}
	ct = lookupcmd(cb, archctlmsg, nelem(archctlmsg));
	switch(ct->index){
	case CMpge:
		if(!m->havepge)
			error("processor does not support pge");
		if(strcmp(cb->f[1], "on") == 0)
			putcr4(getcr4() | 0x80);
		else if(strcmp(cb->f[1], "off") == 0)
			putcr4(getcr4() & ~0x80);
		else
			cmderror(cb, "invalid pge ctl");
		break;
	case CMcoherence:
		if(strcmp(cb->f[1], "mb386") == 0)
			coherence = mb386;
		else if(strcmp(cb->f[1], "mb586") == 0){
			if(X86FAMILY(m->cpuidax) < 5)
				error("invalid coherence ctl on this cpu family");
			coherence = mb586;
		}
		else if(strcmp(cb->f[1], "nop") == 0){
			/* only safe on vmware */
			if(conf.nmach > 1)
				error("cannot disable coherence on a multiprocessor");
			coherence = nop;
		}else
			cmderror(cb, "invalid coherence ctl");
		break;
	case CMi8253set:
		if(strcmp(cb->f[1], "on") == 0)
			doi8253set = 1;
		else if(strcmp(cb->f[1], "off") == 0){
			doi8253set = 0;
			(*arch->timerset)(0);
		}else
			cmderror(cb, "invalid i2853set ctl");
		break;
	}
	free(cb);
	poperror();
	return n;
}

void
archinit(void)
{
	arch = &archlguest;
	/*
	 *  Decide whether to use copy-on-reference (386 and mp).
	 *  We get another chance to set it in mpinit() for a
	 *  multiprocessor.
	 */
	if(X86FAMILY(m->cpuidax) == 3)
		conf.copymode = 1;

	if(X86FAMILY(m->cpuidax) >= 4)
		cmpswap = cmpswap486;

	if(X86FAMILY(m->cpuidax) >= 5)
		coherence = mb586;

	addarchfile("cputype", 0444, cputyperead, nil);
	addarchfile("archctl", 0664, archctlread, archctlwrite);
	addarchfile("lguest", 0444, lgread, nil);
}

/*
 *  call either the pcmcia or pccard device setup
 */
int
pcmspecial(char *idstr, ISAConf *isa)
{
	return (_pcmspecial != nil)? _pcmspecial(idstr, isa): -1;
}

/*
 *  call either the pcmcia or pccard device teardown
 */
void
pcmspecialclose(int a)
{
	if (_pcmspecialclose != nil)
		_pcmspecialclose(a);
}

/*
 *  return value and speed of timer set in arch->clockenable
 */
uvlong
fastticks(uvlong *hz)
{
	return (*arch->fastclock)(hz);
}

/*
 *  set next timer interrupt
 */
void
timerset(vlong x)
{
	if(doi8253set)
		(*arch->timerset)(x);
}

ulong
µs(void)
{
	return fastticks2us((*arch->fastclock)(nil));
}
