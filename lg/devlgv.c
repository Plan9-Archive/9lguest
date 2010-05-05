#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include "io.h"
#include "lguest.h"
#include "virtio_ring.h"

#ifdef ABIGCOMMENT
/* so, how does this ring stuff line up anyways? At start time, there is an lguest device page, mapped "just after" memory, 
 * i.e. at the page right after top of mem. This page contains an array of device descriptors. 
 * They  like this:
       /* The device type: console, network, disk etc.  Type 0 terminates. */
        u8 type;
        /* The number of virtqueues (first in config array) */
        u8 num_vq;
        /* The number of bytes of feature bits.  Multiply by 2: one for host
         * features and one for Guest acknowledgements. */
        u8 feature_len;
        /* The number of bytes of the config array after virtqueues. */
        u8 config_len;
        /* A status byte, written by the Guest. */
        u8 status;
        u8 config[];

 * That config thing is a variable-sized area, with fairly arbitrary layout. The layout by convention is devices, then feature_len, 
 * then config_len. The devices area is num_vq * sizeof the ring descriptor, which is a 2-byte # pages, a 2-byte irq #, and a 
 * 4-byte page frame number. You have to map in that page frame number for the # pages. This gets you access to the 
 * actual virtio ring. Recall too that there are num_vq of these. 
 *
 * Now we are down to it. What is this bunch of pages we mapped in? 
 * I *think* it is a vring. Time to try to find out. Yes it is, from lguest launcher:
 	/* Initialize the vring. */
	vring_init(&vq->vring, num_descs, p, getpagesize());
 * So the ring is basically set up for us. To do IO, we just use the add_buf stuff, and get_buf, and it ought to work. 
 * So we'll add those functions here and if we see a console we'll try a hello from virtq type thing. 
 * see console functions below. This is kind of working.
 */
#endif

#define START_USE(vq) \
	do { if ((vq)->in_use) panic("in_use = %i\n", (vq)->in_use); (vq)->in_use = __LINE__; mb(); } while(0)
#define END_USE(vq) \
	do { BUG_ON(!(vq)->in_use); (vq)->in_use = 0; mb(); } while(0)

/*
 *  lguest virtio driver
 */

enum{
	Qdir = 0,
	Qlgv,
	Qmax = 32,
	QTNET = 1, 
	QTBLOCK,
	QTCONS,
	FirstDev = 2,
};

struct lguest_device {
	Lock;
	int open;
	int devno;
	struct lguest_device_desc *lgd;
	unsigned char *features;
	unsigned char *config;
	int featurelen, configlen;
	/* we only use 2 max now, so I worrieth not */
	struct vqring ring[32];
};

struct devinfo {
	char *format;
	int unit;
};
	
struct devinfo devs[] = {
	[QTNET] = {"net%d", 0},
	[QTBLOCK] = {"block%d",0},
	/* there really should be only one */
	[QTCONS] = {"cons", 0},
};

Dirtab lgvdir[Qmax]={
	".",	{Qdir, 0, QTDIR},	0,	0555,
	"lgv",		{Qlgv, 0},	0,	0664,
};



struct lguest_device lgv[Qmax];

static int ndev = FirstDev;
int console = -1;
int watchdevice = -1;

/* just like iprint but it will ALWAYS go straight to a notify -- not whatever
 * we happen to think the console is.
 */
int
inotify(char *fmt, ...)
{
	int n, s;
	va_list arg;
	static char buf[PRINTSIZE];

	s = splhi();
	va_start(arg, fmt);
	n = vseprint(buf, buf+sizeof(buf), fmt, arg) - buf;
	va_end(arg);
	hcall(LHCALL_NOTIFY, paddr(buf), 0, 0, 0);
	splx(s);

	return n;
}

/* from the linux stuff. addbuf and getbuf */
/* clean this up as it it GPL! */
int vring_add_buf(struct vqring *vqring, 
			 void *v[], int *len, 
			 unsigned int out,
			 unsigned int in,
			 void *data)
{
	unsigned int i, avail, head, index = 0;
	int prev = 0;
//	inotify("vring_add_buf: vqring %p, v %p, len %d, out %d, in %d, data %p\n", vqring, v, *len, out, in, data);
	if (data == nil)
		panic("vring_add_buf: data is nil");
	if (out + in > vqring->vring.num)
		panic("vring_add_buf: out %d + in %d is > vq->vring.num %d", out, in, vqring->vring.num);
	if (out + in == 0)
		panic("vring_add_buf: out %d + in %d == 0", out, in);

	if (vqring->num_free < out + in) {
	//	inotify("Can't add buf len %ud - avail = %ud\n", out + in, vqring->num_free);
		/* We notify *even if* VRING_USED_F_NO_NOTIFY is set here. */
		//vq->notify(&vq->vq);
		return 0;
	}

	/* We're about to use some buffers from the free list. */
	vqring->num_free -= out + in;

	head = vqring->free_head;
	for (i = vqring->free_head; out; i = vqring->vring.desc[i].next, out--) {
		vqring->vring.desc[i].flags = VRING_DESC_F_NEXT;
		vqring->vring.desc[i].addr = paddr(v[index]);
		vqring->vring.desc[i].len = len[index];
		prev = i;
		index++;
	} 
//inotify("after loop i is %d\n", i);

	for (; in; i = vqring->vring.desc[i].next, in--) {
		vqring->vring.desc[i].flags = VRING_DESC_F_NEXT|VRING_DESC_F_WRITE;
		vqring->vring.desc[i].addr = paddr(v[index]);
		vqring->vring.desc[i].len = len[index];
		prev = i;
		index++;
	}
 
	/* Last one doesn't continue. */
	vqring->vring.desc[prev].flags &= ~VRING_DESC_F_NEXT;

	/* Update free pointer */
	vqring->free_head = i;

	/* Set token. */
	vqring->data[head] = data;

	/* Put entry in available array (but don't update avail->idx until they
	 * do sync).  FIXME: avoid modulus here? */
	avail = (vqring->vring.avail->idx + vqring->num_added++) % vqring->vring.num;
	vqring->vring.avail->ring[avail] = head;
//	inotify("Added buffer head %ud to %p\n", head, vqring);
	return 0;
}

void vring_kick(struct vqring *vqring)
{
	/* Descriptors and available array need to be set before we expose the
	 * new available array entries. */
	coherence();

	vqring->vring.avail->idx += vqring->num_added;
	vqring->num_added = 0;

	/* Need to update avail index before checking if we should notify */
	coherence();
//inotify("vqring: num %d irq %d pages %p ppages %p\n", vqring->num, vqring->irq, vqring->pages, (void *)vqring->ppages);

	if (!(vqring->vring.used->flags & VRING_USED_F_NO_NOTIFY))
		/* Prod other side to tell it about changes. */
		hcall(LHCALL_NOTIFY, vqring->ppages, 0, 0, 0);

}

void detach_buf(struct vqring *vqring, unsigned int head)
{

	unsigned int i;

	/* Clear data ptr. */
	vqring->data[head] = nil;

	/* Put back on free list: find end */
	i = head;
	while (vqring->vring.desc[i].flags & VRING_DESC_F_NEXT) {
		i = vqring->vring.desc[i].next;
		vqring->num_free++;
	}

	vqring->vring.desc[i].next = vqring->free_head;
	vqring->free_head = head;
	/* Plus final descriptor */
	vqring->num_free++;
}

int more_used(struct vqring *vqring)
{
	return vqring->last_used_idx != vqring->vring.used->idx;
}
void *vring_get_buf(struct vqring *vqring, unsigned int *len)
{
	void *ret;
	unsigned int i;

	if (!more_used(vqring)) {
//	//	inotify("No more buffers in queue\n");
		return nil;
	}

	i = vqring->vring.used->ring[vqring->last_used_idx%vqring->vring.num].id;
	*len = vqring->vring.used->ring[vqring->last_used_idx%vqring->vring.num].len;

	if (i >= vqring->vring.num) {
		panic("id %d out of range\n", i);
		return nil;
	}
	if (!vqring->data[i]) {
		panic("id %d is not a head!\n", i);
		return nil;
	}

	/* detach_buf clears data, so grab it now. */
	ret = vqring->data[i];
	detach_buf(vqring, i);
	vqring->last_used_idx++;
	return ret;
}

/* The standard layout for the ring is a continuous chunk of memory which looks
 * like this.  We assume num is a power of 2.
 *
 * struct vring
 * {
 *	// The actual descriptors (16 bytes each)
 *	struct vring_desc desc[num];
 *
 *	// A ring of available descriptor heads with free-running index.
 *	__u16 avail_flags;
 *	__u16 avail_idx;
 *	__u16 available[num];
 *
 *	// Padding to the next page boundary.
 *	char pad[];
 *
 *	// A ring of used descriptor heads with free-running index.
 *	__u16 used_flags;
 *	__u16 used_idx;
 *	struct vring_used_elem used[num];
 * };
 */
static void vring_init(struct vring *vr, unsigned int num, void *p,
			      unsigned long pagesize)
{
	vr->num = num;
	vr->desc = p;
	vr->avail = (void *)((unsigned char *)p + num*sizeof(struct vring_desc));
	vr->used = (void *)(((unsigned long)&vr->avail->ring[num] + pagesize-1)
			    & ~(pagesize - 1));
}

static unsigned vring_size(unsigned int num, unsigned long pagesize)
{
	return ((sizeof(struct vring_desc) * num + sizeof(u16) * (2 + num)
		 + pagesize - 1) & ~(pagesize - 1))
		+ sizeof(u16) * 2 + sizeof(struct vring_used_elem) * num;
}


/* end Linux */
/* config and feature management */

struct lguest_device *devtolg(int devno)
{
	struct lguest_device *dev;
	/* valid device? */
	if ((devno < FirstDev) || (FirstDev > ndev))
		panic("devtolg: bad dev %d", devno);
	
	dev = &lgv[devno];
	return dev;

}

struct vqring *lgtoring(struct lguest_device *dev, int ring)
{
	struct lguest_device_desc *lg = dev->lgd;
	struct vqring *vring;

	if (ring > lg->num_vq)
		panic("Bad ring %d dev %p\n", ring, dev);
	vring = &dev->ring[ring];
	return vring;
}

struct vqring *devtoring(int dev, int ring)
{
	struct lguest_device *pdev;
	struct vqring *vring;
	pdev = devtolg(dev);
	vring = lgtoring(pdev, ring);
	return vring;
}

/* these ought to be replaced, maybe, with some common function. Ah well. */
int
lgvirq(int devno, int ring)
{
	struct vqring *vring = devtoring(devno, ring);
	return vring->irq;
}

int
lgvnumfree(int devno, int ring)
{
	struct vqring *vring = devtoring(devno, ring);
	return vring->num_free;
}

int
lgvfeaturelen(int devno)
{
	struct lguest_device *dev;
	dev = devtolg(devno);
	return dev->featurelen;
}

int
lgvconfiglen(int devno)
{
	struct lguest_device *dev;
	dev = devtolg(devno);
	return dev->configlen;
}
/* this really needs to be a file io thing at some point. Perhaps each dev should be a 
 * directory with a feaures file. We're still figuring this out 
 */
int
lgvfeature(int devno, unsigned int feature)
{
	struct lguest_device *dev;
	u32 word, bit;
	int ret;
	dev = devtolg(devno);
	/* recall that it is a bit number! */
	if (feature/32 > dev->featurelen)
		panic("Feature out of bounds: %x on %d\n", feature, devno);

	word = feature/32;
	bit = feature % 32;
	ret = dev->config[word] & bit;
	return ret;
}

void
lgvconfig(int devno, unsigned char *config, int off ,int len)
{
	struct lguest_device *dev;
	dev = devtolg(devno);
	/* recall that it is a bit number! */
	if ((off + len) > dev->configlen)
		panic("Config out of bounds: %d/%d on %d\n", off, len, devno);

	memmove(config, &dev->config[off], len);
}

void
lgvdumpconfig(int devno)
{
	void dumphex(char *name, unsigned char *s, int len);
	struct lguest_device *dev;
	dev = devtolg(devno);
	dumphex("Config", dev->config, dev->configlen);
}

int
lgvconsout(char *a, int len)
{
	unsigned int getbuflen;
	char *x;

//	inotify("We would add buf %p to vqring %p\n", a, &lgv[console].ring[1]);
//	inotify("console is %d; ring is 1\n", console);
//	inotify(a);
	if (console < 0)
		return 0;

	x = malloc(len);
//	inotify("x %p\n", x);
	memmove(x, a, len);
	vring_add_buf(&lgv[console].ring[1], &x, &len, 1, 0, x);

	vring_kick(&lgv[console].ring[1]);
//	inotify("back from kick\n");
	while (x = vring_get_buf(&lgv[console].ring[1], &getbuflen)){
//		inotify("free x %p\n", x);
		free(x);
	}
	return getbuflen;
}

int
lgvaddrecvbuf(int dev, int ring, void *v[], int len[], int nbuf, void *tag)
{
	vring_add_buf(&lgv[dev].ring[ring], v, len, 0, nbuf, tag);
	vring_kick(&lgv[dev].ring[ring]);
	return 0;
}

int
lgvaddxmitbuf(int dev, int ring, void *v[], int len[], int nbuf, void *tag)
{
//	int i;
////	inotify("smit dev %d ring %d nbuf %d: ", dev, ring, nbuf);
//	for(i = 0; i < nbuf; i++) inotify("%p/%d", v[i], len[i]);
////	inotify("\n");
	vring_add_buf(&lgv[dev].ring[ring], v, len, nbuf, 0, tag);
	vring_kick(&lgv[dev].ring[ring]);
	return 0;
}

int
lgvaddbuf(int dev, int ring, void *v[], int len[], int out, int in, void *tag)
{
//	int i;
////	inotify("smit dev %d ring %d out %d in  %d: ", dev, ring, out, in);
//	for(i = 0; i < out + in; i++) inotify("%p/%d,", v[i], len[i]);
////	inotify("\n");
	vring_add_buf(&lgv[dev].ring[ring], v, len, out, in, tag);
	vring_kick(&lgv[dev].ring[ring]);
	return 0;
}


int
lgvaddconsbuf(unsigned char *v, int len)
{
	return lgvaddrecvbuf(console, 0, &v, &len, 1, v);
}

void *
lgvgetbuf(int dev, int ring, int *plen)
{
	void *vring_get_buf(struct vqring *vqring, unsigned int *len);
	void *ret;
	*plen = 0;
	ret = vring_get_buf(&lgv[dev].ring[ring], (unsigned int *)plen);
	return ret;

}
int
lgvgetconsbuf(unsigned char *, int)
{
	int plen = 0;
	lgvgetbuf(console, 0, &plen);
	return plen;
}


void
lgvconsin(void *a, int len, char *name, void *f) //(void *)f(void))
{
//	inotify("lgv consin a %p len %d f %p\n", a, len, f);
	if (f) {
		intrenable(lgv[console].ring[0].irq + 32, f, a, BUSUNKNOWN, name);
	}
	
	vring_add_buf(&lgv[console].ring[0], &a, &len, 0, 1, a);

////	inotify("ret from add buf is %d\n", ret);
	vring_kick(&lgv[console].ring[0]);
}

int
findlgv(char *name)
{
	int ret = -1;
	int i;
	for(i = 0; i < ndev; i++) {
		if (! strcmp(name, lgvdir[i].name)){
		//	inotify("findlgv %s, @%d\n", name, i);
			ret = i;	
			break;
		}			
	}
	return ret;
}

static Chan*
lgvattach(char* spec)
{
	return devattach('Z', spec);
}

static Walkqid*	 
lgvwalk(Chan* c, Chan *nc, char** name, int nname)
{
	return devwalk(c, nc, name, nname, lgvdir, ndev, devgen);
}

static int	 
lgvstat(Chan* c, uchar* dp, int n)
{
	return devstat(c, dp, n, lgvdir, ndev, devgen);
}

static void
lgvintr(Ureg *, void *)
{
//	hcall(LHCALL_CRASH, paddr("data is not aligned"), 0, 0);
//	panic("lgvintr");
	hcall(LHCALL_NOTIFY, paddr("lgvintr\n"), 0, 0, 0);
}


static Chan*
lgvopen(Chan* c, int omode)
{
	Chan *ret;
	ret = devopen(c, omode, lgvdir, ndev, devgen);

	return ret;
}

static void	 
lgvclose(Chan*)
{
}

static long	 
lgvread(Chan* c, void* buf, long n, vlong off)
{
	ulong offset = off;
	if(c->qid.type & QTDIR)
		return devdirread(c, buf, n, lgvdir, ndev, devgen);

	switch((ulong)c->qid.path){
	case Qlgv:{
		int l;
		int i, j;
		char *p;

		if((p = malloc(READSTR)) == nil)
			error(Enomem);
		
		for(l = 0, i = 2; i < ndev; i++){
			l += snprint(p+l, READSTR-l, "%s:  numq %d status %d", 
				lgvdir[i].name, lgv[i].lgd->num_vq, lgv[i].lgd->status); 
			for(j = 0; j < lgv[i].lgd->num_vq; j++){
				l += snprint(p+l, READSTR-l, "[%d irq %d]", j, lgvirq(i, j));
			}
			l += snprint(p+l, READSTR-l, "\n");
		}	

		n = readstr(offset, buf, n, p);
		free(p);
		return n;
		}
	}
	error(Ebadarg);
	return 0;
}

/* Not ready yet to make the write go where it should. 
  */
static long	 
lgvwrite(Chan* , void*, long , vlong)
{
	error(Eperm);
	return -1;
}

void
configdesc(struct vqring *ring)
{
	int k;
	for (k = 0; k < ring->num-1; k++) {
		ring->vring.desc[k].next = k+1;
	}

}

void
configring(struct vqring *ring, unsigned char *v, char *)
{
	unsigned long pfn;
	memmove(&ring->num, &v[0], 2);
	memmove(&ring->irq, &v[2], 2);
	memmove(&pfn, &v[4], 4);

//	inotify("configring: v %p num %d irq %#ux pfn %#ulx\n", v, ring->num, ring->irq, pfn);
	ring->ppages = pfn << PGSHIFT;
	/* 16 bytes per entry. So it is ring->num * BY2PG / 16 */
	ring->pages = vmap(pfn<<PGSHIFT, ring->num*BY2PG/16);

	vring_init(&ring->vring, ring->num, ring->pages,BY2PG);

//	inotify("ring %p: num %d irq %d pages %p\n", ring, ring->num, ring->irq, ring->pages);

	ring->q = qopen(4096, 0, 0, 0);

	ring->num_free =   ring->num;
	ring->free_head = 0;
	configdesc(ring);

}

struct lguest_device_desc *
configldev(struct lguest_device_desc *l, struct lguest_device *lg, char *name)
{
	unsigned char *v;
	int j;
	unsigned char *cp;
//	inotify("Dev %d, %d, %d, %d, %d, ...\n", l->type, l->num_vq, l->feature_len, l->config_len, l->status);
	for(j = 0, v = l->config; j < l->num_vq; j++) {
		configring(&lg->ring[j], &v[j*8], name);
	}
	l->status = 1;

	/* set up feature and configure pointers */
	cp = (unsigned char *)l;
	cp += 5 +  l->num_vq*8;
	lg->features = cp;
	lg->featurelen = l->feature_len;
	cp += l->feature_len*2;
	lg->config = cp;
	lg->configlen = l->config_len;
	cp += l->config_len;
	l = (struct lguest_device_desc *)cp;

	return l;

}

void
lgdir(struct lguest_device_desc *l, int path, Dirtab *d)
{
	/* set up the name */
	sprint(d->name, devs[l->type].format, devs[l->type].unit);
	d->qid.path = path;
	d->qid.vers = 0;
	d->qid.type = l->type;
	d->length = 0;
	d->perm = 0644;
}

void
lgvreset(void)
{
	void dumphex(char *name, unsigned char *s, int len);
	extern struct lguest_device_desc *lgd;
	struct lguest_device_desc *l = lgd, *nextl;
	int i, setconsole = -1;

//	inotify("lgv reset\n");
	dumphex("lgd", (uchar *)lgd, 256);
	/* let's dump some devices */
	{ extern struct lguest_device_desc *lgd;struct lguest_device_desc *l = lgd; unsigned char *cp;int i;
		for(i = 0; i < 10 && l->type; i++){
	//	inotify("Dev %d, %d, %d, %d, %d, ...\n", l->type, l->num_vq, l->feature_len, l->config_len, l->status);
		cp = (unsigned char *)l;
		cp += 5 + l->feature_len*2 + l->config_len + l->num_vq*8;
		l = (struct lguest_device_desc *)cp;
		}
	}

	for(i = 2; i < Qmax && l->type; i++, l = nextl, ndev++){
		lgv[i].lgd = l;
		nextl = configldev(l, &lgv[i], lgvdir[i].name );
		lgdir(l, i, &lgvdir[i]);
		if (l->type == QTCONS) {
			char *a = "============================== hi there ========================\n";
		//	inotify("Found a console! try output!\n");
			setconsole = i;
			lgvconsout(a, strlen(a));
		}

	}
	
//	inotify("lgv reset done\n");
	console = setconsole;
}

Dev lgvdevtab = {
	'Z',
	"lgv",

	lgvreset,
	devinit,
	devshutdown,
	lgvattach,
	lgvwalk,
	lgvstat,
	lgvopen,
	devcreate,
	lgvclose,
	lgvread,
	devbread,
	lgvwrite,
	devbwrite,
	devremove,
	devwstat,
};
