/*
 * lguest block storage, derived from:
 * Xen block storage device frontend
 *
 * The present implementation follows the principle of
 * "what's the simplest thing that could possibly work?".
 * We can think about performance later.
 * We can think about dynamically attaching and removing devices later.
 */

#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "ureg.h"
#include "../port/error.h"

#include "../port/sd.h"
#include "lguest.h"
extern SDifc sdlgifc;
#define LOG(a) 

enum {
	Ndevs		= 32,
	idstart 		= '0',
	BufPageSize	= 16 * BY2PG,
	/* if we make MaxWriteSize large, then things go to hell. Not sure why. 
	  * but we very quickly corrupt the disk. I *think* that reads and writes
	  * are interfering, but how knows?
	  */
	MaxWriteSize		= 1 * BY2PG,
	MaxReadSize		= 16 * BY2PG,
	/* features */
	VIRTIO_BLK_F_BARRIER = 1,	/* Does host support barriers? */
	VIRTIO_BLK_F_SIZE_MAX = 2, 	/* Indicates maximum segment size */
	VIRTIO_BLK_F_SEG_MAX = 4,	/* Indicates maximum # of segments */
	/* configuration information */
	SDSize = 0,				/* disk size */
	SDMSS = 8,				/* max segment size if VIRTIO_BLK_F_SIZE_MAX*/
	SDMaxSet = 12,			/* max # segments if VIRTIO_BLK_F_SEG_MAX */
	/* which Queue is Which */
	SDIN = 0,
	SDOUT,
	
};

/* This is the first element of the read scatter-gather list. */
struct virtio_blk_outhdr
{
        /* SDIN or SDOUT* */
        u32 type;
        /* io priority. */
        u32 ioprio;
        /* Sector (ie. 512 byte offset) */
        uvlong sector;
};

/* This is the first element of the write scatter-gather list */
struct virtio_blk_inhdr
{
        unsigned char status;
};

typedef struct Ctlr Ctlr;

struct Ctlr {
	int	online;
	/* we can't just copy it here, as there are state bits
	  * which host and guest OS share 
	  */
	int devno;
	/* at some point make this a linked list of qio. For now, synchronous */
	ulong	secsize;
	uvlong	sectors;
	Lock	dmalock;
	QLock	iolock;
	int	iodone;
	Rendez	wiodone;
};

/* 
  */
static SDev*
lgpnp(void)
{
	void lgvdumpconfig(int devno);
	int findlgv(char *name);
	int lgvconfig(int devno, unsigned char *config, int off ,int len);
	SDev *sdev[Ndevs];
	unsigned char idno = idstart;
	int i, lgno, devno;
	int numsd = 0;
	Ctlr *ctlr;
	char name[32];
	/* just loop through finding #Z/block%d until no more */
	
//iprint("block dev lgpnp Ndevs %d\n", Ndevs);
	sprint(name, "block0");
	for (i = 0, lgno = 0, devno = findlgv(name); devno > -1; devno = findlgv(name)) {
		numsd++;
//iprint("Found block at %d\n", devno);
		sdev[i] = mallocz(sizeof(SDev), 1);
		sdev[i]->idno = idno;
		sdev[i]->nunit = 1;
		sdev[i]->ifc = &sdlgifc;
		sdev[i]->ctlr = (Ctlr*)mallocz(sizeof(Ctlr), 1);
		ctlr = sdev[i]->ctlr;
		ctlr->devno = devno;
//iprint("ctlr->lbp is %p\n", ctlr->lbp);
		lgvdumpconfig(devno);
		/* get the size */
		lgvconfig(devno, (unsigned char *)&ctlr->sectors, 0, sizeof(ctlr->sectors));
//iprint("sectors is %lld\n", ctlr->sectors);
		ctlr->secsize = 512;
		if (i > 0)
			sdev[i]->next = sdev[i-1];
		idno++;
		lgno++;
		sprint(name, "block%d", lgno);
	}

	if (numsd > 0){
//iprint("block lgpnp returns %p\n", sdev[0]);
		return sdev[0];
	}
	return nil;
}

static int
lgverify(SDunit *unit)
{

	if (unit->subno > unit->dev->nunit)
		return 0;

	unit->inquiry[0] = 0;		// XXX how do we know if it's a CD?
	unit->inquiry[2] = 2;
	unit->inquiry[3] = 2;
	unit->inquiry[4] = sizeof(unit->inquiry)-4;
	strcpy((char*)&unit->inquiry[8], "Lguest block device");
	return 1;
}

static int
wiodone(void *a)
{
	void *lgvgetbuf(int dev, int ring, int *plen);
	Ctlr *ctlr = (Ctlr *)a;
	int len;
//iprint("wiodone ctlr %p dev %d\n", ctlr, ctlr->devno);
	lgvgetbuf(ctlr->devno, 0, &len);
//iprint("WI%d", len);
	return ((Ctlr*)a)->iodone != 0;
}

static void
sdlgintr(Ureg *, void *a)
{
	Ctlr *ctlr = a;
//iprint("SI");
	ilock(&ctlr->dmalock);	// XXX conservative
	ctlr->iodone = 1;
	iunlock(&ctlr->dmalock);
	if (ctlr->iodone)
		wakeup(&ctlr->wiodone);
}

static Ctlr *kickctlr;

static void
kickme(void)
{
	Ctlr *ctlr = kickctlr;

	if (ctlr) {
		sdlgintr(0, ctlr);
	}
}

static int
lgonline(SDunit *unit)
{
	int lgvirq(int devno, int ring);
	Ctlr *ctlr;
	int irq;
//iprint("lgonline return 1\n");
	ctlr = unit->dev->ctlr;
	unit->sectors = ctlr->sectors;
	unit->secsize = ctlr->secsize;
	if (ctlr->online == 0) {
		irq = lgvirq(ctlr->devno, 0);
		print("devno %d ring %d irq %d\n", ctlr->devno, 0, irq+32);
		intrenable(irq+32, sdlgintr, ctlr, BUSUNKNOWN, "lgsd");
		addclock0link(kickme, 10000);
		ctlr->online = 1;
	}

	return 1;
}

static int
lgrio(SDreq*)
{
	return -1;
}

static long
lgbio(SDunit* unit, int, int write, void* data, long nb, long bno)
{
	int lgvaddbuf(int dev, int ring, void *v[], int len[], int out, int in, void *tag);
	void *v[3];
	int len[3];
	Ctlr *ctlr;
	long total = nb * unit->secsize, amt = 0;
	struct virtio_blk_outhdr hdr;
	struct virtio_blk_inhdr status;
	ctlr = unit->dev->ctlr;
	LOG(iprint("lgbio %c %lux %ld %ld total %d\n", write? 'w' : 'r', (ulong)data, nb, bno, total);)

	qlock(&ctlr->iolock);
	while(amt < total) {
		ctlr->iodone = 0;
		v[0] = &hdr;
		len[0] = sizeof(hdr);
		v[1] = data;
		len[1] = total;
		v[2] = &status;
		/* how amusing. gcc doesn't give the same answer as plan 9 for this */
		len[2] = sizeof(status);
		len[2] = 1;

		if (write){
			hdr.type = SDOUT;
			hdr.sector = bno;
//iprint("addxmitbuf\n");
			lgvaddbuf(ctlr->devno, 0, v, len, 2, 1, data);
//iprint("added xmitbuf\n");
		} else {
			hdr.type = SDIN;
			hdr.sector = bno;
//iprint("addrecvfbuf\n");
			lgvaddbuf(ctlr->devno, 0, v, len, 1, 2, data);
//iprint("added recdvbuf\n");
		}
//iprint("sleep %p\n", ctlr);
		sleep(&ctlr->wiodone, wiodone, ctlr);
		if (ctlr->iodone < 0)
			break;
//dumphex("L", ctlr->bufpage, len);
		amt += total;
	}
	qunlock(&ctlr->iolock);
	return total;
}

static void
lgclear(SDev *)
{
}

SDifc sdlgifc = {
	"lg",				/* name */

	lgpnp,				/* pnp */
	0,			/* legacy */
	0,			/* enable */
	0,			/* disable */

	lgverify,			/* verify */
	lgonline,			/* online */
	lgrio,				/* rio */
	0,			/* rctl */
	0,			/* wctl */

	lgbio,				/* bio */
	0,			/* probe */
	lgclear,			/* clear */
	0,			/* stat */
};
