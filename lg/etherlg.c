/*
  * lguest ethernet, from:
 * Realtek 8139 (but not the 8129).
 */
#include "u.h"
#include "../port/lib.h"
#include "mem.h"
#include "dat.h"
#include "fns.h"
#include "io.h"
#include "../port/error.h"
#include "../port/netif.h"

#include "etherif.h"
#include "lguest.h"

enum {
	NRDMA = 1, /* number RDMAs in flight */
	/* The feature bitmap for virtio net */
	VIRTIO_NET_F_CSUM = 1,	/* Can handle pkts w/ partial csum */
	VIRTIO_NET_F_MAC= (1<<5),	/* Host has given MAC address. */
	VIRTIO_NET_F_GSO = (1<<6),	/* Can handle pkts w/ any GSO type */
	/* features and where they live */
	config_mac = 0,
	/* packet flags */
	VIRTIO_NET_HDR_F_NEEDS_CSUM = 1, // Use csum_start, csum_offset
	/* GSO flags */
	VIRTIO_NET_HDR_GSO_NONE = 0,	// Not a GSO frame
	VIRTIO_NET_HDR_GSO_TCPV4 = 1,	// GSO frame, IPv4 TCP (TSO)
	VIRTIO_NET_HDR_GSO_UDP = 3,	// GSO frame, IPv4 UDP (UFO)
	VIRTIO_NET_HDR_GSO_TCPV6 = 4,	// GSO frame, IPv6 TCP
	VIRTIO_NET_HDR_GSO_ECN = 0x80,	// TCP has ECN set
	/* header structure */
	hsize = 80,
	/* byte offsets for xdr */
	hflags = 0,
	hgso = 1,
	hdr_len = 2,          /* Ethernet + IP + tcp/udp hdrs */
	gso_size = 4,         /* Bytes to append to gso_hdr_len per frame */
	hcsum_start = 6,       /* Position to start checksumming from */
	hcsum_off = 8,      /* Offset after that to place checksum */
	
};

/* on xmit, we malloc this and use it for two things: 
 * the hdr data for lguest
 * the "tag" that we dq from the virtio ring and know how to deallocate. 
 */
struct xmit {
	Block *block;
	unsigned char hdr[10];
};


typedef struct Ctlr Ctlr;
typedef struct Ctlr {
	int	instance;
	Ctlr*	next;
	int	active;
	int	id;
	int devno;

	QLock	alock;			/* attach */
	Lock	ilock;			/* init */
	void*	alloc;			/* base of per-Ctlr allocated data */

	Block *bp[NRDMA];

	Lock	tlock;			/* transmit */
} Ctlr;

int nether = 0;
static Ctlr* ctlrhead;
static Ctlr* ctlrtail;
static void
lginterrupt(Ureg*, void* arg);
int inotify(char *fmt, ...);
extern struct lguest_device_desc *lgd;

static void
lgpromiscuous(void*, int )
{
}

static void
lgmulticast(void*, uchar*, int)
{
}

static long
lgifstat(Ether*, void* a, long n, ulong offset)
{
	int i;
	int l = 0;
	char *p;
	struct Ctlr *ctlr = ctlrhead;

	if((p = malloc(READSTR)) == nil)
		error(Enomem);
	
	for(i = 0; i <  nether; i++, ctlr= ctlr->next){
		l += snprint(p+l, READSTR-l, "ether%d: devno %d", 
			i, ctlr->devno);
/*		for(j = 0; j < lgv[i].lgd->num_vq; j++){
			l += snprint(p+l, READSTR-l, "[%d irq %d]", j, lgvirq(i, j));
		}
 */
		l += snprint(p+l, READSTR-l, "\n");
	}	

	n = readstr(offset, a, n, p);
	free(p);
	return n;

}

static int
lgreset(Ether*e)
{
	iprint("LG RESET: e->ctlrno %d\n", e->ctlrno);
	/* FIXME: be sure to probe the ethers, right now we just do 1 */
	if (e->ctlrno)
		return -1;
	return 0;
}

static void
lghalt(Ctlr*)
{
}


void
setmac(u8 *)
{
	iprint("SETMAC: NO\n");

}

static void
lginit(Ether* edev)
{
	Ctlr *ctlr = edev->ctlr;
	ilock(&ctlr->ilock);
	iprint("LGINIT\n");
	lghalt(ctlr);

	/*
	 * MAC Address.
	 */
	/* todo: ask jmk why we're doing this in two places */
	setmac(edev->ea);
	/*
	 * Interrupts.
	 */
//	intrenable(NETINTR, lginterrupt, edev, BUSUNKNOWN, "lgether");
	/*
	 * Enable receiver/transmitter.
	 * Nothing to do. 
	 */

	iunlock(&ctlr->ilock);
}

void
fillslot(Ether* edev)
{
	int lgvaddbuf(int dev, int ring, void *v[], int len[], int out, int in, void *tag);
	int lgvnumfree(int devno, int ring);
	Ctlr *ctlr;
	Block *bp;
	void *v[2];
	int len[2];
	/* fix this later. */
	unsigned char *nh;

	ctlr = edev->ctlr;

	while(lgvnumfree(ctlr->devno, 0)){

		bp = iallocb(ETHERMAXTU + 16);

		/* the header goes in the first 16 bytes. 16 to hopefully align the 
		 * receive area in a reasonable way
		 */
	
		if (! bp)
			panic("alloc bp in fillslot: no memory\n");
		bp->flag |= Bipck | Budpck | Btcpck | Bpktck;
	
		/* now set up the dma for it */
		nh = bp->wp;
		bp->wp += 16;
		bp->rp = bp->wp;
		v[0] = nh;
		len[0] = 10;
		v[1] = bp->wp;
		len[1] = ETHERMAXTU;
		inotify("fillslot: add %p\n", bp);
		lgvaddbuf(ctlr->devno, 0, v, len, 0, 2, bp);
	}
}


static void
lgattach(Ether* edev)
{
	int lgvirq(int devno, int ring);
	Ctlr *ctlr;
	int irq;
iprint("LGATTACH edev %p ctlr %p\n", edev, edev->ctlr);
	ctlr = edev->ctlr;
	qlock(&ctlr->alock);
	if(ctlr->alloc == nil){
		ctlr->alloc = xspanalloc(BY2PG, BY2PG, 0);
		lginit(edev);
	}
	qunlock(&ctlr->alock);
	/* enable interrupts */
	/* receive is ring 0 */
	irq = lgvirq(ctlr->devno, 0);
	print("ETHERIN: devno %d ring %d irq %d\n", ctlr->devno, 0, irq+32);
	intrenable(irq+32, lginterrupt, edev, BUSUNKNOWN, "rxether");
	irq = lgvirq(ctlr->devno, 1);
	print("ETHEROUT: devno %d ring %d irq %d\n", ctlr->devno, 0, irq+32);
	intrenable(irq+32, lginterrupt, edev, BUSUNKNOWN, "txether");
	/* now queue up some receive bps */
	fillslot(edev);
}

/* we're talking to Linux. So every interface is  ever so slightly different. It's amazing */
static void
lgtxstart(Ether* edev)
{
	int lgvaddbuf(int dev, int ring, void *v[], int len[], int out, int in, void *tag);
	/* gcc and kenc disagree about structs. Screw it. */
	struct xmit *xm;
	unsigned char *nh;
	void *v[2];
	int len[2];
	int size;
	Block *bp;
	Ctlr *ctlr;
	inotify("lgtxstart\n");
	ctlr = edev->ctlr;
	while(bp = qget(edev->oq)){
		xm = malloc(sizeof(*xm));
		nh = xm->hdr;
		xm->block = bp;
		memset(nh, 0, 10);
		nh[0] = VIRTIO_NET_HDR_F_NEEDS_CSUM;
		nh[1] = VIRTIO_NET_HDR_GSO_NONE;
		v[0] = nh;
		len[0] = sizeof(xm->hdr);
		size = BLEN(bp);
		v[1] = bp->rp;
		len[1] = size;
		/* non blocking IO. Basically, we'll get interrupted and do a getbuf, which is the bp, and free the bp */
		lgvaddbuf(ctlr->devno, 1, v, len, 2, 0, xm);
	}
}

#ifdef NOT
	hflags = 0,
	hgso = 1,
	hdr_len = 2,          /* Ethernet + IP + tcp/udp hdrs */
	gso_size = 4,         /* Bytes to append to gso_hdr_len per frame */
	hcsum_start = 6,       /* Position to start checksumming from */
	hcsum_off = 8,      /* Offset after that to place checksum */
	
};

struct netheader {
	unsigned char h[10];
};

#endif
static void
lgtransmit(Ether* edev)
{
	Ctlr *ctlr;
	inotify("lgtransmit...\n");
	ctlr = edev->ctlr;
	ilock(&ctlr->tlock);
	lgtxstart(edev);
	iunlock(&ctlr->tlock);
	inotify("lgtransmit done\n");
}

static void
lginterrupt(Ureg*, void* arg)
{
	void *lgvgetbuf(int dev, int ring, int *plen);
	Ether *edev = arg;
	Ctlr *ctlr;
	Block *bp;
	int len;
	struct xmit *xm;
	inotify("L");
	ctlr = edev->ctlr;
	/* suck up stuff while there's stuff to suck. */
	while (xm = lgvgetbuf(ctlr->devno, 1, &len)) {
		inotify("IF%d %p %p", len, xm, xm->block);
		freeb(xm->block);
		free(xm);
	}
	inotify("l");

	while (bp = lgvgetbuf(ctlr->devno, 0, &len)) {
		bp->wp += len;
		inotify("IR%d %p ", len, bp);
		if (BLEN(bp) > 4096)
			inotify("BLEN(%p) is %d > 4K! len says %d\n", bp, BLEN(bp), len);
		//dumphex("RP", bp->rp, BLEN(bp));
		etheriq(edev, bp, 1);
		fillslot(edev);
	}
}

static int
lgpnp(Ether* edev)
{
	int lgvfeature(int devno, unsigned int feature);
	void lgvdumpconfig(int devno);
	int findlgv(char *name);
	void lgvconfig(int devno, unsigned char *config, int off ,int len);
	Ctlr *ctlr = nil;
	uchar ea[Eaddrlen];
	int i, devno;
	char name[32];
	/* just loop through finding #Z/ether%d until no more */

	/*
	 * Make a list of all ethernet controllers
	 * if not already done.
	 */
	if(ctlrhead == nil){
		sprint(name, "net0");
		for (i = 0, devno = findlgv(name); devno > -1; devno = findlgv(name)) {
			nether++;
			ctlr = malloc(sizeof(Ctlr));
			ctlr->devno = devno;
			ctlr->id = i;
			lgvdumpconfig(devno);
			if(ctlrhead != nil)
				ctlrtail->next = ctlr;
			else
				ctlrhead = ctlr;
			ctlrtail = ctlr;
			iprint("found one enet ctlr\n");
			sprint(name, "net%d", nether);
		}
	}
	if (edev->ctlrno >= nether)
		return -1;
	if (! ctlr)
		return -1;
	edev->ctlr = ctlr;
	edev->port = 0;
	edev->irq = NETINTR;
	edev->tbdf = BUSUNKNOWN;
	/*
	 * Check if the adapter's station address is to be overridden.
	 * If not, read it from the device and set in edev->ea.
	 */
	memset(ea, 0, Eaddrlen);
	if(memcmp(ea, edev->ea, Eaddrlen) == 0){
		lgvconfig(ctlr->devno, edev->ea, config_mac, sizeof(edev->ea));
		/* but if they did not give us one, make one up. Just add one to lowest octet for now */
		if (! lgvfeature(ctlr->devno, VIRTIO_NET_F_MAC)) {
			iprint("Jiggering address\n");
			edev->ea[5] += 1;
		}
	}

	edev->attach = lgattach;
	edev->transmit = lgtransmit;
	edev->interrupt = lginterrupt;
	edev->ifstat = lgifstat;

	edev->arg = edev;
	edev->promiscuous = lgpromiscuous;
	edev->multicast = lgmulticast;
//	edev->shutdown = lgshutdown;

	return 0;
}

void
etherlglink(void)
{
	addethercard("lg", lgpnp);
}
