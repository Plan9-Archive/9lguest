#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include "lguest.h"
/*
 *  lguest real time clock
 */

enum{
	Qdir = 0,
	Qrtc,
};

Dirtab rtcdir[]={
	".",	{Qdir, 0, QTDIR},	0,	0555,
	"rtc",		{Qrtc, 0},	0,	0664,
};

static Chan*
rtcattach(char* spec)
{
	return devattach('r', spec);
}

static Walkqid*	 
rtcwalk(Chan* c, Chan *nc, char** name, int nname)
{
	return devwalk(c, nc, name, nname, rtcdir, nelem(rtcdir), devgen);
}

static int	 
rtcstat(Chan* c, uchar* dp, int n)
{
	return devstat(c, dp, n, rtcdir, nelem(rtcdir), devgen);
}

static Chan*
rtcopen(Chan* c, int omode)
{
	omode = openmode(omode);
	switch((ulong)c->qid.path){
	case Qrtc:
		if(strcmp(up->user, eve)!=0 && omode!=OREAD)
			error(Eperm);
		break;
	}
	return devopen(c, omode, rtcdir, nelem(rtcdir), devgen);
}

static void	 
rtcclose(Chan*)
{
}

static Lock nvrtlock;

long
rtctime(void)
{
	int i;
	long t, ot;

	ilock(&nvrtlock);

	/* loop till we get two reads in a row the same */
	t = lguest_get_wallclock();
	for(i = 0; i < 100; i++){
		ot = t;
		t = lguest_get_wallclock();
		if(ot == t)
			break;
	}
	if(i == 100) print("we are boofheads\n");

	iunlock(&nvrtlock);

	return t;
}

static long	 
rtcread(Chan* c, void* buf, long n, vlong off)
{
	ulong t;
	ulong offset = off;

	if(c->qid.type & QTDIR)
		return devdirread(c, buf, n, rtcdir, nelem(rtcdir), devgen);

	switch((ulong)c->qid.path){
	case Qrtc:
		t = rtctime();
		n = readnum(offset, buf, n, t, 12);
		return n;
	}
	error(Ebadarg);
	return 0;
}

/* at some point we can set a fixed offset here. But I don't see any reason
  * (yet) to support this.
  */
static long	 
rtcwrite(Chan* c, void*, long n, vlong off)
{
	ulong offset = off;

	error(Eperm);
	if(offset!=0)
		error(Ebadarg);

	switch((ulong)c->qid.path){
	case Qrtc:
		/*
		 *  write the clock
		 */
		ilock(&nvrtlock);
		iunlock(&nvrtlock);
		return n;
	}
	error(Ebadarg);
	return 0;
}

Dev rtcdevtab = {
	'r',
	"rtc",

	devreset,
	devinit,
	devshutdown,
	rtcattach,
	rtcwalk,
	rtcstat,
	rtcopen,
	devcreate,
	rtcclose,
	rtcread,
	devbread,
	rtcwrite,
	devbwrite,
	devremove,
	devwstat,
};
