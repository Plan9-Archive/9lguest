#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include "lguest.h"

/* I'm really tired of trying to make port/tod.c work right, and it makes
  * no sense to try -- we have a ns clock tied to tod, so let's use it. 
  * also the clock on lguest is 4*1024*1024*1024, not 1000000000
  * tod.c coughs and dies on this, it seems.
  */
/*
 * Compute nanosecond epoch time from the fastest ticking clock
 * on the system.  Converting the time to nanoseconds requires
 * the following formula
 *
 *	t = (((1000000000<<31)/f)*ticks)>>31
 *
 *  where
 *
 *	'f'		is the clock frequency
 *	'ticks'		are clock ticks
 *
 *  to avoid too much calculation in todget(), we calculate
 *
 *	mult = (1000000000<<32)/f
 *
 *  each time f is set.  f is normally set by a user level
 *  program writing to /dev/fastclock.  mul64fract will then
 *  take that fractional multiplier and a 64 bit integer and
 *  return the resulting integer product.
 *
 *  We assume that the cpu's of a multiprocessor are synchronized.
 *  This assumption needs to be questioned with each new architecture.
 */

/* frequency of the tod clock */
#define TODFREQ		1000000000ULL
#define MicroFREQ	1000000ULL

struct {
	int	init;		// true if initialized
	ulong	cnt;
	Lock;
	uvlong	multiplier;	// ns = off + (multiplier*ticks)>>31
	uvlong	divider;	// ticks = (divider*(ns-off))>>31
	uvlong	umultiplier;	// µs = (µmultiplier*ticks)>>31
	uvlong	udivider;	// ticks = (µdivider*µs)>>31
	vlong	hz;		// frequency of fast clock
	vlong	last;		// last reading of fast clock
	vlong	off;		// offset from epoch to last
	vlong	lasttime;	// last return value from todget
	vlong	delta;		// add 'delta' each slow clock tick from sstart to send
	ulong	sstart;		// ...
	ulong	send;		// ...
} tod;

void
todinit(void)
{
	if(tod.init)
		return;
iprint("todiit ... ");
	ilock(&tod);
iprint("ilock &tod ok\n");
	tod.last = fastticks((uvlong*)&tod.hz);
	iunlock(&tod);
	tod.init = 1;
}

void
todsetfreq(vlong )
{
}
/*
 *  Set the time of day struct  -- we don't allow this on lguest. nop. 
 */
void
todset(vlong , vlong, int )
{
	if(!tod.init)
		todinit();
}

/*
 *  get time of day
 */
vlong
todget(vlong *ticksp)
{
	uvlong x;
	if(!tod.init)
		todinit();

	// we don't want time to pass twixt the measuring of fastticks
	// and grabbing tod.last.  Also none of the vlongs are atomic so
	// we have to look at them inside the lock.
	ilock(&tod);
	tod.cnt++;
	x = fastticks(nil);

	// time can't go backwards
	if(x < tod.lasttime)
		x = tod.lasttime;
	else
		tod.lasttime = x;

	iunlock(&tod);
	/* what a mess this is. */
	/* scale lguest time to real nanoseconds. */
	x /= 4398;
	x *= 1024;
	/* this is now scaled to ns. */

	if(ticksp != nil)
		*ticksp = x;
//iprint("todget %#ulx:%#ulx\n", (long)(x>>32), (long)x);
	return x;
}

long
seconds(void)
{
	int i;

	i = lguest_get_wallclock();
	return i;
}

uvlong
fastticks2us(uvlong ticks)
{
	uvlong res;

	if(!tod.init)
		todinit();
	res = ticks / 1000;
	return res;
}

uvlong
us2fastticks(uvlong us)
{
	uvlong res;

	if(!tod.init)
		todinit();
	res = us * 1000;
	return res;
}

/*
 *  convert milliseconds to fast ticks
 */
uvlong
ms2fastticks(ulong ms)
{
	if(!tod.init)
		todinit();
	return ms * 1000000ULL;
}

/*
 *  convert nanoseconds to fast ticks
 */
uvlong
ns2fastticks(uvlong ns)
{
	return ns;
}

/*
 *  convert fast ticks to ns
 */
uvlong
fastticks2ns(uvlong ticks)
{
	return ticks;
}
