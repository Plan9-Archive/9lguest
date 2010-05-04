#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"../port/error.h"
#include	"lguest.h"

#include	<authsrv.h>

/* The Lguest uart. Pretty simple. And now kludgy. 
  */

extern PhysUart  lgphysuart;

/*static */Uart lguart = {	.regs	= 0,
	.name	= "lgCOM1",
	.freq	= 1000000,
	.phys	= &lgphysuart,
	.special= 0,
	.next	= nil,
};

extern int console;

void
lgconswrite(char *str, int len)
{
	void lgvconsout(char *a, int len);

	if (console < 0)
		hcall(LHCALL_NOTIFY, paddr(str), 0, 0);

	if (console >-1){
		lgvconsout(str, len);
	}

}

static char dmabuf[1024];

#ifdef NOT
/* TODO: find the write place to put the dmabuf pointer!*/
static void
lgintr(Ureg *, void *arg){
	int i;
	Uart *uart = arg;
	/* XXX should  be p = uart->something->dmabuf or some such */
	char *p = dmabuf;

/*	for(i = 0; i < consin.used_len; i++){
		lgconswrite(p, 1);
		uartrecv(uart, *p++);
	}*/

}
#endif
static Uart*
lgpnp(void)
{
	return &lguart;
}

unsigned char *in = nil;
unsigned char inbuf[1024];
unsigned char inlen = 0;

void
consinintr(void)
{
	int lgvgetconsbuf(unsigned char *v, int len);
	int lgvaddconsbuf(unsigned char *v, int len);
	unsigned char *p = inbuf;
//	iprint("CONSININTR\n");
//	iprint("consin %d\n", inlen);

	inlen = lgvgetconsbuf(inbuf, sizeof(inbuf));
//	iprint("consin %d\n", inlen);
		
	for(;inlen> 0;inlen--){
		lgconswrite((char *)p, 1);
		uartrecv(consuart, *p++);	
	}

	inlen =  lgvaddconsbuf(inbuf, sizeof(inbuf));

//	iprint("consin %d\n", inlen);
//iprint("EXIT CONSININTR");
		
}
void
lgenable(Uart *uart, int){
	void	lgvconsin(void *, int, char *, void *f); //(void *f)(void));
	uart->console = 1;
	/* set up to receive data into our one single buffer. 
	 * further lguest input will block until we resubmit this one
	 */
	lgvconsin(inbuf, sizeof(inbuf), "consinintr", consinintr);
}

static void
lgdisable(Uart*)
{
}


static long
lgstatus(Uart*, void* buf, long n, long offset)
{
	char *p;
	p = malloc(READSTR);
	snprint(p, READSTR,"one kickass uart\n");
	n = readstr(offset, buf, n, p);
	free(p);

	return n;
}

static void
lgputc(Uart *, int c)
{
	void lgvconsout(char *a, int len);
	/* need to use static -- lguest is asynchronous to us */
	static char cc[1];
	cc[0] = c;
	lgconswrite(cc, 1);
}

static int
lgnop(Uart *, int){
	return 0;
}

static void
lgnopv(Uart *, int){
}

static int
lgnogetcyet(Uart *){
iprint("NO GETC YET\n");
return 0;
}


PhysUart lgphysuart = {
	.name		= "lguestuart",
	.pnp		= lgpnp,
	.enable		= lgenable,
	.disable	= lgdisable,
	.kick		= nil,
	.dobreak	= nil,
	.baud		= lgnop,
	.bits		= lgnop,
	.stop		= lgnop,
	.parity		= lgnop,
	.modemctl	= nil,
	.rts		= nil,
	.dtr		= nil,
	.status		= lgstatus,
	.fifo		= nil,
	.getc		= lgnogetcyet,
	.putc		= lgputc,
};


void
lgconsole(void)
{
	int	findlgv(char *name);
	Uart *uart;
	uart = &lguart;
	uartctl(uart, "b9600 l8 pn s1");
/*	if(*cmd != '\0')
		uartctl(uart, cmd);
*/
	consuart = uart;
	uart->console = 1;

	iprint("findlgv is %d\n", findlgv("cons"));
/*if only. 
lgconswrite("lgpnp\n", 7);
	iprint("lgpnp, open cons\n");
	c = namec("#Z/cons", Aopen, ORDWR, 0);
	iprint("lgpnp, c %p\n", c);
	if (! c)
		return;
	console = c;
*/
}
