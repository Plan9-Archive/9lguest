/*P:010
 * A hypervisor allows multiple Operating Systems to run on a single machine.
 * To quote David Wheeler: "Any problem in computer science can be solved with
 * another layer of indirection."
 *
 * We keep things simple in two ways.  First, we start with a normal Linux
 * kernel and insert a module (lg.ko) which allows us to run other Linux
 * kernels the same way we'd run processes.  We call the first kernel the Host,
 * and the others the Guests.  The program which sets up and configures Guests
 * (such as the example in Documentation/lguest/lguest.c) is called the
 * Launcher.
 *
 * Secondly, we only run specially modified Guests, not normal kernels.  When
 * you set CONFIG_LGUEST to 'y' or 'm', this automatically sets
 * CONFIG_LGUEST_GUEST=y, which compiles this file into the kernel so it knows
 * how to be a Guest.  This means that you can use the same kernel you boot
 * normally (ie. as a Host) as a Guest.
 *
 * These Guests know that they cannot do privileged operations, such as disable
 * interrupts, and that they have to ask the Host to do such things explicitly.
 * This file consists of all the replacements for such low-level native
 * hardware operations: these special Guest versions call the Host.
 *
 * So how does the kernel know it's a Guest?  The very first instructions the
 * 32-bit kernel runs are at startup_32 in arch/i386/kernel/head.S.  This tests
 * if we're fully privileged: if we're not, we know we're under some kind of
 * hypervisor.  We end up here, where we replace the native functions in
 * "struct paravirt_ops" with our Guest versions. :*/

/*
 * Copyright (C) 2006, Rusty Russell <rusty@rustcorp.com.au> IBM Corporation.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, GOOD TITLE or
 * NON INFRINGEMENT.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*G:010 Welcome to the Guest!
 *
 * The Guest in our tale is a simple creature: identical to the Host but
 * behaving in simplified but equivalent ways.  In particular, the Guest is the
 * same kernel as the Host (or at least, built from the same source code). :*/
#include	"u.h"
#include	"../port/lib.h"
#include	"mem.h"
#include	"dat.h"
#include	"fns.h"
#include	"io.h"
#include	"ureg.h"
#include	"pool.h"
#include	"lguest.h"

extern void lguest_iret(void);

extern struct lguest_data lguest_data;
struct lguest_device_desc *lguest_devices;

/*G:035 Notice the lazy_hcall() above, rather than hcall().  This is our first
 * real optimization trick!
 *
 * When lazy_mode is set, it means we're allowed to defer all hypercalls and do
 * them as a batch when lazy_mode is eventually turned off.  Because hypercalls
 * are reasonably expensive, batching them up makes sense.  For example, a
 * large mmap might update dozens of page table entries: that code calls
 * lguest_lazy_mode(PARAVIRT_LAZY_MMU), does the dozen updates, then calls
 * lguest_lazy_mode(PARAVIRT_LAZY_NONE).
 *
 * So, when we're in lazy mode, we call async_hypercall() to store the call for
 * future processing.  When lazy mode is turned off we issue a hypercall to
 * flush the stored calls. */
static int lazy_mode; /* Note: not SMP-safe! */
static void lguest_lazy_mode(int mode)
{
	lazy_mode = mode;
	if (mode == PARAVIRT_LAZY_NONE)
		hcall(LHCALL_FLUSH_ASYNC, 0, 0, 0, 0);
}

static void lazy_hcall(unsigned long call,
		       unsigned long arg1,
		       unsigned long arg2,
		       unsigned long arg3,
			unsigned long arg4)
{
	if (lazy_mode == PARAVIRT_LAZY_NONE)
		hcall(call, arg1, arg2, arg3, arg4);
	else
		async_hcall(call, arg1, arg2, arg3, arg4);
}

/* async_hcall() is pretty simple: I'm quite proud of it really.  We have a
 * ring buffer of stored hypercalls which the Host will run though next time we
 * do a normal hypercall.  Each entry in the ring has 4 slots for the hypercall
 * arguments, and a "hcall_status" word which is 0 if the call is ready to go,
 * and 255 once the Host has finished with it.
 *
 * If we come around to a slot which hasn't been finished, then the table is
 * full and we just make the hypercall directly.  This has the nice side
 * effect of causing the Host to run all the stored calls in the ring buffer
 * which empties it for next time! */
void async_hcall(unsigned long call,
		 unsigned long arg1, unsigned long arg2, unsigned long arg3, unsigned long arg4)
{
	/* Note: This code assumes we're uniprocessor. */
	static unsigned int next_call;
	int s;

	/* Disable interrupts if not already disabled: we don't want an
	 * interrupt handler making a hypercall while we're already doing
	 * one! */
	s = splhi();
	if (lguest_data.hcall_status[next_call] != 0xFF) {
		/* Table full, so do normal hcall which will flush table. */
		hcall(call, arg1, arg2, arg3, arg4);
	} else {
		lguest_data.hcalls[next_call].eax = call;
		lguest_data.hcalls[next_call].ebx = arg1;
		lguest_data.hcalls[next_call].ecx = arg2;
		lguest_data.hcalls[next_call].edx = arg3;
		lguest_data.hcalls[next_call].esi = arg4;
		/* Arguments must all be written before we mark it to go */
		coherence();
		lguest_data.hcall_status[next_call] = 0;
		if (++next_call == LHCALL_RING_SIZE)
			next_call = 0;
	}
	splx(s);
}
/*:*/

void *lguest_map(unsigned long phys_addr, unsigned long pages)
{
	dumpstack();
	panic("called lguest_map with %#lx, pages %ld", phys_addr, pages);
	return nil;
}

void lguest_unmap(void *addr)
{
	dumpstack();
	panic("called lguest_unmap with %p", addr);
}

/*G:033
 * Here are our first native-instruction replacements: four functions for
 * interrupt control.
 *
 * The simplest way of implementing these would be to have "turn interrupts
 * off" and "turn interrupts on" hypercalls.  Unfortunately, this is too slow:
 * these by far the most commonly called functions of those we override.
 *
 * So instead we keep an "irq_enabled" field inside our "struct lguest_data",
 * which the Guest can update with a single instruction.  The Host knows to
 * check there when it wants to deliver an interrupt.
 */

/* save_flags() is expected to return the processor state (ie. "eflags").  The
 * eflags word contains all kind of stuff, but in practice Linux only cares
 * about the interrupt flag.  Our "save_flags()" just returns that. */
int islo(void)
{
	return lguest_data.irq_enabled;
}

/* "restore_flags" just sets the flags back to the value given. */
void splx(int flags)
{
	lguest_data.irq_enabled = flags;
}

/* Interrupts go off... */
int splhi(void)
{
	int old = lguest_data.irq_enabled;
	lguest_data.irq_enabled = 0;
	return old;
}

/* Interrupts go on... */
int spllo(void)
{
	int old = lguest_data.irq_enabled;
	/* we use the Linux define here since it may change in lguest at some point */
	lguest_data.irq_enabled = X86_EFLAGS_IF;
	return old;
}
/*:*/
/*M:003 Note that we don't check for outstanding interrupts when we re-enable
 * them (or when we unmask an interrupt).  This seems to work for the moment,
 * since interrupts are rare and we'll just get the interrupt on the next timer
 * tick, but when we turn on CONFIG_NO_HZ, we should revisit this.  One way
 * would be to put the "irq_enabled" field in a page by itself, and have the
 * Host write-protect it when an interrupt comes in when irqs are disabled.
 * There will then be a page fault as soon as interrupts are re-enabled. :*/

static inline void write_dt_entry(void *dt, int entry, u32 entry_a, u32 entry_b)
{
	u32 *lp = (u32 *)((char *)dt + entry*8);
	*lp = entry_a;
	*(lp+1) = entry_b;
}

/*G:034
 * The Interrupt Descriptor Table (IDT).
 *
 * The IDT tells the processor what to do when an interrupt comes in.  Each
 * entry in the table is a 64-bit descriptor: this holds the privilege level,
 * address of the handler, and... well, who cares?  The Guest just asks the
 * Host to make the change anyway, because the Host controls the real IDT.
 */
void lguest_write_idt_entry(void *dt,
				   int entrynum, u32 low, u32 high)
{
	/* Keep the local copy up to date. */
	write_dt_entry(dt, entrynum, low, high);
	/* Tell Host about this new entry. */
	hcall(LHCALL_LOAD_IDT_ENTRY, entrynum, low, high, 0);
}

/* Changing to a different IDT is very rare: we keep the IDT up-to-date every
 * time it is written, so we can simply loop through all entries and tell the
 * Host about them. */
void lidt(ushort *idtp)
{
	unsigned int i;
	/* size is the first short */
	u16 size = idtp[0];
	/* descriptor is in the long after the first short */
	u32 address = *(u32 *)&idtp[1];
	struct desc_struct *idt = (void *)address;
	/* only do 64, no matter how many it says. lguest ignores the others
	  * and, if you do 128  by mistake, it will overwrite the plan 9 system call
	  * entry at 64. We have to fix lguest to take system call # as a parameter
	  */
	size = size > 64 ? 64 : size;
	for (i = 0; i < size; i++){
		//iprint("Vec %d low %p, high %p\n", i, (void *)idt[i].a, (void *)idt[i].b);
		hcall(LHCALL_LOAD_IDT_ENTRY, i, idt[i].a, idt[i].b, 0);
	}
	/* now do the system call entry */
	i = 0x40;
	hcall(LHCALL_LOAD_IDT_ENTRY, i, idt[i].a, idt[i].b, 0);
}

/*
 * The Global Descriptor Table.
 *
 * The Intel architecture defines another table, called the Global Descriptor
 * Table (GDT).  You tell the CPU where it is (and its size) using the "lgdt"
 * instruction, and then several other instructions refer to entries in the
 * table.  There are three entries which the Switcher needs, so the Host simply
 * controls the entire thing and the Guest asks it to make changes using the
 * LOAD_GDT hypercall.
 *
 * This is the opposite of the IDT code where we have a LOAD_IDT_ENTRY
 * hypercall and use that repeatedly to load a new IDT.  I don't think it
 * really matters, but wouldn't it be nice if they were the same?
 */

static void lguest_write_gdt_entry(void *dt,
				   int entrynum, u32 low, u32 high)
{
	write_dt_entry(dt, entrynum, low, high);
	hcall(LHCALL_LOAD_GDT_ENTRY, entrynum, low, high, 0);
}

void lgdt(ushort *gdtp)
{
	/* size is the first short */
	u16 size = gdtp[0];
	int i;
	/* descriptor is in the long after the first short */
	u32 address = *(u32 *)&gdtp[1];
	struct desc_struct *gdt = (struct desc_struct *) address;
	iprint("lgdt at %p, entries at %p\n", gdtp, gdt);
	if (((size+1)/8) != GDT_ENTRIES)
		panic("bad gdt in lguest_load_gdt: need %d entries, got %d\n",
				GDT_ENTRIES, (size+1)/8);
	//iprint("Load the gdt at %p\n", gdt);
	for(i = 0; i < (size+1)/8; i++) {
		//iprint("Load el %d a %#ulx %#ulx\n", i, gdt[i].a, gdt[i].b);
		hcall(LHCALL_LOAD_GDT_ENTRY, i, gdt[i].a, gdt[i].b, 0);
	}
}

/*:*/

/*G:038 That's enough excitement for now, back to ploughing through each of
 * the paravirt_ops (we're about 1/3 of the way through).
 *
 * This is the Local Descriptor Table, another weird Intel thingy.  Linux only
 * uses this for some strange applications like Wine.  We don't do anything
 * here, so they'll get an informative and friendly Segmentation Fault. */
static void lguest_set_ldt(const void *, unsigned)
{
	dumpstack();
	iprint("lguest_set_ldt: ignored\n");
}

/* This loads a GDT entry into the "Task Register": that entry points to a
 * structure called the Task State Segment.  Some comments scattered though the
 * kernel code indicate that this used for task switching in ages past, along
 * with blood sacrifice and astrology.
 *
 * Now there's nothing interesting in here that we don't get told elsewhere.
 * But the native version uses the "ltr" instruction, which makes the Host
 * complain to the Guest about a Segmentation Fault and it'll oops.  So we
 * override the native version with a do-nothing version. */
void ltr(ulong)
{
}

/* The "cpuid" instruction is a way of querying both the CPU identity
 * (manufacturer, model, etc) and its features.  It was introduced before the
 * Pentium in 1993 and keeps getting extended by both Intel and AMD.  As you
 * might imagine, after a decade and a half this treatment, it is now a giant
 * ball of hair.  Its entry in the current Intel manual runs to 28 pages.
 *
 * This instruction even it has its own Wikipedia entry.  The Wikipedia entry
 * has been translated into 4 languages.  I am not making this up!
 *
 * We could get funky here and identify ourselves as "GenuineLguest", but
 * instead we just use the real "cpuid" instruction.  Then I pretty much turned
 * off feature bits until the Guest booted.  (Don't say that: you'll damage
 * lguest sales!)  Shut up, inner voice!  (Hey, just pointing out that this is
 * hardly future proof.)  Noone's listening!  They don't like you anyway,
 * parenthetic weirdo!
 *
 * Replacing the cpuid so we can turn features off is great for the kernel, but
 * anyone (including userspace) can just use the raw "cpuid" instruction and
 * the Host won't even notice since it isn't privileged.  So we try not to get
 * too worked up about it. */
void lguest_cpuid(u32 *eax, u32 *ebx,u32 *ecx, u32 *edx)
{
	int function = *eax;
	u32 data[4];
	void native_cpuid(u32, u32 *);

	native_cpuid(*eax, data);
	*eax = data[0];
	*ebx = data[1];
	*ecx = data[2];
	*edx = data[3];
	switch (function) {
	case 1:	/* Basic feature request. */
		/* We only allow kernel to see SSE3, CMPXCHG16B and SSSE3 */
		*ecx &= 0x00002201;
		/* Similarly: SSE, SSE2, FXSR, MMX, CMOV, CMPXCHG8B, FPU. */
		*edx &= 0x07808101;
		/* The Host can do a nice optimization if it knows that the
		 * kernel mappings (addresses above 0xC0000000 or whatever
		 * PAGE_OFFSET is set to) haven't changed.  But Linux calls
		 * flush_tlb_user() for both user and kernel mappings unless
		 * the Page Global Enable (PGE) feature bit is set. */
		*edx |= 0x00002000;
		break;
	case 0x80000000:
		/* Futureproof this a little: if they ask how much extended
		 * processor information there is, limit it to known fields. */
		if (*eax > 0x80000008)
			*eax = 0x80000008;
		break;
	}
}

/* Intel has four control registers, imaginatively named cr0, cr2, cr3 and cr4.
 * I assume there's a cr1, but it hasn't bothered us yet, so we'll not bother
 * it.  The Host needs to know when the Guest wants to change them, so we have
 * a whole series of functions like read_cr0() and write_cr0().
 *
 * We start with CR0.  CR0 allows you to turn on and off all kinds of basic
 * features, but Linux only really cares about one: the horrifically-named Task
 * Switched (TS) bit at bit 3 (ie. 8)
 *
 * What does the TS bit do?  Well, it causes the CPU to trap (interrupt 7) if
 * the floating point unit is used.  Which allows us to restore FPU state
 * lazily after a task switch, and Linux uses that gratefully, but wouldn't a
 * name like "FPUTRAP bit" be a little less cryptic?
 *
 * We store cr0 (and cr3) locally, because the Host never changes it.  The
 * Guest sometimes wants to read it and we'd prefer not to bother the Host
 * unnecessarily. */
static unsigned long current_cr0, current_cr3;
void putcr0(unsigned long val)
{
	/* 8 == TS bit. */
	lazy_hcall(LHCALL_TS, val & 8, 0, 0, 0);
	current_cr0 = val;
}

unsigned long getcr0(void)
{
	return current_cr0;
}

/* Intel provided a special instruction to clear the TS bit for people too cool
 * to use write_cr0() to do it.  This "clts" instruction is faster, because all
 * the vowels have been optimized out. */
void lguest_clts(void)
{
	lazy_hcall(LHCALL_TS, 0, 0, 0, 0);
	current_cr0 &= ~8U;
}

/* CR2 is the virtual address of the last page fault, which the Guest only ever
 * reads.  The Host kindly writes this into our "struct lguest_data", so we
 * just read it out of there. */
unsigned long getcr2(void)
{
	return lguest_data.cr2;
}

/* CR3 is the current toplevel pagetable page: the principle is the same as
 * cr0.  Keep a local copy, and tell the Host when it changes. */
void putcr3(unsigned long cr3)
{
	/*lazy_*/hcall(LHCALL_NEW_PGTABLE, cr3, 0, 0, 0);
	current_cr3 = cr3;
}

unsigned long getcr3(void)
{
	return current_cr3;
}

/* CR4 is used to enable and disable PGE, but we don't care. */
unsigned long getcr4(void)
{
	return 0;
}

void putcr4(unsigned long)
{
}

/*
 * Page Table Handling.
 *
 * Now would be a good time to take a rest and grab a coffee or similarly
 * relaxing stimulant.  The easy parts are behind us, and the trek gradually
 * winds uphill from here.
 *
 * Quick refresher: memory is divided into "pages" of 4096 bytes each.  The CPU
 * maps virtual addresses to physical addresses using "page tables".  We could
 * use one huge index of 1 million entries: each address is 4 bytes, so that's
 * 1024 pages just to hold the page tables.   But since most virtual addresses
 * are unused, we use a two level index which saves space.  The CR3 register
 * contains the physical address of the top level "page directory" page, which
 * contains physical addresses of up to 1024 second-level pages.  Each of these
 * second level pages contains up to 1024 physical addresses of actual pages,
 * or Page Table Entries (PTEs).
 *
 * Here's a diagram, where arrows indicate physical addresses:
 *
 * CR3 ---> +---------+
 *	    |  	   --------->+---------+
 *	    |	      |	     | PADDR1  |
 *	  Top-level   |	     | PADDR2  |
 *	  (PMD) page  |	     | 	       |
 *	    |	      |	   Lower-level |
 *	    |	      |	   (PTE) page  |
 *	    |	      |	     |	       |
 *	      ....    	     	 ....
 *
 * So to convert a virtual address to a physical address, we look up the top
 * level, which points us to the second level, which gives us the physical
 * address of that page.  If the top level entry was not present, or the second
 * level entry was not present, then the virtual address is invalid (we
 * say "the page was not mapped").
 *
 * Put another way, a 32-bit virtual address is divided up like so:
 *
 *  1 1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
 * |<---- 10 bits ---->|<---- 10 bits ---->|<------ 12 bits ------>|
 *    Index into top     Index into second      Offset within page
 *  page directory page    pagetable page
 *
 * The kernel spends a lot of time changing both the top-level page directory
 * and lower-level pagetable pages.  The Guest doesn't know physical addresses,
 * so while it maintains these page tables exactly like normal, it also needs
 * to keep the Host informed whenever it makes a change: the Host will create
 * the real page tables based on the Guests'.
 */

/* The Guest calls this to set a second-level entry (pte), ie. to map a page
 * into a process' address space.  We set the entry then tell the Host the
 * toplevel and address this corresponds to.  The Guest uses one pagetable per
 * process, so we need to tell the Host which one we're changing (mm->pgd). */
void lguest_set_pte_at(ulong pdb, u32 addr, u32 *ptep, u32 pteval)
{
	*ptep = pteval;
	lazy_hcall(LHCALL_SET_PTE, paddr((void *)pdb), addr, pteval&0xfff, 0);
}

/* The Guest calls this to set a top-level entry.  Again, we set the entry then
 * tell the Host which top-level page we changed, and the index of the entry we
 * changed. */
void lguest_set_pmd(u32 *pmdp, u32 pmdval)
{
	*pmdp = pmdval;
iprint("Set pmpd %p to %#ulx\n", pmdp, pmdval);
	lazy_hcall(LHCALL_SET_PMD, PPN(paddr(pmdp)),
		   (paddr(pmdp)&(BY2PG-1))/4, 0, 0);
}

/* There are a couple of legacy places where the kernel sets a PTE, but we
 * don't know the top level any more.  This is useless for us, since we don't
 * know which pagetable is changing or what address, so we just tell the Host
 * to forget all of them.  Fortunately, this is very rare.
 *
 * ... except in early boot when the kernel sets up the initial pagetables,
 * which makes booting astonishingly slow.  So we don't even tell the Host
 * anything changed until we've done the first page table switch.
 */
void lguest_set_pte(u32 *ptep, u32 pteval)
{
	*ptep = pteval;
	//iprint("Set ptep %p to %#ulx\n", ptep, pteval);
	/* Don't bother with hypercall before initial setup. */
	if (current_cr3)
		lazy_hcall(LHCALL_FLUSH_TLB, 1, 0, 0, 0);
}

/* Unfortunately for Lguest, the paravirt_ops for page tables were based on
 * native page table operations.  On native hardware you can set a new page
 * table entry whenever you want, but if you want to remove one you have to do
 * a TLB flush (a TLB is a little cache of page table entries kept by the CPU).
 *
 * So the lguest_set_pte_at() and lguest_set_pmd() functions above are only
 * called when a valid entry is written, not when it's removed (ie. marked not
 * present).  Instead, this is where we come when the Guest wants to remove a
 * page table entry: we tell the Host to set that entry to 0 (ie. the present
 * bit is zero). */
void lguest_flush_tlb_single(u32 addr)
{
	/* Simply set it to zero: if it was not, it will fault back in. */
	lazy_hcall(LHCALL_SET_PTE, current_cr3, addr, 0, 0);
}

/* This is what happens after the Guest has removed a large number of entries.
 * This tells the Host that any of the page table entries for userspace might
 * have changed, ie. virtual addresses below PAGE_OFFSET. */
void lguest_flush_tlb_user(void)
{
	lazy_hcall(LHCALL_FLUSH_TLB, 0, 0, 0, 0);
}

/* This is called when the kernel page tables have changed.  That's not very
 * common (unless the Guest is using highmem, which makes the Guest extremely
 * slow), so it's worth separating this from the user flushing above. */
void lguest_flush_tlb_kernel(void)
{
	lazy_hcall(LHCALL_FLUSH_TLB, 1, 0, 0, 0);
}

/*
 * The Unadvanced Programmable Interrupt Controller.
 *
 * This is an attempt to implement the simplest possible interrupt controller.
 * I spent some time looking though routines like set_irq_chip_and_handler,
 * set_irq_chip_and_handler_name, set_irq_chip_data and set_phasers_to_stun and
 * I *think* this is as simple as it gets.
 *
 * We can tell the Host what interrupts we want blocked ready for using the
 * lguest_data.interrupts bitmap, so disabling (aka "masking") them is as
 * simple as setting a bit.  We don't actually "ack" interrupts as such, we
 * just mask and unmask them.  I wonder if we should be cleverer?
 */
void disable_lguest_irq(unsigned int irq)
{
	/* probably some asm goo. Fooey */
	/* we may pull over the xen stuff for this at some point. */
//	set_bit(irq, lguest_data.blocked_interrupts);
	lguest_data.blocked_interrupts[irq/32] |= 1 << (irq % 32);
}

void enable_lguest_irq(unsigned int irq)
{
//	clear_bit(irq, lguest_data.blocked_interrupts);
	lguest_data.blocked_interrupts[irq/32] &= ~(1 << (irq % 32));
}

#ifdef NOT
/* This sets up the Interrupt Descriptor Table (IDT) entry for each hardware
 * interrupt (except 128, which is used for system calls), and then tells the
 * Linux infrastructure that each interrupt is controlled by our level-based
 * lguest interrupt controller. */
void lguest_init_IRQ(void)
{
	unsigned int i;

	for (i = 0; i < LGUEST_IRQS; i++) {
		int vector = FIRST_EXTERNAL_VECTOR + i;
		if (vector != SYSCALL_VECTOR) {
			set_intr_gate(vector, interrupt[i]);
			set_irq_chip_and_handler(i, &lguest_irq_controller,
						 handle_level_irq);
		}
	}
	/* This call is required to set up for 4k stacks, where we have
	 * separate stacks for hard and soft interrupts. */
	irq_ctx_init(smp_processor_id());
}
#endif
/* now begins the fun with Plan 9 time. how many ticks again? */
/* we may need to look at the Xen port. This is a nightmare in Plan 9 */
/*
 * Time.
 *
 * It would be far better for everyone if the Guest had its own clock, but
 * until then it must ask the Host for the time.
 */
unsigned long lguest_get_wallclock(void)
{
	return lguest_data.time.seconds;
}

/* get the nanoseconds */
/* let's pretend we don't get interupted here. */
uvlong lguest_get_ns(void)
{
	uvlong ret = lguest_data.time.seconds;
	ret <<= 32;
	ret |=  lguest_data.time.nanoseconds;
	return ret;
}




/*
 * Miscellaneous bits and pieces.
 *
 * Here is an oddball collection of functions which the Guest needs for things
 * to work.  They're pretty simple.
 */

void
lguest_interval_timer(u32 nanoseconds){
	lazy_hcall(LHCALL_SET_CLOCKEVENT, nanoseconds, 0, 0, 0);
}

/* The Guest needs to tell the host what stack it expects traps to use.  For
 * native hardware, this is part of the Task State Segment mentioned above in
 * lguest_load_tr_desc(), but to help hypervisors there's this special call.
 *
 * We tell the Host the segment we want to use (__KERNEL_DS is the kernel data
 * segment), the privilege level (we're privilege level 1, the Host is 0 and
 * will not tolerate us trying to use that), the stack pointer, and the number
 * of pages in the stack. */
void lguest_load_esp0(u32 stack)
{
//iprint("load stack %p\n", (void *)stack);
	lazy_hcall(LHCALL_FLUSH_TLB, 1, 0, 0, 0);
	lazy_hcall(LHCALL_SET_STACK, (KDSEG<<3)|1, stack, KSTACK/BY2PG, 0);

}

/* Let's just say, I wouldn't do debugging under a Guest. */
void lguest_set_debugreg(int, unsigned long)
{
	/* FIXME: Implement */
}

/* There are times when the kernel wants to make sure that no memory writes are
 * caught in the cache (that they've all reached real hardware devices).  This
 * doesn't matter for the Guest which has virtual hardware.
 *
 * On the Pentium 4 and above, cpuid() indicates that the Cache Line Flush
 * (clflush) instruction is available and the kernel uses that.  Otherwise, it
 * uses the older "Write Back and Invalidate Cache" (wbinvd) instruction.
 * Unlike clflush, wbinvd can only be run at privilege level 0.  So we can
 * ignore clflush, but replace wbinvd.
 */
void lguest_wbinvd(void)
{
}

/* If the Guest expects to have an Advanced Programmable Interrupt Controller,
 * we play dumb by ignoring writes and returning 0 for reads.  So it's no
 * longer Programmable nor Controlling anything, and I don't think 8 lines of
 * code qualifies for Advanced.  It will also never interrupt anything.  It
 * does, however, allow us to get through the Linux boot code. */
#ifdef CONFIG_X86_LOCAL_APIC
void lguest_apic_write(unsigned long reg, unsigned long v)
{
}

unsigned long lguest_apic_read(unsigned long reg)
{
	return 0;
}
#endif

/* STOP!  Until an interrupt comes in. */
void halt(void)
{
	
	hcall(LHCALL_HALT, 0, 0, 0, 0);
}

/* Perhaps CRASH isn't the best name for this hypercall, but we use it to get a
 * message out when we're crashing as well as elegant termination like powering
 * off.
 *
 * Note that the Host always prefers that the Guest speak in physical addresses
 * rather than virtual addresses, so we use paddr() here. */
void lguest_power_off(void)
{
	hcall(LHCALL_SHUTDOWN, paddr("Power down"), 0, 0, 0);
}

/*
 * Panicing.
 *
 * Don't.  But if you did, this is what happens.
 */
int lguest_panic( void *p)
{
	hcall(LHCALL_SHUTDOWN, paddr(p), 0, 0, 0);
	return -1;
}

/* Setting up memory is fairly easy. */
	/* The Linux bootloader header contains an "e820" memory map: the
	 * Launcher populated the first entry with our memory limit. */
/*	add_memory_region(E820_MAP->addr, E820_MAP->size, E820_MAP->type);*/

/*G:030 Once we get to lguest_init(), we know we're a Guest.  The paravirt_ops
 * structure in the kernel provides a single point for (almost) every routine
 * we have to override to avoid privileged instructions. */
/* FIXME: use the boot info passed in, sasuming it is good */
void lguest_init(void */* boot info -- use this someday */)
{
#ifdef NOT
	/* Copy boot parameters first: the Launcher put the physical location
	 * in %esi, and head.S converted that to a virtual address and handed
	 * it to us. */
	memcpy(boot_params, boot, PARAM_SIZE);
	/* The boot parameters also tell us where the command-line is: save
	 * that, too. */
	memcpy(boot_command_line,
	       __va(*(unsigned long *)(boot_params + NEW_CL_POINTER)),
	       COMMAND_LINE_SIZE);
#endif
	/* Now is a good time to look at the implementations of these functions
	 * before returning to the rest of lguest_init(). :*/

	/*G:070 Now we've seen all the paravirt_ops, we return to
	 * lguest_init() where the rest of the fairly chaotic boot setup
	 * occurs.
	 *
	 * The Host expects our first hypercall to tell it where our "struct
	 * lguest_data" is, so we do that first. */
	hcall(LHCALL_LGUEST_INIT, paddr(&lguest_data), 0, 0, 0);

	/* The native boot code sets up initial page tables immediately after
	 * the kernel itself, and sets init_pg_tables_end so they're not
	 * clobbered.  The Launcher places our initial pagetables somewhere at
	 * the top of our physical memory, so we don't need extra space: set
	 * init_pg_tables_end to the end of the kernel. */
	/* not on plan 9, sadly ... */
//	init_pg_tables_end = paddr(pg0);

	/* Set up the Percpu Data Area (going away in 2.6.22: yay!) */
//	load_gdt(&early_gdt_descr);
//	asm volatile ("mov %0, %%fs" : : "r" (__KERNEL_PDA) : "memory");

	/* The Host uses the top of the Guest's virtual address space for the
	 * Host<->Guest Switcher, and it tells us how much it needs in
	 * lguest_data.reserve_mem, set up on the LGUEST_INIT hypercall. */
//	reserve_top_address(lguest_data.reserve_mem);

	/* This is messy CPU setup stuff which the native boot code does before
	 * start_kernel, so we have to do, too: */
//	cpu_detect(&new_cpu_data);
	/* Need this before paging_init. */
//	set_bit(X86_FEATURE_PGE, new_cpu_data.x86_capability);
	/* Math is always hard! */
//	new_cpu_data.hard_math = 1;

	/* Turn off stuff which a Guest never has to deal with */

}
/*
 * This marks the end of stage II of our journey, The Guest.
 *
 * It is now time for us to explore the nooks and crannies of the three Guest
 * devices and complete our understanding of the Guest in "make Drivers".
 */
