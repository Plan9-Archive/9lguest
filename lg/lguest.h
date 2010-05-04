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
/* mods for Plan 9 by Ron Minnich, but the GPL stays I think. */

/* lguest structs */
/* these were automatically created by the program buildlgh.c in this directory */


#define GUEST_PL 1
#define IDT_ENTRIES 256
#define GDT_ENTRIES 32

typedef unsigned long u32;
typedef unsigned short u16;
typedef unsigned char u8;
struct Xgt_desc_struct {
  u8 data[8];
};
struct desc_struct {
	u32 a,b;
};

/* futex_key has no meaning to Plan 9 ...*/
struct futex_key {
	u32 pgoff;
	void *v;
	u32 offset;
};


struct lguest_regs
{
	/* Manually saved part. */
	u32 ebx, ecx, edx;
	u32 esi, edi, ebp;
	u32 gs;
	u32 eax;
	u32 fs, ds, es;
	u32 trapnum, errcode;
	/* Trap pushed part */
	u32 eip;
	u32 cs;
	u32 eflags;
	u32 esp;
	u32 ss;
};

/* Full 4G segment descriptors, suitable for CS and DS. */
#define FULL_EXEC_SEGMENT ((struct desc_struct){0x0000ffff, 0x00cf9b00})
#define FULL_SEGMENT ((struct desc_struct){0x0000ffff, 0x00cf9300})

struct list_head {
	struct list_head *next, *prev;
};

struct lguest_dma_info
{
	struct list_head list;
	struct futex_key key;
	u32 dmas;
	u16 next_dma;
	u16 num_dmas;
	u16 guestid;
	u8 interrupt; 	/* 0 when not registered */
};
struct pgdir
{
	unsigned long cr3;
	u32 *pgdir;
};

struct tss_struct { u8 data[104];};


/* This is a guest-specific page (mapped ro) into the guest. */
struct lguest_ro_state
{
	/* Host information we need to restore when we switch back. */
	u32 host_cr3;
	struct Xgt_desc_struct host_idt_desc;
	struct Xgt_desc_struct host_gdt_desc;
	u32 host_sp;

	/* Fields which are used when guest is running. */
	struct Xgt_desc_struct guest_idt_desc;
	struct Xgt_desc_struct guest_gdt_desc;
	struct tss_struct guest_tss;
	struct desc_struct guest_idt[IDT_ENTRIES];
	struct desc_struct guest_gdt[GDT_ENTRIES];
};

/* We have two pages shared with guests, per cpu.  */
struct lguest_pages
{
	/* This is the stack page mapped rw in guest */
	char spare[BY2PG - sizeof(struct lguest_regs)];
	struct lguest_regs regs;

	/* This is the host state & guest descriptor page, ro in guest */
	struct lguest_ro_state state;
};

#define LHCALL_FLUSH_ASYNC      0        
#define LHCALL_LGUEST_INIT      1        
#define LHCALL_SHUTDOWN         2
#define LHCALL_NEW_PGTABLE      4
#define LHCALL_FLUSH_TLB        5
#define LHCALL_LOAD_IDT_ENTRY   6
#define LHCALL_SET_STACK        7
#define LHCALL_TS               8                                      
#define LHCALL_SET_CLOCKEVENT   9                                           
#define LHCALL_HALT             10                                          
#define LHCALL_SET_PMD          13     
#define LHCALL_SET_PTE          14                                          
#define LHCALL_SET_PGD          15                                            
#define LHCALL_LOAD_TLS         16                                            
#define LHCALL_NOTIFY           17                                            
#define LHCALL_LOAD_GDT_ENTRY   18                                            
#define LHCALL_SEND_INTERRUPTS  19  

/* Argument number 3 to LHCALL_LGUEST_SHUTDOWN */
#define LGUEST_SHUTDOWN_POWEROFF	1
#define LGUEST_SHUTDOWN_RESTART		2


#define LGUEST_TRAP_ENTRY 0x1F

#define LG_CLOCK_MIN_DELTA	100UL
#define LG_CLOCK_MAX_DELTA	ULONG_MAX

unsigned long
hcall(unsigned long call,
      unsigned long arg1, unsigned long arg2, unsigned long arg3, unsigned long arg4);
/*:*/

void async_hcall(unsigned long call,
		 unsigned long arg1, unsigned long arg2, unsigned long arg3, unsigned long arg4);

/* Can't use our min() macro here: needs to be a constant */
#define LGUEST_IRQS (NR_IRQS < 32 ? NR_IRQS: 32)

#define LHCALL_RING_SIZE 64
struct hcall_ring
{
	u32 eax, edx, ebx, ecx, esi; /* EVH: added esi? */
};

struct timespec {
	u32 seconds, nanoseconds;
};

/*G:032 The second method of communicating with the Host is to via "struct
 * lguest_data".  The Guest's very first hypercall is to tell the Host where
 * this is, and then the Guest and Host both publish information in it. :*/
#define NR_IRQS 32
struct lguest_data
{
	/* 512 == enabled (same as eflags in normal hardware).  The Guest
	 * changes interrupts so often that a hypercall is too slow. */
	 
	unsigned int irq_enabled;
	
	/* Fine-grained interrupt disabling by the Guest */
	
	u32 blocked_interrupts[LGUEST_IRQS/32];

	/* The Host writes the virtual address of the last page fault here,
	 * which saves the Guest a hypercall.  CR2 is the native register where
	 * this address would normally be found. */
	 
	unsigned long cr2;

	/* Wallclock time set by the Host. */
	
	struct timespec time;
	
	/*
	 * Interrupt pending set by the Host.  The Guest should do a hypercall
	 * if it re-enables interrupts and sees this set (to X86_EFLAGS_IF).
	 */
	 
	int irq_pending;	

	/* Async hypercall ring.  Instead of directly making hypercalls, we can
	 * place them in here for processing the next time the Host wants.
	 * This batching can be quite efficient. */

	/* 0xFF == done (set by Host), 0 == pending (set by Guest). */
	
	u8 hcall_status[LHCALL_RING_SIZE];
	/* The actual registers for the hypercalls. */
	struct hcall_ring hcalls[LHCALL_RING_SIZE];

/* Fields initialized by the Host at boot: */
	/* Memory not to try to access */
	unsigned long reserve_mem;

	/* KHz for the TSC clock. */
	u32 tsc_khz;

        /* Page where the top-level pagetable is */
        unsigned long pgdir;

/* Fields initialized by the Guest at boot: */
	/* Instruction range to suppress interrupts even if enabled */
	unsigned long noirq_start, noirq_end;

   	/* Address above which page tables are all identical. */
	unsigned long kernel_address;

     /* The vector to try to use for system calls (0x40 or 0x80). */
        unsigned int syscall_vec;
};

extern void lguest_noirq_start(void);
extern void lguest_noirq_end(void);
extern struct lguest_data lguest_data;

/* defines for I/O etc. */
/* Everything the "lguest" userspace program needs to know. */

struct lguest_device_desc {
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
};

/* layout: These come first in the config array, there are num_vq of them */
struct lguest_vqconfig {
        /* The number of entries in the virtio_ring */
        u16 num;
        /* The interrupt we get when something happens. */
        u16 irq;
        /* The page number of the virtio ring for this device. */
        u32 pfn;
};  

enum lguest_req
{
	LHREQ_INITIALIZE, /* + pfnlimit, pgdir, start, pageoffset */
	LHREQ_GETDMA, /* + unused (old lguest) */
	LHREQ_IRQ, /* + irq */
	LHREQ_BREAK, /* + on/off flag (on blocks until someone does off) */
};

/* and a few bits from Linux itself. */
#define PARAVIRT_LAZY_NONE 0
#define PARAVIRT_LAZY_MMU  1
#define PARAVIRT_LAZY_CPU  2
#define PARAVIRT_LAZY_FLUSH 3
#define X86_EFLAGS_IF      0x00000200

/* e820 structs as Linux sets them up */
#define E820MAP 0x2d0           /* our map */
#define E820MAX 128             /* number of entries in E820MAP */
#define E820NR  0x1e8           /* # entries in E820MAP */

#define E820_RAM        1
#define E820_RESERVED   2
#define E820_ACPI       3
#define E820_NVS        4

#define HIGH_MEMORY     (1024*1024)

struct e820map {
    int nr_map;
    struct e820entry {
        unsigned long long addr;        /* start of memory segment */
        unsigned long long size;        /* size of memory segment */
        unsigned long type;             /* type of memory segment */
    } map[E820MAX];
};

/* prototypes */
void lguest_send_dma(unsigned long key, struct lguest_dma *dma);
void lguest_load_esp0(u32 stack);
void lguest_flush_tlb_kernel(void);
void lguest_flush_tlb_single(u32 addr);
void lguest_set_pmd(u32 *pmdp, u32 pmdval);
void lguest_set_pte(u32 *ptep, u32 pteval);
void lguest_set_pte_at(ulong pdb, u32 addr, u32 *ptep, u32 pteval);
int lguest_bind_dma(unsigned long key, struct lguest_dma *dmas,
		    unsigned int num, u8 irq);
unsigned long lguest_get_wallclock(void);
uvlong lguest_get_ns(void);
void lguest_interval_timer(u32);
int setupdma(void *buf, int len, struct lguest_dma *dma);
/* interrupt numbers -- stupid hardcodes */
/* FIX ME SOON */
#define CONSINTR 33
#define BLOCKINTR 34
#define NETINTR 40
