
/* An interface for efficient virtio implementation, currently for use by KVM
 * and lguest, but hopefully others soon.  Do NOT change this since it will
 * break existing servers and clients.
 *
 * This header is BSD licensed so anyone can use the definitions to implement
 * compatible drivers/servers.
 *
 * Copyright Rusty Russell IBM Corporation 2007. */

/* This marks a buffer as continuing via the next field. */
#define VRING_DESC_F_NEXT	1
/* This marks a buffer as write-only (otherwise read-only). */
#define VRING_DESC_F_WRITE	2
/* This means the buffer contains a list of buffer descriptors. */
#define VRING_DESC_F_INDIRECT	4

/* The Host uses this in used->flags to advise the Guest: don't kick me when
 * you add a buffer.  It's unreliable, so it's simply an optimization.  Guest
 * will still kick if it's out of buffers. */
#define VRING_USED_F_NO_NOTIFY	1
/* The Guest uses this in avail->flags to advise the Host: don't interrupt me
 * when you consume a buffer.  It's unreliable, so it's simply an
 * optimization.  */
#define VRING_AVAIL_F_NO_INTERRUPT	1

/* We support indirect buffer descriptors (we do right?) */
#define VIRTIO_RING_F_INDIRECT_DESC	28

/* Virtio devices use a standardized configuration space to define their
 * features and pass configuration information, but each implementation can
 * store and access that space differently. */

/* Status byte for guest to report progress, and synchronize features. */
/* We have seen device and processed generic fields (VIRTIO_CONFIG_F_VIRTIO) */
#define VIRTIO_CONFIG_S_ACKNOWLEDGE	1
/* We have found a driver for the device. */
#define VIRTIO_CONFIG_S_DRIVER		2
/* Driver has used its parts of the config, and is happy */
#define VIRTIO_CONFIG_S_DRIVER_OK	4
/* We've given up on this device. */
#define VIRTIO_CONFIG_S_FAILED		0x80

/* Some virtio feature bits (currently bits 28 through 31) are reserved for the
 * transport being used (eg. virtio_ring), the rest are per-device feature
 * bits. */
#define VIRTIO_TRANSPORT_F_START	28
#define VIRTIO_TRANSPORT_F_END		32

/* Do we get callbacks when the ring is completely used, even if we've
 * suppressed them? */
#define VIRTIO_F_NOTIFY_ON_EMPTY	24

typedef uvlong u64;

/* Virtio ring descriptors: 16 bytes.  These can chain together via "next". */
struct vring_desc
{
	/* Address (guest-physical). */
	u64 addr;
	/* Length. */
	u32 len;
	/* The flags as indicated above. */
	u16 flags;
	/* We chain unused descriptors via this, too */
	u16 next;
};

struct vring_avail
{
	u16 flags;
	u16 idx;
	u16 ring[];
};

/* u32 is used here for ids for padding reasons. */
struct vring_used_elem
{
	/* Index of start of used descriptor chain. */
	u32 id;
	/* Total length of the descriptor chain which was used (written to) */
	u32 len;
};

struct vring_used
{
	u16 flags;
	u16 idx;
	struct vring_used_elem ring[];
};

struct vring {
	unsigned int num;

	struct vring_desc *desc;

	struct vring_avail *avail;

	struct vring_used *used;
};

/* The standard layout for the ring is a continuous chunk of memory which looks
 * like this.  We assume num is a power of 2.
 *
 * struct vring
 * {
 *	// The actual descriptors (16 bytes each)
 *	struct vring_desc desc[num];
 *
 *	// A ring of available descriptor heads with free-running index.
 *	u16 avail_flags;
 *	u16 avail_idx;
 *	u16 available[num];
 *
 *	// Padding to the next page boundary.
 *	char pad[];
 *
 *	// A ring of used descriptor heads with free-running index.
 *	u16 used_flags;
 *	u16 used_idx;
 *	struct vring_used_elem used[num];
 * };
 */

/* normalized from the lguest passed-in data */

struct vqring {
	int num;
	int irq;
	ulong ppages; /* physical page address */
	void *pages;
	struct vring vring;
	/* Other side has made a mess, don't try any more. */
	int broken;

	/* Number of free buffers */
	unsigned int num_free;
	/* Head of free buffer list. */
	unsigned int free_head;
	/* Number we've added since last sync. */
	unsigned int num_added;

	/* Last used index we've seen. */
	u16 last_used_idx;

	/* How to notify other side. FIXME: commonalize hcalls! */
	void (*notify)(struct virtqueue *vq);

#ifdef DEBUG
	/* They're supposed to lock for us. */
	unsigned int in_use;
#endif

	/* Tokens for callbacks. */
	unsigned char *data[128];

	/* and one queue for each ring. Good idea? Who knows? */
	/* need to model this after lgether, which was zero-copy. Later. */
	Queue *q;
};

#ifdef NO
static inline void vring_init(struct vring *vr, unsigned int num, void *p,
			      unsigned long pagesize)
{
	vr->num = num;
	vr->desc = p;
	vr->avail = p + num*sizeof(struct vring_desc);
	vr->used = (void *)(((unsigned long)&vr->avail->ring[num] + pagesize-1)
			    & ~(pagesize - 1));
}

static inline unsigned vring_size(unsigned int num, unsigned long pagesize)
{
	return ((sizeof(struct vring_desc) * num + sizeof(u16) * (2 + num)
		 + pagesize - 1) & ~(pagesize - 1))
		+ sizeof(u16) * 2 + sizeof(struct vring_used_elem) * num;
}

#ifdef __KERNEL__
#include <linux/irqreturn.h>
struct virtio_device;
struct virtqueue;

struct virtqueue *vring_new_virtqueue(unsigned int num,
				      struct virtio_device *vdev,
				      void *pages,
				      void (*notify)(struct virtqueue *vq),
				      void (*callback)(struct virtqueue *vq));
void vring_del_virtqueue(struct virtqueue *vq);

irqreturn_t vring_interrupt(int irq, void *_vq);
#endif

#endif