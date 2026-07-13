/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:     H5B2cache.c
 *
 * Purpose:     Implement v2 B-tree metadata cache methods
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5B2module.h" /* This source code file is part of the H5B2 module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5ACprivate.h" /* Metadata Cache                           */
#include "H5B2pkg.h"     /* B-Trees (Version 2)                      */
#include "H5Eprivate.h"  /* Error Handling                           */
#include "H5Fprivate.h"  /* Files                                    */
#include "H5FLprivate.h" /* Free Lists                               */
#include "H5MMprivate.h" /* Memory Management                        */

/****************/
/* Local Macros */
/****************/

/* B-tree format version #'s */
#define H5B2_HDR_VERSION  0 /* Header */
#define H5B2_INT_VERSION  0 /* Internal node */
#define H5B2_LEAF_VERSION 0 /* Leaf node */

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/

/* Metadata cache callbacks */
static herr_t H5B2__cache_hdr_get_initial_load_size(void *udata, size_t *image_len);
static htri_t H5B2__cache_hdr_verify_chksum(const void *image_ptr, size_t len, void *udata);
static void  *H5B2__cache_hdr_deserialize(const void *image, size_t len, void *udata, bool *dirty);
static herr_t H5B2__cache_hdr_image_len(const void *thing, size_t *image_len);
static herr_t H5B2__cache_hdr_serialize(const H5F_t *f, void *image, size_t len, void *thing);
static herr_t H5B2__cache_hdr_notify(H5AC_notify_action_t action, void *thing);
static herr_t H5B2__cache_hdr_free_icr(void *thing);
static herr_t H5B2__cache_hdr_refresh(H5F_t *f, void *thing, const void *image, size_t *len_ptr);

static herr_t H5B2__cache_int_get_initial_load_size(void *udata, size_t *image_len);
static htri_t H5B2__cache_int_verify_chksum(const void *image_ptr, size_t len, void *udata);
static void  *H5B2__cache_int_deserialize(const void *image, size_t len, void *udata, bool *dirty);
static herr_t H5B2__cache_int_image_len(const void *thing, size_t *image_len);
static herr_t H5B2__cache_int_serialize(const H5F_t *f, void *image, size_t len, void *thing);
static herr_t H5B2__cache_int_notify(H5AC_notify_action_t action, void *thing);
static herr_t H5B2__cache_int_free_icr(void *thing);

static herr_t H5B2__cache_leaf_get_initial_load_size(void *udata, size_t *image_len);
static htri_t H5B2__cache_leaf_verify_chksum(const void *image_ptr, size_t len, void *udata);
static void  *H5B2__cache_leaf_deserialize(const void *image, size_t len, void *udata, bool *dirty);
static herr_t H5B2__cache_leaf_image_len(const void *thing, size_t *image_len);
static herr_t H5B2__cache_leaf_serialize(const H5F_t *f, void *image, size_t len, void *thing);
static herr_t H5B2__cache_leaf_notify(H5AC_notify_action_t action, void *thing);
static herr_t H5B2__cache_leaf_free_icr(void *thing);

/*********************/
/* Package Variables */
/*********************/

/* H5B2 inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_BT2_HDR[1] = {{
    H5AC_BT2_HDR_ID,                       /* Metadata client ID */
    "v2 B-tree header",                    /* Metadata client name (for debugging) */
    H5FD_MEM_BTREE,                        /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,              /* Client class behavior flags */
    H5B2__cache_hdr_get_initial_load_size, /* 'get_initial_load_size' callback */
    NULL,                                  /* 'get_final_load_size' callback */
    H5B2__cache_hdr_verify_chksum,         /* 'verify_chksum' callback */
    H5B2__cache_hdr_deserialize,           /* 'deserialize' callback */
    H5B2__cache_hdr_image_len,             /* 'image_len' callback */
    NULL,                                  /* 'pre_serialize' callback */
    H5B2__cache_hdr_serialize,             /* 'serialize' callback */
    H5B2__cache_hdr_notify,                /* 'notify' callback */
    H5B2__cache_hdr_free_icr,              /* 'free_icr' callback */
    NULL,                                  /* 'fsf_size' callback */
    H5B2__cache_hdr_refresh,               /* VFD SWMR 'refresh' callback */
}};

/* H5B2 inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_BT2_INT[1] = {{
    H5AC_BT2_INT_ID,                       /* Metadata client ID */
    "v2 B-tree internal node",             /* Metadata client name (for debugging) */
    H5FD_MEM_BTREE,                        /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,              /* Client class behavior flags */
    H5B2__cache_int_get_initial_load_size, /* 'get_initial_load_size' callback */
    NULL,                                  /* 'get_final_load_size' callback */
    H5B2__cache_int_verify_chksum,         /* 'verify_chksum' callback */
    H5B2__cache_int_deserialize,           /* 'deserialize' callback */
    H5B2__cache_int_image_len,             /* 'image_len' callback */
    NULL,                                  /* 'pre_serialize' callback */
    H5B2__cache_int_serialize,             /* 'serialize' callback */
    H5B2__cache_int_notify,                /* 'notify' callback */
    H5B2__cache_int_free_icr,              /* 'free_icr' callback */
    NULL,                                  /* 'fsf_size' callback */
    NULL,                                  /* VFD SWMR 'refresh' callback */
}};

/* H5B2 inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_BT2_LEAF[1] = {{
    H5AC_BT2_LEAF_ID,                       /* Metadata client ID */
    "v2 B-tree leaf node",                  /* Metadata client name (for debugging) */
    H5FD_MEM_BTREE,                         /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,               /* Client class behavior flags */
    H5B2__cache_leaf_get_initial_load_size, /* 'get_initial_load_size' callback */
    NULL,                                   /* 'get_final_load_size' callback */
    H5B2__cache_leaf_verify_chksum,         /* 'verify_chksum' callback */
    H5B2__cache_leaf_deserialize,           /* 'deserialize' callback */
    H5B2__cache_leaf_image_len,             /* 'image_len' callback */
    NULL,                                   /* 'pre_serialize' callback */
    H5B2__cache_leaf_serialize,             /* 'serialize' callback */
    H5B2__cache_leaf_notify,                /* 'notify' callback */
    H5B2__cache_leaf_free_icr,              /* 'free_icr' callback */
    NULL,                                   /* 'fsf_size' callback */
    NULL,                                   /* VFD SWMR 'refresh' callback */
}};

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_hdr_get_initial_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_hdr_get_initial_load_size(void *_udata, size_t *image_len)
{
    H5B2_hdr_cache_ud_t *udata = (H5B2_hdr_cache_ud_t *)_udata; /* User data for callback */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check arguments */
    assert(udata);
    assert(udata->f);
    assert(image_len);

    /* Set the image length size */
    *image_len = H5B2_HEADER_SIZE_FILE(udata->f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2__cache_hdr_get_initial_load_size() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_hdr_verify_chksum
 *
 * Purpose:     Verify the computed checksum of the data structure is the
 *              same as the stored chksum.
 *
 * Return:      Success:        true/false
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5B2__cache_hdr_verify_chksum(const void *_image, size_t len, void H5_ATTR_UNUSED *_udata)
{
    const uint8_t *image = (const uint8_t *)_image; /* Pointer into raw data buffer */
    uint32_t       stored_chksum;                   /* Stored metadata checksum value */
    uint32_t       computed_chksum;                 /* Computed metadata checksum value */
    htri_t         ret_value = true;                /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(image);

    /* Get stored and computed checksums */
    if (H5F_get_checksums(image, len, &stored_chksum, &computed_chksum) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTGET, FAIL, "can't get checksums");

    if (stored_chksum != computed_chksum)
        ret_value = false;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2__cache_hdr_verify_chksum() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_hdr_deserialize
 *
 * Purpose:	Loads a B-tree header from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree.
 *		Failure:	NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5B2__cache_hdr_deserialize(const void *_image, size_t H5_ATTR_UNUSED len, void *_udata,
                            bool H5_ATTR_UNUSED *dirty)
{
    H5B2_hdr_t          *hdr   = NULL; /* B-tree header */
    H5B2_hdr_cache_ud_t *udata = (H5B2_hdr_cache_ud_t *)_udata;
    H5B2_create_t        cparam;                              /* B-tree creation parameters */
    H5B2_subid_t         id;                                  /* ID of B-tree class, as found in file */
    uint16_t             depth;                               /* Depth of B-tree */
    uint32_t             stored_chksum;                       /* Stored metadata checksum value */
    const uint8_t       *image     = (const uint8_t *)_image; /* Pointer into raw data buffer */
    H5B2_hdr_t          *ret_value = NULL;                    /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(image);
    assert(udata);

    /* Allocate new B-tree header and reset cache info */
    if (NULL == (hdr = H5B2__hdr_alloc(udata->f)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTALLOC, NULL, "allocation failed for B-tree header");

    /* Magic number */
    if (memcmp(image, H5B2_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC) != 0)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "wrong B-tree header signature");
    image += H5_SIZEOF_MAGIC;

    /* Version */
    if (*image++ != H5B2_HDR_VERSION)
        HGOTO_ERROR(H5E_BTREE, H5E_BADRANGE, NULL, "wrong B-tree header version");

    /* B-tree class */
    id = (H5B2_subid_t)*image++;
    if (id >= H5B2_NUM_BTREE_ID)
        HGOTO_ERROR(H5E_BTREE, H5E_BADTYPE, NULL, "incorrect B-tree type");

    /* Node size (in bytes) */
    UINT32DECODE(image, cparam.node_size);

    /* Raw key size (in bytes) */
    UINT16DECODE(image, cparam.rrec_size);

    /* Depth of tree */
    UINT16DECODE(image, depth);

    /* Split & merge %s */
    cparam.split_percent = *image++;
    cparam.merge_percent = *image++;

    /* Root node pointer */
    H5F_addr_decode(udata->f, (const uint8_t **)&image, &(hdr->root.addr));
    UINT16DECODE(image, hdr->root.node_nrec);
    H5F_DECODE_LENGTH(udata->f, image, hdr->root.all_nrec);

    /* checksum verification already done in verify_chksum cb */

    /* Metadata checksum */
    UINT32DECODE(image, stored_chksum);

    /* Sanity check */
    assert((size_t)(image - (const uint8_t *)_image) == hdr->hdr_size);

    /* Initialize B-tree header info */
    cparam.cls = H5B2_client_class_g[id];
    if (H5B2__hdr_init(hdr, &cparam, udata->ctx_udata, depth) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, NULL, "can't initialize B-tree header info");

    /* Set the B-tree header's address */
    hdr->addr = udata->addr;

    /* Sanity check */
    assert((size_t)(image - (const uint8_t *)_image) <= len);

    /* Set return value */
    ret_value = hdr;

done:
    if (!ret_value && hdr)
        if (H5B2__hdr_free(hdr) < 0)
            HDONE_ERROR(H5E_BTREE, H5E_CANTRELEASE, NULL, "can't release v2 B-tree header");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2__cache_hdr_deserialize() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_hdr_refresh
 *
 * Purpose:     VFD SWMR reader callback: examine a freshly re-read v2
 *              B-tree header image and update the live, pinned in-memory
 *              header in place, instead of evicting it.
 *
 *              A v2 B-tree header used as a dataset's chunk index (2D+
 *              chunk growth) is opened once and kept open -- and reference-
 *              count-pinned via H5B2__hdr_incr() -- for the dataset's
 *              entire lifetime (H5D__bt2_idx_open()/H5D__bt2_idx_close(),
 *              src/H5Dbtree2.c). Without this callback,
 *              H5C_evict_or_refresh_all_entries_in_page() has no way to
 *              bring a stale, rc-pinned header up to date: its fallback,
 *              H5C_evict_tagged_entries() (src/H5Ctag.c), can only unpin
 *              entries pinned via a flush-dependency relationship, not a
 *              plain reference-count pin, so it fails outright with
 *              "Pinned entries still need evicted?!".
 *
 *              Most of the header's on-disk image never legitimately
 *              changes post-creation (node_size, rrec_size, split/merge
 *              percent, B-tree class) -- those are checked defensively
 *              against the live header, since this decodes freshly
 *              re-read bytes from a (possibly corrupted) shadow file, the
 *              same posture H5B2__cache_hdr_deserialize() itself takes.
 *              The two fields that DO legitimately change as the writer
 *              inserts records are depth and the root node pointer.
 *
 *              A depth change is the tricky case: hdr->node_info[]/
 *              hdr->nat_off[] are NOT part of the on-disk image -- they're
 *              derived, sized to (old) depth, tables built once by
 *              H5B2__hdr_init(). If depth grows (a root split) and this
 *              callback only updated hdr->depth without rebuilding those
 *              tables, later traversals would index them out of bounds --
 *              worse than today's loud, safe error. So: build the new
 *              tables first (via H5B2__hdr_compute_node_info(), the same
 *              logic H5B2__hdr_init() itself uses), and only after that
 *              succeeds, free the old ones (via H5B2__hdr_free_node_info())
 *              -- never the reverse order, so a failure partway through
 *              leaves the live header exactly as it was before the failed
 *              refresh (a loud, safe HGOTO_ERROR) rather than a torn-down,
 *              half-rebuilt header still in active use by a pinned entry.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_hdr_refresh(H5F_t H5_ATTR_UNUSED *f, void *_thing, const void *_image, size_t *len_ptr)
{
    H5B2_hdr_t     *hdr   = (H5B2_hdr_t *)_thing;
    const uint8_t  *image = (const uint8_t *)_image;
    const uint8_t  *end   = image + *len_ptr - 1;
    H5B2_subid_t    id;
    uint32_t        node_size;
    uint16_t        rrec_size;
    uint16_t        new_depth;
    uint8_t         split_percent;
    uint8_t         merge_percent;
    H5B2_node_ptr_t new_root;
    uint32_t        stored_chksum;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(hdr);
    assert(hdr->cache_info.type == H5AC_BT2_HDR);
    assert(image);
    assert(len_ptr);
    assert(*len_ptr == hdr->hdr_size);

    /* Magic number */
    if (H5_IS_BUFFER_OVERFLOW(image, H5_SIZEOF_MAGIC, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    if (memcmp(image, H5B2_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC) != 0)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, FAIL, "wrong B-tree header signature");
    image += H5_SIZEOF_MAGIC;

    /* Version */
    if (H5_IS_BUFFER_OVERFLOW(image, 1, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    if (*image++ != H5B2_HDR_VERSION)
        HGOTO_ERROR(H5E_BTREE, H5E_BADRANGE, FAIL, "wrong B-tree header version");

    /* B-tree class -- must never change post-creation */
    if (H5_IS_BUFFER_OVERFLOW(image, 1, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    id = (H5B2_subid_t)*image++;
    if (id != hdr->cls->id)
        HGOTO_ERROR(H5E_BTREE, H5E_BADTYPE, FAIL, "v2 B-tree class changed underneath refresh");

    /* Node size (in bytes) -- must never change post-creation */
    if (H5_IS_BUFFER_OVERFLOW(image, 4, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    UINT32DECODE(image, node_size);
    if (node_size != hdr->node_size)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, FAIL, "v2 B-tree node size changed underneath refresh");

    /* Raw key size (in bytes) -- must never change post-creation */
    if (H5_IS_BUFFER_OVERFLOW(image, 2, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    UINT16DECODE(image, rrec_size);
    if (rrec_size != hdr->rrec_size)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, FAIL, "v2 B-tree raw record size changed underneath refresh");

    /* Depth of tree -- legitimately changes as the writer splits/merges the root */
    if (H5_IS_BUFFER_OVERFLOW(image, 2, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    UINT16DECODE(image, new_depth);

    /* Split & merge %s -- must never change post-creation */
    if (H5_IS_BUFFER_OVERFLOW(image, 2, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    split_percent = *image++;
    merge_percent = *image++;
    if (split_percent != hdr->split_percent)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, FAIL, "v2 B-tree split percent changed underneath refresh");
    if (merge_percent != hdr->merge_percent)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, FAIL, "v2 B-tree merge percent changed underneath refresh");

    /* Root node pointer -- legitimately changes on every insert/remove */
    if (H5_IS_BUFFER_OVERFLOW(image, (size_t)hdr->sizeof_addr + 2 + hdr->sizeof_size, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    H5F_addr_decode(hdr->f, (const uint8_t **)&image, &new_root.addr);
    UINT16DECODE(image, new_root.node_nrec);
    H5F_DECODE_LENGTH(hdr->f, image, new_root.all_nrec);

    /* Metadata checksum -- already verified upstream by verify_chksum */
    if (H5_IS_BUFFER_OVERFLOW(image, 4, end))
        HGOTO_ERROR(H5E_BTREE, H5E_OVERFLOW, FAIL, "ran off end of input buffer while decoding");
    UINT32DECODE(image, stored_chksum);

    /* Sanity check */
    assert((size_t)(image - (const uint8_t *)_image) == hdr->hdr_size);

    /* If the tree grew deeper, extend the depth-derived node_info[] table
     * to cover the new levels (nat_off[] never depends on depth, so it's
     * never touched here -- see H5B2__hdr_extend_node_info()'s header
     * comment for why this must be grow-only, never free-and-rebuild:
     * existing levels' free-list factories can have live allocations from
     * leaf/internal node cache entries still resident in the reader's
     * cache).
     *
     * If the tree instead got shallower (depth decreased -- not expected
     * for the append-only growth this VFD SWMR reader path is exercised
     * with today, but not assumed impossible either), node_info[] is
     * already large enough from an earlier, deeper state; nothing needs
     * reallocating; hdr->depth just tracks the smaller current value.
     * The already-allocated higher levels' factories remain harmlessly in
     * place (tracked by node_info_depth_alloc, not depth) until the header
     * itself is finally closed.
     */
    if (new_depth > hdr->depth) {
        if (H5B2__hdr_extend_node_info(hdr, new_depth) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't extend v2 B-tree node info on refresh");
    }
    hdr->depth = new_depth;

    /* Always pick up the writer's current root */
    hdr->root = new_root;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2__cache_hdr_refresh() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_hdr_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_hdr_image_len(const void *_thing, size_t *image_len)
{
    const H5B2_hdr_t *hdr = (const H5B2_hdr_t *)_thing; /* Pointer to the B-tree header */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check arguments */
    assert(hdr);
    assert(image_len);

    /* Set the image length size */
    *image_len = hdr->hdr_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2__cache_hdr_image_len() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_hdr_serialize
 *
 * Purpose:	Flushes a dirty B-tree header to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_hdr_serialize(const H5F_t *f, void *_image, size_t H5_ATTR_UNUSED len, void *_thing)
{
    H5B2_hdr_t *hdr   = (H5B2_hdr_t *)_thing; /* Pointer to the B-tree header */
    uint8_t    *image = (uint8_t *)_image;    /* Pointer into raw data buffer */
    uint32_t    metadata_chksum;              /* Computed metadata checksum value */

    FUNC_ENTER_PACKAGE_NOERR

    /* check arguments */
    assert(f);
    assert(image);
    assert(hdr);

    /* Magic number */
    H5MM_memcpy(image, H5B2_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    image += H5_SIZEOF_MAGIC;

    /* Version # */
    *image++ = H5B2_HDR_VERSION;

    /* B-tree type */
    assert(hdr->cls->id <= 255);
    *image++ = (uint8_t)hdr->cls->id;

    /* Node size (in bytes) */
    UINT32ENCODE(image, hdr->node_size);

    /* Raw key size (in bytes) */
    UINT16ENCODE(image, hdr->rrec_size);

    /* Depth of tree */
    UINT16ENCODE(image, hdr->depth);

    /* Split & merge %s */
    H5_CHECK_OVERFLOW(hdr->split_percent, /* From: */ unsigned, /* To: */ uint8_t);
    *image++ = (uint8_t)hdr->split_percent;
    H5_CHECK_OVERFLOW(hdr->merge_percent, /* From: */ unsigned, /* To: */ uint8_t);
    *image++ = (uint8_t)hdr->merge_percent;

    /* Root node pointer */
    H5F_addr_encode(f, &image, hdr->root.addr);
    UINT16ENCODE(image, hdr->root.node_nrec);
    H5F_ENCODE_LENGTH(f, image, hdr->root.all_nrec);

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(_image, (hdr->hdr_size - H5B2_SIZEOF_CHKSUM), 0);

    /* Metadata checksum */
    UINT32ENCODE(image, metadata_chksum);

    /* Sanity check */
    assert((size_t)(image - (uint8_t *)_image) == len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2__cache_hdr_serialize() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_hdr_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_hdr_notify(H5AC_notify_action_t action, void *_thing)
{
    H5B2_hdr_t *hdr       = (H5B2_hdr_t *)_thing;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /*
     * Check arguments.
     */
    assert(hdr);

    /* Check if the file was opened with SWMR-write access */
    if (hdr->swmr_write) {
        switch (action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
            case H5AC_NOTIFY_ACTION_AFTER_LOAD:
                /* do nothing */
                break;

            case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
                /* Increment the shadow epoch, forcing new modifications to
                 * internal and leaf nodes to create new shadow copies */
                hdr->shadow_epoch++;
                break;

            case H5AC_NOTIFY_ACTION_ENTRY_DIRTIED:
            case H5AC_NOTIFY_ACTION_ENTRY_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_DIRTIED:
            case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_UNSERIALIZED:
            case H5AC_NOTIFY_ACTION_CHILD_SERIALIZED:
                /* do nothing */
                break;

            case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
                /* If hdr->parent != NULL, hdr->parent is used to destroy
                 * the flush dependency before the header is evicted.
                 */
                if (hdr->parent) {
                    /* Sanity check */
                    assert(hdr->top_proxy);

                    /* Destroy flush dependency on object header proxy */
                    if (H5AC_proxy_entry_remove_child((H5AC_proxy_entry_t *)hdr->parent,
                                                      (void *)hdr->top_proxy) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNDEPEND, FAIL,
                                    "unable to destroy flush dependency between v2 B-tree and proxy");
                    hdr->parent = NULL;
                } /* end if */

                /* Detach from 'top' proxy for extensible array */
                if (hdr->top_proxy) {
                    if (H5AC_proxy_entry_remove_child(hdr->top_proxy, hdr) < 0)
                        HGOTO_ERROR(
                            H5E_BTREE, H5E_CANTUNDEPEND, FAIL,
                            "unable to destroy flush dependency between header and v2 B-tree 'top' proxy");
                    /* Don't reset hdr->top_proxy here, it's destroyed when the header is freed -QAK */
                } /* end if */
                break;

            default:
#ifdef NDEBUG
                HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, FAIL, "unknown action from metadata cache");
#else     /* NDEBUG */
                assert(0 && "Unknown action?!?");
#endif    /* NDEBUG */
        } /* end switch */
    }     /* end if */
    else
        assert(NULL == hdr->parent);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2__cache_hdr_notify() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_hdr_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_hdr_free_icr(void *thing)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(thing);

    /* Destroy v2 B-tree header */
    if (H5B2__hdr_free((H5B2_hdr_t *)thing) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to free v2 B-tree header");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2__cache_hdr_free_icr() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_int_get_initial_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_int_get_initial_load_size(void *_udata, size_t *image_len)
{
    H5B2_internal_cache_ud_t *udata = (H5B2_internal_cache_ud_t *)_udata; /* User data for callback */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check arguments */
    assert(udata);
    assert(udata->hdr);
    assert(image_len);

    /* Set the image length size */
    *image_len = udata->hdr->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2__cache_int_get_initial_load_size() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_int_verify_chksum
 *
 * Purpose:     Verify the computed checksum of the data structure is the
 *              same as the stored chksum.
 *
 * Return:      Success:        true/false
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5B2__cache_int_verify_chksum(const void *_image, size_t H5_ATTR_UNUSED len, void *_udata)
{
    const uint8_t            *image = (const uint8_t *)_image;            /* Pointer into raw data buffer */
    H5B2_internal_cache_ud_t *udata = (H5B2_internal_cache_ud_t *)_udata; /* Pointer to user data */
    size_t                    chk_size;         /* Exact size of the node with checksum at the end */
    uint32_t                  stored_chksum;    /* Stored metadata checksum value */
    uint32_t                  computed_chksum;  /* Computed metadata checksum value */
    htri_t                    ret_value = true; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(image);
    assert(udata);

    /* Internal node prefix header + records + child pointer triplets: size with checksum at the end */
    chk_size = H5B2_INT_PREFIX_SIZE + (udata->nrec * udata->hdr->rrec_size) +
               ((size_t)(udata->nrec + 1) * H5B2_INT_POINTER_SIZE(udata->hdr, udata->depth));

    /* Get stored and computed checksums */
    if (H5F_get_checksums(image, chk_size, &stored_chksum, &computed_chksum) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTGET, FAIL, "can't get checksums");

    if (stored_chksum != computed_chksum)
        ret_value = false;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2__cache_int_verify_chksum() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_int_deserialize
 *
 * Purpose:	Deserialize a B-tree internal node from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree internal node.
 *              Failure:        NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5B2__cache_int_deserialize(const void *_image, size_t H5_ATTR_UNUSED len, void *_udata,
                            bool H5_ATTR_UNUSED *dirty)
{
    H5B2_internal_cache_ud_t *udata    = (H5B2_internal_cache_ud_t *)_udata; /* Pointer to user data */
    H5B2_internal_t          *internal = NULL;                               /* Internal node read */
    const uint8_t            *image    = (const uint8_t *)_image; /* Pointer into raw data buffer */
    uint8_t                  *native;                             /* Pointer to native record info */
    H5B2_node_ptr_t          *int_node_ptr;                       /* Pointer to node pointer info */
    uint32_t                  stored_chksum;                      /* Stored metadata checksum value */
    unsigned                  u;                                  /* Local index variable */
    H5B2_internal_t          *ret_value = NULL;                   /* Return value */
    int                       node_nrec = 0;

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(image);
    assert(udata);

    /* Allocate new internal node and reset cache info */
    if (NULL == (internal = H5FL_CALLOC(H5B2_internal_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Increment ref. count on B-tree header */
    if (H5B2__hdr_incr(udata->hdr) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTINC, NULL, "can't increment ref. count on B-tree header");

    /* Share B-tree information */
    internal->hdr          = udata->hdr;
    internal->parent       = udata->parent;
    internal->shadow_epoch = udata->hdr->shadow_epoch;

    /* Magic number */
    if (memcmp(image, H5B2_INT_MAGIC, (size_t)H5_SIZEOF_MAGIC) != 0)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "wrong B-tree internal node signature");
    image += H5_SIZEOF_MAGIC;

    /* Version */
    if (*image++ != H5B2_INT_VERSION)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "wrong B-tree internal node version");

    /* B-tree type */
    if (*image++ != (uint8_t)udata->hdr->cls->id)
        HGOTO_ERROR(H5E_BTREE, H5E_BADTYPE, NULL, "incorrect B-tree type");

    /* Allocate space for the native keys in memory */
    if (NULL ==
        (internal->int_native = (uint8_t *)H5FL_FAC_MALLOC(udata->hdr->node_info[udata->depth].nat_rec_fac)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                    "memory allocation failed for B-tree internal native keys");

    /* Allocate space for the node pointers in memory */
    if (NULL == (internal->node_ptrs =
                     (H5B2_node_ptr_t *)H5FL_FAC_MALLOC(udata->hdr->node_info[udata->depth].node_ptr_fac)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                    "memory allocation failed for B-tree internal node pointers");

    /* Set the number of records in the leaf & it's depth */
    internal->nrec  = udata->nrec;
    internal->depth = udata->depth;

    /* Deserialize records for internal node */
    native = internal->int_native;
    for (u = 0; u < internal->nrec; u++) {
        /* Decode record */
        if ((udata->hdr->cls->decode)(image, native, udata->hdr->cb_ctx) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "unable to decode B-tree record");

        /* Move to next record */
        image += udata->hdr->rrec_size;
        native += udata->hdr->cls->nrec_size;
    } /* end for */

    /* Deserialize node pointers for internal node */
    int_node_ptr = internal->node_ptrs;
    for (u = 0; u < (unsigned)(internal->nrec + 1); u++) {
        /* Decode node pointer */
        H5F_addr_decode(udata->f, (const uint8_t **)&image, &(int_node_ptr->addr));
        UINT64DECODE_VAR(image, node_nrec, udata->hdr->max_nrec_size);
        H5_CHECKED_ASSIGN(int_node_ptr->node_nrec, uint16_t, node_nrec, int);
        if (udata->depth > 1)
            UINT64DECODE_VAR(image, int_node_ptr->all_nrec,
                             udata->hdr->node_info[udata->depth - 1].cum_max_nrec_size);
        else
            int_node_ptr->all_nrec = int_node_ptr->node_nrec;

        /* Move to next node pointer */
        int_node_ptr++;
    } /* end for */

    /* checksum verification already done in verify_chksum cb */

    /* Metadata checksum */
    UINT32DECODE(image, stored_chksum);

    /* Sanity check parsing */
    assert((size_t)(image - (const uint8_t *)_image) <= len);

    /* Set return value */
    ret_value = internal;

done:
    if (!ret_value && internal)
        if (H5B2__internal_free(internal) < 0)
            HDONE_ERROR(H5E_BTREE, H5E_CANTFREE, NULL, "unable to destroy B-tree internal node");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2__cache_int_deserialize() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_int_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_int_image_len(const void *_thing, size_t *image_len)
{
    const H5B2_internal_t *internal =
        (const H5B2_internal_t *)_thing; /* Pointer to the B-tree internal node */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check arguments */
    assert(internal);
    assert(internal->hdr);
    assert(image_len);

    /* Set the image length size */
    *image_len = internal->hdr->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2__cache_int_image_len() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_int_serialize
 *
 * Purpose:	Serializes a B-tree internal node for writing to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_int_serialize(const H5F_t *f, void *_image, size_t H5_ATTR_UNUSED len, void *_thing)
{
    H5B2_internal_t *internal = (H5B2_internal_t *)_thing; /* Pointer to the B-tree internal node */
    uint8_t         *image    = (uint8_t *)_image;         /* Pointer into raw data buffer */
    uint8_t         *native;                               /* Pointer to native record info */
    H5B2_node_ptr_t *int_node_ptr;                         /* Pointer to node pointer info */
    uint32_t         metadata_chksum;                      /* Computed metadata checksum value */
    unsigned         u;                                    /* Local index variable */
    herr_t           ret_value = SUCCEED;                  /* Return value */

    FUNC_ENTER_PACKAGE

    /* check arguments */
    assert(f);
    assert(image);
    assert(internal);
    assert(internal->hdr);

    /* Magic number */
    H5MM_memcpy(image, H5B2_INT_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    image += H5_SIZEOF_MAGIC;

    /* Version # */
    *image++ = H5B2_INT_VERSION;

    /* B-tree type */
    assert(internal->hdr->cls->id <= 255);
    *image++ = (uint8_t)internal->hdr->cls->id;
    assert((size_t)(image - (uint8_t *)_image) == (H5B2_INT_PREFIX_SIZE - H5B2_SIZEOF_CHKSUM));

    /* Serialize records for internal node */
    native = internal->int_native;
    for (u = 0; u < internal->nrec; u++) {
        /* Encode record */
        if ((internal->hdr->cls->encode)(image, native, internal->hdr->cb_ctx) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree record");

        /* Move to next record */
        image += internal->hdr->rrec_size;
        native += internal->hdr->cls->nrec_size;
    } /* end for */

    /* Serialize node pointers for internal node */
    int_node_ptr = internal->node_ptrs;
    for (u = 0; u < (unsigned)(internal->nrec + 1); u++) {
        /* Encode node pointer */
        H5F_addr_encode(f, &image, int_node_ptr->addr);
        UINT64ENCODE_VAR(image, int_node_ptr->node_nrec, internal->hdr->max_nrec_size);
        if (internal->depth > 1)
            UINT64ENCODE_VAR(image, int_node_ptr->all_nrec,
                             internal->hdr->node_info[internal->depth - 1].cum_max_nrec_size);

        /* Move to next node pointer */
        int_node_ptr++;
    } /* end for */

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(_image, (size_t)(image - (uint8_t *)_image), 0);

    /* Metadata checksum */
    UINT32ENCODE(image, metadata_chksum);

    /* Sanity check */
    assert((size_t)(image - (uint8_t *)_image) <= len);

    /* Clear rest of internal node */
    memset(image, 0, len - (size_t)(image - (uint8_t *)_image));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2__cache_int_serialize() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_int_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_int_notify(H5AC_notify_action_t action, void *_thing)
{
    H5B2_internal_t *internal  = (H5B2_internal_t *)_thing;
    herr_t           ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /*
     * Check arguments.
     */
    assert(internal);
    assert(internal->hdr);

    /* Check if the file was opened with SWMR-write access */
    if (internal->hdr->swmr_write) {
        switch (action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
            case H5AC_NOTIFY_ACTION_AFTER_LOAD:
                /* Create flush dependency on parent */
                if (H5B2__create_flush_depend((H5AC_info_t *)internal->parent, (H5AC_info_t *)internal) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTDEPEND, FAIL, "unable to create flush dependency");
                break;

            case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
            case H5AC_NOTIFY_ACTION_ENTRY_DIRTIED:
            case H5AC_NOTIFY_ACTION_ENTRY_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_DIRTIED:
            case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_UNSERIALIZED:
            case H5AC_NOTIFY_ACTION_CHILD_SERIALIZED:
                /* do nothing */
                break;

            case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
                /* Destroy flush dependency on parent */
                if (H5B2__destroy_flush_depend((H5AC_info_t *)internal->parent, (H5AC_info_t *)internal) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency");

                /* Detach from 'top' proxy for v2 B-tree */
                if (internal->top_proxy) {
                    if (H5AC_proxy_entry_remove_child(internal->top_proxy, internal) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNDEPEND, FAIL,
                                    "unable to destroy flush dependency between internal node and v2 B-tree "
                                    "'top' proxy");
                    internal->top_proxy = NULL;
                } /* end if */
                break;

            default:
#ifdef NDEBUG
                HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, FAIL, "unknown action from metadata cache");
#else     /* NDEBUG */
                assert(0 && "Unknown action?!?");
#endif    /* NDEBUG */
        } /* end switch */
    }     /* end if */
    else
        assert(NULL == internal->top_proxy);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2__cache_int_notify() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_int_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_int_free_icr(void *_thing)
{
    H5B2_internal_t *internal  = (H5B2_internal_t *)_thing;
    herr_t           ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(internal);

    /* Release v2 B-tree internal node */
    if (H5B2__internal_free(internal) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to release v2 B-tree internal node");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2__cache_int_free_icr() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_leaf_get_initial_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_leaf_get_initial_load_size(void *_udata, size_t *image_len)
{
    H5B2_leaf_cache_ud_t *udata = (H5B2_leaf_cache_ud_t *)_udata; /* User data for callback */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check arguments */
    assert(udata);
    assert(udata->hdr);
    assert(image_len);

    /* Set the image length size */
    *image_len = udata->hdr->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2__cache_leaf_get_initial_load_size() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_leaf_verify_chksum
 *
 * Purpose:     Verify the computed checksum of the data structure is the
 *              same as the stored chksum.
 *
 * Return:      Success:        true/false
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5B2__cache_leaf_verify_chksum(const void *_image, size_t H5_ATTR_UNUSED len, void *_udata)
{
    const uint8_t            *image = (const uint8_t *)_image;            /* Pointer into raw data buffer */
    H5B2_internal_cache_ud_t *udata = (H5B2_internal_cache_ud_t *)_udata; /* Pointer to user data */
    size_t                    chk_size;         /* Exact size of the node with checksum at the end */
    uint32_t                  stored_chksum;    /* Stored metadata checksum value */
    uint32_t                  computed_chksum;  /* Computed metadata checksum value */
    htri_t                    ret_value = true; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(image);
    assert(udata);

    /* Leaf node prefix header + records: size with checksum at the end */
    chk_size = H5B2_LEAF_PREFIX_SIZE + (udata->nrec * udata->hdr->rrec_size);

    /* Get stored and computed checksums */
    if (H5F_get_checksums(image, chk_size, &stored_chksum, &computed_chksum) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTGET, FAIL, "can't get checksums");

    if (stored_chksum != computed_chksum)
        ret_value = false;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2__cache_leaf_verify_chksum() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_leaf_deserialize
 *
 * Purpose:	Deserialize a B-tree leaf from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree leaf node.
 *		Failure:	NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5B2__cache_leaf_deserialize(const void *_image, size_t H5_ATTR_UNUSED len, void *_udata,
                             bool H5_ATTR_UNUSED *dirty)
{
    H5B2_leaf_cache_ud_t *udata = (H5B2_leaf_cache_ud_t *)_udata;
    H5B2_leaf_t          *leaf  = NULL;                    /* Pointer to lead node loaded */
    const uint8_t        *image = (const uint8_t *)_image; /* Pointer into raw data buffer */
    uint8_t              *native;                          /* Pointer to native keys */
    uint32_t              stored_chksum;                   /* Stored metadata checksum value */
    unsigned              u;                               /* Local index variable */
    H5B2_leaf_t          *ret_value = NULL;                /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(image);
    assert(udata);

    /* Allocate new leaf node and reset cache info */
    if (NULL == (leaf = H5FL_CALLOC(H5B2_leaf_t)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTALLOC, NULL, "memory allocation failed");

    /* Increment ref. count on B-tree header */
    if (H5B2__hdr_incr(udata->hdr) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTINC, NULL, "can't increment ref. count on B-tree header");

    /* Share B-tree header information */
    leaf->hdr          = udata->hdr;
    leaf->parent       = udata->parent;
    leaf->shadow_epoch = udata->hdr->shadow_epoch;

    /* Magic number */
    if (memcmp(image, H5B2_LEAF_MAGIC, (size_t)H5_SIZEOF_MAGIC) != 0)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "wrong B-tree leaf node signature");
    image += H5_SIZEOF_MAGIC;

    /* Version */
    if (*image++ != H5B2_LEAF_VERSION)
        HGOTO_ERROR(H5E_BTREE, H5E_BADRANGE, NULL, "wrong B-tree leaf node version");

    /* B-tree type */
    if (*image++ != (uint8_t)udata->hdr->cls->id)
        HGOTO_ERROR(H5E_BTREE, H5E_BADTYPE, NULL, "incorrect B-tree type");

    /* Allocate space for the native keys in memory */
    if (NULL == (leaf->leaf_native = (uint8_t *)H5FL_FAC_MALLOC(udata->hdr->node_info[0].nat_rec_fac)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTALLOC, NULL, "memory allocation failed for B-tree leaf native keys");

    /* Set the number of records in the leaf */
    leaf->nrec = udata->nrec;

    /* Deserialize records for leaf node */
    native = leaf->leaf_native;
    for (u = 0; u < leaf->nrec; u++) {
        /* Decode record */
        if ((udata->hdr->cls->decode)(image, native, udata->hdr->cb_ctx) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, NULL, "unable to decode B-tree record");

        /* Move to next record */
        image += udata->hdr->rrec_size;
        native += udata->hdr->cls->nrec_size;
    } /* end for */

    /* checksum verification already done in verify_chksum cb */

    /* Metadata checksum */
    UINT32DECODE(image, stored_chksum);

    /* Sanity check parsing */
    assert((size_t)(image - (const uint8_t *)_image) <= udata->hdr->node_size);

    /* Sanity check */
    assert((size_t)(image - (const uint8_t *)_image) <= len);

    /* Set return value */
    ret_value = leaf;

done:
    if (!ret_value && leaf)
        if (H5B2__leaf_free(leaf) < 0)
            HDONE_ERROR(H5E_BTREE, H5E_CANTFREE, NULL, "unable to destroy B-tree leaf node");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2__cache_leaf_deserialize() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_leaf_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_leaf_image_len(const void *_thing, size_t *image_len)
{
    const H5B2_leaf_t *leaf = (const H5B2_leaf_t *)_thing; /* Pointer to the B-tree leaf node  */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check arguments */
    assert(leaf);
    assert(leaf->hdr);
    assert(image_len);

    /* Set the image length size */
    *image_len = leaf->hdr->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2__cache_leaf_image_len() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_leaf_serialize
 *
 * Purpose:	Serializes a B-tree leaf node for writing to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_leaf_serialize(const H5F_t H5_ATTR_UNUSED *f, void *_image, size_t H5_ATTR_UNUSED len,
                           void *_thing)
{
    H5B2_leaf_t *leaf  = (H5B2_leaf_t *)_thing; /* Pointer to the B-tree leaf node  */
    uint8_t     *image = (uint8_t *)_image;     /* Pointer into raw data buffer */
    uint8_t     *native;                        /* Pointer to native keys */
    uint32_t     metadata_chksum;               /* Computed metadata checksum value */
    unsigned     u;                             /* Local index variable */
    herr_t       ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_PACKAGE

    /* check arguments */
    assert(f);
    assert(image);
    assert(leaf);
    assert(leaf->hdr);

    /* magic number */
    H5MM_memcpy(image, H5B2_LEAF_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    image += H5_SIZEOF_MAGIC;

    /* version # */
    *image++ = H5B2_LEAF_VERSION;

    /* B-tree type */
    assert(leaf->hdr->cls->id <= 255);
    *image++ = (uint8_t)leaf->hdr->cls->id;
    assert((size_t)(image - (uint8_t *)_image) == (H5B2_LEAF_PREFIX_SIZE - H5B2_SIZEOF_CHKSUM));

    /* Serialize records for leaf node */
    native = leaf->leaf_native;
    for (u = 0; u < leaf->nrec; u++) {
        /* Encode record */
        if ((leaf->hdr->cls->encode)(image, native, leaf->hdr->cb_ctx) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree record");

        /* Move to next record */
        image += leaf->hdr->rrec_size;
        native += leaf->hdr->cls->nrec_size;
    } /* end for */

    /* Compute metadata checksum */
    metadata_chksum =
        H5_checksum_metadata(_image, (size_t)((const uint8_t *)image - (const uint8_t *)_image), 0);

    /* Metadata checksum */
    UINT32ENCODE(image, metadata_chksum);

    /* Sanity check */
    assert((size_t)(image - (uint8_t *)_image) <= len);

    /* Clear rest of leaf node */
    memset(image, 0, len - (size_t)(image - (uint8_t *)_image));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2__cache_leaf_serialize() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_leaf_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_leaf_notify(H5AC_notify_action_t action, void *_thing)
{
    H5B2_leaf_t *leaf      = (H5B2_leaf_t *)_thing;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /*
     * Check arguments.
     */
    assert(leaf);
    assert(leaf->hdr);

    /* Check if the file was opened with SWMR-write access */
    if (leaf->hdr->swmr_write) {
        switch (action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
            case H5AC_NOTIFY_ACTION_AFTER_LOAD:
                /* Create flush dependency on parent */
                if (H5B2__create_flush_depend((H5AC_info_t *)leaf->parent, (H5AC_info_t *)leaf) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTDEPEND, FAIL, "unable to create flush dependency");
                break;

            case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
            case H5AC_NOTIFY_ACTION_ENTRY_DIRTIED:
            case H5AC_NOTIFY_ACTION_ENTRY_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_DIRTIED:
            case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_UNSERIALIZED:
            case H5AC_NOTIFY_ACTION_CHILD_SERIALIZED:
                /* do nothing */
                break;

            case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
                /* Destroy flush dependency on parent */
                if (H5B2__destroy_flush_depend((H5AC_info_t *)leaf->parent, (H5AC_info_t *)leaf) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency");

                /* Detach from 'top' proxy for v2 B-tree */
                if (leaf->top_proxy) {
                    if (H5AC_proxy_entry_remove_child(leaf->top_proxy, leaf) < 0)
                        HGOTO_ERROR(
                            H5E_BTREE, H5E_CANTUNDEPEND, FAIL,
                            "unable to destroy flush dependency between leaf node and v2 B-tree 'top' proxy");
                    leaf->top_proxy = NULL;
                } /* end if */
                break;

            default:
#ifdef NDEBUG
                HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, FAIL, "unknown action from metadata cache");
#else     /* NDEBUG */
                assert(0 && "Unknown action?!?");
#endif    /* NDEBUG */
        } /* end switch */
    }     /* end if */
    else
        assert(NULL == leaf->top_proxy);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2__cache_leaf_notify() */

/*-------------------------------------------------------------------------
 * Function:	H5B2__cache_leaf_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2__cache_leaf_free_icr(void *_thing)
{
    H5B2_leaf_t *leaf      = (H5B2_leaf_t *)_thing;
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments */
    assert(leaf);

    /* Destroy v2 B-tree leaf node */
    if (H5B2__leaf_free(leaf) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree leaf node");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2__cache_leaf_free_icr() */
