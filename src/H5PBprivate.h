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
 * Created:		H5PBprivate.h
 *
 *-------------------------------------------------------------------------
 */

#ifndef H5PBprivate_H
#define H5PBprivate_H

/* Private headers needed by this header */
#include "H5private.h"   /* Generic Functions			*/
#include "H5Fprivate.h"  /* File access				*/
#include "H5FLprivate.h" /* Free Lists                           */
#include "H5SLprivate.h" /* Skip List (mf_slist_ptr only -- see H5PB_t) */

/**************************/
/* Library Private Macros */
/**************************/

/* "Actual" default page buffer size (this is the size of the page buffer used when the value in the property
 * list is H5F_PAGE_BUFFER_SIZE_DEFAULT) */
#define H5PB_SIZE_DEFAULT_VALUE 0

/* Number of hash buckets in the page-buffer index; must be a power of 2 --
 * see H5PB__HASH_FCN() in H5PBpkg.h. */
#define H5PB__HASH_TABLE_LEN 4096

/* Number of distinct access-statistics categories: metadata, raw data,
 * multi-page metadata entry (VFD SWMR only). */
#define H5PB__NUM_STAT_TYPES 3
#define H5PB__STATS_MD       0
#define H5PB__STATS_RD       1
#define H5PB__STATS_MPMDE    2

/****************************/
/* Library Private Typedefs */
/****************************/

/* Forward declaration for a page buffer entry */
struct H5PB_entry_t;

/* Typedef for the main structure for the page buffer */
typedef struct H5PB_t {
    uint32_t magic;          /* H5PB__H5PB_T_MAGIC; sanity-check field */
    size_t   max_size;       /* The total page buffer size */
    size_t   page_size;      /* Size of a single page */
    unsigned min_meta_perc;  /* Minimum ratio of metadata entries required before evicting meta entries */
    unsigned min_raw_perc;   /* Minimum ratio of raw data entries required before evicting raw entries */
    unsigned min_meta_count; /* Minimum # of entries for metadata */
    unsigned min_raw_count;  /* Minimum # of entries for raw data */

    /* Hash-table index of all active page entries (H5PB__HASH_TABLE_LEN
     * buckets, chained on H5PB_entry_t::ht_next/ht_prev), plus an
     * insertion-ordered index list (il_head/il_tail, via il_next/il_prev)
     * used for unordered full-index traversal (flush, destroy). Ordered
     * iteration is not required: the shadow index is sorted separately in
     * the F-layer, and flush walks the index unordered (matches the
     * reference implementation this was restored from). */
    struct H5PB_entry_t *(ht[H5PB__HASH_TABLE_LEN]);
    int64_t              index_len;        /* Number of entries in the index (curr_pages + mpmde_count) */
    int64_t              clean_index_len;  /* Number of clean entries in the index */
    int64_t              dirty_index_len;  /* Number of dirty entries in the index */
    int64_t              index_size;       /* Total size (bytes) of entries in the index */
    int64_t              clean_index_size; /* Total size of clean entries in the index */
    int64_t              dirty_index_size; /* Total size of dirty entries in the index */
    int64_t              il_len;           /* Number of entries on the index list; must equal index_len */
    int64_t              il_size; /* Total size of entries on the index list; must equal index_size */
    struct H5PB_entry_t *il_head; /* Head pointer of the index list */
    struct H5PB_entry_t *il_tail; /* Tail pointer of the index list */

    /* Separate, small staging structure for the free-space/MF layer's "new
     * page" notifications (H5PB_add_new_page()): entries here are NOT part
     * of the main index above and are consumed (removed) once the page is
     * actually written for the first time, at which point the real entry is
     * inserted into the main index. Independent of the primary-index data
     * structure choice; left as a skip list. */
    H5SL_t *mf_slist_ptr;

    int64_t curr_pages;    /* Number of one-page entries in the index (curr_md_pages + curr_rd_pages) */
    int64_t curr_md_pages; /* Number of one-page metadata entries in the index */
    int64_t curr_rd_pages; /* Number of one-page raw-data entries in the index */
    int64_t mpmde_count;   /* Number of multi-page metadata entries (VFD SWMR).  Never on
                            * the LRU and not included in curr_pages, since they aren't
                            * exactly one page in size. */

    int64_t              LRU_len;      /* Number of entries in the LRU (identical to curr_pages) */
    int64_t              LRU_size;     /* Total size (bytes) of entries in the LRU */
    struct H5PB_entry_t *LRU_head_ptr; /* Head pointer of the LRU */
    struct H5PB_entry_t *LRU_tail_ptr; /* Tail pointer of the LRU */

    H5FL_fac_head_t *page_fac; /* Factory for allocating pages */

    /* VFD SWMR fields */
    hbool_t  vfd_swmr;        /* TRUE if the file is opened with VFD SWMR */
    hbool_t  vfd_swmr_writer; /* TRUE if this is the VFD SWMR writer */
    uint64_t cur_tick;        /* Current tick as known to the page buffer */

    /* Delayed write list: entries whose write to the HDF5 file must be
     * delayed until they have appeared in the shadow index for max_lag
     * ticks, to avoid "message from the future" bugs on readers.  Kept
     * sorted so delay_write_until is non-increasing head -> tail.
     */
    int64_t              max_delay;    /* Maximum delay of any entry in a tick */
    int64_t              dwl_len;      /* Number of entries on the delayed write list */
    int64_t              dwl_size;     /* Total size of entries on the delayed write list */
    struct H5PB_entry_t *dwl_head_ptr; /* Head pointer of the delayed write list */
    struct H5PB_entry_t *dwl_tail_ptr; /* Tail pointer of the delayed write list */

    /* Tick list: all entries modified in the current tick.  Drained at
     * end of tick to update the shadow index (see H5PB_vfd_swmr__update_index).
     */
    int64_t              tl_len;      /* Number of entries on the tick list */
    int64_t              tl_size;     /* Total size of entries on the tick list */
    struct H5PB_entry_t *tl_head_ptr; /* Head pointer of the tick list */
    struct H5PB_entry_t *tl_tail_ptr; /* Tail pointer of the tick list */

    /* Statistics -- general */
    int64_t accesses[H5PB__NUM_STAT_TYPES];
    int64_t hits[H5PB__NUM_STAT_TYPES];
    int64_t misses[H5PB__NUM_STAT_TYPES];
    int64_t evictions[H5PB__NUM_STAT_TYPES];
    int64_t bypasses[H5PB__NUM_STAT_TYPES];
    int64_t loads[H5PB__NUM_STAT_TYPES];
    int64_t insertions[H5PB__NUM_STAT_TYPES];
    int64_t flushes[H5PB__NUM_STAT_TYPES];
    int64_t clears[H5PB__NUM_STAT_TYPES];

    /* Statistics -- index/hash-table */
    int64_t total_ht_insertions;
    int64_t total_ht_deletions;
    int64_t successful_ht_searches;
    int64_t total_successful_ht_search_depth;
    int64_t failed_ht_searches;
    int64_t total_failed_ht_search_depth;
    int64_t max_index_len;
    int64_t max_clean_index_len;
    int64_t max_dirty_index_len;
    int64_t max_index_size;
    int64_t max_clean_index_size;
    int64_t max_dirty_index_size;
    int64_t max_md_pages;
    int64_t max_rd_pages;
    int64_t max_mpmde_count;

    /* Statistics -- LRU */
    int64_t max_lru_len;
    int64_t max_lru_size;
    int64_t lru_md_skips;
    int64_t lru_rd_skips;
    int64_t lru_tl_skips;
    int64_t lru_dwl_skips;

    /* Statistics -- VFD SWMR tick/delayed-write lists */
    int64_t max_tl_len;
    int64_t max_tl_size;
    int64_t delayed_writes;
    int64_t total_delay;
    int64_t max_dwl_len;
    int64_t max_dwl_size;
    int64_t total_dwl_ins_depth;
    int64_t md_read_splits;
    int64_t md_write_splits;
} H5PB_t;

/*****************************/
/* Library-private Variables */
/*****************************/

/***************************************/
/* Library-private Function Prototypes */
/***************************************/

/* General routines */
H5_DLL herr_t H5PB_create(H5F_shared_t *f_sh, size_t page_buffer_size, unsigned page_buf_min_meta_perc,
                          unsigned page_buf_min_raw_perc);
H5_DLL herr_t H5PB_flush(H5F_shared_t *f_sh);
H5_DLL herr_t H5PB_dest(H5F_shared_t *f_sh);
H5_DLL herr_t H5PB_add_new_page(H5F_shared_t *f_sh, H5FD_mem_t type, haddr_t page_addr);
H5_DLL herr_t H5PB_update_entry(H5PB_t *page_buf, haddr_t addr, size_t size, const void *buf);
H5_DLL herr_t H5PB_remove_entry(const H5F_shared_t *f_sh, haddr_t addr);
H5_DLL herr_t H5PB_read(H5F_shared_t *f_sh, H5FD_mem_t type, haddr_t addr, size_t size, void *buf /*out*/);
H5_DLL herr_t H5PB_write(H5F_shared_t *f_sh, H5FD_mem_t type, haddr_t addr, size_t size, const void *buf);
H5_DLL herr_t H5PB_enabled(H5F_shared_t *f_sh, H5FD_mem_t type, bool *enabled);

/* Testing support: query whether a page at the given (page-aligned) address
 * is currently resident in the index, without exposing the package-private
 * search macros/hash internals to test code. Not const: a successful search
 * moves the found entry to the front of its hash chain (a cache-locality
 * optimization) and updates search-depth statistics. */
H5_DLL bool H5PB_entry_exists(H5PB_t *page_buf, haddr_t addr);

/* VFD SWMR routines */
H5_DLL herr_t H5PB_vfd_swmr__release_delayed_writes(H5F_shared_t *f_sh);
H5_DLL herr_t H5PB_vfd_swmr__release_tick_list(H5F_shared_t *f_sh);
H5_DLL herr_t H5PB_vfd_swmr__set_tick(H5F_shared_t *f_sh);
H5_DLL herr_t H5PB_vfd_swmr__update_index(H5F_t *f, uint32_t *idx_ent_added_ptr,
                                          uint32_t *idx_ent_modified_ptr, uint32_t *idx_ent_not_in_tl_ptr,
                                          uint32_t *idx_ent_not_in_tl_flushed_ptr);

/* Statistics routines */
H5_DLL herr_t H5PB_reset_stats(H5PB_t *page_buf);
H5_DLL herr_t H5PB_get_stats(const H5PB_t *page_buf, unsigned accesses[2], unsigned hits[2],
                             unsigned misses[2], unsigned evictions[2], unsigned bypasses[2]);
H5_DLL herr_t H5PB_print_stats(const H5PB_t *page_buf);

#endif /* H5PBprivate_H */
