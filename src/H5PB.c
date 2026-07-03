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
 * Created:             H5PB.c
 *
 * Purpose:             Page Buffer routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_FRIEND      /* Suppress error about including H5Fpkg            */
#include "H5PBmodule.h" /* This source code file is part of the H5PB module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                */
#include "H5Eprivate.h"  /* Error handling                   */
#include "H5Fpkg.h"      /* Files                            */
#include "H5FDprivate.h" /* File drivers                     */
#include "H5FLprivate.h" /* Free Lists                               */
#include "H5MMprivate.h" /* Memory management                */
#include "H5PBpkg.h"     /* File access                      */
#include "H5SLprivate.h" /* Skip List                        */

/****************/
/* Local Macros */
/****************/
#define H5PB__PREPEND(page_ptr, head_ptr, tail_ptr, len)                                                     \
    {                                                                                                        \
        if ((head_ptr) == NULL) {                                                                            \
            (head_ptr) = (page_ptr);                                                                         \
            (tail_ptr) = (page_ptr);                                                                         \
        } /* end if */                                                                                       \
        else {                                                                                               \
            (head_ptr)->prev = (page_ptr);                                                                   \
            (page_ptr)->next = (head_ptr);                                                                   \
            (head_ptr)       = (page_ptr);                                                                   \
        } /* end else */                                                                                     \
        (len)++;                                                                                             \
    } /* H5PB__PREPEND() */

#define H5PB__REMOVE(page_ptr, head_ptr, tail_ptr, len)                                                      \
    {                                                                                                        \
        if ((head_ptr) == (page_ptr)) {                                                                      \
            (head_ptr) = (page_ptr)->next;                                                                   \
            if ((head_ptr) != NULL)                                                                          \
                (head_ptr)->prev = NULL;                                                                     \
        } /* end if */                                                                                       \
        else                                                                                                 \
            (page_ptr)->prev->next = (page_ptr)->next;                                                       \
        if ((tail_ptr) == (page_ptr)) {                                                                      \
            (tail_ptr) = (page_ptr)->prev;                                                                   \
            if ((tail_ptr) != NULL)                                                                          \
                (tail_ptr)->next = NULL;                                                                     \
        } /* end if */                                                                                       \
        else                                                                                                 \
            (page_ptr)->next->prev = (page_ptr)->prev;                                                       \
        page_ptr->next = NULL;                                                                               \
        page_ptr->prev = NULL;                                                                               \
        (len)--;                                                                                             \
    }

#define H5PB__INSERT_LRU(page_buf, page_ptr)                                                                 \
    {                                                                                                        \
        assert(page_buf);                                                                                    \
        assert(page_ptr);                                                                                    \
        /* insert the entry at the head of the list. */                                                      \
        H5PB__PREPEND((page_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                        \
                      (page_buf)->LRU_list_len)                                                              \
    }

#define H5PB__REMOVE_LRU(page_buf, page_ptr)                                                                 \
    {                                                                                                        \
        assert(page_buf);                                                                                    \
        assert(page_ptr);                                                                                    \
        /* remove the entry from the list. */                                                                \
        H5PB__REMOVE((page_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                         \
                     (page_buf)->LRU_list_len)                                                               \
    }

#define H5PB__MOVE_TO_TOP_LRU(page_buf, page_ptr)                                                            \
    {                                                                                                        \
        assert(page_buf);                                                                                    \
        assert(page_ptr);                                                                                    \
        /* Remove entry and insert at the head of the list. */                                               \
        H5PB__REMOVE((page_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                         \
                     (page_buf)->LRU_list_len)                                                               \
        H5PB__PREPEND((page_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                        \
                      (page_buf)->LRU_list_len)                                                              \
    }

/******************/
/* Local Typedefs */
/******************/

/* Iteration context for destroying page buffer */
typedef struct {
    H5PB_t *page_buf;
    bool    actual_slist;
} H5PB_ud1_t;

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/
static herr_t H5PB__insert_entry(H5PB_t *page_buf, H5PB_entry_t *page_entry);
static htri_t H5PB__make_space(H5F_shared_t *f_sh, H5PB_t *page_buf, H5FD_mem_t inserted_type);
static herr_t H5PB__write_entry(H5F_shared_t *f_sh, H5PB_entry_t *page_entry);
static herr_t H5PB__vfd_swmr_track_write(H5F_shared_t *f_sh, H5PB_t *page_buf, H5PB_entry_t *entry_ptr,
                                         H5FD_mem_t type);
static herr_t H5PB__write_mpmde(H5F_shared_t *f_sh, H5PB_t *page_buf, H5FD_mem_t type, haddr_t addr,
                                size_t size, const void *buf);

/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
bool H5_PKG_INIT_VAR = false;

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/
/* Declare a free list to manage the H5PB_t struct */
H5FL_DEFINE_STATIC(H5PB_t);

/* Declare a free list to manage the H5PB_entry_t struct */
H5FL_DEFINE_STATIC(H5PB_entry_t);

/*-------------------------------------------------------------------------
 * Function:    H5PB_reset_stats
 *
 * Purpose:     This function was created without documentation.
 *              What follows is my best understanding of Mohamad's intent.
 *
 *              Reset statistics collected for the page buffer layer.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_reset_stats(H5PB_t *page_buf)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    assert(page_buf);

    page_buf->accesses[0]  = 0;
    page_buf->accesses[1]  = 0;
    page_buf->hits[0]      = 0;
    page_buf->hits[1]      = 0;
    page_buf->misses[0]    = 0;
    page_buf->misses[1]    = 0;
    page_buf->evictions[0] = 0;
    page_buf->evictions[1] = 0;
    page_buf->bypasses[0]  = 0;
    page_buf->bypasses[1]  = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5PB_reset_stats() */

/*-------------------------------------------------------------------------
 * Function:    H5PB_get_stats
 *
 * Purpose:     This function was created without documentation.
 *              What follows is my best understanding of Mohamad's intent.
 *
 *              Retrieve statistics collected about page accesses for the page buffer layer.
 *              --accesses: the number of metadata and raw data accesses to the page buffer layer
 *              --hits: the number of metadata and raw data hits in the page buffer layer
 *              --misses: the number of metadata and raw data misses in the page buffer layer
 *              --evictions: the number of metadata and raw data evictions from the page buffer layer
 *              --bypasses: the number of metadata and raw data accesses that bypass the page buffer layer
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_get_stats(const H5PB_t *page_buf, unsigned accesses[2], unsigned hits[2], unsigned misses[2],
               unsigned evictions[2], unsigned bypasses[2])
{
    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    assert(page_buf);

    accesses[0]  = page_buf->accesses[0];
    accesses[1]  = page_buf->accesses[1];
    hits[0]      = page_buf->hits[0];
    hits[1]      = page_buf->hits[1];
    misses[0]    = page_buf->misses[0];
    misses[1]    = page_buf->misses[1];
    evictions[0] = page_buf->evictions[0];
    evictions[1] = page_buf->evictions[1];
    bypasses[0]  = page_buf->bypasses[0];
    bypasses[1]  = page_buf->bypasses[1];

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5PB_get_stats */

/*-------------------------------------------------------------------------
 * Function:    H5PB_print_stats()
 *
 * Purpose:     This function was created without documentation.
 *              What follows is my best understanding of Mohamad's intent.
 *
 *              Print out statistics collected for the page buffer layer.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_print_stats(const H5PB_t *page_buf)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    assert(page_buf);

    printf("PAGE BUFFER STATISTICS:\n");

    printf("******* METADATA\n");
    printf("\t Total Accesses: %u\n", page_buf->accesses[0]);
    printf("\t Hits: %u\n", page_buf->hits[0]);
    printf("\t Misses: %u\n", page_buf->misses[0]);
    printf("\t Evictions: %u\n", page_buf->evictions[0]);
    printf("\t Bypasses: %u\n", page_buf->bypasses[0]);
    printf("\t Hit Rate = %f%%\n",
           ((double)page_buf->hits[0] / (page_buf->accesses[0] - page_buf->bypasses[0])) * 100);
    printf("*****************\n\n");

    printf("******* RAWDATA\n");
    printf("\t Total Accesses: %u\n", page_buf->accesses[1]);
    printf("\t Hits: %u\n", page_buf->hits[1]);
    printf("\t Misses: %u\n", page_buf->misses[1]);
    printf("\t Evictions: %u\n", page_buf->evictions[1]);
    printf("\t Bypasses: %u\n", page_buf->bypasses[1]);
    printf("\t Hit Rate = %f%%\n",
           ((double)page_buf->hits[1] / (page_buf->accesses[1] - page_buf->bypasses[0])) * 100);
    printf("*****************\n\n");

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5PB_print_stats */

/*-------------------------------------------------------------------------
 * Function:    H5PB_create
 *
 * Purpose:     Create and setup the PB on the file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_create(H5F_shared_t *f_sh, size_t size, unsigned page_buf_min_meta_perc, unsigned page_buf_min_raw_perc)
{
    H5PB_t *page_buf  = NULL;
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    assert(f_sh);

    /* Check args */
    if (f_sh->fs_strategy != H5F_FSPACE_STRATEGY_PAGE)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL,
                    "Enabling Page Buffering requires PAGE file space strategy");
    /* round down the size if it is larger than the page size */
    else if (size > f_sh->fs_page_size) {
        hsize_t temp_size;

        temp_size = (size / f_sh->fs_page_size) * f_sh->fs_page_size;
        H5_CHECKED_ASSIGN(size, size_t, temp_size, hsize_t);
    } /* end if */
    else if (0 != size % f_sh->fs_page_size)
        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTINIT, FAIL, "Page Buffer size must be >= to the page size");

    /* Allocate the new page buffering structure */
    if (NULL == (page_buf = H5FL_CALLOC(H5PB_t)))
        HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, FAIL, "memory allocation failed");

    page_buf->max_size = size;
    H5_CHECKED_ASSIGN(page_buf->page_size, size_t, f_sh->fs_page_size, hsize_t);
    page_buf->min_meta_perc = page_buf_min_meta_perc;
    page_buf->min_raw_perc  = page_buf_min_raw_perc;

    /* Derive VFD SWMR status directly from the (already-ingested) FAPL
     * config rather than from shared->vfd_swmr/vfd_swmr_writer: the page
     * buffer is created before H5F_vfd_swmr_init() runs (which is what
     * actually sets those two fields), so they aren't set yet at this
     * point in the open sequence.
     */
    if (H5F_SHARED_VFD_SWMR_CONFIG(f_sh)) {
        page_buf->vfd_swmr        = true;
        page_buf->vfd_swmr_writer = (H5F_SHARED_INTENT(f_sh) & H5F_ACC_RDWR) ? true : false;
    }

    /* Calculate the minimum page count for metadata and raw data
     * based on the fractions provided
     */
    page_buf->min_meta_count = (unsigned)((size * page_buf_min_meta_perc) / (f_sh->fs_page_size * 100));
    page_buf->min_raw_count  = (unsigned)((size * page_buf_min_raw_perc) / (f_sh->fs_page_size * 100));

    if (NULL == (page_buf->slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)))
        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTCREATE, FAIL, "can't create skip list");
    if (NULL == (page_buf->mf_slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)))
        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTCREATE, FAIL, "can't create skip list");

    if (NULL == (page_buf->page_fac = H5FL_fac_init(page_buf->page_size)))
        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTINIT, FAIL, "can't create page factory");

    f_sh->page_buf = page_buf;

done:
    if (ret_value < 0) {
        if (page_buf != NULL) {
            if (page_buf->slist_ptr != NULL)
                H5SL_close(page_buf->slist_ptr);
            if (page_buf->mf_slist_ptr != NULL)
                H5SL_close(page_buf->mf_slist_ptr);
            if (page_buf->page_fac != NULL)
                H5FL_fac_term(page_buf->page_fac);
            page_buf = H5FL_FREE(H5PB_t, page_buf);
        } /* end if */
    }     /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_create */

/*-------------------------------------------------------------------------
 * Function:    H5PB__flush_cb
 *
 * Purpose:     Callback to flush PB skiplist entries.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__flush_cb(void *item, void H5_ATTR_UNUSED *key, void *_op_data)
{
    H5PB_entry_t *page_entry = (H5PB_entry_t *)item; /* Pointer to page entry node */
    H5F_shared_t *f_sh       = (H5F_shared_t *)_op_data;
    herr_t        ret_value  = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(page_entry);
    assert(f_sh);

    /* Flush the page if it's dirty */
    if (page_entry->is_dirty)
        if (H5PB__write_entry(f_sh, page_entry) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "file write failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB__flush_cb() */

/*-------------------------------------------------------------------------
 * Function:    H5PB_flush
 *
 * Purpose:     Flush/Free all the PB entries to the file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_flush(H5F_shared_t *f_sh)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    assert(f_sh);

    /* Flush all the entries in the PB skiplist, if we have write access on the file */
    if (f_sh->page_buf && (H5F_ACC_RDWR & H5F_SHARED_INTENT(f_sh))) {
        H5PB_t *page_buf = f_sh->page_buf;

        /* Iterate over all entries in page buffer skip list */
        if (H5SL_iterate(page_buf->slist_ptr, H5PB__flush_cb, f_sh))
            HGOTO_ERROR(H5E_PAGEBUF, H5E_BADITER, FAIL, "can't flush page buffer skip list");
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_flush */

/*-------------------------------------------------------------------------
 * Function:    H5PB__dest_cb
 *
 * Purpose:     Callback to free PB skiplist entries.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__dest_cb(void *item, void H5_ATTR_UNUSED *key, void *_op_data)
{
    H5PB_entry_t *page_entry = (H5PB_entry_t *)item; /* Pointer to page entry node */
    H5PB_ud1_t   *op_data    = (H5PB_ud1_t *)_op_data;

    FUNC_ENTER_PACKAGE_NOERR

    /* Sanity checking */
    assert(page_entry);
    assert(op_data);
    assert(op_data->page_buf);

    /* Remove entry from LRU list.  Under VFD SWMR, an entry awaiting a
     * delayed write was already pulled off the LRU by
     * H5PB__vfd_swmr_track_write() -- its next/prev pointers are threaded
     * onto the delayed-write list instead, so running it through
     * H5PB__REMOVE_LRU() here would corrupt that list.  We are about to
     * tear down the whole page buffer anyway, so it is safe to simply skip
     * the (already-done) LRU unlink for such entries.  Multi-page metadata
     * entries were never inserted into the LRU in the first place (see
     * H5PB__write_mpmde()), so they must be skipped here too, and their
     * image must be freed with H5MM_xfree() rather than the page buffer's
     * fixed-size factory allocator, since it was never allocated from it.
     */
    if (op_data->actual_slist) {
        if (page_entry->is_mpmde)
            page_entry->page_buf_ptr = H5MM_xfree(page_entry->page_buf_ptr);
        else {
            if (0 == page_entry->delay_write_until)
                H5PB__REMOVE_LRU(op_data->page_buf, page_entry)
            page_entry->page_buf_ptr = H5FL_FAC_FREE(op_data->page_buf->page_fac, page_entry->page_buf_ptr);
        }
    } /* end if */

    /* Free page entry */
    page_entry = H5FL_FREE(H5PB_entry_t, page_entry);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5PB__dest_cb() */

/*-------------------------------------------------------------------------
 * Function:    H5PB_dest
 *
 * Purpose:     Flush and destroy the PB on the file if it exists.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_dest(H5F_shared_t *f_sh)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    assert(f_sh);

    /* flush and destroy the page buffer, if it exists */
    if (f_sh->page_buf) {
        H5PB_t    *page_buf = f_sh->page_buf;
        H5PB_ud1_t op_data; /* Iteration context */

        if (H5PB_flush(f_sh) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTFLUSH, FAIL, "can't flush page buffer");

        /* Set up context info */
        op_data.page_buf = page_buf;

        /* Destroy the skip list containing all the entries in the PB */
        op_data.actual_slist = true;
        if (H5SL_destroy(page_buf->slist_ptr, H5PB__dest_cb, &op_data))
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTCLOSEOBJ, FAIL, "can't destroy page buffer skip list");

        /* Destroy the skip list containing the new entries */
        op_data.actual_slist = false;
        if (H5SL_destroy(page_buf->mf_slist_ptr, H5PB__dest_cb, &op_data))
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTCLOSEOBJ, FAIL, "can't destroy page buffer skip list");

        /* Destroy the page factory */
        if (H5FL_fac_term(page_buf->page_fac) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTRELEASE, FAIL, "can't destroy page buffer page factory");

        f_sh->page_buf = H5FL_FREE(H5PB_t, page_buf);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_dest */

/*-------------------------------------------------------------------------
 * Function:    H5PB_add_new_page
 *
 * Purpose:     Add a new page to the new page skip list. This is called
 *              from the MF layer when a new page is allocated to
 *              indicate to the page buffer layer that a read of the page
 *              from the file is not necessary since it's an empty page.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_add_new_page(H5F_shared_t *f_sh, H5FD_mem_t type, haddr_t page_addr)
{
    H5PB_t       *page_buf;             /* Page buffer to operate on */
    H5PB_entry_t *page_entry = NULL;    /* Pointer to the corresponding page entry */
    herr_t        ret_value  = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    assert(f_sh);
    page_buf = f_sh->page_buf;
    assert(page_buf);

    /* If there is an existing page, this means that at some point the
     * file free space manager freed and re-allocated a page at the same
     * address.  No need to do anything here then...
     */
    /* MSC - to be safe, might want to dig in the MF layer and remove
     * the page when it is freed from this list if it still exists and
     * remove this check
     */
    if (NULL == H5SL_search(page_buf->mf_slist_ptr, &(page_addr))) {
        /* Create the new PB entry */
        if (NULL == (page_entry = H5FL_CALLOC(H5PB_entry_t)))
            HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, FAIL, "memory allocation failed");

        /* Initialize page fields */
        page_entry->addr     = page_addr;
        page_entry->type     = (H5F_mem_page_t)type;
        page_entry->is_dirty = false;
        page_entry->size     = page_buf->page_size;

        /* Insert entry in skip list */
        if (H5SL_insert(page_buf->mf_slist_ptr, page_entry, &(page_entry->addr)) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_BADVALUE, FAIL, "Can't insert entry in skip list");
    } /* end if */

done:
    if (ret_value < 0)
        if (page_entry)
            page_entry = H5FL_FREE(H5PB_entry_t, page_entry);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_add_new_page */

/*-------------------------------------------------------------------------
 * Function:    H5PB_update_entry
 *
 * Purpose:     In PHDF5, entries that are written by other processes and just
 *              marked clean by this process have to have their corresponding
 *              pages updated if they exist in the page buffer.
 *              This routine checks and update the pages.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_update_entry(H5PB_t *page_buf, haddr_t addr, size_t size, const void *buf)
{
    H5PB_entry_t *page_entry; /* Pointer to the corresponding page entry */
    haddr_t       page_addr;

    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    assert(page_buf);
    assert(size <= page_buf->page_size);
    assert(buf);

    /* calculate the aligned address of the first page */
    page_addr = (addr / page_buf->page_size) * page_buf->page_size;

    /* search for the page and update if found */
    page_entry = (H5PB_entry_t *)H5SL_search(page_buf->slist_ptr, (void *)(&page_addr));
    if (page_entry) {
        haddr_t offset;

        assert(addr + size <= page_addr + page_buf->page_size);
        offset = addr - page_addr;
        H5MM_memcpy((uint8_t *)page_entry->page_buf_ptr + offset, buf, size);

        /* move to top of LRU list, unless VFD SWMR write tracking has
         * already pulled this entry off the LRU for the duration of the
         * current tick (see H5PB__vfd_swmr_track_write())
         */
        if (!page_entry->modified_this_tick)
            H5PB__MOVE_TO_TOP_LRU(page_buf, page_entry)
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5PB_update_entry */

/*-------------------------------------------------------------------------
 * Function:    H5PB_remove_entry
 *
 * Purpose:     Remove possible metadata entry with ADDR from the PB cache.
 *              This is in response to the data corruption bug from fheap.c
 *              with page buffering + page strategy.
 *              Note: Large metadata page bypasses the PB cache.
 *              Note: Update of raw data page (large or small sized) is handled by the PB cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_remove_entry(const H5F_shared_t *f_sh, haddr_t addr)
{
    H5PB_t       *page_buf;             /* Page buffer to operate on */
    H5PB_entry_t *page_entry = NULL;    /* Pointer to the page entry being searched */
    herr_t        ret_value  = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    assert(f_sh);
    page_buf = f_sh->page_buf;
    assert(page_buf);

    /* Search for address in the skip list */
    page_entry = (H5PB_entry_t *)H5SL_search(page_buf->slist_ptr, (void *)(&addr));

    /* If found, remove the entry from the PB cache */
    if (page_entry) {
        assert(page_entry->type != H5F_MEM_PAGE_DRAW);
        if (NULL == H5SL_remove(page_buf->slist_ptr, &(page_entry->addr)))
            HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "Page Entry is not in skip list");

        /* Remove from LRU list.  Multi-page metadata entries are never on
         * the LRU (see H5PB__write_mpmde()), so skip the unlink and the
         * count check for those, and free their image with H5MM_xfree()
         * rather than the page buffer's fixed-size factory allocator,
         * since it was never allocated from it.
         */
        if (page_entry->is_mpmde) {
            page_buf->mpmde_count--;
            page_entry->page_buf_ptr = H5MM_xfree(page_entry->page_buf_ptr);
        }
        else {
            H5PB__REMOVE_LRU(page_buf, page_entry)
            assert(H5SL_count(page_buf->slist_ptr) == page_buf->LRU_list_len);

            page_buf->meta_count--;

            page_entry->page_buf_ptr = H5FL_FAC_FREE(page_buf->page_fac, page_entry->page_buf_ptr);
        }
        page_entry = H5FL_FREE(H5PB_entry_t, page_entry);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_remove_entry */

/*-------------------------------------------------------------------------
 * Function:    H5PB_read
 *
 * Purpose:     Reads in the data from the page containing it if it exists
 *              in the PB cache; otherwise reads in the page through the VFD.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_read(H5F_shared_t *f_sh, H5FD_mem_t type, haddr_t addr, size_t size, void *buf /*out*/)
{
    H5PB_t       *page_buf;                        /* Page buffering info for this file */
    H5PB_entry_t *page_entry;                      /* Pointer to the corresponding page entry */
    H5FD_t       *file;                            /* File driver pointer */
    haddr_t       first_page_addr, last_page_addr; /* Addresses of the first and last pages covered by I/O */
    haddr_t       offset;
    haddr_t       search_addr;       /* Address of current page */
    hsize_t       num_touched_pages; /* Number of pages accessed */
    size_t        access_size = 0;
    bool          bypass_pb   = false; /* Whether to bypass page buffering */
    hsize_t       i;                   /* Local index variable */
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    assert(f_sh);
    assert(type != H5FD_MEM_GHEAP);

    /* Get pointer to page buffer info for this file */
    page_buf = f_sh->page_buf;

#ifdef H5_HAVE_PARALLEL
    if (H5F_SHARED_HAS_FEATURE(f_sh, H5FD_FEAT_HAS_MPI)) {
#if 1
        bypass_pb = true;
#else
        /* MSC - why this stopped working ? */
        int mpi_size;

        if ((mpi_size = H5F_shared_mpi_get_size(f_sh)) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "can't retrieve MPI communicator size");
        if (1 != mpi_size)
            bypass_pb = true;
#endif
    } /* end if */
#endif

    /* If page buffering is disabled, or the I/O size is larger than that of a
     * single page, or if this is a parallel raw data access, bypass page
     * buffering.
     */
    if (NULL == page_buf || size >= page_buf->page_size || (bypass_pb && H5FD_MEM_DRAW == type)) {
        if (H5F__accum_read(f_sh, type, addr, size, buf) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "read through metadata accumulator failed");

        /* Update statistics */
        if (page_buf) {
            if (type == H5FD_MEM_DRAW)
                page_buf->bypasses[1]++;
            else
                page_buf->bypasses[0]++;
        } /* end if */

        /* If page buffering is disabled, or if this is a large metadata access,
         * or if this is parallel raw data access, we are done here
         */
        if (NULL == page_buf || (size >= page_buf->page_size && H5FD_MEM_DRAW != type) ||
            (bypass_pb && H5FD_MEM_DRAW == type))
            HGOTO_DONE(SUCCEED);
    } /* end if */

    /* Update statistics */
    if (page_buf) {
        if (type == H5FD_MEM_DRAW)
            page_buf->accesses[1]++;
        else
            page_buf->accesses[0]++;
    } /* end if */

    /* Calculate the aligned address of the first page */
    first_page_addr = (addr / page_buf->page_size) * page_buf->page_size;

    /* For Raw data calculate the aligned address of the last page and
     * the number of pages accessed if more than 1 page is accessed
     */
    if (H5FD_MEM_DRAW == type) {
        last_page_addr = ((addr + size - 1) / page_buf->page_size) * page_buf->page_size;

        /* How many pages does this read span */
        num_touched_pages =
            (last_page_addr / page_buf->page_size + 1) - (first_page_addr / page_buf->page_size);
        if (first_page_addr == last_page_addr) {
            assert(1 == num_touched_pages);
            last_page_addr = HADDR_UNDEF;
        } /* end if */
    }     /* end if */
    /* Otherwise set last page addr to HADDR_UNDEF */
    else {
        num_touched_pages = 1;
        last_page_addr    = HADDR_UNDEF;
    } /* end else */

    /* Translate to file driver I/O info object */
    file = f_sh->lf;

    /* Copy raw data from dirty pages into the read buffer if the read
       request spans pages in the page buffer*/
    if (H5FD_MEM_DRAW == type && size >= page_buf->page_size) {
        H5SL_node_t *node;

        /* For each touched page in the page buffer, check if it
         * exists in the page Buffer and is dirty. If it does, we
         * update the buffer with what's in the page so we get the up
         * to date data into the buffer after the big read from the file.
         */
        node = H5SL_find(page_buf->slist_ptr, (void *)(&first_page_addr));
        for (i = 0; i < num_touched_pages; i++) {
            search_addr = i * page_buf->page_size + first_page_addr;

            /* if we still haven't located a starting page, search again */
            if (!node && i != 0)
                node = H5SL_find(page_buf->slist_ptr, (void *)(&search_addr));

            /* if the current page is in the Page Buffer, do the updates */
            if (node) {
                page_entry = (H5PB_entry_t *)H5SL_item(node);

                assert(page_entry);

                /* If the current page address falls out of the access
                   block, then there are no more pages to go over */
                if (page_entry->addr >= addr + size)
                    break;

                assert(page_entry->addr == search_addr);

                if (page_entry->is_dirty) {
                    /* special handling for the first page if it is not a full page access */
                    if (i == 0 && first_page_addr != addr) {
                        offset = addr - first_page_addr;
                        assert(page_buf->page_size > offset);

                        H5MM_memcpy(buf, (uint8_t *)page_entry->page_buf_ptr + offset,
                                    page_buf->page_size - (size_t)offset);

                        /* move to top of LRU list, unless VFD SWMR write
                         * tracking has already pulled this entry off the
                         * LRU for the duration of the current tick (see
                         * H5PB__vfd_swmr_track_write())
                         */
                        if (!page_entry->modified_this_tick)
                            H5PB__MOVE_TO_TOP_LRU(page_buf, page_entry)
                    } /* end if */
                    /* special handling for the last page if it is not a full page access */
                    else if (num_touched_pages > 1 && i == num_touched_pages - 1 &&
                             search_addr < addr + size) {
                        offset = (num_touched_pages - 2) * page_buf->page_size +
                                 (page_buf->page_size - (addr - first_page_addr));

                        H5MM_memcpy((uint8_t *)buf + offset, page_entry->page_buf_ptr,
                                    (size_t)((addr + size) - last_page_addr));

                        /* move to top of LRU list, unless VFD SWMR write
                         * tracking has already pulled this entry off the
                         * LRU for the duration of the current tick
                         */
                        if (!page_entry->modified_this_tick)
                            H5PB__MOVE_TO_TOP_LRU(page_buf, page_entry)
                    } /* end else-if */
                    /* copy the entire fully accessed pages */
                    else {
                        offset = i * page_buf->page_size;

                        H5MM_memcpy((uint8_t *)buf + (i * page_buf->page_size), page_entry->page_buf_ptr,
                                    page_buf->page_size);
                    } /* end else */
                }     /* end if */
                node = H5SL_next(node);
            } /* end if */
        }     /* end for */
    }         /* end if */
    else {
        /* A raw data access could span 1 or 2 PB entries at this point so
           we need to handle that */
        assert(1 == num_touched_pages || 2 == num_touched_pages);
        for (i = 0; i < num_touched_pages; i++) {
            haddr_t buf_offset;

            /* Calculate the aligned address of the page to search for it in the skip list */
            search_addr = (0 == i ? first_page_addr : last_page_addr);

            /* Calculate the access size if the access spans more than 1 page */
            if (1 == num_touched_pages)
                access_size = size;
            else
                access_size = (0 == i ? (size_t)((first_page_addr + page_buf->page_size) - addr)
                                      : (size - access_size));

            /* Lookup the page in the skip list */
            page_entry = (H5PB_entry_t *)H5SL_search(page_buf->slist_ptr, (void *)(&search_addr));

            /* if found */
            if (page_entry) {
                offset     = (0 == i ? addr - page_entry->addr : 0);
                buf_offset = (0 == i ? 0 : size - access_size);

                /* Account for reads that would overflow a page */
                if (offset + access_size > page_buf->page_size)
                    access_size = page_buf->page_size - offset;

                /* copy the requested data from the page into the input buffer */
                H5MM_memcpy((uint8_t *)buf + buf_offset, (uint8_t *)page_entry->page_buf_ptr + offset,
                            access_size);

                /* Update LRU, unless VFD SWMR write tracking has already
                 * pulled this entry off the LRU for the duration of the
                 * current tick (see H5PB__vfd_swmr_track_write())
                 */
                if (!page_entry->modified_this_tick)
                    H5PB__MOVE_TO_TOP_LRU(page_buf, page_entry)

                /* Update statistics */
                if (type == H5FD_MEM_DRAW)
                    page_buf->hits[1]++;
                else
                    page_buf->hits[0]++;
            } /* end if */
            /* if not found */
            else {
                void   *new_page_buf = NULL;
                size_t  page_size    = page_buf->page_size;
                haddr_t eoa;

                /* make space for new entry */
                if ((H5SL_count(page_buf->slist_ptr) * page_buf->page_size) >= page_buf->max_size) {
                    htri_t can_make_space;

                    /* check if we can make space in page buffer */
                    if ((can_make_space = H5PB__make_space(f_sh, page_buf, type)) < 0)
                        HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, FAIL, "make space in Page buffer Failed");

                    /* if make_space returns 0, then we can't use the page
                       buffer for this I/O and we need to bypass */
                    if (0 == can_make_space) {
                        /* make space can't return false on second touched page since the first is of the same
                         * type */
                        assert(0 == i);

                        /* read entire block from VFD and return */
                        if (H5FD_read(file, type, addr, size, buf) < 0)
                            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "driver read request failed");

                        /* Break out of loop */
                        break;
                    } /* end if */
                }     /* end if */

                /* Read page from VFD */
                if (NULL == (new_page_buf = H5FL_FAC_MALLOC(page_buf->page_fac)))
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTALLOC, FAIL,
                                "memory allocation failed for page buffer entry");

                /* Read page through the VFD layer, but make sure we don't read past the EOA. */

                /* Retrieve the 'eoa' for the file */
                if (HADDR_UNDEF == (eoa = H5F_shared_get_eoa(f_sh, type))) {
                    new_page_buf = H5FL_FAC_FREE(page_buf->page_fac, new_page_buf);
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "driver get_eoa request failed");
                }

                /* If the entire page falls outside the EOA, then fail */
                if (search_addr > eoa) {
                    new_page_buf = H5FL_FAC_FREE(page_buf->page_fac, new_page_buf);
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_BADVALUE, FAIL,
                                "reading an entire page that is outside the file EOA");
                }

                /* Adjust the read size to not go beyond the EOA */
                if (search_addr + page_size > eoa)
                    page_size = (size_t)(eoa - search_addr);

                /* Read page from VFD */
                if (H5FD_read(file, type, search_addr, page_size, new_page_buf) < 0)
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "driver read request failed");

                /* Copy the requested data from the page into the input buffer */
                offset     = (0 == i ? addr - search_addr : 0);
                buf_offset = (0 == i ? 0 : size - access_size);

                /* Account for reads that would overflow a page */
                if (offset + access_size > page_buf->page_size)
                    access_size = page_buf->page_size - offset;

                H5MM_memcpy((uint8_t *)buf + buf_offset, (uint8_t *)new_page_buf + offset, access_size);

                /* Create the new PB entry */
                if (NULL == (page_entry = H5FL_CALLOC(H5PB_entry_t)))
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, FAIL, "memory allocation failed");

                page_entry->page_buf_ptr = new_page_buf;
                page_entry->addr         = search_addr;
                page_entry->type         = (H5F_mem_page_t)type;
                page_entry->is_dirty     = false;
                page_entry->size         = page_buf->page_size;

                /* Insert page into PB */
                if (H5PB__insert_entry(page_buf, page_entry) < 0)
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTSET, FAIL, "error inserting new page in page buffer");

                /* Update statistics */
                if (type == H5FD_MEM_DRAW)
                    page_buf->misses[1]++;
                else
                    page_buf->misses[0]++;
            } /* end else */
        }     /* end for */
    }         /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PB_read() */

/*-------------------------------------------------------------------------
 * Function:    H5PB__vfd_swmr_track_write
 *
 * Purpose:     Called whenever a metadata page-buffer entry is marked
 *              dirty, when this is the VFD SWMR writer.  Raw data is
 *              excluded: VFD SWMR only tracks metadata pages, since only
 *              metadata is published through the shadow file index.
 *
 *              1) Force the page buffer to retain the entry until the end
 *                 of the tick: add it to the tick list if not already
 *                 present.  H5PB_vfd_swmr__update_index() walks this list
 *                 at end-of-tick to update the shadow index.
 *
 *              2) If the entry has pre-existing on-disk content (loaded
 *                 from disk, or previously published) and isn't already
 *                 on the delayed write list, ask whether the write must
 *                 be delayed to avoid a "message from the future" bug on
 *                 a lagging VFD SWMR reader.  If so, move the entry from
 *                 the LRU (not eligible for eviction while delayed) onto
 *                 the delayed write list.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__vfd_swmr_track_write(H5F_shared_t *f_sh, H5PB_t *page_buf, H5PB_entry_t *entry_ptr, H5FD_mem_t type)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(f_sh);
    assert(page_buf);
    assert(entry_ptr);

    if (!page_buf->vfd_swmr_writer || type == H5FD_MEM_DRAW)
        HGOTO_DONE(SUCCEED);

    if (!entry_ptr->modified_this_tick) {
        entry_ptr->modified_this_tick = true;
        H5PB__INSERT_IN_TL(page_buf, entry_ptr, FAIL)

        /* An entry on the tick list must survive, with a valid image, until
         * H5PB_vfd_swmr__update_index() reads it at the end of the tick.
         * Pull it off the LRU so H5PB__make_space() cannot evict (and free)
         * it out from under the tick list in the meantime; it is returned
         * to the LRU in H5PB_vfd_swmr__release_tick_list(), unless it is
         * also on the delayed write list, in which case
         * H5PB_vfd_swmr__release_delayed_writes() returns it later.
         *
         * Multi-page metadata entries are never inserted into the LRU in
         * the first place (see H5PB__write_mpmde()), so there is nothing
         * to pull off here.
         */
        if (!entry_ptr->is_mpmde)
            H5PB__REMOVE_LRU(page_buf, entry_ptr)
    }

    if (entry_ptr->loaded && entry_ptr->delay_write_until == 0) {
        uint64_t page = entry_ptr->addr / page_buf->page_size;

        if (H5F_vfd_swmr_writer_delay_write(f_sh, page, &entry_ptr->delay_write_until) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "get delayed write request failed");

        if (entry_ptr->delay_write_until > 0) {
            /* Already off the LRU: either just pulled off above (first
             * write to this entry in the current tick), or pulled off by
             * an earlier write in this same tick and not yet returned
             * (release happens only at end of tick).
             */
            H5PB__INSERT_IN_DWL(page_buf, entry_ptr, FAIL)
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PB__vfd_swmr_track_write() */

/*-------------------------------------------------------------------------
 * Function:    H5PB__write_mpmde
 *
 * Purpose:     Write a multi-page metadata entry (MPMDE) -- a VFD SWMR
 *              metadata write of size >= page_buf->page_size.  Such
 *              writes must never fall through to the plain metadata
 *              accumulator bypass (H5F__accum_write()), since only
 *              H5PB__vfd_swmr_track_write() publishes a write to the VFD
 *              SWMR shadow index, and readers can only ever see writes
 *              that reach that index.
 *
 *              An mpmde entry is indexed in the same skip list as regular,
 *              one-page entries (keyed by the same page-aligned address),
 *              but is never inserted into the LRU replacement policy: it
 *              is pinned by tick-list membership for as long as it exists,
 *              and the page buffer is allowed to exceed max_size for it,
 *              matching the reference implementation's rationale that VFD
 *              SWMR ignores page buffer size limits for tracked metadata.
 *              Its image is allocated with H5MM_malloc()/H5MM_xfree(),
 *              not the page buffer's fixed-size (one page) factory
 *              allocator, since its size is variable and larger than one
 *              page.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__write_mpmde(H5F_shared_t *f_sh, H5PB_t *page_buf, H5FD_mem_t type, haddr_t addr, size_t size,
                  const void *buf)
{
    H5PB_entry_t *entry_ptr = NULL;
    haddr_t       page_addr;
    void         *new_image = NULL;
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(f_sh);
    assert(page_buf);
    assert(page_buf->vfd_swmr_writer);
    assert(type != H5FD_MEM_DRAW);
    assert(size >= page_buf->page_size);

    page_addr = (addr / page_buf->page_size) * page_buf->page_size;

    /* VFD SWMR metadata publishing requires the shadow-file image to start
     * at the beginning of the entry, so mpmde writes must be page-aligned.
     */
    assert(addr == page_addr);

    entry_ptr = (H5PB_entry_t *)H5SL_search(page_buf->slist_ptr, (void *)(&page_addr));

    if (entry_ptr == NULL) {
        /* No existing entry -- create a brand new mpmde entry.  Don't
         * bother trying to make space for it first: VFD SWMR ignores page
         * buffer size limits for tracked metadata (see H5PB_write()'s own
         * "let it exceed max_size" exception for the same reason).
         */
        if (NULL == (new_image = H5MM_malloc(size)))
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTALLOC, FAIL, "memory allocation failed for mpmde entry");

        if (NULL == (entry_ptr = H5FL_CALLOC(H5PB_entry_t))) {
            H5MM_xfree(new_image);
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTALLOC, FAIL, "memory allocation failed");
        }

        entry_ptr->page_buf_ptr = new_image;
        entry_ptr->addr         = page_addr;
        entry_ptr->type         = (H5F_mem_page_t)type;
        entry_ptr->size         = size;
        entry_ptr->is_mpmde     = true;
        entry_ptr->is_dirty     = false;
        entry_ptr->loaded       = false;

        /* Index only -- never the LRU.  mpmde entries are pinned by tick
         * list membership, never eviction candidates.
         */
        if (H5SL_insert(page_buf->slist_ptr, entry_ptr, &(entry_ptr->addr)) < 0) {
            H5MM_xfree(new_image);
            entry_ptr = H5FL_FREE(H5PB_entry_t, entry_ptr);
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTINSERT, FAIL, "can't insert mpmde entry in skip list");
        }

        page_buf->mpmde_count++;
    }
    else if (entry_ptr->size < size) {
        /* An existing entry -- either a regular one-page entry, or a
         * smaller mpmde entry -- must grow to accommodate this write.
         */
        if (NULL == (new_image = H5MM_malloc(size)))
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTALLOC, FAIL, "memory allocation failed for mpmde entry");

        if (entry_ptr->is_mpmde)
            H5MM_xfree(entry_ptr->page_buf_ptr);
        else {
            /* Transitioning from a regular, factory-allocated entry to an
             * mpmde: free with the allocator that created it, and remove
             * it from the LRU if it is still there (it may already be off
             * the LRU if a write earlier in the current tick pulled it via
             * H5PB__vfd_swmr_track_write()).
             */
            H5FL_FAC_FREE(page_buf->page_fac, entry_ptr->page_buf_ptr);
            if (!entry_ptr->modified_this_tick)
                H5PB__REMOVE_LRU(page_buf, entry_ptr)
            page_buf->meta_count--;
            page_buf->mpmde_count++;
            entry_ptr->is_mpmde = true;
        }

        entry_ptr->page_buf_ptr = new_image;
        entry_ptr->size         = size;
    }

    H5MM_memcpy(entry_ptr->page_buf_ptr, buf, size);
    entry_ptr->is_dirty = true;

    if (H5PB__vfd_swmr_track_write(f_sh, page_buf, entry_ptr, type) < 0)
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "VFD SWMR write tracking failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PB__write_mpmde() */

/*-------------------------------------------------------------------------
 * Function:    H5PB_write
 *
 * Purpose:     Write data into the Page Buffer. If the page exists in the
 *              cache, update it; otherwise read it from disk, update it, and
 *              insert into cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_write(H5F_shared_t *f_sh, H5FD_mem_t type, haddr_t addr, size_t size, const void *buf)
{
    H5PB_t       *page_buf;                        /* Page buffering info for this file */
    H5PB_entry_t *page_entry;                      /* Pointer to the corresponding page entry */
    H5FD_t       *file;                            /* File driver pointer */
    haddr_t       first_page_addr, last_page_addr; /* Addresses of the first and last pages covered by I/O */
    haddr_t       offset;
    haddr_t       search_addr;       /* Address of current page */
    hsize_t       num_touched_pages; /* Number of pages accessed */
    size_t        access_size = 0;
    bool          bypass_pb   = false; /* Whether to bypass page buffering */
    hsize_t       i;                   /* Local index variable */
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    assert(f_sh);

    /* Get pointer to page buffer info for this file */
    page_buf = f_sh->page_buf;

#ifdef H5_HAVE_PARALLEL
    if (H5F_SHARED_HAS_FEATURE(f_sh, H5FD_FEAT_HAS_MPI)) {
#if 1
        bypass_pb = true;
#else
        /* MSC - why this stopped working ? */
        int mpi_size;

        if ((mpi_size = H5F_shared_mpi_get_size(f_sh)) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "can't retrieve MPI communicator size");
        if (1 != mpi_size)
            bypass_pb = true;
#endif
    } /* end if */
#endif

    /* A VFD SWMR metadata write of a page or more (a multi-page metadata
     * entry, or "mpmde") must never fall through to the generic bypass
     * below: that bypass writes straight through H5F__accum_write(),
     * which never calls H5PB__vfd_swmr_track_write(), so the write would
     * never be published to the VFD SWMR shadow index and would be
     * invisible to readers.  Intercept it here instead.
     */
    if (page_buf != NULL && page_buf->vfd_swmr_writer && type != H5FD_MEM_DRAW &&
        size >= page_buf->page_size) {
        if (H5PB__write_mpmde(f_sh, page_buf, type, addr, size, buf) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "VFD SWMR mpmde write failed");
        HGOTO_DONE(SUCCEED);
    }

    /* If page buffering is disabled, or the I/O size is larger than that of a
     * single page, or if this is a parallel raw data access, bypass page
     * buffering.
     */
    if (NULL == page_buf || size >= page_buf->page_size || bypass_pb) {
        if (H5F__accum_write(f_sh, type, addr, size, buf) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "write through metadata accumulator failed");

        /* Update statistics */
        if (page_buf) {
            if (type == H5FD_MEM_DRAW || type == H5FD_MEM_GHEAP)
                page_buf->bypasses[1]++;
            else
                page_buf->bypasses[0]++;
        } /* end if */

        /* If page buffering is disabled, or if this is a large metadata access,
         * or if this is a parallel raw data access, we are done here
         */
        if (NULL == page_buf || (size >= page_buf->page_size && H5FD_MEM_DRAW != type) ||
            (bypass_pb && H5FD_MEM_DRAW == type))
            HGOTO_DONE(SUCCEED);

#ifdef H5_HAVE_PARALLEL
        if (bypass_pb) {
            if (H5PB_update_entry(page_buf, addr, size, buf) > 0)
                HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTUPDATE, FAIL, "failed to update PB with metadata cache");
            HGOTO_DONE(SUCCEED);
        } /* end if */
#endif
    } /* end if */

    /* Update statistics */
    if (page_buf) {
        if (type == H5FD_MEM_DRAW || type == H5FD_MEM_GHEAP)
            page_buf->accesses[1]++;
        else
            page_buf->accesses[0]++;
    } /* end if */

    /* Calculate the aligned address of the first page */
    first_page_addr = (addr / page_buf->page_size) * page_buf->page_size;

    /* For raw data calculate the aligned address of the last page and
     * the number of pages accessed if more than 1 page is accessed
     */
    if (H5FD_MEM_DRAW == type) {
        last_page_addr = (addr + size - 1) / page_buf->page_size * page_buf->page_size;

        /* how many pages does this write span */
        num_touched_pages =
            (last_page_addr / page_buf->page_size + 1) - (first_page_addr / page_buf->page_size);
        if (first_page_addr == last_page_addr) {
            assert(1 == num_touched_pages);
            last_page_addr = HADDR_UNDEF;
        } /* end if */
    }     /* end if */
    /* Otherwise set last page addr to HADDR_UNDEF */
    else {
        num_touched_pages = 1;
        last_page_addr    = HADDR_UNDEF;
    } /* end else */

    /* Translate to file driver I/O info object */
    file = f_sh->lf;

    /* Check if existing pages for raw data need to be updated since raw data access is not atomic */
    if (H5FD_MEM_DRAW == type && size >= page_buf->page_size) {
        /* For each touched page, check if it exists in the page buffer, and
         * update it with the data in the buffer to keep it up to date
         */
        for (i = 0; i < num_touched_pages; i++) {
            search_addr = i * page_buf->page_size + first_page_addr;

            /* Special handling for the first page if it is not a full page update */
            if (i == 0 && first_page_addr != addr) {
                /* Lookup the page in the skip list */
                page_entry = (H5PB_entry_t *)H5SL_search(page_buf->slist_ptr, (void *)(&search_addr));
                if (page_entry) {
                    offset = addr - first_page_addr;
                    assert(page_buf->page_size > offset);

                    /* Update page's data */
                    H5MM_memcpy((uint8_t *)page_entry->page_buf_ptr + offset, buf,
                                page_buf->page_size - (size_t)offset);

                    /* Mark page dirty and push to top of LRU */
                    page_entry->is_dirty = true;
                    H5PB__MOVE_TO_TOP_LRU(page_buf, page_entry)
                } /* end if */
            }     /* end if */
            /* Special handling for the last page if it is not a full page update */
            else if (num_touched_pages > 1 && i == (num_touched_pages - 1) &&
                     (search_addr + page_buf->page_size) != (addr + size)) {
                assert(search_addr + page_buf->page_size > addr + size);

                /* Lookup the page in the skip list */
                page_entry = (H5PB_entry_t *)H5SL_search(page_buf->slist_ptr, (void *)(&search_addr));
                if (page_entry) {
                    offset = (num_touched_pages - 2) * page_buf->page_size +
                             (page_buf->page_size - (addr - first_page_addr));

                    /* Update page's data */
                    H5MM_memcpy(page_entry->page_buf_ptr, (const uint8_t *)buf + offset,
                                (size_t)((addr + size) - last_page_addr));

                    /* Mark page dirty and push to top of LRU */
                    page_entry->is_dirty = true;
                    H5PB__MOVE_TO_TOP_LRU(page_buf, page_entry)
                } /* end if */
            }     /* end else-if */
            /* Discard all fully written pages from the page buffer */
            else {
                page_entry = (H5PB_entry_t *)H5SL_remove(page_buf->slist_ptr, (void *)(&search_addr));
                if (page_entry) {
                    /* Remove from LRU list */
                    H5PB__REMOVE_LRU(page_buf, page_entry)

                    /* Decrement page count of appropriate type */
                    if (H5F_MEM_PAGE_DRAW == page_entry->type || H5F_MEM_PAGE_GHEAP == page_entry->type)
                        page_buf->raw_count--;
                    else
                        page_buf->meta_count--;

                    /* Free page info */
                    page_entry->page_buf_ptr = H5FL_FAC_FREE(page_buf->page_fac, page_entry->page_buf_ptr);
                    page_entry               = H5FL_FREE(H5PB_entry_t, page_entry);
                } /* end if */
            }     /* end else */
        }         /* end for */
    }             /* end if */
    else {
        /* An access could span 1 or 2 PBs at this point so we need to handle that */
        assert(1 == num_touched_pages || 2 == num_touched_pages);
        for (i = 0; i < num_touched_pages; i++) {
            haddr_t buf_offset;

            /* Calculate the aligned address of the page to search for it in the skip list */
            search_addr = (0 == i ? first_page_addr : last_page_addr);

            /* Calculate the access size if the access spans more than 1 page */
            if (1 == num_touched_pages)
                access_size = size;
            else
                access_size =
                    (0 == i ? (size_t)(first_page_addr + page_buf->page_size - addr) : (size - access_size));

            /* Lookup the page in the skip list */
            page_entry = (H5PB_entry_t *)H5SL_search(page_buf->slist_ptr, (void *)(&search_addr));

            /* If found */
            if (page_entry) {
                offset     = (0 == i ? addr - page_entry->addr : 0);
                buf_offset = (0 == i ? 0 : size - access_size);

                /* Copy the requested data from the input buffer into the page */
                H5MM_memcpy((uint8_t *)page_entry->page_buf_ptr + offset, (const uint8_t *)buf + buf_offset,
                            access_size);

                /* Mark page dirty and push to top of LRU, unless VFD SWMR
                 * write tracking already pulled this entry off the LRU
                 * earlier in the current tick (see
                 * H5PB__vfd_swmr_track_write(), called just below) -- in
                 * that case it stays off the LRU until the tick ends.
                 */
                page_entry->is_dirty = true;
                if (!page_entry->modified_this_tick)
                    H5PB__MOVE_TO_TOP_LRU(page_buf, page_entry)

                if (H5PB__vfd_swmr_track_write(f_sh, page_buf, page_entry, type) < 0)
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "VFD SWMR write tracking failed");

                /* Update statistics */
                if (type == H5FD_MEM_DRAW || type == H5FD_MEM_GHEAP)
                    page_buf->hits[1]++;
                else
                    page_buf->hits[0]++;
            } /* end if */
            /* If not found */
            else {
                void  *new_page_buf;
                size_t page_size = page_buf->page_size;

                /* Make space for new entry */
                if ((H5SL_count(page_buf->slist_ptr) * page_buf->page_size) >= page_buf->max_size) {
                    htri_t can_make_space;

                    /* Check if we can make space in page buffer */
                    if ((can_make_space = H5PB__make_space(f_sh, page_buf, type)) < 0)
                        HGOTO_ERROR(H5E_PAGEBUF, H5E_NOSPACE, FAIL, "make space in Page buffer Failed");

                    /* If make_space returns 0, then we can't use the page
                     * buffer for this I/O and we need to bypass.
                     *
                     * Exception: under VFD SWMR, a metadata write must never
                     * be diverted to a direct, untracked VFD write, since
                     * H5PB__vfd_swmr_track_write() below is what makes the
                     * write visible to readers at all -- a page written this
                     * way would never be published to the shadow index.
                     * make_space() legitimately finds nothing evictable here
                     * only when every resident page is protected because it
                     * is on the current tick list (see
                     * H5PB__vfd_swmr_track_write()); that is exactly the
                     * scenario where staying within max_size isn't possible
                     * without breaking correctness, so let the page buffer
                     * temporarily exceed max_size instead of bypassing.  It
                     * shrinks back down once the tick ends and those entries
                     * are returned to the LRU.
                     */
                    if (0 == can_make_space && !(page_buf->vfd_swmr_writer && type != H5FD_MEM_DRAW)) {
                        assert(0 == i);

                        /* Write to VFD and return */
                        if (H5FD_write(file, type, addr, size, buf) < 0)
                            HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "driver write request failed");

                        /* Break out of loop */
                        break;
                    } /* end if */
                }     /* end if */

                /* Don't bother searching if there is no write access */
                if (H5F_ACC_RDWR & H5F_SHARED_INTENT(f_sh))
                    /* Lookup & remove the page from the new skip list page if
                     * it exists to see if this is a new page from the MF layer
                     */
                    page_entry = (H5PB_entry_t *)H5SL_remove(page_buf->mf_slist_ptr, (void *)(&search_addr));

                /* Calculate offset into the buffer of the page and the user buffer */
                offset     = (0 == i ? addr - search_addr : 0);
                buf_offset = (0 == i ? 0 : size - access_size);

                /* If found, then just update the buffer pointer to the newly allocate buffer */
                if (page_entry) {
                    /* Allocate space for the page buffer */
                    if (NULL == (new_page_buf = H5FL_FAC_MALLOC(page_buf->page_fac)))
                        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTALLOC, FAIL,
                                    "memory allocation failed for page buffer entry");
                    memset(new_page_buf, 0, (size_t)offset);
                    memset((uint8_t *)new_page_buf + offset + access_size, 0,
                           page_size - ((size_t)offset + access_size));

                    page_entry->page_buf_ptr = new_page_buf;

                    /* Update statistics */
                    if (type == H5FD_MEM_DRAW || type == H5FD_MEM_GHEAP)
                        page_buf->hits[1]++;
                    else
                        page_buf->hits[0]++;
                } /* end if */
                /* Otherwise read page through the VFD layer, but make sure we don't read past the EOA. */
                else {
                    haddr_t eoa, eof = HADDR_UNDEF;

                    /* Allocate space for the page buffer */
                    if (NULL == (new_page_buf = H5FL_FAC_CALLOC(page_buf->page_fac)))
                        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTALLOC, FAIL,
                                    "memory allocation failed for page buffer entry");

                    /* Create the new loaded PB entry */
                    if (NULL == (page_entry = H5FL_CALLOC(H5PB_entry_t)))
                        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTALLOC, FAIL, "memory allocation failed");

                    page_entry->page_buf_ptr = new_page_buf;
                    page_entry->addr         = search_addr;
                    page_entry->type         = (H5F_mem_page_t)type;
                    /* The in-memory image is always a full page (the factory
                     * allocator above hands out page_buf->page_size chunks,
                     * zero-padded past whatever is actually read from disk
                     * below), and that is what gets published to the VFD SWMR
                     * shadow file, so record the full page size here rather
                     * than the EOA-clipped read size computed below.
                     */
                    page_entry->size = page_buf->page_size;

                    /* Retrieve the 'eoa' for the file */
                    if (HADDR_UNDEF == (eoa = H5F_shared_get_eoa(f_sh, type))) {
                        page_entry->page_buf_ptr = H5FL_FAC_FREE(page_buf->page_fac, new_page_buf);
                        page_entry               = H5FL_FREE(H5PB_entry_t, page_entry);
                        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "driver get_eoa request failed");
                    }

                    /* If the entire page falls outside the EOA, then fail */
                    if (search_addr > eoa) {
                        page_entry->page_buf_ptr = H5FL_FAC_FREE(page_buf->page_fac, new_page_buf);
                        page_entry               = H5FL_FREE(H5PB_entry_t, page_entry);
                        HGOTO_ERROR(H5E_PAGEBUF, H5E_BADVALUE, FAIL,
                                    "writing to a page that is outside the file EOA");
                    }

                    /* Retrieve the 'eof' for the file - The MPI-VFD EOF
                     * returned will most likely be HADDR_UNDEF, so skip
                     * that check.
                     */
                    if (!H5F_SHARED_HAS_FEATURE(f_sh, H5FD_FEAT_HAS_MPI))
                        if (HADDR_UNDEF == (eof = H5FD_get_eof(f_sh->lf, H5FD_MEM_DEFAULT))) {
                            page_entry->page_buf_ptr = H5FL_FAC_FREE(page_buf->page_fac, new_page_buf);
                            page_entry               = H5FL_FREE(H5PB_entry_t, page_entry);
                            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "driver get_eof request failed");
                        }

                    /* Adjust the read size to not go beyond the EOA */
                    if (search_addr + page_size > eoa)
                        page_size = (size_t)(eoa - search_addr);

                    /* Pre-existing on-disk content: a VFD SWMR writer must
                     * consider delaying overwrites of this page (see
                     * H5PB__vfd_swmr_track_write() below).  A page entirely
                     * beyond the old EOF has no prior version any reader
                     * could be depending on, so it is always safe to write
                     * immediately.
                     */
                    page_entry->loaded = (search_addr < eof);

                    if (search_addr < eof) {
                        if (H5FD_read(file, type, search_addr, page_size, new_page_buf) < 0)
                            HGOTO_ERROR(H5E_PAGEBUF, H5E_READERROR, FAIL, "driver read request failed");

                        /* Update statistics */
                        if (type == H5FD_MEM_DRAW || type == H5FD_MEM_GHEAP)
                            page_buf->misses[1]++;
                        else
                            page_buf->misses[0]++;
                    } /* end if */
                }     /* end else */

                /* Copy the requested data from the page into the input buffer */
                H5MM_memcpy((uint8_t *)new_page_buf + offset, (const uint8_t *)buf + buf_offset, access_size);

                /* Page is dirty now */
                page_entry->is_dirty = true;

                /* Insert page into PB, evicting other pages as necessary */
                if (H5PB__insert_entry(page_buf, page_entry) < 0)
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTSET, FAIL, "error inserting new page in page buffer");

                if (H5PB__vfd_swmr_track_write(f_sh, page_buf, page_entry, type) < 0)
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "VFD SWMR write tracking failed");
            } /* end else */
        }     /* end for */
    }         /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PB_write() */

/*-------------------------------------------------------------------------
 * Function:    H5PB_enabled
 *
 * Purpose:     Check if the page buffer may be enabled for the specified
 *              file and data access type.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_enabled(H5F_shared_t *f_sh, H5FD_mem_t type, bool *enabled)
{
    H5PB_t *page_buf;            /* Page buffering info for this file */
    bool    bypass_pb = false;   /* Whether to bypass page buffering */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    assert(f_sh);

    /* Get pointer to page buffer info for this file */
    page_buf = f_sh->page_buf;

#ifdef H5_HAVE_PARALLEL
    if (H5F_SHARED_HAS_FEATURE(f_sh, H5FD_FEAT_HAS_MPI)) {
#if 1
        bypass_pb = true;
#else
        /* MSC - why this stopped working ? */
        int mpi_size;

        if ((mpi_size = H5F_shared_mpi_get_size(f_sh)) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "can't retrieve MPI communicator size");
        if (1 != mpi_size)
            bypass_pb = true;
#endif
    } /* end if */
#endif

    /* If page buffering is disabled, or if this is a parallel raw data access,
     * bypass page buffering. Note that page buffering may still be disabled for
     * large metadata access or large non-parallel raw data access, but this
     * function doesn't take I/O size into account so if it returns true the
     * page buffer may still be disabled for some I/O. If it returns false it is
     * always disabled for this access type.
     */
    if (NULL == page_buf || (bypass_pb && H5FD_MEM_DRAW == type)) {
        /* Update statistics, since wherever this function is called, if it
         * returns false, the calling function performs I/O avoiding the page
         * buffer layer */
        if (page_buf) {
            assert(type == H5FD_MEM_DRAW);
            page_buf->bypasses[1]++;
        } /* end if */

        /* Page buffer is disabled, at least for this data access type */
        *enabled = false;
    } /* end if */
    else
        /* Page buffer may be enabled */
        *enabled = true;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PB_enabled() */

/*-------------------------------------------------------------------------
 * Function:    H5PB__insert_entry()
 *
 * Purpose:     This function was created without documentation.
 *              What follows is my best understanding of Mohamad's intent.
 *
 *              Insert the supplied page into the page buffer, both the
 *              skip list and the LRU.
 *
 *              As best I can tell, this function imposes no limit on the
 *              number of entries in the page buffer beyond an assertion
 *              failure it the page count exceeds the limit.
 *
 *                                               JRM -- 12/22/16
 *
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__insert_entry(H5PB_t *page_buf, H5PB_entry_t *page_entry)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Insert entry in skip list */
    if (H5SL_insert(page_buf->slist_ptr, page_entry, &(page_entry->addr)) < 0)
        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTINSERT, FAIL, "can't insert entry in skip list");
    assert(H5SL_count(page_buf->slist_ptr) * page_buf->page_size <= page_buf->max_size);

    /* Increment appropriate page count */
    if (H5F_MEM_PAGE_DRAW == page_entry->type || H5F_MEM_PAGE_GHEAP == page_entry->type)
        page_buf->raw_count++;
    else
        page_buf->meta_count++;

    /* Insert entry in LRU */
    H5PB__INSERT_LRU(page_buf, page_entry)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PB__insert_entry() */

/*-------------------------------------------------------------------------
 * Function:    H5PB__make_space()
 *
 * Purpose:     This function was created without documentation.
 *              What follows is my best understanding of Mohamad's intent.
 *
 *              If necessary and if possible, evict a page from the page
 *              buffer to make space for the supplied page.  Depending on
 *              the page buffer configuration and contents, and the page
 *              supplied this may or may not be possible.
 *
 *                                             JRM -- 12/22/16
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5PB__make_space(H5F_shared_t *f_sh, H5PB_t *page_buf, H5FD_mem_t inserted_type)
{
    H5PB_entry_t *page_entry;       /* Pointer to page eviction candidate */
    htri_t        ret_value = true; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(f_sh);
    assert(page_buf);

    /* Get oldest entry */
    page_entry = page_buf->LRU_tail_ptr;

    /* Under VFD SWMR, entries awaiting a delayed write are pulled off the
     * LRU (see H5PB__vfd_swmr_track_write()) even though they still count
     * against the page buffer's size limits.  If every resident page is
     * currently delayed, the LRU can be empty even though the page buffer
     * is "full", and there is nothing here that can be evicted.
     */
    if (NULL == page_entry)
        HGOTO_DONE(false);

    if (H5FD_MEM_DRAW == inserted_type) {
        /* If threshould is 100% metadata and page buffer is full of
           metadata, then we can't make space for raw data */
        if (0 == page_buf->raw_count && page_buf->min_meta_count == page_buf->meta_count) {
            assert(page_buf->meta_count * page_buf->page_size == page_buf->max_size);
            HGOTO_DONE(false);
        } /* end if */

        /* check the metadata threshold before evicting metadata items */
        while (1) {
            if (page_entry->prev && H5F_MEM_PAGE_META == page_entry->type &&
                page_buf->min_meta_count >= page_buf->meta_count)
                page_entry = page_entry->prev;
            else
                break;
        } /* end while */
    }     /* end if */
    else {
        /* If threshould is 100% raw data and page buffer is full of
           raw data, then we can't make space for meta data */
        if (0 == page_buf->meta_count && page_buf->min_raw_count == page_buf->raw_count) {
            assert(page_buf->raw_count * page_buf->page_size == page_buf->max_size);
            HGOTO_DONE(false);
        } /* end if */

        /* check the raw data threshold before evicting raw data items */
        while (1) {
            if (page_entry->prev &&
                (H5F_MEM_PAGE_DRAW == page_entry->type || H5F_MEM_PAGE_GHEAP == page_entry->type) &&
                page_buf->min_raw_count >= page_buf->raw_count)
                page_entry = page_entry->prev;
            else
                break;
        } /* end while */
    }     /* end else */

    /* Remove from page index */
    if (NULL == H5SL_remove(page_buf->slist_ptr, &(page_entry->addr)))
        HGOTO_ERROR(H5E_PAGEBUF, H5E_BADVALUE, FAIL, "Tail Page Entry is not in skip list");

    /* Remove entry from LRU list */
    H5PB__REMOVE_LRU(page_buf, page_entry)
    assert(H5SL_count(page_buf->slist_ptr) == page_buf->LRU_list_len);

    /* Decrement appropriate page type counter */
    if (H5F_MEM_PAGE_DRAW == page_entry->type || H5F_MEM_PAGE_GHEAP == page_entry->type)
        page_buf->raw_count--;
    else
        page_buf->meta_count--;

    /* Flush page if dirty */
    if (page_entry->is_dirty)
        if (H5PB__write_entry(f_sh, page_entry) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "file write failed");

    /* Update statistics */
    if (page_entry->type == H5F_MEM_PAGE_DRAW || H5F_MEM_PAGE_GHEAP == page_entry->type)
        page_buf->evictions[1]++;
    else
        page_buf->evictions[0]++;

    /* Release page */
    page_entry->page_buf_ptr = H5FL_FAC_FREE(page_buf->page_fac, page_entry->page_buf_ptr);
    page_entry               = H5FL_FREE(H5PB_entry_t, page_entry);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PB__make_space() */

/*-------------------------------------------------------------------------
 * Function:    H5PB__write_entry()
 *
 * Purpose:     ???
 *
 *              This function was created without documentation.
 *              What follows is my best understanding of Mohamad's intent.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5PB__write_entry(H5F_shared_t *f_sh, H5PB_entry_t *page_entry)
{
    haddr_t eoa;                 /* Current EOA for the file */
    herr_t  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    assert(f_sh);
    assert(page_entry);

    /* Retrieve the 'eoa' for the file */
    if (HADDR_UNDEF == (eoa = H5F_shared_get_eoa(f_sh, (H5FD_mem_t)page_entry->type)))
        HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTGET, FAIL, "driver get_eoa request failed");

    /* If the starting address of the page is larger than
     * the EOA, then the entire page is discarded without writing.
     */
    if (page_entry->addr <= eoa) {
        H5FD_t *file; /* File driver I/O info */
        /* Use the entry's own size, not page_buf->page_size: a multi-page
         * metadata entry (is_mpmde) can be larger than one page, and
         * writing only page_buf->page_size bytes for it would silently
         * truncate the write.
         */
        size_t page_size = page_entry->size;

        /* Adjust the page length if it exceeds the EOA */
        if ((page_entry->addr + page_size) > eoa)
            page_size = (size_t)(eoa - page_entry->addr);

        /* Translate to file driver I/O info object */
        file = f_sh->lf;

        if (H5FD_write(file, (H5FD_mem_t)page_entry->type, page_entry->addr, page_size,
                       page_entry->page_buf_ptr) < 0)
            HGOTO_ERROR(H5E_PAGEBUF, H5E_WRITEERROR, FAIL, "file write failed");
    } /* end if */

    page_entry->is_dirty = false;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PB__write_entry() */

/*-------------------------------------------------------------------------
 * VFD SWMR page buffer stub functions.
 *
 * These are minimal stubs for the VFD SWMR page buffer functions.
 * Full implementations are in the feature/vfd_swmr branch H5PB.c.
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function:    H5PB_vfd_swmr__release_delayed_writes
 *
 * Purpose:     After the tick list has been released, and before the
 *              beginning of the next tick, scan the delayed write list
 *              and release those entries whose delays have expired,
 *              returning them to the replacement policy.
 *
 *              Since the delayed write list is sorted in decreasing
 *              delay_write_until order, scan starts at the tail and
 *              continues while expired entries remain.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_vfd_swmr__release_delayed_writes(H5F_shared_t *shared)
{
    H5PB_t       *page_buf  = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(shared);
    assert(shared->vfd_swmr);
    assert(shared->vfd_swmr_writer);

    page_buf = shared->page_buf;
    assert(page_buf);
    assert(page_buf->vfd_swmr_writer);

    while (page_buf->dwl_tail_ptr && page_buf->dwl_tail_ptr->delay_write_until <= shared->tick_num) {

        entry_ptr = page_buf->dwl_tail_ptr;

        assert(entry_ptr->is_dirty);

        entry_ptr->delay_write_until = 0;

        H5PB__REMOVE_FROM_DWL(page_buf, entry_ptr, FAIL)

        /* return the entry to the replacement policy, unless it is a
         * multi-page metadata entry -- those are never on the LRU (see
         * H5PB__write_mpmde())
         */
        if (!entry_ptr->is_mpmde)
            H5PB__INSERT_LRU(page_buf, entry_ptr)
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_vfd_swmr__release_delayed_writes() */

/*-------------------------------------------------------------------------
 * Function:    H5PB_vfd_swmr__release_tick_list
 *
 * Purpose:     After the metadata file has been updated, and before the
 *              beginning of the next tick, release the tick list.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_vfd_swmr__release_tick_list(H5F_shared_t *shared)
{
    H5PB_t       *page_buf  = NULL;
    H5PB_entry_t *entry_ptr = NULL;
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(shared);
    assert(shared->vfd_swmr);
    assert(shared->vfd_swmr_writer);

    page_buf = shared->page_buf;
    assert(page_buf);
    assert(page_buf->vfd_swmr_writer);

    /* remove all entries from the tick list */
    while (page_buf->tl_head_ptr) {

        entry_ptr = page_buf->tl_head_ptr;

        H5PB__REMOVE_FROM_TL(page_buf, entry_ptr, FAIL)

        entry_ptr->modified_this_tick = false;

        /* H5PB__vfd_swmr_track_write() pulled this entry off the LRU for
         * the duration of the tick (see comment there).  Return it now,
         * unless it is instead on the delayed write list, in which case
         * H5PB_vfd_swmr__release_delayed_writes() will return it once its
         * delay has expired.  Multi-page metadata entries are never
         * returned to the LRU -- they were never on it in the first place
         * (see H5PB__write_mpmde()).
         */
        if (0 == entry_ptr->delay_write_until && !entry_ptr->is_mpmde)
            H5PB__INSERT_LRU(page_buf, entry_ptr)
    }

    assert(page_buf->tl_head_ptr == NULL);
    assert(page_buf->tl_tail_ptr == NULL);
    assert(page_buf->tl_len == 0);
    assert(page_buf->tl_size == 0);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_vfd_swmr__release_tick_list() */

/*-------------------------------------------------------------------------
 * Function:    H5PB_vfd_swmr__set_tick
 *
 * Purpose:     At the beginning of each tick, synchronize the page
 *              buffer's copy of the current tick with that of the file
 *              to which the page buffer belongs.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_vfd_swmr__set_tick(H5F_shared_t *shared)
{
    H5PB_t *page_buf  = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(shared);
    assert(shared->vfd_swmr);
    assert(shared->vfd_swmr_writer);

    page_buf = shared->page_buf;
    assert(page_buf);
    assert(page_buf->vfd_swmr_writer);

    /* the tick must always increase by 1 -- verify this */
    if (shared->tick_num != page_buf->cur_tick + 1)
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL,
                    "shared->tick_num (%" PRIu64 ") != (%" PRIu64 ") page_buf->cur_tick + 1 ?!?!",
                    shared->tick_num, page_buf->cur_tick);

    page_buf->cur_tick = shared->tick_num;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_vfd_swmr__set_tick() */

/*-------------------------------------------------------------------------
 * Function:    H5PB_vfd_swmr__update_index
 *
 * Purpose:     In the VFD SWMR writer, all metadata writes to the page
 *              buffer during a tick are buffered in the page buffer's
 *              tick list.  The metadata cache is flushed to the page
 *              buffer at the end of the tick so that all metadata
 *              changes during the tick are reflected there.
 *
 *              Once this is done, the internal representation of the
 *              metadata file index must be updated from the tick list so
 *              that the metadata file can be updated, and the tick list
 *              can be emptied and prepared to buffer metadata changes in
 *              the next tick.  Specifically:
 *
 *              1) Scan the tick list.  For each entry, test whether it
 *                 appears in the index.  If it does, update the index
 *                 entry (image pointer, tick of last change, dirty
 *                 state).  If it doesn't, allocate a new index entry.
 *
 *              2) Scan the index for entries that don't appear in the
 *                 tick list.  For each such entry, if it's dirty and
 *                 either doesn't appear in the page buffer, or is clean
 *                 there, mark it clean and flushed this tick.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5PB_vfd_swmr__update_index(H5F_t *f, uint32_t *idx_ent_added_ptr, uint32_t *idx_ent_modified_ptr,
                            uint32_t *idx_ent_not_in_tl_ptr, uint32_t *idx_ent_not_in_tl_flushed_ptr)
{
    H5F_shared_t *const        shared   = f->shared;
    const uint64_t             tick_num = shared->tick_num;
    uint32_t                   i;
    uint32_t                   idx_ent_added             = 0;
    uint32_t                   idx_ent_modified          = 0;
    uint32_t                   idx_ent_not_in_tl         = 0;
    uint32_t                   idx_ent_not_in_tl_flushed = 0;
    H5PB_t                    *page_buf                  = NULL;
    H5PB_entry_t              *entry;
    H5FD_vfd_swmr_idx_entry_t *ie_ptr    = NULL;
    H5FD_vfd_swmr_idx_entry_t *idx       = NULL;
    herr_t                     ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    assert(shared->vfd_swmr);
    assert(shared->vfd_swmr_writer);

    idx = shared->mdf_idx;
    assert(idx);

    page_buf = shared->page_buf;
    assert(page_buf);
    assert(page_buf->vfd_swmr_writer);

    assert(idx_ent_added_ptr);
    assert(idx_ent_modified_ptr);
    assert(idx_ent_not_in_tl_ptr);
    assert(idx_ent_not_in_tl_flushed_ptr);

    /* scan the tick list and insert or update metadata file index entries
     * as appropriate.
     */
    for (entry = page_buf->tl_head_ptr; entry != NULL; entry = entry->tl_next) {
        uint64_t target_page = entry->addr / page_buf->page_size;

        assert(entry->magic == H5PB__H5PB_ENTRY_T_MAGIC);

        /* see if the shadow index already contains an entry for *entry. */
        ie_ptr =
            H5FD_vfd_swmr_pageno_to_mdf_idx_entry(idx, shared->mdf_idx_entries_used, target_page, false);

        if (ie_ptr == NULL) { /* alloc new entry in the metadata file index */
            uint32_t new_index_entry_index;

            new_index_entry_index = shared->mdf_idx_entries_used + idx_ent_added++;

            if (new_index_entry_index >= shared->mdf_idx_len &&
                (idx = H5F_vfd_swmr_enlarge_shadow_index(f)) == NULL)
                HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTALLOC, FAIL, "max mdf index len exceeded");

            ie_ptr = idx + new_index_entry_index;

            /* partial initialization of new entry -- rest done below */
            ie_ptr->hdf5_page_offset    = target_page;
            ie_ptr->md_file_page_offset = 0; /* undefined at this point */
            ie_ptr->checksum            = 0; /* undefined at this point */
            ie_ptr->delayed_flush       = entry->delay_write_until;
            ie_ptr->moved_to_lower_file = false;
            ie_ptr->garbage             = false;
            ie_ptr->length              = (uint32_t)entry->size;
        }
        else {
            /* If entry->size changed, discard the too-small (too-big?)
             * shadow region and set the shadow-file page number to 0
             * so that H5F_update_vfd_swmr_metadata_file() will
             * allocate a new one.
             */
            if (ie_ptr->length != (uint32_t)entry->size) {
                if (H5F_shadow_image_defer_free(shared, ie_ptr) < 0)
                    HGOTO_ERROR(H5E_PAGEBUF, H5E_CANTFREE, FAIL, "can't defer-free shadow image");

                ie_ptr->md_file_page_offset = 0;
                ie_ptr->length              = (uint32_t)entry->size;
            }

            idx_ent_modified++;
        }

        /* image_ptr is a legacy alias never populated by the skip-list page
         * buffer (see the "M3 compat" comment on H5PB_entry_t); the real
         * in-memory page image lives in page_buf_ptr.
         */
        ie_ptr->entry_ptr           = entry->page_buf_ptr;
        ie_ptr->tick_of_last_change = tick_num;
        assert(entry->is_dirty);
        ie_ptr->clean              = false;
        ie_ptr->tick_of_last_flush = 0;
    }

    /* scan the metadata file index for entries that don't appear in the
     * tick list.  If the index entry is dirty, and either doesn't appear
     * in the page buffer, or is clean in the page buffer, mark the index
     * entry clean and as having been flushed in the current tick.
     */
    for (i = 0; i < shared->mdf_idx_entries_used; i++) {
        haddr_t page_addr;

        assert(i == 0 || idx[i - 1].hdf5_page_offset < idx[i].hdf5_page_offset);

        ie_ptr = idx + i;

        if (ie_ptr->tick_of_last_change == tick_num)
            continue;

        idx_ent_not_in_tl++;

        if (ie_ptr->clean)
            continue;

        page_addr = (haddr_t)(ie_ptr->hdf5_page_offset * page_buf->page_size);
        entry     = (H5PB_entry_t *)H5SL_search(page_buf->slist_ptr, &page_addr);

        if (entry == NULL || !entry->is_dirty) {
            idx_ent_not_in_tl_flushed++;
            ie_ptr->clean              = true;
            ie_ptr->tick_of_last_flush = tick_num;
        }
    }

    assert(idx_ent_modified + idx_ent_not_in_tl == shared->mdf_idx_entries_used);
    assert(idx_ent_modified + idx_ent_not_in_tl + idx_ent_added <= shared->mdf_idx_len);

    *idx_ent_added_ptr             = idx_ent_added;
    *idx_ent_modified_ptr          = idx_ent_modified;
    *idx_ent_not_in_tl_ptr         = idx_ent_not_in_tl;
    *idx_ent_not_in_tl_flushed_ptr = idx_ent_not_in_tl_flushed;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5PB_vfd_swmr__update_index() */
