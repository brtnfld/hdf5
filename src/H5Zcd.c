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

/*
 * H5Zcd.c — Typed cd_values packing and unpacking helpers.
 *
 * Normative packing convention (from RFC-HDFG-2026-001 §cd-packing-convention):
 *
 *   C type       Slots   Encoding
 *   ----------   -----   ------------------------------------------------
 *   unsigned      1      Direct store
 *   float         1      IEEE 754 memcpy into one slot
 *   double        2      IEEE 754 memcpy; slot[0]=low 32 bits (LE),
 *                         slot[1]=high 32 bits (always little-endian layout)
 *   char[]       1+⌈len/4⌉  slot[0]=byte length; remaining=null-padded 4B chunks
 *
 * On big-endian hosts the double pack/unpack helpers byte-swap so that the
 * two-slot layout is always little-endian in the slots, matching the existing
 * HDF5 convention that all cd_values are little-endian on disk.
 */

#define H5Z_FRIEND

#include "H5Zmodule.h"

#include "H5private.h"   /* Generic Functions   */
#include "H5Eprivate.h"  /* Error handling      */
#include "H5MMprivate.h" /* Memory management   */
#include "H5Zpkg.h"      /* Filter internals    */

/* Helper: detect host byte-order at compile time (HDF5 already defines H5_WORDS_BIGENDIAN) */
#ifdef H5_WORDS_BIGENDIAN
#define H5ZCD_BIG_ENDIAN 1
#else
#define H5ZCD_BIG_ENDIAN 0
#endif

/*-------------------------------------------------------------------------
 * Function: H5Zcd_pack_double
 *
 * Purpose:  Encode a double into two consecutive unsigned int slots using
 *           IEEE 754 little-endian layout.
 *           slot[0] = lower 32 bits, slot[1] = upper 32 bits (always LE).
 *
 * Return:   Non-negative on success / Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Zcd_pack_double(double val, unsigned *slots, size_t cap, size_t *n_used)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT

    if (n_used)
        *n_used = 2;

    if (slots) {
        uint8_t  buf[8];
        uint32_t lo, hi;

        if (cap < 2)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cd_values buffer too small for double (need 2 slots)");

        /* Byte-for-byte copy of the double into a uint8 buffer */
        H5MM_memcpy(buf, &val, 8);

#if H5ZCD_BIG_ENDIAN
        /* Reverse the bytes to produce little-endian slot layout */
        lo = ((uint32_t)buf[7])       | ((uint32_t)buf[6] << 8) |
             ((uint32_t)buf[5] << 16) | ((uint32_t)buf[4] << 24);
        hi = ((uint32_t)buf[3])       | ((uint32_t)buf[2] << 8) |
             ((uint32_t)buf[1] << 16) | ((uint32_t)buf[0] << 24);
#else
        lo = ((uint32_t)buf[0])       | ((uint32_t)buf[1] << 8) |
             ((uint32_t)buf[2] << 16) | ((uint32_t)buf[3] << 24);
        hi = ((uint32_t)buf[4])       | ((uint32_t)buf[5] << 8) |
             ((uint32_t)buf[6] << 16) | ((uint32_t)buf[7] << 24);
#endif
        slots[0] = lo;
        slots[1] = hi;
    }

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function: H5Zcd_unpack_double
 *
 * Purpose:  Decode a double from two consecutive unsigned int slots.
 *
 * Return:   Non-negative on success / Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Zcd_unpack_double(const unsigned *slots, size_t n_slots, double *val)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT

    if (!slots || !val)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer");
    if (n_slots < 2)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "need at least 2 slots to unpack double");

    {
        uint32_t lo = slots[0];
        uint32_t hi = slots[1];
        uint8_t  buf[8];

#if H5ZCD_BIG_ENDIAN
        buf[7] = (uint8_t)(lo & 0xFF);        buf[6] = (uint8_t)((lo >> 8) & 0xFF);
        buf[5] = (uint8_t)((lo >> 16) & 0xFF); buf[4] = (uint8_t)((lo >> 24) & 0xFF);
        buf[3] = (uint8_t)(hi & 0xFF);        buf[2] = (uint8_t)((hi >> 8) & 0xFF);
        buf[1] = (uint8_t)((hi >> 16) & 0xFF); buf[0] = (uint8_t)((hi >> 24) & 0xFF);
#else
        buf[0] = (uint8_t)(lo & 0xFF);        buf[1] = (uint8_t)((lo >> 8) & 0xFF);
        buf[2] = (uint8_t)((lo >> 16) & 0xFF); buf[3] = (uint8_t)((lo >> 24) & 0xFF);
        buf[4] = (uint8_t)(hi & 0xFF);        buf[5] = (uint8_t)((hi >> 8) & 0xFF);
        buf[6] = (uint8_t)((hi >> 16) & 0xFF); buf[7] = (uint8_t)((hi >> 24) & 0xFF);
#endif
        H5MM_memcpy(val, buf, 8);
    }

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function: H5Zcd_pack_float
 *
 * Purpose:  Encode a float into one unsigned int slot via IEEE 754 memcpy.
 *
 * Return:   Non-negative on success / Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Zcd_pack_float(float val, unsigned *slots, size_t cap, size_t *n_used)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT

    if (n_used)
        *n_used = 1;

    if (slots) {
        if (cap < 1)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "cd_values buffer too small for float (need 1 slot)");
        H5MM_memcpy(slots, &val, 4);
    }

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function: H5Zcd_unpack_float
 *
 * Purpose:  Decode a float from one unsigned int slot.
 *
 * Return:   Non-negative on success / Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Zcd_unpack_float(const unsigned *slots, size_t n_slots, float *val)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT

    if (!slots || !val)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL pointer");
    if (n_slots < 1)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "need at least 1 slot to unpack float");

    H5MM_memcpy(val, slots, 4);

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function: H5Zcd_pack_string
 *
 * Purpose:  Encode a NUL-terminated string into cd_values slots.
 *
 *   slot[0]          = byte length of string (not counting NUL)
 *   slot[1..1+⌈len/4⌉-1] = string bytes packed 4 per slot, null-padded
 *
 * Return:   Non-negative on success / Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Zcd_pack_string(const char *str, unsigned *slots, size_t cap, size_t *n_used)
{
    size_t len;
    size_t data_slots;
    size_t total_slots;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT

    if (!str)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL string pointer");

    len         = strlen(str);
    data_slots  = (len + 3) / 4;
    total_slots = 1 + data_slots;

    if (n_used)
        *n_used = total_slots;

    if (slots) {
        size_t i;

        if (cap < total_slots)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "cd_values buffer too small for string (need %zu slots)", total_slots);

        slots[0] = (unsigned)len;

        for (i = 0; i < data_slots; i++) {
            size_t   offset = i * 4;
            uint32_t chunk  = 0;
            size_t   j;
            for (j = 0; j < 4 && (offset + j) < len; j++)
                chunk |= ((uint32_t)(unsigned char)str[offset + j]) << (j * 8);
            slots[1 + i] = (unsigned)chunk;
        }
    }

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function: H5Zcd_unpack_string
 *
 * Purpose:  Decode a NUL-terminated string from cd_values slots.
 *
 *   slots[0]         = byte length
 *   slots[1..]       = packed string data
 *
 *   If buf is NULL or bufsz == 0, the function returns success and the
 *   caller may read the length from slots[0] directly.
 *
 * Return:   Non-negative on success / Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Zcd_unpack_string(const unsigned *slots, size_t n_slots, char *buf, size_t bufsz)
{
    size_t len;
    size_t data_slots;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API_NOINIT

    if (!slots)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL slots pointer");
    if (n_slots < 1)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "need at least 1 slot (length slot)");

    len        = slots[0];
    data_slots = (len + 3) / 4;

    if (n_slots < 1 + data_slots)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "too few slots to unpack string of length %zu (need %zu)", len, 1 + data_slots);

    if (buf && bufsz > 0) {
        size_t copy_len = (len < bufsz) ? len : bufsz - 1;
        size_t i;
        size_t written = 0;

        for (i = 0; i < data_slots && written < copy_len; i++) {
            uint32_t chunk = (uint32_t)slots[1 + i];
            size_t   j;
            for (j = 0; j < 4 && written < copy_len; j++)
                buf[written++] = (char)((chunk >> (j * 8)) & 0xFF);
        }
        buf[written] = '\0';
    }

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}
