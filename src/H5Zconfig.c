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
 * H5Zconfig.c — TOML parameter string parser for the string-based filter
 *               configuration API (RFC-HDFG-2026-001).
 *
 * Uses the vendored tomlc17 library for all TOML parsing.
 *
 * Public typed accessor functions:
 *   H5Zconfig_has_key    — key presence check
 *   H5Zconfig_get_int    — TOML integer  → int64_t
 *   H5Zconfig_get_double — TOML float    → double
 *   H5Zconfig_get_bool   — TOML boolean  → hbool_t
 *   H5Zconfig_get_str    — TOML string   → char buffer
 *
 * Package-internal:
 *   H5Z__config_validate_keys — validate all keys in params against a
 *                               known-key list; called by built-in filter
 *                               set_config callbacks.
 */

#define H5Z_FRIEND /* suppress error on H5Zpkg.h include */

#include "H5Zmodule.h"

#include "H5private.h"   /* Generic Functions   */
#include "H5Eprivate.h"  /* Error handling      */
#include "H5MMprivate.h" /* Memory management   */
#include "H5Zpkg.h"      /* Filter internals    */

#include "tomlc17/tomlc17.h"

/*
 * H5Z__rewrite_hexfloats — return a copy of `src` with every C99 hex-float
 * literal (e.g. "0x1.8p+1", "-0x1p-1") replaced by an equivalent decimal
 * string.  Uses %.17g which guarantees IEEE 754 double round-trip fidelity.
 *
 * This pre-processing step lets callers produce parameter strings with `%a`
 * for exact float encoding (RFC-HDFG-2026-001 §float-precision) without
 * requiring changes to the vendored tomlc17 scanner, which does not support
 * hex-float syntax natively.
 *
 * Returns a heap-allocated NUL-terminated string; caller frees with
 * H5MM_xfree().  Returns NULL on allocation failure.
 */
static char *
H5Z__rewrite_hexfloats(const char *src)
{
    const char *p   = src;
    size_t      len = strlen(src);
    /* Worst case: every 3-char token "0x1" expands to ~24 chars "%.17g" → 3x */
    size_t cap = len * 8 + 1;
    char  *out = (char *)H5MM_malloc(cap);
    size_t pos = 0;

    if (!out)
        return NULL;

    while (*p) {
        /* Detect optional sign followed by "0x" or "0X" */
        const char *tok_start = p;
        if (*p == '+' || *p == '-')
            p++;

        if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
            /* Scan hex digits */
            const char *q = p + 2;
            while (isxdigit((unsigned char)*q) || *q == '_')
                q++;
            /* Is this a hex-float? Requires '.' or 'p'/'P' after hex digits */
            if (*q == '.' || *q == 'p' || *q == 'P') {
                if (*q == '.')
                    q++;
                while (isxdigit((unsigned char)*q) || *q == '_')
                    q++;
                if (*q == 'p' || *q == 'P') {
                    q++;
                    if (*q == '+' || *q == '-')
                        q++;
                    while (isdigit((unsigned char)*q))
                        q++;
                    /* q now points past the hex-float token; convert it */
                    size_t tok_len = (size_t)(q - tok_start);
                    char   tmp[64];
                    if (tok_len < sizeof(tmp)) {
                        memcpy(tmp, tok_start, tok_len);
                        tmp[tok_len] = '\0';
                        char  *end;
                        double val = strtod(tmp, &end);
                        if (end == tmp + tok_len) {
                            /* Emit decimal equivalent as a TOML float.
                             * Use %e (scientific notation) which always
                             * contains a decimal point and exponent, so
                             * tomlc17 parses it as TOML_FP64 not TOML_INTEGER.
                             * 17 significant digits guarantee IEEE 754
                             * double round-trip fidelity (C99 DBL_DECIMAL_DIG). */
                            char dec[32];
                            int  n = snprintf(dec, sizeof(dec), "%.17e", val);
                            if (n > 0 && pos + (size_t)n < cap) {
                                memcpy(out + pos, dec, (size_t)n);
                                pos += (size_t)n;
                                p = q;
                                continue;
                            }
                        }
                    }
                }
            }
        }

        /* Not a hex-float: copy one character verbatim */
        p = tok_start;
        if (pos + 1 < cap)
            out[pos++] = *p++;
    }
    out[pos] = '\0';
    return out;
}

/*
 * H5Z__toml_wrap — allocate a NUL-terminated TOML document that wraps the
 * inline-table content in params.  Returns a heap buffer that the caller
 * must free with H5MM_xfree().
 *
 * Accepts both bare content and an already-braced inline table:
 *   "level = 6"        →  "__p__ = {level = 6}"
 *   "{level = 6}"      →  "__p__ = {level = 6}"
 *   "{ level = 6 }"   →  "__p__ = {level = 6}"  (whitespace trimmed inside braces)
 */
static char *
H5Z__toml_wrap(const char *params)
{
    const char *p = params ? params : "";
    const char *e;
    size_t      content_len;
    size_t      wlen;
    char       *buf;

    /* skip leading whitespace */
    while (*p == ' ' || *p == '\t')
        p++;

    /* strip optional outer { } */
    if (*p == '{') {
        p++;
        e = p + strlen(p);
        while (e > p && (*(e - 1) == ' ' || *(e - 1) == '\t'))
            e--;
        if (e > p && *(e - 1) == '}')
            e--;
    }
    else {
        e = p + strlen(p);
    }
    content_len = (size_t)(e - p);

    wlen = content_len + 12; /* "__p__ = {" (9) + content + "}" (1) + NUL */
    buf  = (char *)H5MM_malloc(wlen);
    if (buf)
        snprintf(buf, wlen, "__p__ = {%.*s}", (int)content_len, p);
    return buf;
}

/*
 * H5Z__toml_parse_params — wrap params as a TOML document and parse it.
 *
 * On success: *tr_out holds a valid result; *ptab_out is the inline-table
 *             datum.  The caller MUST call toml_free(*tr_out) when done.
 * On failure: *tr_out is zeroed; an HDF5 error is pushed; returns FAIL.
 */
static htri_t
H5Z__toml_parse_params(const char *params, toml_result_t *tr_out, toml_datum_t *ptab_out)
{
    char  *expanded  = NULL;
    char  *wrapped   = NULL;
    htri_t ret_value = true;

    FUNC_ENTER_PACKAGE

    /* Replace hex-float literals (e.g. 0x1.8p+1) with decimal equivalents
     * so the tomlc17 scanner, which does not support C99 hex-float syntax,
     * can parse the resulting string without modification. */
    if (params && *params) {
        if (NULL == (expanded = H5Z__rewrite_hexfloats(params)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory rewriting hex-float literals");
        params = expanded;
    }

    if (NULL == (wrapped = H5Z__toml_wrap(params)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory for TOML wrapper buffer");

    *tr_out = toml_parse(wrapped, (int)strlen(wrapped));
    H5MM_xfree(wrapped);
    wrapped = NULL;

    if (!tr_out->ok) {
        /* Copy errmsg before toml_free invalidates it */
        char errbuf[sizeof(tr_out->errmsg)];
        memcpy(errbuf, tr_out->errmsg, sizeof(errbuf));
        toml_free(*tr_out);
        memset(tr_out, 0, sizeof(*tr_out));
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "TOML parse error in filter parameter string: %s", errbuf);
    }

    *ptab_out = toml_get(tr_out->toptab, "__p__");
    if (ptab_out->type != TOML_TABLE) {
        toml_free(*tr_out);
        memset(tr_out, 0, sizeof(*tr_out));
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "malformed filter parameter string (not a valid TOML inline table)");
    }

done:
    H5MM_xfree(wrapped);
    H5MM_xfree(expanded);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * H5Z__config_validate_keys — verify every key in params is in known_keys.
 * Package-internal; called by built-in filter set_config callbacks.
 */
herr_t
H5Z__config_validate_keys(const char *params, const char *const *known_keys)
{
    toml_result_t tr;
    toml_datum_t  ptab;
    bool          tr_valid  = false;
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (!params || *params == '\0')
        HGOTO_DONE(SUCCEED);

    if (strlen(params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_STRING_MAX (%d bytes)",
                    H5Z_CONFIG_STRING_MAX);

    if (H5Z__toml_parse_params(params, &tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse filter parameter string");
    tr_valid = true;

    if (known_keys) {
        int32_t i;
        for (i = 0; i < ptab.u.tab.size; i++) {
            const char *k = ptab.u.tab.key[i];
            size_t      ki;
            bool        found = false;

            for (ki = 0; known_keys[ki] != NULL; ki++) {
                if (strcmp(k, known_keys[ki]) == 0) {
                    found = true;
                    break;
                }
            }
            if (!found)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                            "unknown parameter key '%s' in filter configuration", k);
        }
    }

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * H5Z__config_get_datum — shared lookup core for all public accessors.
 *
 * Parses params, looks up key, and returns the raw toml_datum_t.
 *
 * Return:  > 0  key found;   *tr is valid — caller MUST toml_free(*tr)
 *           0   key absent;  helper already called toml_free(*tr)
 *         < 0   error;       error pushed; helper already cleaned up *tr
 *-------------------------------------------------------------------------
 */
static htri_t
H5Z__config_get_datum(const char *params, const char *key, toml_result_t *tr, toml_datum_t *d)
{
    toml_datum_t ptab;
    bool         tr_valid = false;
    htri_t       ret_value;

    FUNC_ENTER_PACKAGE

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || !*key)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (!params[0])
        HGOTO_DONE(false);

    if (H5Z__toml_parse_params(params, tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse parameter string");
    tr_valid = true;

    *d = toml_get(ptab, key);
    if (d->type == TOML_UNKNOWN)
        HGOTO_DONE(false);

    ret_value = true;

done:
    if (tr_valid && ret_value <= 0)
        toml_free(*tr);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_has_key
 *
 * Purpose:     Check whether a key exists in a TOML parameter string.
 *
 * Return:      > 0 present, 0 absent, < 0 error.
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_has_key(const char *params, const char *key)
{
    toml_result_t tr;
    toml_datum_t  d;
    htri_t        ret_value = FAIL;

    FUNC_ENTER_API_NOINIT

    ret_value = H5Z__config_get_datum(params, key, &tr, &d);

    if (ret_value > 0)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * H5Z__config_get_int — package-level integer lookup (no API lock).
 * Called by set_config callbacks which already run inside an API context.
 *-------------------------------------------------------------------------
 */
htri_t
H5Z__config_get_int(const char *params, const char *key, int64_t *out)
{
    toml_result_t tr;
    toml_datum_t  d;
    bool          tr_valid = false;
    htri_t        found;
    htri_t        ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_INT64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML integer", key);
    *out      = d.u.int64;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_int
 *
 * Purpose:     Look up a key and return its TOML integer value (int64_t).
 *
 * Return:      > 0 found and converted, 0 not found, < 0 error (includes
 *              type mismatch and parse error).
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_int(const char *params, const char *key, int64_t *out)
{
    htri_t ret_value = FAIL;

    FUNC_ENTER_API_NOINIT

    ret_value = H5Z__config_get_int(params, key, out);

    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_double
 *
 * Purpose:     Look up a key and return its TOML float value (double).
 *              inf and nan are rejected with H5E_BADVALUE.
 *
 * Return:      > 0 found and converted, 0 not found, < 0 error.
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_double(const char *params, const char *key, double *out)
{
    toml_result_t tr;
    toml_datum_t  d;
    bool          tr_valid = false;
    htri_t        found;
    htri_t        ret_value;

    FUNC_ENTER_API_NOINIT

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_FP64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML float", key);
    if (isnan(d.u.fp64) || isinf(d.u.fp64))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "inf/nan float values are not supported for filter parameters (key '%s')", key);
    *out      = d.u.fp64;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_bool
 *
 * Purpose:     Look up a key and return its TOML boolean value (hbool_t).
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_bool(const char *params, const char *key, hbool_t *out)
{
    toml_result_t tr;
    toml_datum_t  d;
    bool          tr_valid = false;
    htri_t        found;
    htri_t        ret_value;

    FUNC_ENTER_API_NOINIT

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_BOOLEAN)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML boolean", key);
    *out      = d.u.boolean ? true : false;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * H5Z__config_get_str — package-level string lookup (no API lock).
 * Called by set_config callbacks which already run inside an API context.
 *-------------------------------------------------------------------------
 */
htri_t
H5Z__config_get_str(const char *params, const char *key, char *buf, size_t *buf_size)
{
    toml_result_t tr;
    toml_datum_t  d;
    bool          tr_valid = false;
    htri_t        found;
    size_t        vlen;
    htri_t        ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_STRING)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "type mismatch: key '%s' is not a TOML string (value must be quoted)", key);

    vlen = (size_t)d.u.str.len;

    {
        size_t cap = buf_size ? *buf_size : 0;

        if (buf_size)
            *buf_size = vlen;

        if (buf) {
            if (cap == 0) {
                memcpy(buf, d.u.s, vlen + 1);
            }
            else if (cap > vlen) {
                memcpy(buf, d.u.s, vlen + 1);
            }
            else {
                if (cap > 0) {
                    memcpy(buf, d.u.s, cap - 1);
                    buf[cap - 1] = '\0';
                }
                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                            "output buffer too small for string value of key '%s'", key);
            }
        }
    }

    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_str
 *
 * Purpose:     Look up a key and return its TOML string value (decoded,
 *              without surrounding quotes).  Only TOML_STRING values are
 *              accepted; bare integers, floats, and booleans are type errors.
 *
 *              Size-query pattern:
 *                - buf == NULL: only *buf_size is set to the required length
 *                  (excluding NUL), returns > 0.
 *                - buf != NULL, *buf_size > 0: copies up to *buf_size - 1
 *                  bytes plus NUL; always sets *buf_size to required length.
 *                - buf != NULL, buf_size == NULL or *buf_size == 0: copies
 *                  unconditionally (caller is responsible for buffer size).
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_str(const char *params, const char *key, char *buf, size_t *buf_size)
{
    htri_t ret_value = FAIL;

    FUNC_ENTER_API_NOINIT

    ret_value = H5Z__config_get_str(params, key, buf, buf_size);

    FUNC_LEAVE_API_NOINIT(ret_value)
}
