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
 * When HDF5 is built with tomlc17 support (H5_HAVE_TOMLC17 defined), the
 * tomlc17 library handles all TOML parsing.  When tomlc17 is not available,
 * the built-in custom TOML-subset parser retained under the #else branch is
 * used as a fallback.
 *
 * Public typed accessor functions (both paths):
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

/* ======================================================================
 * tomlc17-based implementation (compiled when H5_HAVE_TOMLC17 is defined)
 * ====================================================================== */

#ifdef H5_HAVE_TOMLC17

/*
 * Allow the header path to be overridden at compile time (e.g. for a
 * system-installed tomlc17).  Defaults to the bundled copy in src/tomlc17/.
 */
#ifndef H5_TOMLC17_HEADER
#define H5_TOMLC17_HEADER "tomlc17/tomlc17.h"
#endif
#include H5_TOMLC17_HEADER /* tomlc17 TOML parser */

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
    size_t  cap = len * 8 + 1;
    char   *out = (char *)H5MM_malloc(cap);
    size_t  pos = 0;

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
    size_t      clen;
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
    clen = (size_t)(e - p);

    wlen = clen + 12; /* "__p__ = {" (9) + content + "}" (1) + NUL */
    buf  = (char *)H5MM_malloc(wlen);
    if (buf)
        snprintf(buf, wlen, "__p__ = {%.*s}", (int)clen, p);
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
    toml_datum_t  ptab, d;
    bool          tr_valid = false;
    htri_t        ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || !*key)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");

    if (!params[0])
        HGOTO_DONE(false);

    if (H5Z__toml_parse_params(params, &tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse parameter string");
    tr_valid = true;

    d         = toml_get(ptab, key);
    ret_value = (d.type != TOML_UNKNOWN) ? true : false;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT(ret_value)
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
    toml_result_t tr;
    toml_datum_t  ptab, d;
    bool          tr_valid = false;
    htri_t        ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || !*key)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");

    if (!params[0])
        HGOTO_DONE(false);

    if (H5Z__toml_parse_params(params, &tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse parameter string");
    tr_valid = true;

    d = toml_get(ptab, key);
    if (d.type == TOML_UNKNOWN)
        HGOTO_DONE(false);
    if (d.type != TOML_INT64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML integer", key);
    *out      = d.u.int64;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
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
    toml_datum_t  ptab, d;
    bool          tr_valid = false;
    htri_t        ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || !*key)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");

    if (!params[0])
        HGOTO_DONE(false);

    if (H5Z__toml_parse_params(params, &tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse parameter string");
    tr_valid = true;

    d = toml_get(ptab, key);
    if (d.type == TOML_UNKNOWN)
        HGOTO_DONE(false);
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
    toml_datum_t  ptab, d;
    bool          tr_valid = false;
    htri_t        ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || !*key)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");

    if (!params[0])
        HGOTO_DONE(false);

    if (H5Z__toml_parse_params(params, &tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse parameter string");
    tr_valid = true;

    d = toml_get(ptab, key);
    if (d.type == TOML_UNKNOWN)
        HGOTO_DONE(false);
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
    toml_result_t tr;
    toml_datum_t  ptab, d;
    bool          tr_valid = false;
    size_t        vlen;
    htri_t        ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || !*key)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");

    if (!params[0])
        HGOTO_DONE(false);

    if (H5Z__toml_parse_params(params, &tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse parameter string");
    tr_valid = true;

    d = toml_get(ptab, key);
    if (d.type == TOML_UNKNOWN)
        HGOTO_DONE(false);
    if (d.type != TOML_STRING)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "type mismatch: key '%s' is not a TOML string (value must be quoted)", key);

    vlen = (size_t)d.u.str.len;

    /* Save the caller's capacity before overwriting *buf_size. */
    {
        size_t cap = buf_size ? *buf_size : 0;

        if (buf_size)
            *buf_size = vlen; /* always report required length */

        if (buf) {
            if (cap == 0) {
                /* No capacity hint — caller guarantees sufficient space. */
                memcpy(buf, d.u.s, vlen + 1);
            }
            else if (cap > vlen) {
                memcpy(buf, d.u.s, vlen + 1);
            }
            else {
                /* Buffer too small — truncate and report overflow. */
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
    FUNC_LEAVE_API_NOINIT(ret_value)
}

#else /* !H5_HAVE_TOMLC17 — built-in custom TOML-subset parser (fallback) */

/* ======================================================================
 * Custom TOML-subset parser
 *
 * This hand-written parser is retained as a fallback for builds that do
 * not have tomlc17 available.  It handles the restricted subset of TOML
 * inline-table syntax used by HDF5 filter configuration strings:
 *
 *   Integer : decimal (optional leading sign), 0x/0o/0b prefixes, and
 *             underscore digit separators.
 *   Float   : decimal with '.' or exponent.  inf/nan are classified as
 *             FLOAT but rejected at the API level with H5E_BADVALUE.
 *   Boolean : exactly "true" or "false" (lowercase, per TOML).
 *   String  : double-quoted with backslash escapes (\\ \" \n \t \r \b
 *             \f \0), or single-quoted with no escape processing.
 *
 * Grammar:
 *   param-string = '' | param (',' param)*
 *   param        = key '=' value | key      (bare key = boolean flag)
 *   key          = [A-Za-z0-9_-]+
 *   value        = integer | float | boolean | dquoted-str | squoted-str
 * ====================================================================== */

/* Maximum key length (internal) */
#define H5Z_CONFIG_MAX_KEY_LEN 256

/* Internal value-type enum */
typedef enum {
    H5Z__CONFIG_VTYPE_NONE,   /* bare key (no '=' sign) */
    H5Z__CONFIG_VTYPE_INT,    /* TOML integer */
    H5Z__CONFIG_VTYPE_FLOAT,  /* TOML float   */
    H5Z__CONFIG_VTYPE_BOOL,   /* TOML boolean */
    H5Z__CONFIG_VTYPE_DSTR,   /* double-quoted string */
    H5Z__CONFIG_VTYPE_SSTR,   /* single-quoted string */
    H5Z__CONFIG_VTYPE_INVALID /* bare value that is not valid TOML */
} H5Z__config_vtype_t;

static const char *
H5Z__config_skip_ws(const char *p)
{
    while (*p == ' ' || *p == '\t')
        p++;
    return p;
}

static size_t
H5Z__config_rtrim(char *buf, size_t len)
{
    while (len > 0 && (buf[len - 1] == ' ' || buf[len - 1] == '\t'))
        len--;
    buf[len] = '\0';
    return len;
}

static H5Z__config_vtype_t
H5Z__config_classify_bare(const char *val, size_t len)
{
    const char *p       = val;
    const char *end     = val + len;
    bool        has_dot = false;
    bool        has_exp = false;

    if (len == 0)
        return H5Z__CONFIG_VTYPE_INVALID;

    if (len == 4 && memcmp(val, "true", 4) == 0)
        return H5Z__CONFIG_VTYPE_BOOL;
    if (len == 5 && memcmp(val, "false", 5) == 0)
        return H5Z__CONFIG_VTYPE_BOOL;

    /* Special float literals (inf/nan) — valid TOML, rejected at API level */
    if (len == 3 && (memcmp(val, "inf", 3) == 0 || memcmp(val, "nan", 3) == 0))
        return H5Z__CONFIG_VTYPE_FLOAT;
    if (len == 4 && (*val == '+' || *val == '-') &&
        (memcmp(val + 1, "inf", 3) == 0 || memcmp(val + 1, "nan", 3) == 0))
        return H5Z__CONFIG_VTYPE_FLOAT;

    if (*p == '+' || *p == '-')
        p++;
    if (p >= end || !(*p >= '0' && *p <= '9'))
        return H5Z__CONFIG_VTYPE_INVALID;

    /* Radix prefix: 0x / 0o / 0b */
    if (*p == '0' && p + 1 < end) {
        char nx = p[1];
        if (nx == 'x' || nx == 'X' || nx == 'o' || nx == 'O' || nx == 'b' || nx == 'B') {
            p += 2;
            if (p >= end)
                return H5Z__CONFIG_VTYPE_INVALID;
            while (p < end) {
                char c = *p;
                if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || c == '_')
                    p++;
                else
                    return H5Z__CONFIG_VTYPE_INVALID;
            }
            return H5Z__CONFIG_VTYPE_INT;
        }
    }

    while (p < end) {
        char c = *p;
        if (c >= '0' && c <= '9') {
            p++;
        }
        else if (c == '_') {
            p++;
        }
        else if (c == '.') {
            if (has_dot || has_exp)
                return H5Z__CONFIG_VTYPE_INVALID;
            has_dot = true;
            p++;
        }
        else if (c == 'e' || c == 'E') {
            if (has_exp)
                return H5Z__CONFIG_VTYPE_INVALID;
            has_exp = true;
            p++;
            if (p < end && (*p == '+' || *p == '-'))
                p++;
            if (p >= end || !(*p >= '0' && *p <= '9'))
                return H5Z__CONFIG_VTYPE_INVALID;
            while (p < end && ((*p >= '0' && *p <= '9') || *p == '_'))
                p++;
        }
        else {
            return H5Z__CONFIG_VTYPE_INVALID;
        }
    }

    return (has_dot || has_exp) ? H5Z__CONFIG_VTYPE_FLOAT : H5Z__CONFIG_VTYPE_INT;
}

static herr_t
H5Z__config_parse_token(const char **pp, char *key_out, size_t key_cap, char *val_out, size_t val_cap,
                        H5Z__config_vtype_t *vtype_out)
{
    const char *p         = *pp;
    size_t      klen      = 0;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    while (*p && *p != ',' && *p != '=') {
        unsigned char c = (unsigned char)*p;
        if (c == ';')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bare semicolon in parameter string (reserved)");
        if (c == '"' || c == '\'')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "quote character in key position");
        if (klen + 1 >= key_cap)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key exceeds maximum length");
        key_out[klen++] = (char)((c >= 'A' && c <= 'Z') ? (c + ('a' - 'A')) : c);
        p++;
    }
    key_out[klen] = '\0';

    klen = H5Z__config_rtrim(key_out, klen);
    {
        size_t lead = 0;
        while (lead < klen && (key_out[lead] == ' ' || key_out[lead] == '\t'))
            lead++;
        if (lead > 0) {
            memmove(key_out, key_out + lead, klen - lead + 1);
            klen -= lead;
        }
    }

    if (klen == 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "empty key in parameter string");

    if (*p != '=') {
        *vtype_out = H5Z__CONFIG_VTYPE_NONE;
        val_out[0] = '\0';
        *pp        = p;
        HGOTO_DONE(SUCCEED);
    }

    p++;
    p = H5Z__config_skip_ws(p);

    if (*p == '\0' || *p == ',')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "parameter '%s' has '=' but no value (use bare key for boolean flags)", key_out);

    if (*p == '"') {
        size_t vlen = 0;
        p++;
        for (;;) {
            if (*p == '\0')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unbalanced double-quote in value for key '%s'",
                            key_out);
            if (*p == '"') {
                p++;
                break;
            }
            if (*p == '\\') {
                p++;
                char esc;
                switch (*p) {
                    case '\\':
                        esc = '\\';
                        break;
                    case '"':
                        esc = '"';
                        break;
                    case 'n':
                        esc = '\n';
                        break;
                    case 't':
                        esc = '\t';
                        break;
                    case 'r':
                        esc = '\r';
                        break;
                    case 'b':
                        esc = '\b';
                        break;
                    case 'f':
                        esc = '\f';
                        break;
                    case '0':
                        esc = '\0';
                        break;
                    default:
                        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                    "unsupported escape '\\%c' in value for key '%s'", *p, key_out);
                }
                if (vlen + 1 >= val_cap)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "value exceeds maximum length");
                val_out[vlen++] = esc;
                p++;
            }
            else {
                if (vlen + 1 >= val_cap)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "value exceeds maximum length");
                val_out[vlen++] = *p++;
            }
        }
        val_out[vlen] = '\0';
        if (vlen == 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "empty double-quoted value for key '%s' is not allowed",
                        key_out);
        p = H5Z__config_skip_ws(p);
        if (*p != '\0' && *p != ',')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "unexpected characters after closing double-quote for key '%s'", key_out);
        *vtype_out = H5Z__CONFIG_VTYPE_DSTR;
        *pp        = p;
        HGOTO_DONE(SUCCEED);
    }

    if (*p == '\'') {
        size_t vlen = 0;
        p++;
        for (;;) {
            if (*p == '\0')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unbalanced single-quote in value for key '%s'",
                            key_out);
            if (*p == '\'') {
                p++;
                break;
            }
            if (vlen + 1 >= val_cap)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "value exceeds maximum length");
            val_out[vlen++] = *p++;
        }
        val_out[vlen] = '\0';
        if (vlen == 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "empty single-quoted value for key '%s' is not allowed",
                        key_out);
        p = H5Z__config_skip_ws(p);
        if (*p != '\0' && *p != ',')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "unexpected characters after closing single-quote for key '%s'", key_out);
        *vtype_out = H5Z__CONFIG_VTYPE_SSTR;
        *pp        = p;
        HGOTO_DONE(SUCCEED);
    }

    {
        size_t vlen = 0;
        while (*p && *p != ',') {
            unsigned char c = (unsigned char)*p;
            if (c == ';')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bare semicolon in value for key '%s' (reserved)",
                            key_out);
            if (c == '"' || c == '\'')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "quote character in bare value for key '%s'",
                            key_out);
            if (vlen + 1 >= val_cap)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "value exceeds maximum length");
            val_out[vlen++] = *p++;
        }
        val_out[vlen] = '\0';
        H5Z__config_rtrim(val_out, vlen);
        vlen = strlen(val_out);

        if (vlen == 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parameter '%s' has empty value after '='", key_out);

        H5Z__config_vtype_t vt = H5Z__config_classify_bare(val_out, vlen);
        if (vt == H5Z__CONFIG_VTYPE_INVALID)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "value '%s' for key '%s' is not a valid TOML integer, float, or boolean", val_out,
                        key_out);

        *vtype_out = vt;
        *pp        = p;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * H5Z__config_strip_braces — if params begins (after whitespace) with '{',
 * return a heap-allocated NUL-terminated copy of the inner content with the
 * outer braces removed.  Returns NULL if no braces are present (caller uses
 * params directly).  The returned buffer must be freed with H5MM_xfree().
 */
static char *
H5Z__config_strip_braces(const char *params)
{
    const char *p, *e;
    size_t      len;
    char       *copy;

    if (!params)
        return NULL;
    p = params;
    while (*p == ' ' || *p == '\t')
        p++;
    if (*p != '{')
        return NULL;
    p++;
    len = strlen(p);
    e   = p + len;
    while (e > p && (*(e - 1) == ' ' || *(e - 1) == '\t'))
        e--;
    if (e > p && *(e - 1) == '}')
        e--;
    len  = (size_t)(e - p);
    copy = (char *)H5MM_malloc(len + 1);
    if (copy) {
        memcpy(copy, p, len);
        copy[len] = '\0';
    }
    return copy;
}

herr_t
H5Z__config_validate_keys(const char *params, const char *const *known_keys)
{
    char       *stripped = NULL;
    const char *use_params;
    const char *p;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (!params || *params == '\0')
        HGOTO_DONE(SUCCEED);

    stripped   = H5Z__config_strip_braces(params);
    use_params = stripped ? stripped : params;

    if (strlen(use_params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_STRING_MAX (%d bytes)",
                    H5Z_CONFIG_STRING_MAX);

    p = H5Z__config_skip_ws(use_params);
    if (*p == ',')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parameter string starts with a comma");

    while (*p) {
        char                tok_key[H5Z_CONFIG_MAX_KEY_LEN + 1];
        char                tok_val[H5Z_CONFIG_STRING_MAX + 1];
        H5Z__config_vtype_t vtype;
        bool                found_key = false;
        size_t              ki;

        p = H5Z__config_skip_ws(p);
        if (*p == '\0')
            break;

        if (H5Z__config_parse_token(&p, tok_key, sizeof(tok_key), tok_val, sizeof(tok_val), &vtype) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "malformed parameter token");

        if (known_keys) {
            for (ki = 0; known_keys[ki] != NULL; ki++) {
                if (strcmp(tok_key, known_keys[ki]) == 0) {
                    found_key = true;
                    break;
                }
            }
        }
        if (!found_key)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown parameter key '%s'", tok_key);

        p = H5Z__config_skip_ws(p);
        if (*p == ',') {
            p++;
            p = H5Z__config_skip_ws(p);
            if (*p == '\0')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parameter string ends with a trailing comma");
        }
        else if (*p != '\0')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unexpected character after token");
    }

done:
    H5MM_xfree(stripped);
    FUNC_LEAVE_NOAPI(ret_value)
}

static bool
H5Z__config_toml_int(const char *val, int64_t *out)
{
    char        tmp[64];
    size_t      j    = 0;
    int         base = 10;
    const char *p    = val;

    if (!val || !out)
        return false;

    if (*p == '-') {
        if (j >= sizeof(tmp) - 1)
            return false;
        tmp[j++] = '-';
        p++;
    }
    else if (*p == '+') {
        p++;
    }

    if (!(*p >= '0' && *p <= '9'))
        return false;

    if (*p == '0' && (p[1] == 'x' || p[1] == 'X')) {
        base = 16;
        p += 2;
        if (j + 2 < sizeof(tmp)) {
            tmp[j++] = '0';
            tmp[j++] = 'x';
        }
        else
            return false;
    }
    else if (*p == '0' && (p[1] == 'o' || p[1] == 'O')) {
        base = 8;
        p += 2;
    }
    else if (*p == '0' && (p[1] == 'b' || p[1] == 'B')) {
        base = 2;
        p += 2;
    }

    while (*p && j < sizeof(tmp) - 1) {
        if (*p != '_')
            tmp[j++] = *p;
        p++;
    }
    tmp[j] = '\0';

    if (j == 0 || (j == 1 && tmp[0] == '-'))
        return false;

    char         *end;
    long long int lv;
    errno = 0;
    lv    = strtoll(tmp, &end, base);
    if (*end != '\0' || errno == ERANGE)
        return false;
    *out = (int64_t)lv;
    return true;
}

static htri_t
H5Z__config_lookup(const char *params, const char *key, char *val_out, size_t val_cap,
                   H5Z__config_vtype_t *vtype_out)
{
    char       *stripped = NULL;
    const char *use_params;
    const char *p;
    size_t      param_count = 0;
    bool        key_found   = false;
    char        seen_keys[H5Z_CONFIG_MAX_PARAMS][H5Z_CONFIG_MAX_KEY_LEN + 1];
    size_t      seen_count = 0;
    char        norm_key[H5Z_CONFIG_MAX_KEY_LEN + 1];
    size_t      ki;
    htri_t      ret_value = false;

    FUNC_ENTER_PACKAGE

    if (!params || !key || *key == '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params and key must be non-NULL non-empty strings");
    if (!params[0])
        HGOTO_DONE(false);

    stripped   = H5Z__config_strip_braces(params);
    use_params = stripped ? stripped : params;

    if (strlen(use_params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_STRING_MAX (%d bytes)",
                    H5Z_CONFIG_STRING_MAX);

    for (ki = 0; key[ki] && ki < H5Z_CONFIG_MAX_KEY_LEN; ki++) {
        unsigned char c = (unsigned char)key[ki];
        norm_key[ki]    = (char)((c >= 'A' && c <= 'Z') ? (c + ('a' - 'A')) : c);
    }
    norm_key[ki] = '\0';
    if (ki >= H5Z_CONFIG_MAX_KEY_LEN && key[ki] != '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "search key exceeds maximum length");

    p = H5Z__config_skip_ws(use_params);
    if (*p == ',')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parameter string starts with a comma");

    while (*p) {
        char                tok_key[H5Z_CONFIG_MAX_KEY_LEN + 1];
        char                tok_val[H5Z_CONFIG_STRING_MAX + 1];
        H5Z__config_vtype_t vtype;
        size_t              si;

        if (param_count >= H5Z_CONFIG_MAX_PARAMS)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parameter string exceeds %d tokens",
                        H5Z_CONFIG_MAX_PARAMS);

        p = H5Z__config_skip_ws(p);
        if (*p == '\0')
            break;

        if (H5Z__config_parse_token(&p, tok_key, sizeof(tok_key), tok_val, sizeof(tok_val), &vtype) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "malformed parameter token");

        for (si = 0; si < seen_count; si++) {
            if (strcmp(seen_keys[si], tok_key) == 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "duplicate key '%s' in parameter string", tok_key);
        }
        H5MM_memcpy(seen_keys[seen_count++], tok_key, strlen(tok_key) + 1);

        if (!key_found && strcmp(tok_key, norm_key) == 0) {
            key_found = true;
            if (val_out && val_cap > 0) {
                size_t vlen = strlen(tok_val);
                if (vlen >= val_cap)
                    vlen = val_cap - 1;
                H5MM_memcpy(val_out, tok_val, vlen);
                val_out[vlen] = '\0';
            }
            if (vtype_out)
                *vtype_out = vtype;
        }

        param_count++;

        p = H5Z__config_skip_ws(p);
        if (*p == ',') {
            p++;
            p = H5Z__config_skip_ws(p);
            if (*p == '\0')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parameter string ends with a trailing comma");
        }
        else if (*p != '\0')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unexpected character after token");
    }

    ret_value = key_found ? true : false;

done:
    H5MM_xfree(stripped);
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
    char                dummy[1];
    H5Z__config_vtype_t vtype;
    htri_t              ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || *key == '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");

    if (!params[0])
        HGOTO_DONE(false);

    ret_value = H5Z__config_lookup(params, key, dummy, sizeof(dummy), &vtype);

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_int
 *
 * Purpose:     Look up a key and return its value as a TOML integer (int64_t).
 *
 * Return:      > 0 found and converted, 0 not found, < 0 error.
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_int(const char *params, const char *key, int64_t *out)
{
    char                val_buf[H5Z_CONFIG_STRING_MAX + 1];
    H5Z__config_vtype_t vtype;
    htri_t              found;
    htri_t              ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || *key == '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");

    if (!params[0])
        HGOTO_DONE(false);

    found = H5Z__config_lookup(params, key, val_buf, sizeof(val_buf), &vtype);
    if (found < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "error parsing parameter string");
    if (!found)
        HGOTO_DONE(false);

    if (vtype != H5Z__CONFIG_VTYPE_INT)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML integer", key);

    if (!H5Z__config_toml_int(val_buf, out))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse integer value '%s' for key '%s'", val_buf,
                    key);

    ret_value = true;

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_double
 *
 * Purpose:     Look up a key and return its value as a TOML float (double).
 *              inf and nan are rejected.
 *
 * Return:      > 0 found and converted, 0 not found, < 0 error.
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_double(const char *params, const char *key, double *out)
{
    char                val_buf[H5Z_CONFIG_STRING_MAX + 1];
    H5Z__config_vtype_t vtype;
    htri_t              found;
    htri_t              ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || *key == '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");

    if (!params[0])
        HGOTO_DONE(false);

    found = H5Z__config_lookup(params, key, val_buf, sizeof(val_buf), &vtype);
    if (found < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "error parsing parameter string");
    if (!found)
        HGOTO_DONE(false);

    if (vtype != H5Z__CONFIG_VTYPE_FLOAT)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML float", key);

    {
        const char *v = val_buf;
        if (*v == '+' || *v == '-')
            v++;
        if (strcmp(v, "inf") == 0 || strcmp(v, "nan") == 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "inf/nan float values are not supported for filter parameters (key '%s')", key);
    }

    {
        char  *end;
        double dv;
        errno = 0;
        dv    = strtod(val_buf, &end);
        if (*end != '\0' || errno == ERANGE)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse float value '%s' for key '%s'",
                        val_buf, key);
        *out = dv;
    }

    ret_value = true;

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_bool
 *
 * Purpose:     Look up a key and return its TOML boolean value (hbool_t).
 *              Bare keys (boolean flags without '=') are treated as TRUE.
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_bool(const char *params, const char *key, hbool_t *out)
{
    char                val_buf[H5Z_CONFIG_STRING_MAX + 1];
    H5Z__config_vtype_t vtype;
    htri_t              found;
    htri_t              ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || *key == '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");

    if (!params[0])
        HGOTO_DONE(false);

    found = H5Z__config_lookup(params, key, val_buf, sizeof(val_buf), &vtype);
    if (found < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "error parsing parameter string");
    if (!found)
        HGOTO_DONE(false);

    if (vtype == H5Z__CONFIG_VTYPE_NONE) {
        *out = true; /* bare key = implicit TRUE */
    }
    else if (vtype == H5Z__CONFIG_VTYPE_BOOL) {
        *out = (strcmp(val_buf, "true") == 0) ? true : false;
    }
    else {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML boolean", key);
    }

    ret_value = true;

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_str
 *
 * Purpose:     Look up a key and return its TOML string value (decoded,
 *              without surrounding quotes).  Only quoted values (double- or
 *              single-quoted) are accepted; bare values are type errors.
 *
 *              Size-query pattern: see H5Zconfig_get_str in H5Zdevelop.h.
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_str(const char *params, const char *key, char *buf, size_t *buf_size)
{
    char                val_buf[H5Z_CONFIG_STRING_MAX + 1];
    H5Z__config_vtype_t vtype;
    htri_t              found;
    size_t              vlen;
    htri_t              ret_value;

    FUNC_ENTER_API_NOINIT

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || *key == '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");

    if (!params[0])
        HGOTO_DONE(false);

    found = H5Z__config_lookup(params, key, val_buf, sizeof(val_buf), &vtype);
    if (found < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "error parsing parameter string");
    if (!found)
        HGOTO_DONE(false);

    if (vtype != H5Z__CONFIG_VTYPE_DSTR && vtype != H5Z__CONFIG_VTYPE_SSTR)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "type mismatch: key '%s' is not a TOML string (value must be quoted)", key);

    vlen = strlen(val_buf);

    /* Save the caller's capacity before overwriting *buf_size. */
    {
        size_t cap = buf_size ? *buf_size : 0;

        if (buf_size)
            *buf_size = vlen; /* always report required length */

        if (buf) {
            if (cap == 0) {
                /* No capacity hint — caller guarantees sufficient space. */
                H5MM_memcpy(buf, val_buf, vlen + 1);
            }
            else if (cap > vlen) {
                H5MM_memcpy(buf, val_buf, vlen + 1);
            }
            else {
                /* Buffer too small — truncate and report overflow. */
                if (cap > 0) {
                    H5MM_memcpy(buf, val_buf, cap - 1);
                    buf[cap - 1] = '\0';
                }
                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                            "output buffer too small for string value of key '%s'", key);
            }
        }
    }

    ret_value = true;

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
}

#endif /* H5_HAVE_TOMLC17 */
