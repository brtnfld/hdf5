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
 * H5Zconfig.c — Parameter string parser for the string-based filter API.
 *
 * Implements H5Zconfig_get_param(), which searches a comma-separated
 * key=value parameter string for a named key and returns its value.
 *
 * Grammar (normative, from RFC-HDFG-2026-001):
 *
 *   param-string  = '' | param-list
 *   param-list    = param (',' param)*
 *   param         = key '=' value | key          (bare key = boolean flag)
 *   key           = printable-ascii-no-special+
 *   value         = bare-value | quoted-value
 *   bare-value    = printable-ascii-no-special+
 *   quoted-value  = '"' (non-quote | '""')* '"'
 *
 * where printable-ascii-no-special is U+0021-U+007E excluding ',', '=', '"', ';'.
 *
 * Additional constraints (all cause H5E_BADVALUE on error return):
 *   - String length must not exceed H5Z_CONFIG_STRING_MAX
 *   - Token count must not exceed H5Z_CONFIG_MAX_PARAMS
 *   - Empty keys (token starts with '=') are rejected
 *   - 'key=' (equals with no value) is rejected
 *   - key="" (quoted empty string) is rejected
 *   - Unbalanced opening quote is rejected
 *   - Duplicate keys are rejected
 *   - Bare semicolons outside quotes are rejected (reserved)
 */

#define H5Z_FRIEND /* suppress error on H5Zpkg.h include */

#include "H5Zmodule.h"

#include "H5private.h"   /* Generic Functions   */
#include "H5Eprivate.h"  /* Error handling      */
#include "H5MMprivate.h" /* Memory management   */
#include "H5Zpkg.h"      /* Filter internals    */

/* Maximum key length (generous limit; keys are typically <32 chars) */
#define H5Z_CONFIG_MAX_KEY_LEN 256

/*
 * Skip leading ASCII whitespace (space and tab).
 */
static const char *
H5Z__config_skip_ws(const char *p)
{
    while (*p == ' ' || *p == '\t')
        p++;
    return p;
}

/*
 * Strip trailing ASCII whitespace from a string in-place.
 * Returns the new length (not including NUL).
 */
static size_t
H5Z__config_rtrim(char *buf, size_t len)
{
    while (len > 0 && (buf[len - 1] == ' ' || buf[len - 1] == '\t'))
        len--;
    buf[len] = '\0';
    return len;
}

/*
 * H5Z__config_parse_token — Parse one key (and optionally value) from *pp.
 *
 * On entry *pp points to the start of a token (after any leading comma has
 * been consumed and whitespace stripped).  On exit *pp points to the next
 * comma (or end of string).
 *
 * key_out   — receives the key string (NUL-terminated, lowercase, trimmed).
 * val_out   — receives the value string (NUL-terminated, trimmed, unescaped).
 *             If the token is a bare key, *val_out is set to NULL.
 * is_bare   — set to true for bare keys (no '=' sign).
 *
 * Returns SUCCEED or FAIL (with error pushed).
 */
static herr_t
H5Z__config_parse_token(const char **pp, char *key_out, size_t key_cap, char *val_out, size_t val_cap,
                        bool *is_bare)
{
    const char *p   = *pp;
    size_t      klen = 0;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* --- Collect key --- */
    while (*p && *p != ',' && *p != '=') {
        unsigned char c = (unsigned char)*p;
        if (c == ';')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bare semicolon in parameter string (reserved)");
        if (c == '"')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "double-quote in key position");
        if (klen + 1 >= key_cap)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key exceeds maximum length");
        /* Normalise to lowercase using C-locale tolower */
        key_out[klen++] = (char)(((unsigned char)*p >= 'A' && (unsigned char)*p <= 'Z')
                                     ? (*p + ('a' - 'A'))
                                     : *p);
        p++;
    }
    key_out[klen] = '\0';

    /* Strip trailing whitespace from key */
    klen = H5Z__config_rtrim(key_out, klen);

    /* Strip leading whitespace from key (already done by caller, but be safe) */
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

    /* --- Check for '=' --- */
    if (*p != '=') {
        /* Bare key */
        *is_bare = true;
        val_out[0] = '\0';
        *pp = p;
        HGOTO_DONE(SUCCEED);
    }

    /* Consume '=' */
    p++;

    *is_bare = false;

    /* Skip whitespace after '=' */
    p = H5Z__config_skip_ws(p);

    /* Empty value (key= with nothing after)? */
    if (*p == '\0' || *p == ',')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "parameter '%s' has an equals sign but no value (use bare key for boolean flags)",
                    key_out);

    /* --- Collect value --- */
    if (*p == '"') {
        /* Quoted value */
        size_t vlen = 0;
        p++; /* consume opening quote */
        for (;;) {
            if (*p == '\0')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                            "unbalanced double-quote in parameter value for key '%s'", key_out);
            if (*p == '"') {
                p++;
                if (*p == '"') {
                    /* Escaped double-quote ("") */
                    if (vlen + 1 >= val_cap)
                        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "value exceeds maximum length");
                    val_out[vlen++] = '"';
                    p++;
                }
                else {
                    /* Closing quote */
                    break;
                }
            }
            else {
                if (vlen + 1 >= val_cap)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "value exceeds maximum length");
                val_out[vlen++] = *p++;
            }
        }
        val_out[vlen] = '\0';

        if (vlen == 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "quoted empty string value for key '%s' is not allowed", key_out);

        /* After closing quote, must be end-of-string or comma */
        p = H5Z__config_skip_ws(p);
        if (*p != '\0' && *p != ',')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "unexpected characters after closing quote for key '%s'", key_out);
    }
    else {
        /* Bare value */
        size_t vlen = 0;
        while (*p && *p != ',') {
            unsigned char c = (unsigned char)*p;
            if (c == ';')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                            "bare semicolon in value for key '%s' (reserved)", key_out);
            if (c == '"')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                            "double-quote in bare value for key '%s'", key_out);
            if (vlen + 1 >= val_cap)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "value exceeds maximum length");
            val_out[vlen++] = *p++;
        }
        val_out[vlen] = '\0';
        H5Z__config_rtrim(val_out, vlen);

        if (val_out[0] == '\0')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "parameter '%s' has empty value after '='", key_out);
    }

    *pp = p;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_param
 *
 * Purpose:     Search a filter parameter string for the given key and
 *              return its value.
 *
 *              The parser validates the entire string on every call.
 *              Duplicate keys cause an error even if neither is the sought key.
 *
 * Parameters:
 *   params    — Comma-separated key=value string, or NULL (= no parameters).
 *   key       — Key to look for (compared case-insensitively).
 *   value_buf — Buffer for the result value, or NULL for size query.
 *   buf_size  — In/out: on entry, capacity of value_buf; on return, bytes
 *               needed (excluding NUL).  May be NULL if value_buf is also NULL.
 *
 * Return:
 *   > 0  key found; value written to value_buf (if non-NULL and large enough)
 *   = 0  key not found
 *   < 0  error (malformed string)
 *
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_param(const char *params, const char *key, char *value_buf, size_t *buf_size)
{
    const char *p;
    size_t      param_count = 0;
    bool        key_found   = false;
    char        found_val[H5Z_CONFIG_STRING_MAX + 1];
    size_t      found_val_len = 0;
    /* Seen-keys array to detect duplicates — on-stack for small counts */
    char        seen_keys[H5Z_CONFIG_MAX_PARAMS][H5Z_CONFIG_MAX_KEY_LEN + 1];
    size_t      seen_count = 0;
    /* Normalised search key */
    char        norm_key[H5Z_CONFIG_MAX_KEY_LEN + 1];
    size_t      ki;
    htri_t      ret_value = false;

    FUNC_ENTER_API_NOINIT

    /* NULL params is a caller error; empty string means no parameters */
    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params string is NULL");
    if (*params == '\0')
        HGOTO_DONE(false);

    /* Validate key argument */
    if (!key || *key == '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");

    /* Validate total string length */
    if (strlen(params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "parameter string exceeds H5Z_CONFIG_STRING_MAX (%d) bytes", H5Z_CONFIG_STRING_MAX);

    /* Normalise search key to lowercase */
    for (ki = 0; key[ki] && ki < H5Z_CONFIG_MAX_KEY_LEN; ki++) {
        unsigned char c = (unsigned char)key[ki];
        norm_key[ki]    = (char)((c >= 'A' && c <= 'Z') ? (c + ('a' - 'A')) : c);
    }
    norm_key[ki] = '\0';
    if (ki >= H5Z_CONFIG_MAX_KEY_LEN && key[ki] != '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "search key exceeds maximum length");

    p = H5Z__config_skip_ws(params);

    /* Reject leading comma */
    if (*p == ',')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parameter string starts with a comma");

    while (*p) {
        char tok_key[H5Z_CONFIG_MAX_KEY_LEN + 1];
        char tok_val[H5Z_CONFIG_STRING_MAX + 1];
        bool is_bare;
        size_t si;

        if (param_count >= H5Z_CONFIG_MAX_PARAMS)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                        "parameter string exceeds maximum of %d tokens", H5Z_CONFIG_MAX_PARAMS);

        p = H5Z__config_skip_ws(p);
        if (*p == '\0')
            break;

        if (H5Z__config_parse_token(&p, tok_key, sizeof(tok_key), tok_val, sizeof(tok_val), &is_bare) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "malformed parameter token");

        /* Duplicate key check */
        for (si = 0; si < seen_count; si++) {
            if (strcmp(seen_keys[si], tok_key) == 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                            "duplicate key '%s' in parameter string", tok_key);
        }
        H5MM_memcpy(seen_keys[seen_count++], tok_key, strlen(tok_key) + 1);

        /* Match? */
        if (strcmp(tok_key, norm_key) == 0) {
            key_found     = true;
            found_val_len = strlen(tok_val);
            H5MM_memcpy(found_val, tok_val, found_val_len + 1);
        }

        param_count++;

        /* Consume comma or stop */
        p = H5Z__config_skip_ws(p);
        if (*p == ',') {
            p++;
            /* Reject trailing comma */
            p = H5Z__config_skip_ws(p);
            if (*p == '\0')
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "parameter string ends with a trailing comma");
        }
        else if (*p != '\0')
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unexpected character after token");
    } /* while */

    if (!key_found)
        HGOTO_DONE(false);

    /* Key was found — report size and optionally copy value */
    if (buf_size)
        *buf_size = found_val_len;
    if (value_buf && buf_size && *buf_size >= found_val_len) {
        H5MM_memcpy(value_buf, found_val, found_val_len + 1);
    }
    else if (value_buf) {
        /* Caller provided a buffer but size wasn't passed correctly;
         * still populate buf_size so caller can retry */
    }

    ret_value = true;

done:
    FUNC_LEAVE_API_NOINIT(ret_value)
} /* H5Zconfig_get_param() */
