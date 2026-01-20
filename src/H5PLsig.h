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

#ifndef H5PLsig_H
#define H5PLsig_H

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

/*
 * Appended Signature Format
 * =========================
 *
 * Plugin files use an appended signature format:
 *
 *   [ Binary Data (ELF/DLL/Mach-O) ] [ RSA Signature ] [ Footer ]
 *
 * The footer contains metadata about the signature and a magic number
 * to identify signed plugins. The binary loader ignores trailing data,
 * so the plugin loads normally.
 *
 * This approach:
 *   - Works on all platforms (Linux, Windows, macOS)
 *   - No ELF parsing required
 *   - No external tools needed (objcopy, etc.)
 *   - Simple append operation for signing
 *   - Simple read-from-end for verification
 */

/* Magic number to identify HDF5 signed plugins */
#define H5PL_SIG_MAGIC 0x48444635  /* "HDF5" in hex */

/* Signature footer structure (placed at end of file) */
typedef struct H5PL_sig_footer_t {
    uint32_t signature_length; /* Length of RSA signature in bytes */
    uint32_t magic;            /* Magic number (H5PL_SIG_MAGIC) */
} H5PL_sig_footer_t;

/*
 * Public Key (PEM format, hardcoded for security)
 *
 * This is the RSA public key used to verify plugin signatures.
 * It is hardcoded in the library to prevent tampering.
 *
 * IMPORTANT: In production, replace this with your actual public key.
 * This key should match the private key used to sign plugins.
 *
 * To generate a key pair:
 *   openssl genrsa -out private.pem 2048
 *   openssl rsa -in private.pem -pubout -out public.pem
 */

/* Default public key (REPLACE THIS IN PRODUCTION) */
#ifndef H5PL_PUBLIC_KEY_PEM
#define H5PL_PUBLIC_KEY_PEM                                                                                  \
    "-----BEGIN PUBLIC KEY-----\n"                                                                           \
    "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA1234567890ABCDEFGHIJ\n"                                       \
    "KLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz1234567890\n"                                       \
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz\n"                                       \
    "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnop\n"                                       \
    "qrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdef\n"                                       \
    "ghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ123456\n"                                       \
    "7890abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUV\n"                                       \
    "WXYZ1234567890abcdefghijklmnopqrstuvwxyz12345678901234567890AB\n"                                       \
    "-----END PUBLIC KEY-----\n"
#endif

/* CMake can define this to use a custom public key */
/* Example: -DH5PL_PUBLIC_KEY_PEM="$(cat /path/to/public.pem)" */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */

#endif /* H5PLsig_H */
