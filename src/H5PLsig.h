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
#define H5PL_SIG_MAGIC 0x48444635 /* "HDF5" in hex */

/* Signature footer structure (placed at end of file)
 *
 * IMPORTANT: Footer uses little-endian byte order for cross-platform compatibility,
 * following HDF5's metadata encoding convention. When reading from disk, values
 * must be decoded using UINT32DECODE() to convert from little-endian to native
 * byte order. When writing to disk, values must be encoded using UINT32ENCODE()
 * or equivalent (e.g., Python struct.pack('<I')).
 */
typedef struct H5PL_sig_footer_t {
    uint32_t signature_length; /* Length of RSA signature in bytes (little-endian on disk) */
    uint32_t magic;            /* Magic number H5PL_SIG_MAGIC (little-endian on disk) */
} H5PL_sig_footer_t;

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

/*
 * Public Key Configuration (REQUIRED for plugin signature verification)
 *
 * SECURITY REQUIREMENT: H5PL_PUBLIC_KEY_PEM must be defined at compile time
 * via CMake configuration. There is NO default key because:
 *   1. Any default would be insecure (publicly known)
 *   2. Users must generate their own RSA key pair
 *   3. The public key must match the private key used to sign plugins
 *
 * To generate a key pair:
 *   openssl genrsa -out private.pem 2048
 *   openssl rsa -in private.pem -pubout -out public.pem
 *
 * To configure via CMake:
 *   cmake -DHDF5_REQUIRE_SIGNED_PLUGINS=ON \
 *         -DH5PL_PUBLIC_KEY_PEM="$(cat /path/to/public.pem)" \
 *         ..
 *
 * Alternatively, set HDF5_PLUGIN_PUBLIC_KEY_FILE in CMakeLists.txt:
 *   set(HDF5_PLUGIN_PUBLIC_KEY_FILE "/path/to/public.pem")
 */

/* Compile-time validation: key MUST be provided when signature verification is enabled */
#ifndef H5PL_PUBLIC_KEY_PEM
#error                                                                                                       \
    "H5PL_PUBLIC_KEY_PEM must be defined via CMake when HDF5_REQUIRE_SIGNED_PLUGINS=ON. See H5PLsig.h for key generation and configuration instructions."
#endif

/* Note: Size validation is performed at runtime in H5PL__verify_signature() */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */

#endif /* H5PLsig_H */
