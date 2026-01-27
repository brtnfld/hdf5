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
 * KeyStore Configuration for Plugin Signature Verification
 *
 * HDF5 supports multiple trusted keys through the KeyStore approach:
 *
 * KEY LOADING PRIORITY (first found wins):
 *   1. Environment variable: HDF5_PLUGIN_KEYSTORE=/path/to/keys
 *   2. CMake-configured directory: HDF5_PLUGIN_KEYSTORE_DIR=/etc/hdf5/trusted_keys
 *   3. Compile-time embedded key: H5PL_PUBLIC_KEY_PEM (backward compatibility)
 *
 * KEYSTORE DIRECTORY:
 *   - Contains multiple .pem files (one public key per file)
 *   - Allows trusting plugins from multiple organizations (LLNL, ANL, HDFGroup)
 *   - Add/remove keys without recompiling HDF5
 *   - Must NOT be world-writable (security check enforced)
 *
 * VERIFICATION LOGIC:
 *   - Plugin signature is verified against ALL keys in KeyStore (OR logic)
 *   - First matching key succeeds → plugin is trusted
 *   - No matching key → plugin is rejected
 *
 * EXAMPLE USAGE:
 *
 *   # Setup KeyStore with multiple trusted organizations
 *   sudo mkdir -p /etc/hdf5/trusted_keys
 *   sudo chmod 755 /etc/hdf5/trusted_keys
 *   sudo cp llnl_public.pem /etc/hdf5/trusted_keys/
 *   sudo cp anl_public.pem /etc/hdf5/trusted_keys/
 *   sudo cp hdfgroup_public.pem /etc/hdf5/trusted_keys/
 *
 *   # Build HDF5 with KeyStore
 *   cmake -DHDF5_REQUIRE_SIGNED_PLUGINS=ON \
 *         -DHDF5_PLUGIN_KEYSTORE_DIR=/etc/hdf5/trusted_keys \
 *         ..
 *
 *   # Runtime override via environment variable
 *   export HDF5_PLUGIN_KEYSTORE=/tmp/test_keys
 *
 * BACKWARD COMPATIBILITY:
 *   - Compile-time embedded key (H5PL_PUBLIC_KEY_PEM) still supported
 *   - Use this for single-key deployments or backward compatibility
 *
 *   cmake -DHDF5_REQUIRE_SIGNED_PLUGINS=ON \
 *         -DHDF5_PLUGIN_PUBLIC_KEY_FILE=/path/to/public.pem \
 *         ..
 *
 * KEY GENERATION:
 *   openssl genrsa -out private.pem 2048
 *   openssl rsa -in private.pem -pubout -out public.pem
 *
 * SECURITY NOTES:
 *   - KeyStore directory must NOT be world-writable
 *   - This prevents unprivileged users from adding malicious keys
 *   - Symbolic links are IGNORED for security (prevents symlink attacks)
 *   - Only regular .pem files in the directory are loaded
 */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */

#endif /* H5PLsig_H */
