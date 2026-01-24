#!/usr/bin/env python3
################################################################################
# sign-hdf5-plugin.py - Sign an HDF5 plugin with RSA digital signature
#
# Usage: sign-hdf5-plugin.py <plugin-file> <private-key.pem>
#
# This script appends an RSA signature to an HDF5 plugin file using the
# format expected by HDF5's plugin signature verification:
#
#   [ Plugin Binary ] [ RSA Signature ] [ Footer ]
#
# Where Footer contains:
#   - Signature length (4 bytes, little-endian)
#   - Magic number 0x48444635 "HDF5" (4 bytes, little-endian)
#
# Requirements:
#   - Python 3.6+
#   - OpenSSL command-line tools
#
# Security Notes:
#   - Keep private key secure (chmod 600 private.pem)
#   - Use strong keys (2048-bit minimum, 4096-bit recommended)
#   - Verify plugin code before signing
#   - Log all signing operations
#
# Copyright by The HDF Group
################################################################################

import sys
import os
import struct
import subprocess
import tempfile
import shutil
from pathlib import Path

# Configuration
HASH_ALGORITHM = 'sha256'
MAGIC_NUMBER = 0x48444635  # "HDF5" in hex

# ANSI color codes
class Colors:
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    NC = '\033[0m'  # No Color

def print_error(message):
    """Print error message in red"""
    print(f"{Colors.RED}Error: {message}{Colors.NC}", file=sys.stderr)

def print_success(message):
    """Print success message in green"""
    print(f"{Colors.GREEN}{message}{Colors.NC}")

def print_warning(message):
    """Print warning message in yellow"""
    print(f"{Colors.YELLOW}{message}{Colors.NC}")

def check_openssl():
    """Check if OpenSSL is available"""
    try:
        result = subprocess.run(
            ['openssl', 'version'],
            capture_output=True,
            text=True,
            check=True
        )
        return result.stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return None

def validate_private_key(key_path):
    """Validate that the file is a PEM-formatted private key"""
    try:
        with open(key_path, 'r') as f:
            content = f.read()
            if 'BEGIN' not in content or 'PRIVATE KEY' not in content:
                return False
            return True
    except Exception:
        return False

def sign_plugin(plugin_path, private_key_path):
    """Sign a plugin file with RSA signature appended to end"""

    # Validate inputs
    plugin = Path(plugin_path)
    private_key = Path(private_key_path)

    if not plugin.exists():
        print_error(f"Plugin file not found: {plugin_path}")
        return False

    if not private_key.exists():
        print_error(f"Private key not found: {private_key_path}")
        return False

    if not validate_private_key(private_key_path):
        print_error("Invalid private key format")
        print("Expected PEM format with 'BEGIN PRIVATE KEY' or 'BEGIN RSA PRIVATE KEY'")
        return False

    # Get original size
    original_size = plugin.stat().st_size

    print()
    print("HDF5 Plugin Signature Tool")
    print("============================")
    print()
    print(f"Plugin:      {plugin_path}")
    print(f"Private Key: {private_key_path}")
    print(f"Size:        {original_size:,} bytes")
    print()

    # Create temporary directory for intermediate files
    with tempfile.TemporaryDirectory() as temp_dir:
        hash_file = Path(temp_dir) / 'plugin.hash'
        sig_file = Path(temp_dir) / 'plugin.sig'

        try:
            # Step 1: Calculate SHA-256 hash of the plugin binary
            print("Calculating SHA-256 hash... ", end='', flush=True)
            with open(hash_file, 'wb') as f:
                result = subprocess.run(
                    ['openssl', 'dgst', f'-{HASH_ALGORITHM}', '-binary', str(plugin)],
                    stdout=f,
                    stderr=subprocess.PIPE,
                    check=True
                )
            print_success("✓")

            # Step 2: Sign the hash with RSA private key
            print("Signing with RSA private key... ", end='', flush=True)
            with open(sig_file, 'wb') as f:
                result = subprocess.run(
                    [
                        'openssl', 'pkeyutl', '-sign',
                        '-inkey', str(private_key),
                        '-in', str(hash_file),
                        '-pkeyopt', f'digest:{HASH_ALGORITHM}',
                    ],
                    stdout=f,
                    stderr=subprocess.PIPE,
                    check=True
                )
            print_success("✓")

            # Get signature length
            sig_len = sig_file.stat().st_size
            print(f"Signature length: {sig_len} bytes")

            # Step 3: Append signature to plugin file
            print("Appending signature to plugin... ", end='', flush=True)
            with open(plugin, 'ab') as pf:
                with open(sig_file, 'rb') as sf:
                    pf.write(sf.read())
            print_success("✓")

            # Step 4: Append footer (signature length + magic number)
            print("Writing signature footer... ", end='', flush=True)
            with open(plugin, 'ab') as f:
                # Write signature length (4 bytes, little-endian)
                f.write(struct.pack('<I', sig_len))
                # Write magic number (4 bytes, little-endian)
                f.write(struct.pack('<I', MAGIC_NUMBER))
            print_success("✓")

            # Get final size
            final_size = plugin.stat().st_size
            overhead = final_size - original_size

            print()
            print_success("Plugin signed successfully!")
            print()
            print("File size:")
            print(f"  Original: {original_size:,} bytes")
            print(f"  Signed:   {final_size:,} bytes")
            print(f"  Overhead: {overhead:,} bytes (signature: {sig_len}, footer: 8)")
            print()
            print("The signed plugin can now be loaded by HDF5 with signature verification enabled.")
            print()
            print_warning("Security reminders:")
            print(f"  - Keep your private key secure (chmod 600 {private_key_path})")
            print("  - Never share or commit your private key")
            print("  - Verify plugin code before signing")
            print("  - Test the signed plugin before deployment")
            print()

            return True

        except subprocess.CalledProcessError as e:
            print_error("OpenSSL command failed")
            print(f"Command: {' '.join(e.cmd)}", file=sys.stderr)
            if e.stderr:
                print(f"Error output: {e.stderr.decode()}", file=sys.stderr)
            return False
        except Exception as e:
            print_error(f"Unexpected error: {e}")
            return False

def usage():
    """Print usage information"""
    print("Usage: sign-hdf5-plugin.py <plugin-file> <private-key.pem>")
    print()
    print("Sign an HDF5 plugin with RSA digital signature.")
    print()
    print("Arguments:")
    print("  plugin-file       Path to the plugin binary to sign (.so, .dll, .dylib)")
    print("  private-key.pem   Path to RSA private key in PEM format")
    print()
    print("Example:")
    print("  sign-hdf5-plugin.py /path/to/libmyplugin.so hdf5-plugin-private.pem")
    print()
    print("To generate a key pair:")
    print("  openssl genrsa -out hdf5-plugin-private.pem 2048")
    print("  openssl rsa -in hdf5-plugin-private.pem -pubout -out hdf5-plugin-public.pem")
    print()

def main():
    """Main entry point"""

    # Check arguments
    if len(sys.argv) != 3:
        usage()
        sys.exit(1)

    # Check for OpenSSL
    openssl_version = check_openssl()
    if not openssl_version:
        print_error("OpenSSL not found")
        print("Please install OpenSSL command-line tools:")
        print("  Ubuntu/Debian: sudo apt-get install openssl")
        print("  macOS: brew install openssl")
        print("  Windows: Download from https://slproweb.com/products/Win32OpenSSL.html")
        sys.exit(1)

    # Sign the plugin
    plugin_file = sys.argv[1]
    private_key_file = sys.argv[2]

    success = sign_plugin(plugin_file, private_key_file)
    sys.exit(0 if success else 1)

if __name__ == '__main__':
    main()
