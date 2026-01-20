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
 * Purpose: ELF file parsing for plugin signature extraction
 *
 *          This module provides secure, platform-independent ELF parsing
 *          to extract embedded signature sections without relying on
 *          external tools like objcopy.
 */

/****************/
/* Module Setup */
/****************/

#include "H5PLmodule.h" /* This source code file is part of the H5PL module */

/***********/
/* Headers */
/***********/
#include "H5private.h"  /* Generic Functions            */
#include "H5Eprivate.h" /* Error handling               */
#include "H5PLpkg.h"    /* Plugin                       */
#include "H5MMprivate.h" /* Memory management           */

#ifdef H5_REQUIRE_DIGITAL_SIGNATURE

/* Only compile ELF parsing on Unix-like systems */
#if defined(__linux__) || defined(__unix__) || defined(__APPLE__)

#include <elf.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

/*-------------------------------------------------------------------------
 * Function:    H5PL__read_file_contents
 *
 * Purpose:     Read entire file into memory
 *
 * Return:      Success: Pointer to allocated buffer containing file
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5PL__read_file_contents(const char *filename, size_t *file_size)
{
    int           fd         = -1;
    struct stat   st;
    void         *buffer     = NULL;
    ssize_t       bytes_read = 0;
    void         *ret_value  = NULL;

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(filename);
    assert(file_size);

    /* Open file */
    if ((fd = open(filename, O_RDONLY)) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, NULL, "cannot open file");

    /* Get file size */
    if (fstat(fd, &st) < 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTGET, NULL, "cannot get file size");

    *file_size = (size_t)st.st_size;

    /* Allocate buffer */
    if (NULL == (buffer = H5MM_malloc(*file_size)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, NULL, "cannot allocate buffer");

    /* Read entire file */
    bytes_read = read(fd, buffer, *file_size);
    if (bytes_read != (ssize_t)*file_size)
        HGOTO_ERROR(H5E_PLUGIN, H5E_READERROR, NULL, "cannot read file");

    ret_value = buffer;
    buffer    = NULL; /* Prevent cleanup */

done:
    if (fd >= 0)
        close(fd);
    if (buffer)
        H5MM_xfree(buffer);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__read_file_contents() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__extract_elf_signature_64
 *
 * Purpose:     Extract signature section from 64-bit ELF file
 *
 * Return:      Success: Pointer to signature data (caller must free)
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5PL__extract_elf_signature_64(const void *elf_data, size_t elf_size, size_t *sig_size,
                               void **plugin_data_without_sig, size_t *plugin_size_without_sig)
{
    const Elf64_Ehdr *ehdr      = (const Elf64_Ehdr *)elf_data;
    const Elf64_Shdr *shdr      = NULL;
    const char       *shstrtab  = NULL;
    void             *sig_data  = NULL;
    void             *clean_elf = NULL;
    size_t            i;
    void             *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(elf_data);
    assert(sig_size);
    assert(plugin_data_without_sig);
    assert(plugin_size_without_sig);

    /* Validate ELF header */
    if (elf_size < sizeof(Elf64_Ehdr))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "file too small for ELF header");

    if (ehdr->e_shoff == 0 || ehdr->e_shnum == 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "no section headers");

    /* Get section header table */
    if (ehdr->e_shoff + (ehdr->e_shnum * sizeof(Elf64_Shdr)) > elf_size)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "section headers beyond file size");

    shdr = (const Elf64_Shdr *)((const char *)elf_data + ehdr->e_shoff);

    /* Get section header string table */
    if (ehdr->e_shstrndx >= ehdr->e_shnum)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "invalid section header string table index");

    if (shdr[ehdr->e_shstrndx].sh_offset >= elf_size)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "section header string table beyond file size");

    shstrtab = (const char *)elf_data + shdr[ehdr->e_shstrndx].sh_offset;

    /* Find signature section */
    for (i = 0; i < ehdr->e_shnum; i++) {
        if (shdr[i].sh_name < shdr[ehdr->e_shstrndx].sh_size) {
            const char *section_name = shstrtab + shdr[i].sh_name;
            if (strcmp(section_name, "sig") == 0 || strcmp(section_name, ".sig") == 0) {
                /* Found signature section */
                if (shdr[i].sh_offset + shdr[i].sh_size > elf_size)
                    HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "signature section beyond file size");

                *sig_size = (size_t)shdr[i].sh_size;

                /* Allocate and copy signature data */
                if (NULL == (sig_data = H5MM_malloc(*sig_size)))
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, NULL, "cannot allocate signature buffer");

                memcpy(sig_data, (const char *)elf_data + shdr[i].sh_offset, *sig_size);

                /* Create a copy of the ELF without the signature section for verification */
                /* We'll create a modified ELF with the signature section zeroed out */
                if (NULL == (clean_elf = H5MM_malloc(elf_size)))
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, NULL, "cannot allocate clean ELF buffer");

                memcpy(clean_elf, elf_data, elf_size);

                /* Zero out the signature section content */
                memset((char *)clean_elf + shdr[i].sh_offset, 0, shdr[i].sh_size);

                *plugin_data_without_sig = clean_elf;
                *plugin_size_without_sig = elf_size;

                ret_value = sig_data;
                sig_data  = NULL; /* Prevent cleanup */
                clean_elf = NULL; /* Prevent cleanup */

                goto done;
            }
        }
    }

    /* Signature section not found */
    HGOTO_ERROR(H5E_PLUGIN, H5E_NOTFOUND, NULL, "signature section not found in ELF file");

done:
    if (sig_data)
        H5MM_xfree(sig_data);
    if (clean_elf)
        H5MM_xfree(clean_elf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__extract_elf_signature_64() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__extract_elf_signature_32
 *
 * Purpose:     Extract signature section from 32-bit ELF file
 *
 * Return:      Success: Pointer to signature data (caller must free)
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5PL__extract_elf_signature_32(const void *elf_data, size_t elf_size, size_t *sig_size,
                               void **plugin_data_without_sig, size_t *plugin_size_without_sig)
{
    const Elf32_Ehdr *ehdr      = (const Elf32_Ehdr *)elf_data;
    const Elf32_Shdr *shdr      = NULL;
    const char       *shstrtab  = NULL;
    void             *sig_data  = NULL;
    void             *clean_elf = NULL;
    size_t            i;
    void             *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(elf_data);
    assert(sig_size);
    assert(plugin_data_without_sig);
    assert(plugin_size_without_sig);

    /* Validate ELF header */
    if (elf_size < sizeof(Elf32_Ehdr))
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "file too small for ELF header");

    if (ehdr->e_shoff == 0 || ehdr->e_shnum == 0)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "no section headers");

    /* Get section header table */
    if (ehdr->e_shoff + (ehdr->e_shnum * sizeof(Elf32_Shdr)) > elf_size)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "section headers beyond file size");

    shdr = (const Elf32_Shdr *)((const char *)elf_data + ehdr->e_shoff);

    /* Get section header string table */
    if (ehdr->e_shstrndx >= ehdr->e_shnum)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "invalid section header string table index");

    if (shdr[ehdr->e_shstrndx].sh_offset >= elf_size)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "section header string table beyond file size");

    shstrtab = (const char *)elf_data + shdr[ehdr->e_shstrndx].sh_offset;

    /* Find signature section */
    for (i = 0; i < ehdr->e_shnum; i++) {
        if (shdr[i].sh_name < shdr[ehdr->e_shstrndx].sh_size) {
            const char *section_name = shstrtab + shdr[i].sh_name;
            if (strcmp(section_name, "sig") == 0 || strcmp(section_name, ".sig") == 0) {
                /* Found signature section */
                if (shdr[i].sh_offset + shdr[i].sh_size > elf_size)
                    HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "signature section beyond file size");

                *sig_size = (size_t)shdr[i].sh_size;

                /* Allocate and copy signature data */
                if (NULL == (sig_data = H5MM_malloc(*sig_size)))
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, NULL, "cannot allocate signature buffer");

                memcpy(sig_data, (const char *)elf_data + shdr[i].sh_offset, *sig_size);

                /* Create a copy of the ELF without the signature section for verification */
                if (NULL == (clean_elf = H5MM_malloc(elf_size)))
                    HGOTO_ERROR(H5E_PLUGIN, H5E_CANTALLOC, NULL, "cannot allocate clean ELF buffer");

                memcpy(clean_elf, elf_data, elf_size);

                /* Zero out the signature section content */
                memset((char *)clean_elf + shdr[i].sh_offset, 0, shdr[i].sh_size);

                *plugin_data_without_sig = clean_elf;
                *plugin_size_without_sig = elf_size;

                ret_value = sig_data;
                sig_data  = NULL; /* Prevent cleanup */
                clean_elf = NULL; /* Prevent cleanup */

                goto done;
            }
        }
    }

    /* Signature section not found */
    HGOTO_ERROR(H5E_PLUGIN, H5E_NOTFOUND, NULL, "signature section not found in ELF file");

done:
    if (sig_data)
        H5MM_xfree(sig_data);
    if (clean_elf)
        H5MM_xfree(clean_elf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__extract_elf_signature_32() */

/*-------------------------------------------------------------------------
 * Function:    H5PL__extract_signature_from_elf
 *
 * Purpose:     Extract embedded signature from ELF plugin file
 *
 *              This function reads the ELF file and extracts the signature
 *              section without using external tools. It works with both
 *              32-bit and 64-bit ELF files.
 *
 * Return:      Success: Pointer to signature data (caller must free)
 *              Failure: NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5PL__extract_signature_from_elf(const char *plugin_path, size_t *sig_size, void **plugin_data_without_sig,
                                 size_t *plugin_size_without_sig)
{
    void         *elf_data  = NULL;
    size_t        elf_size  = 0;
    unsigned char *elf_ident = NULL;
    void         *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    /* Check args */
    assert(plugin_path);
    assert(sig_size);
    assert(plugin_data_without_sig);
    assert(plugin_size_without_sig);

    /* Read entire ELF file into memory */
    if (NULL == (elf_data = H5PL__read_file_contents(plugin_path, &elf_size)))
        HGOTO_ERROR(H5E_PLUGIN, H5E_CANTOPENFILE, NULL, "cannot read plugin file");

    /* Verify ELF magic number */
    if (elf_size < EI_NIDENT)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "file too small to be ELF");

    elf_ident = (unsigned char *)elf_data;
    if (elf_ident[EI_MAG0] != ELFMAG0 || elf_ident[EI_MAG1] != ELFMAG1 ||
        elf_ident[EI_MAG2] != ELFMAG2 || elf_ident[EI_MAG3] != ELFMAG3)
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "not a valid ELF file");

    /* Determine ELF class (32-bit or 64-bit) and extract signature */
    if (elf_ident[EI_CLASS] == ELFCLASS64) {
        ret_value = H5PL__extract_elf_signature_64(elf_data, elf_size, sig_size, plugin_data_without_sig,
                                                   plugin_size_without_sig);
    }
    else if (elf_ident[EI_CLASS] == ELFCLASS32) {
        ret_value = H5PL__extract_elf_signature_32(elf_data, elf_size, sig_size, plugin_data_without_sig,
                                                   plugin_size_without_sig);
    }
    else {
        HGOTO_ERROR(H5E_PLUGIN, H5E_BADVALUE, NULL, "unsupported ELF class");
    }

done:
    if (elf_data)
        H5MM_xfree(elf_data);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5PL__extract_signature_from_elf() */

#else /* Not Unix-like system */

/*-------------------------------------------------------------------------
 * Function:    H5PL__extract_signature_from_elf
 *
 * Purpose:     Stub for non-Unix platforms
 *
 * Return:      NULL (not supported)
 *
 *-------------------------------------------------------------------------
 */
void *
H5PL__extract_signature_from_elf(const char H5_ATTR_UNUSED *plugin_path, size_t H5_ATTR_UNUSED *sig_size,
                                 void H5_ATTR_UNUSED **plugin_data_without_sig,
                                 size_t H5_ATTR_UNUSED *plugin_size_without_sig)
{
    FUNC_ENTER_PACKAGE_NOERR

    /* ELF parsing not supported on this platform */
    FUNC_LEAVE_NOAPI(NULL)
} /* end H5PL__extract_signature_from_elf() */

#endif /* Unix-like system check */

#endif /* H5_REQUIRE_DIGITAL_SIGNATURE */
