/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Lifeboat, LLC                                                *
 * All rights reserved.                                                      *
 *                                                                           *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the COPYING file, which can be found  *
 * at the root of the source code distribution tree.                         *
 * If you do not have access to either file, you may request a copy from     *
 * help@lifeboat.llc                                                         *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose: The recovery tool applies VFD SWMR updater files to an HDF5 (.h5)
 * file in order to recover a consistent state after a writer crash during
 * SWMR (Single Writer Multiple Reader) operations.
 *
 * It invokes `h5clear -s` automatically, using the `h5clear` binary from
 * either the system PATH or the H5CLEAR_PATH environment variable.
 *
 * NOTE: This tool currently only supports POSIX-compliant systems and is not
 * expected to function on Windows environments.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <ctype.h>
#include <errno.h>
#include <stddef.h>

/* Macro to disable switch case fallthrough warning on supported systems */
#if defined(__clang__) || defined(__GNUC__)
#define FALLTHROUGH __attribute__((fallthrough))
#else
#define FALLTHROUGH ((void)0)
#endif

/*****************************************************************************/
/*** Begin macro definitions common to aux_process.c and recovery_tool.c.  ***/
/*****************************************************************************/
#define FILE_NAME_LEN                  1024
#define SIGNATURE_LEN                  4
#define UPDATER_SIGNATURE              "VUDH"
#define CL_SIGNATURE                   "VUCL"
#define CREATE_METADATA_FILE_ONLY_FLAG 0x0001
#define FINAL_UPDATE_FLAG              0x0002

/* The length for the header of the updater file is 48 bytes, with the breakdown as below:
 *     signature:          4
 *     version:            2
 *     flags:              2
 *     page size:          4
 *     sequence number:    8
 *     tick number:        8
 *     change list offset: 8
 *     change list length: 8
 *     checksum:           4
 */
#define UD_HEADER_LEN 48

/* The length for the top fields of the change list in the updater file is 48 bytes, with the breakdown as
 * below:
 * signature:                                          4
 * tick number:                                        8
 * page offset for metadata file header in updater:    4
 * length for metadata file header:                    4
 * checksum for metadata file header:                  4
 * page offset for metadata file index in updater:     4
 * offset for metadata file index in metadata file:    8
 * length for metadata file index:                     4
 * checksum for metadata file index:                   4
 * number of change list entries:                      4
 */
#define UD_CL_TOP_LEN 48

/* The length for the entry of the change list in the updater file is 20 bytes, with the breakdown as below:
 *     page offset in updater:                             4
 *     page offset in metadata file:                       4
 *     page offset in HDF5 file:                           4
 *     length:                                             4
 *     checksum:                                           4
 */
#define CL_ENTRY_LEN 20

/* These decoding macros are borrowed directly from the HDF5 library for making this program stand-alone in
 * the future */
#define UINT16DECODE(p, i)                                                                                   \
    {                                                                                                        \
        (i) = (uint16_t)(*(p) & 0xff);                                                                       \
        (p)++;                                                                                               \
        (i) |= (uint16_t)((*(p) & 0xff) << 8);                                                               \
        (p)++;                                                                                               \
    }

#define UINT32DECODE(p, i)                                                                                   \
    {                                                                                                        \
        (i) = (uint32_t)(*(p) & 0xff);                                                                       \
        (p)++;                                                                                               \
        (i) |= ((uint32_t)(*(p) & 0xff) << 8);                                                               \
        (p)++;                                                                                               \
        (i) |= ((uint32_t)(*(p) & 0xff) << 16);                                                              \
        (p)++;                                                                                               \
        (i) |= ((uint32_t)(*(p) & 0xff) << 24);                                                              \
        (p)++;                                                                                               \
    }

#define UINT64DECODE(p, n)                                                                                   \
    {                                                                                                        \
        /* WE DON'T CHECK FOR OVERFLOW! */                                                                   \
        size_t _i;                                                                                           \
                                                                                                             \
        n = 0;                                                                                               \
        (p) += 8;                                                                                            \
        for (_i = 0; _i < sizeof(uint64_t); _i++)                                                            \
            n = (n << 8) | *(--p);                                                                           \
        (p) += 8;                                                                                            \
    }

/* These checksum macros are borrowed directly from the HDF5 library for making this program stand-alone in
 * the future */
#define lookup_rot(x, k) (((x) << (k)) ^ ((x) >> (32 - (k))))

#define lookup_mix(a, b, c)                                                                                  \
    {                                                                                                        \
        a -= c;                                                                                              \
        a ^= lookup_rot(c, 4);                                                                               \
        c += b;                                                                                              \
        b -= a;                                                                                              \
        b ^= lookup_rot(a, 6);                                                                               \
        a += c;                                                                                              \
        c -= b;                                                                                              \
        c ^= lookup_rot(b, 8);                                                                               \
        b += a;                                                                                              \
        a -= c;                                                                                              \
        a ^= lookup_rot(c, 16);                                                                              \
        c += b;                                                                                              \
        b -= a;                                                                                              \
        b ^= lookup_rot(a, 19);                                                                              \
        a += c;                                                                                              \
        c -= b;                                                                                              \
        c ^= lookup_rot(b, 4);                                                                               \
        b += a;                                                                                              \
    }

#define lookup_final(a, b, c)                                                                                \
    {                                                                                                        \
        c ^= b;                                                                                              \
        c -= lookup_rot(b, 14);                                                                              \
        a ^= c;                                                                                              \
        a -= lookup_rot(c, 11);                                                                              \
        b ^= a;                                                                                              \
        b -= lookup_rot(a, 25);                                                                              \
        c ^= b;                                                                                              \
        c -= lookup_rot(b, 16);                                                                              \
        a ^= c;                                                                                              \
        a -= lookup_rot(c, 4);                                                                               \
        b ^= a;                                                                                              \
        b -= lookup_rot(a, 14);                                                                              \
        c ^= b;                                                                                              \
        c -= lookup_rot(b, 24);                                                                              \
    }
/*****************************************************************************/
/***** End macro definitions common to aux_process.c and recovery_tool.c *****/
/*****************************************************************************/

/* Handler modified from aux_process.c to suite recovery tool purposes */
typedef struct {
    char *log_file_path; /* path name for the log file                                                   */
    FILE *log_file;      /* log file containing the details of this program                              */
    FILE *output;        /* output the details of this program to STDOUT or a log file                   */
    bool  is_posix;      /* whether the HDF5 file is on a POSIX file system                              */
    bool  verbose;       /* print out the details of this program                                        */
    char *updater_path;  /* path name for the updater files                                              */
    char *h5_file_path;  /* path name for the HDF5 file                                                  */
    int   h5_fd;         /* file descriptor for the HDF5 file.  Must use file descriptor as the C        */
    char *h5clear_path;  /* path name for the h5clear utility                                            */
} handler_t;

/*****************************************************************************/
/****** Begin structure and type definitions common to aux_process.c and *****/
/****** recovery_tool.c                                                  *****/
/*****************************************************************************/

/* Structure for the entry of change list in the updater file */
typedef struct {
    void    *data; /* buffer for the data (changes)                                                */
    uint32_t ud_file_page_offset; /* page offset of the data in the updater file */
    uint32_t md_file_page_offset; /* page offset of the the data in the metadata file */
    uint32_t h5_file_page_offset; /* page offset of the data in the HDF5 file (future usage) */
    uint32_t length;   /* length of the data                                                           */
    uint32_t checksum; /* checksum value of the data                                                   */
} cl_entry_t;

/* Updater file header related fields */
typedef struct {
    FILE          *file;
    unsigned char  ud_header_buf[UD_HEADER_LEN];
    unsigned char  ud_cl_top_buf[UD_CL_TOP_LEN];
    unsigned char *cl_buf;

    /* updater file header related fields */
    char     header_signature[5];
    uint16_t version;
    uint16_t flags;
    uint32_t page_size;
    uint64_t sequence_num;
    uint64_t tick_num;
    uint64_t change_list_offset;
    uint64_t change_list_len;
    uint32_t received_header_checksum;
    uint32_t verified_header_checksum;

    /* Updater file change list related fields. */
    char     cl_signature[5];
    uint64_t cl_tick_num;

    uint32_t md_file_header_ud_page_offset;
    uint32_t md_file_header_len;
    uint32_t md_file_header_chksum;
    void    *md_file_header_buf;

    uint32_t md_file_index_ud_page_offset;
    uint64_t md_file_index_md_file_offset;
    uint32_t md_file_index_len;
    uint32_t md_file_index_chksum;
    void    *md_file_index_buf;

    uint32_t received_cl_checksum;
    uint32_t verified_cl_checksum;

    uint32_t    num_cl_entries;
    cl_entry_t *change_list;
    uint32_t    cl_chksum;
} updater_t;

enum aux_arg_level {
    no_arg = 0,  /* doesn't take an argument     */
    require_arg, /* requires an argument          */
    optional_arg /* argument is optional         */
};

/*
 * aux_get_options is a copy of the H5_get_options in hdf5/src/H5system.c.
 *
 * It supports both POSIX and Windows systems.
 * It determines which options are specified on the command line and
 * returns a pointer to any arguments possibly associated with the option in
 * the ``aux_optarg'' variable. aux_get_options returns the shortname equivalent of
 * the option. The long options are specified in the following way:
 *
 * struct aux_long_options foo[] = {
 *   { "filename", require_arg, 'f' },
 *   { "append", no_arg, 'a' },
 *   { "width", require_arg, 'w' },
 *   { NULL, 0, 0 }
 * };
 *
 * Long named options can have arguments specified as either:
 *
 *   ``--param=arg'' or ``--param arg''
 *
 * Short named options can have arguments specified as either:
 *
 *   ``-w80'' or ``-w 80''
 *
 * and can have more than one short named option specified at one time:
 *
 *   -aw80
 *
 * in which case those options which expect an argument need to come at the
 * end.
 */
typedef struct {
    const char        *name;     /* Name of the long option */
    enum aux_arg_level has_arg;  /* Whether we should look for an arg */
    char               shortval; /* The shortname equivalent of long arg
                                  * this gets returned from get_option
                                  */
} aux_long_options;
/*****************************************************************************/
/****** End structure and type definitions common to aux_process.c and *******/
/****** recovery_tool.c                                                *******/
/*****************************************************************************/

/* Global variables for option parsing, found in both aux_process.c and
 * recovery_tool.c */
int         aux_opterr = 1; /* Get_option prints errors if this is on */
int         aux_optind = 1; /* Token pointer                          */
const char *aux_optarg;     /* Flag argument (or value)               */

/*****************************************************************************/
/******** Begin functions common to aux_process.c and recovery_tool.c ********/
/*****************************************************************************/
/*-------------------------------------------------------------------------
 * Function: aux_get_options
 *
 * Purpose:  Determine the command-line options a user specified. We can
 *           accept both short and long type command-lines.
 *
 * Return:  Success:    The short valued "name" of the command line
 *                      parameter or EOF if there are no more
 *                      parameters to process.
 *
 *          Failure:    A question mark.
 *-------------------------------------------------------------------------
 */
static int
aux_get_options(int argc, char **argv, const char *opts, const aux_long_options *l_opts)
{
    static int sp      = 1;   /* character index in current token */
    int        optchar = '?'; /* option character passed back to user */

    if (sp == 1) {
        /* check for more flag-like tokens */
        if (aux_optind >= argc || argv[aux_optind][0] != '-' || argv[aux_optind][1] == '\0') {
            return EOF;
        }
        else if (strcmp(argv[aux_optind], "--") == 0) {
            aux_optind++;
            return EOF;
        }
    }

    if (sp == 1 && argv[aux_optind][0] == '-' && argv[aux_optind][1] == '-') {
        /* long command line option */
        int        i;
        const char ch      = '=';
        char      *arg     = strdup(&argv[aux_optind][2]);
        size_t     arg_len = 0;

        aux_optarg = strchr(&argv[aux_optind][2], ch);
        arg_len    = strlen(&argv[aux_optind][2]);
        if (aux_optarg) {
            arg_len -= strlen(aux_optarg);
            aux_optarg++; /* skip the equal sign */
        }
        arg[arg_len] = 0;

        for (i = 0; l_opts && l_opts[i].name; i++) {
            if (strcmp(arg, l_opts[i].name) == 0) {
                /* we've found a matching long command line flag */
                optchar = l_opts[i].shortval;

                if (l_opts[i].has_arg != no_arg) {
                    if (aux_optarg == NULL) {
                        if (l_opts[i].has_arg != optional_arg) {
                            if (aux_optind < (argc - 1))
                                if (argv[aux_optind + 1][0] != '-')
                                    aux_optarg = argv[++aux_optind];
                        }
                        else if (l_opts[i].has_arg == require_arg) {
                            if (aux_opterr)
                                fprintf(stderr, "%s: option required for \"--%s\" flag\n", argv[0], arg);

                            optchar = '?';
                        }
                    }
                }
                else {
                    if (aux_optarg) {
                        if (aux_opterr)
                            fprintf(stderr, "%s: no option required for \"%s\" flag\n", argv[0], arg);

                        optchar = '?';
                    }
                }
                break;
            }
        }

        if (l_opts[i].name == NULL) {
            /* exhausted all of the l_opts we have and still didn't match */
            if (aux_opterr)
                fprintf(stderr, "%s: unknown option \"%s\"\n", argv[0], arg);

            optchar = '?';
        }

        aux_optind++;
        sp = 1;

        free(arg);
    }
    else {
        char *cp; /* pointer into current token */

        /* short command line option */
        optchar = argv[aux_optind][sp];

        if (optchar == ':' || (cp = strchr(opts, optchar)) == 0) {
            if (aux_opterr)
                fprintf(stderr, "%s: unknown option \"%c\"\n", argv[0], optchar);

            /* if no chars left in this token, move to next token */
            if (argv[aux_optind][++sp] == '\0') {
                aux_optind++;
                sp = 1;
            }
            return '?';
        }

        if (*++cp == ':') {
            /* if a value is expected, get it */
            if (argv[aux_optind][sp + 1] != '\0') {
                /* flag value is rest of current token */
                aux_optarg = &argv[aux_optind++][sp + 1];
            }
            else if (++aux_optind >= argc) {
                if (aux_opterr)
                    fprintf(stderr, "%s: value expected for option \"%c\"\n", argv[0], optchar);

                optchar = '?';
            }
            else {
                /* flag value is next token */
                aux_optarg = argv[aux_optind++];
            }

            sp = 1;
        }
        /* wildcard argument */
        else if (*cp == '*') {
            /* check the next argument */
            aux_optind++;
            /* we do have an extra argument, check if not last */
            if ((aux_optind + 1) < argc) {
                if (argv[aux_optind][0] != '-') {
                    aux_optarg = argv[aux_optind++];
                }
                else {
                    aux_optarg = NULL;
                }
            }
            else {
                aux_optarg = NULL;
            }
        }
        else {
            /* set up to look at next char in token, next time */
            if (argv[aux_optind][++sp] == '\0') {
                /* no more in current token, so setup next token */
                aux_optind++;
                sp = 1;
            }
            aux_optarg = NULL;
        }
    }

    /* return the current flag character found */
    return optchar;
} /* aux_get_options() */

/*-------------------------------------------------------------------------
 * Function: 	checksum_lookup()
 *
 * Purpose:	checksum_lookup is a copy of the function checksum_lookup3()
 *              in hdf5/src/H5checksum.c
 *
 *              It hashes a variable-length key into a 32-bit value
 *
 * Parameters:  key     : the unaligned variable-length array of bytes
 *              length  : the length of the key, counting by bytes
 *              initval : can be any 4-byte value
 *
 * Returns:	a 32-bit value.  Every bit of the key affects every bit of
 *              the return value.  Two keys differing by one or two bits
 *              will have totally different hash values.
 *
 * Notes:	The best hash table sizes are powers of 2.  There is no need
 *              to do mod a prime (mod is sooo slow!).  If you need less than
 *              32 bits, use a bitmask.  For example, if you need only 10 bits,
 *              do h = (h & hashmask(10));
 *              In which case, the hash table should have hashsize(10) elements.
 *
 *              If you are hashing n strings (uint8_t **)k, do it like this:
 *              for (i=0, h=0; i<n; ++i) h = H5_checksum_lookup( k[i], len[i], h);
 *
 *              By Bob Jenkins, 2006.  bob_jenkins@burtleburtle.net.  You may
 *              use this code any way you wish, private, educational, or commercial.
 *              It's free.
 *
 *              Use for hash table lookup, or anything where one collision in 2^^32
 *              is acceptable.  Do NOT use for cryptographic purposes.
 *-------------------------------------------------------------------------
 */
static uint32_t
checksum_lookup(const void *key, size_t length, uint32_t initval)
{
    const uint8_t *k = (const uint8_t *)key;
    uint32_t       a, b, c = 0; /* internal state */

    /* Sanity check */
    assert(key);
    assert(length > 0);

    /* Set up the internal state */
    a = b = c = 0xdeadbeef + ((uint32_t)length) + initval;

    /*--------------- all but the last block: affect some 32 bits of (a,b,c) */
    while (length > 12) {
        a += k[0];
        a += ((uint32_t)k[1]) << 8;
        a += ((uint32_t)k[2]) << 16;
        a += ((uint32_t)k[3]) << 24;
        b += k[4];
        b += ((uint32_t)k[5]) << 8;
        b += ((uint32_t)k[6]) << 16;
        b += ((uint32_t)k[7]) << 24;
        c += k[8];
        c += ((uint32_t)k[9]) << 8;
        c += ((uint32_t)k[10]) << 16;
        c += ((uint32_t)k[11]) << 24;
        lookup_mix(a, b, c);
        length -= 12;
        k += 12;
    }

    /*-------------------------------- last block: affect all 32 bits of (c) */
    switch (length) /* all the case statements fall through */
    {
        case 12:
            c += ((uint32_t)k[11]) << 24;
            FALLTHROUGH;

        case 11:
            c += ((uint32_t)k[10]) << 16;
            FALLTHROUGH;

        case 10:
            c += ((uint32_t)k[9]) << 8;
            FALLTHROUGH;

        case 9:
            c += k[8];
            FALLTHROUGH;

        case 8:
            b += ((uint32_t)k[7]) << 24;
            FALLTHROUGH;

        case 7:
            b += ((uint32_t)k[6]) << 16;
            FALLTHROUGH;

        case 6:
            b += ((uint32_t)k[5]) << 8;
            FALLTHROUGH;

        case 5:
            b += k[4];
            FALLTHROUGH;

        case 4:
            a += ((uint32_t)k[3]) << 24;
            FALLTHROUGH;

        case 3:
            a += ((uint32_t)k[2]) << 16;
            FALLTHROUGH;

        case 2:
            a += ((uint32_t)k[1]) << 8;
            FALLTHROUGH;

        case 1:
            a += k[0];
            break;
        case 0:
            goto done;
        default:
            assert(0 && "This Should never be executed!");
    }

    lookup_final(a, b, c);

done:
    return c;
} /* checksum_lookup() */

/*-------------------------------------------------------------------------
 * Function: decode_ud_header
 *
 * Purpose:  Decode the header of the updater file
 *
 * Return:   Success:    0 (the flag is CREATE_METADATA_FILE_ONLY_FLAG)
 *
 *                       1 (the flag is not CREATE_METADATA_FILE_ONLY_FLAG)
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
decode_ud_header(updater_t *updater, handler_t *hand)
{
    unsigned char *ptr;

    /* Read the header of the updater file */
    if (fread(updater->ud_header_buf, UD_HEADER_LEN, 1, updater->file) == 0) {
        fprintf(stderr, "failed to read the header of the updater file\n");
        goto error;
    }

    ptr = updater->ud_header_buf;

    /* Check the signature */
    strncpy(updater->header_signature, (char *)ptr, SIGNATURE_LEN);
    updater->header_signature[SIGNATURE_LEN] = '\0';

    if (strcmp(updater->header_signature, UPDATER_SIGNATURE)) {
        fprintf(stderr, "the signature of the updater file is incorrect: %s\n", updater->header_signature);
        goto error;
    }

    /* Check the version number */
    ptr += SIGNATURE_LEN;
    UINT16DECODE(ptr, updater->version);

    if (updater->version != 0) {
        fprintf(stderr, "the version of the updater file is incorrect: %hu\n", updater->version);
        goto error;
    }

    /* Get the flags */
    UINT16DECODE(ptr, updater->flags);

    /* Get the page size */
    UINT32DECODE(ptr, updater->page_size);

    /* Get the sequence number */
    UINT64DECODE(ptr, updater->sequence_num);

    /* Get the tick number */
    UINT64DECODE(ptr, updater->tick_num);

    /* Get the offset for the change list */
    UINT64DECODE(ptr, updater->change_list_offset);

    /* Get the length for the change list */
    UINT64DECODE(ptr, updater->change_list_len);

    /* Get the checksum */
    UINT32DECODE(ptr, updater->received_header_checksum);

    /* Verify the checksum for the header */
    updater->verified_header_checksum = checksum_lookup(updater->ud_header_buf, UD_HEADER_LEN - 4, 0);

    /* Compare the checksum */
    if (updater->received_header_checksum != updater->verified_header_checksum) {
        fprintf(stderr, "received header's checksum (%u) doesn't match the calculated one (%u)\n",
                updater->received_header_checksum, updater->verified_header_checksum);
        goto error;
    }

    /* Output the log info */
    if (hand->output) {
        fprintf(hand->output, "header signature=%s\n", updater->header_signature);
        fprintf(hand->output, "version=%" PRIu16 "\n", updater->version);
        fprintf(hand->output, "flags=%" PRIu16 "\n", updater->flags);
        fprintf(hand->output, "page size (bytes)=%" PRIu32 "\n", updater->page_size);
        fprintf(hand->output, "sequence number=%" PRIu64 "\n", updater->sequence_num);
        fprintf(hand->output, "tick number=%" PRIu64 "\n", updater->tick_num);
        fprintf(hand->output, "change list offset (bytes)=%" PRIu64 "\n", updater->change_list_offset);
        fprintf(hand->output, "change list length (bytes)=%" PRIu64 "\n", updater->change_list_len);
        fprintf(hand->output, "received checksum for header=%" PRIu32 "\n",
                updater->received_header_checksum);
        fprintf(hand->output, "calculated checksum for header=%" PRIu32 "\n\n",
                updater->verified_header_checksum);
    }

    return 0;
error:
    return -1;
} /* decode_ud_header() */

/*-------------------------------------------------------------------------
 * Function: decode_cl_top_fields
 *
 * Purpose:  Decode the top part of the change list in the updater file
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
decode_cl_top_fields(updater_t *updater, handler_t *hand)
{
    unsigned char *ptr;

    /*----------------------------------------------
     * Read in the change list and verify
     * the checksum of the change list
     *----------------------------------------------
     */
    updater->cl_buf = (unsigned char *)malloc(updater->change_list_len);

    /* Seek the beginning of the change list in the updater file */
    if (fseek(updater->file, (long)updater->change_list_offset, SEEK_SET) != 0) {
        fprintf(stderr, "failed to seek the top fields of the change list in the updater file\n");
        goto error;
    }

    /* Read the change list */
    if (fread(updater->cl_buf, updater->change_list_len, 1, updater->file) == 0) {
        fprintf(stderr, "failed to read the top fields of the change list in the updater file\n");
        goto error;
    }

    /* Find the position of the checksum and decode it */
    ptr = updater->cl_buf + updater->change_list_len - 4;

    UINT32DECODE(ptr, updater->received_cl_checksum);

    /* Calculate the checksum of the change list */
    updater->verified_cl_checksum = checksum_lookup(updater->cl_buf, updater->change_list_len - 4, 0);

    /* Compare the checksum */
    if (updater->received_cl_checksum != updater->verified_cl_checksum) {
        fprintf(stderr,
                "received change list's checksum (%u) doesn't match the calculated one (%u) for the updater "
                "file\n",
                updater->received_cl_checksum, updater->verified_cl_checksum);
        goto error;
    }

    /*----------------------------------------------
     * Decode the top fields of the change list
     *----------------------------------------------
     */
    ptr = updater->cl_buf;

    /* Check the signature */
    strncpy(updater->cl_signature, (char *)ptr, SIGNATURE_LEN);
    updater->cl_signature[SIGNATURE_LEN] = '\0';

    if (strcmp(updater->cl_signature, CL_SIGNATURE)) {
        fprintf(stderr, "the signature of the change list in the updater file is incorrect: %s\n",
                updater->cl_signature);
        goto error;
    }

    /* Check the tick number */
    ptr += SIGNATURE_LEN;

    /* Get the sequence number */
    UINT64DECODE(ptr, updater->cl_tick_num);

    /* Get the page offset for metadata file header in updater */
    UINT32DECODE(ptr, updater->md_file_header_ud_page_offset);

    /* Get the length for metadata file header */
    UINT32DECODE(ptr, updater->md_file_header_len);

    /* Get the checksum for metadata file header */
    UINT32DECODE(ptr, updater->md_file_header_chksum);

    /* Get the page offset for metadata file index in updater */
    UINT32DECODE(ptr, updater->md_file_index_ud_page_offset);

    /* Get the offset for metadata file index in metadata file */
    UINT64DECODE(ptr, updater->md_file_index_md_file_offset);

    /* Get the length for metadata file index */
    UINT32DECODE(ptr, updater->md_file_index_len);

    /* Get the checksum for metadata file index */
    UINT32DECODE(ptr, updater->md_file_index_chksum);

    /* Get the number of change list entries */
    UINT32DECODE(ptr, updater->num_cl_entries);

    /* Output the log info */
    if (hand->output) {
        fprintf(hand->output, "change list signature=%s\n", updater->cl_signature);
        fprintf(hand->output, "change list tick number=%" PRIu64 "\n", updater->cl_tick_num);
        fprintf(hand->output, "page offset for metadata file header in updater=%" PRIu32 "\n",
                updater->md_file_header_ud_page_offset);
        fprintf(hand->output, "length for metadata file header (bytes)=%" PRIu32 "\n",
                updater->md_file_header_len);
        fprintf(hand->output, "checksum for metadata file header=%" PRIu32 "\n",
                updater->md_file_header_chksum);
        fprintf(hand->output, "page offset for metadata file index in updater=%" PRIu32 "\n",
                updater->md_file_index_ud_page_offset);
        fprintf(hand->output, "offset for metadata file index in metadata file (bytes)=%" PRIu64 "\n",
                updater->md_file_index_md_file_offset);
        fprintf(hand->output, "length for metadata file index (bytes)=%" PRIu32 "\n",
                updater->md_file_index_len);
        fprintf(hand->output, "checksum for metadata file index=%" PRIu32 "\n",
                updater->md_file_index_chksum);
        fprintf(hand->output, "number of change list entries=%" PRIu32 "\n", updater->num_cl_entries);
        fprintf(hand->output, "received checksum for the change list=%" PRIu32 "\n",
                updater->received_cl_checksum);
        fprintf(hand->output, "calculated checksum for the change list=%" PRIu32 "\n\n",
                updater->verified_cl_checksum);
    }

    return 0;

error:
    return -1;
} /* decode_cl_top_fields() */

/*****************************************************************************/
/******** End functions common to aux_process.c and recovery_tool.c **********/
/*****************************************************************************/

/*****************************************************************************/
/******** Begin functions specific to recovery_tool.c ************************/
/*****************************************************************************/

/*-------------------------------------------------------------------------
 * Function: copy_data
 *
 * Purpose:  Copy data from the source file to the destination file
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
copy_data(handler_t *hand, FILE *src_file, int dst_fd, off_t src_file_offset, off_t dst_file_offset,
          size_t data_len, uint32_t received_checksum)
{
    uint32_t verified_checksum;           /* calculated checksum for the data being copied */
    void    *data_buf = malloc(data_len); /* buffer for the data being copied              */

    /* Seek and read in the data from the source file */
    if (fseek(src_file, src_file_offset, SEEK_SET) != 0) {
        fprintf(stderr, "failed to seek the position of the data in the source file\n");
        goto error;
    }

    if (fread(data_buf, data_len, 1, src_file) == 0) {
        fprintf(stderr, "failed to read the data from the source file\n");
        goto error;
    }

    verified_checksum = checksum_lookup(data_buf, data_len, 0);

    /* Compare the checksum */
    if (received_checksum != verified_checksum) {
        fprintf(stderr, "received checksum (%u) doesn't match the calculated one (%u)\n", received_checksum,
                verified_checksum);
        goto error;
    }

    /* Verbose logging */
    if (hand->output) {
        fprintf(hand->output, "INFO: dst_fd = %d\n", dst_fd);
        fprintf(hand->output, "INFO: dst_file_offset = 0x%llx\n", (unsigned long long)dst_file_offset);
        fprintf(hand->output, "INFO: About to fseek...\n");
        fflush(hand->output);
    }

    /* Seek the correct write position of the destination file */
    if (-1 == lseek(dst_fd, dst_file_offset, SEEK_SET)) {
        fprintf(stderr, "lseek to offset 0x%llx failed.  errno = %d (%s)\n",
                (unsigned long long)dst_file_offset, errno, strerror(errno));
        goto error;
    }

    /* Write the data, being careful of interrupted system calls and partial
     * results.  Note that this code is adapted from H5FD__sec2_write().
     * POSIX ONLY
     */
    {
        size_t size = data_len;
        void  *buf  = data_buf;

        while (size > 0) {
            size_t  bytes_in    = 0;  /* # of bytes to write  */
            ssize_t bytes_wrote = -1; /* # of bytes written   */

            /* Trying to write more bytes than the return type can handle is
             * undefined behavior in POSIX.
             */
            if (size > SIZE_MAX / 2) { /* should be SSIZE_MAX, but doesn't seem to be defined */

                bytes_in = SIZE_MAX / 2;
            }
            else {

                bytes_in = size;
            }

            /* Finish writes that are interrupted */
            do {
                bytes_wrote = write(dst_fd, buf, bytes_in);

            } while (-1 == bytes_wrote && EINTR == errno);

            if (-1 == bytes_wrote) { /* error */

                int myerrno = errno;

                fprintf(stderr, "Write to HDF5 file at offset 0x%llx fails.  errno = %d (%s)\n",
                        (unsigned long long)dst_file_offset, errno, strerror(myerrno));
                goto error;

            } /* end if */

            assert(bytes_wrote > 0);
            assert((size_t)bytes_wrote <= size);

            size -= (size_t)bytes_wrote;
            buf = (void *)((char *)buf + bytes_wrote);
        } /* end while */
    }

    if (data_buf)
        free(data_buf);

    /* Output the log info */
    if (hand->output) {
        fprintf(hand->output, "\tsource file=%p\n", (void *)src_file);
        fprintf(hand->output, "\tdestination fd=%d\n", dst_fd);
        fprintf(hand->output, "\toffset in the source file=0x%llx\n", (unsigned long long)src_file_offset);
        fprintf(hand->output, "\toffset in the destination file=0x%llx\n",
                (unsigned long long)dst_file_offset);
        fprintf(hand->output, "\tlength of data=%lld\n", (unsigned long long)data_len);
        fprintf(hand->output, "\treceived checksum=%u\n", received_checksum);
        fprintf(hand->output, "\tcalculated checksum=%u\n", verified_checksum);
    }

    return 0;
error:
    if (data_buf)
        free(data_buf);

    return -1;
} /* copy_data() */

/*-------------------------------------------------------------------------
 * Function: decode_and_copy_cl_entries_recovery_only
 *
 * Purpose:  Decode the entries of the change list and copy the data
 *           from the source file to the destination file
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-------------------------------------------------------------------------
 */
static int
decode_and_copy_cl_entries_recovery_only(updater_t *updater, handler_t *hand)
{
    unsigned char *ptr; /* pointer to the data location */
    unsigned int   i;

    if (updater->num_cl_entries) {
        /* Allocate the buffer for the change list */
        updater->change_list = (cl_entry_t *)malloc(sizeof(cl_entry_t) * updater->num_cl_entries);

        ptr = updater->cl_buf + UD_CL_TOP_LEN;

        for (i = 0; i < updater->num_cl_entries; i++) {
            UINT32DECODE(ptr, updater->change_list[i].ud_file_page_offset);
            UINT32DECODE(ptr, updater->change_list[i].md_file_page_offset);
            UINT32DECODE(ptr, updater->change_list[i].h5_file_page_offset);
            UINT32DECODE(ptr, updater->change_list[i].length);
            UINT32DECODE(ptr, updater->change_list[i].checksum);

            /* Output the log info */
            if (hand->output) {
                fprintf(hand->output, "change list entry %u\n", i);
                fprintf(hand->output, "\tpage offset of change in updater=%u\n",
                        updater->change_list[i].ud_file_page_offset);
                fprintf(hand->output, "\tpage offset of change in metadata file=%u\n",
                        updater->change_list[i].md_file_page_offset);
                fprintf(hand->output, "\tpage offset of change in HDF5 file=%u\n",
                        updater->change_list[i].h5_file_page_offset);
                fprintf(hand->output, "\tlength of change (bytes)=%u\n", updater->change_list[i].length);
                fprintf(hand->output, "\tchecksum of change=%u\n", updater->change_list[i].checksum);

                fprintf(hand->output, "\ncopy this change to the HDF5 file:\n");
            }

            /* Copy the data from the updater file to the HDF5 file */
            if (copy_data(hand, updater->file, hand->h5_fd,
                          (off_t)(updater->change_list[i].ud_file_page_offset) * (off_t)(updater->page_size),
                          (off_t)(updater->change_list[i].h5_file_page_offset) * (off_t)(updater->page_size),
                          (size_t)(updater->change_list[i].length), updater->change_list[i].checksum) < 0) {
                fprintf(stderr,
                        "failed to copy the data in the change list (%u) from the updater file to the "
                        "HDF5 file\n",
                        i);
                goto error;
            }
        }

        if (updater->change_list)
            free(updater->change_list);
    }

    /* Free the buffer for the change list */
    if (updater->cl_buf)
        free(updater->cl_buf);

    return 0;

error:
    if (updater->change_list)
        free(updater->change_list);

    if (updater->cl_buf)
        free(updater->cl_buf);

    return -1;
} /* decode_and_copy_cl_entries_recovery_only() */

/*-------------------------------------------------------------------------
 * Function: apply_updater_recovery_only
 *
 * Purpose:  Apply the updater file directly to the HDF5 file
 *
 * Return:   Success:    true
 *           Failure:    false
 *-------------------------------------------------------------------------
 */
static int
apply_updater_recovery_only(const char *updater_name, handler_t *hand)
{
    updater_t updater = {0}; /* struct for the updater file header */
    int       ret;

    if (hand->output) {
        fprintf(hand->output, "\nupdater_name: %s\n", updater_name);
    }

    /* Open the updater file */
    if (!(updater.file = fopen(updater_name, "r"))) {
        fprintf(stderr, "failed to open the updater file: %s\n", updater_name);
        goto error;
    }

    /*----------------------------------------------
     * Decode the header of the updater file
     *----------------------------------------------
     */
    ret = decode_ud_header(&updater, hand);

    if (ret < 0) {
        fprintf(stderr, "failed to decode the header of the updater file: %s\n", updater_name);
        goto error;
    }

    /*----------------------------------------------
     * Check the flags
     *----------------------------------------------
     * If the flag is CREATE_METADATA_FILE_ONLY_FLAG (0x0001), then this
     * updater file has no data and only serves to tell us to open the
     * HDF5 file for writing so we can apply updater files. If the HDF5
     * file is not on a POSIX-compliant file system, then we skip opening
     * the HDF5 file until we need to apply an updater file.
     */
    if (updater.flags & CREATE_METADATA_FILE_ONLY_FLAG) {
        /* If the hdf5 file is on a POSIX-compliant file system, then we only open
         * the file once in the beginning. */
        if (hand->is_posix) {
            assert(-1 == hand->h5_fd); /* verify that the HDF5 file is closed */
            if (-1 == (hand->h5_fd = open(hand->h5_file_path, O_WRONLY | O_SYNC))) {
                fprintf(stderr, "failed to open the HDF5 file: %s.  errno = %d (%s)\n", hand->h5_file_path,
                        errno, strerror(errno));
                goto error;
            }
        }

        /* Close the updater file */
        if (fclose(updater.file) == EOF) {
            fprintf(stderr, "updater file close failed: %s\n", updater_name);
            goto error;
        }

        return true;
    }

    if (!hand->is_posix) {
        /* If the hdf5 file is not on a POSIX-compliant file system, then we open
         * the file every time we apply an updater file. */
        assert(-1 == hand->h5_fd); /* verify that the HDF5 file is closed */
        if (-1 == (hand->h5_fd = open(hand->h5_file_path, O_WRONLY | O_SYNC))) {
            fprintf(stderr, "failed to open the HDF5 file: %s.  errno = %d (%s)\n", hand->h5_file_path, errno,
                    strerror(errno));
            goto error;
        }
    }

    /* Check that H5 file has been opened before copying data */
    if (-1 == hand->h5_fd) {
        fprintf(stderr, "HDF5 file is not opened yet, cannot apply updater file: %s\n", updater_name);
        goto error;
    }

    /*----------------------------------------------
     * Decode the top fields of the change list
     *----------------------------------------------
     */
    if (decode_cl_top_fields(&updater, hand) < 0) {
        fprintf(stderr, "failed to decode the top fields of the change list: %s\n", updater_name);
        goto error;
    }

    /*----------------------------------------------
     * Decode the actual change list and copy the changes to
     * the HDF5 file
     *----------------------------------------------
     */
    if (decode_and_copy_cl_entries_recovery_only(&updater, hand) < 0) {
        fprintf(stderr, "failed to decode and copy the change list entries: %s\n", updater_name);
        goto error;
    }

    /* Close the updater file */
    if (fclose(updater.file) == EOF) {
        fprintf(stderr, "updater file %s close failed\n", updater_name);
        goto error;
    }

    /* If the hdf5 file is on a non-POSIX-compliant file system, then we close
     * the file every time we finish applying an updater file. */
    if (!hand->is_posix) {
        if (0 != close(hand->h5_fd)) {
            fprintf(stderr, "failed to close the HDF5 file.  errno = %d (%s)\n", errno, strerror(errno));
            goto error;
        }
        hand->h5_fd = -1; /* to mark the HDF5 file as closed in hand */
    }

    /* If the flag is FINAL_UPDATE_FLAG (0x0002), print a message
     * and do nothing else */
    if (updater.flags & FINAL_UPDATE_FLAG) {
        if (hand->output) {
            fprintf(hand->output, "FINAL_UPDATE_FLAG is set in updater file: %s\n", updater_name);
        }
        else { /* Print it to stdout even if verbose output is not enabled */
            fprintf(stdout, "FINAL_UPDATE_FLAG is set in updater file: %s\n", updater_name);
        }
    }

    return true;

error:
    fprintf(stderr, "*** ERROR occurred while applying updater file: %s ***\n", updater_name);
    fprintf(stderr, "*** The HDF5 file may be corrupted. Aborting recovery. ***\n");
    /* Free the buffer allocated in decode_cl_top_fields() when error happens */
    if (updater.cl_buf) {
        free(updater.cl_buf);
    }

    /* Free the buffer allocated in decode_and_copy_cl_entries_for_recovery() when error happens */
    if (updater.change_list) {
        free(updater.change_list);
    }

    if (updater.file) {
        fclose(updater.file);
        updater.file = NULL;
    }

    if (-1 != hand->h5_fd) {
        close(hand->h5_fd);
        hand->h5_fd = -1;
    }

    return false;

} /* apply_updater_recovery_only() */

/*-----------------------------------------------------------------------------
 * Function: apply_all_updater_files
 *
 * Purpose:  Applies all updater files that can be found using the specified
 *           updater path. Automatically stops if no more updater files or
 *           if the last updater file has the FINAL_UPDATE_FLAG set.
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *-----------------------------------------------------------------------------
 */
static int
apply_all_updater_files(handler_t *hand)
{
    char updater_name[FILE_NAME_LEN];
    int  i;
    int  ret;

    /* We need to remove any numeric extension from the updater path */
    char *last_dot = strrchr(hand->updater_path, '.');
    if (last_dot != NULL) {              /* found a dot */
        char *endptr     = last_dot + 1; /* point to the character after the dot */
        bool  all_digits = true;

        /* Check if all characters after the dot are digits */
        while (*endptr != '\0') {
            if (!isdigit((unsigned char)*endptr)) {
                all_digits = false;
                break;
            }
            endptr++;
        }

        /* If all characters after the dot are digits, then this is a numeric extension. We can remove it. */
        if (all_digits) {
            *last_dot = '\0';
        }
    }

    /* Print out chosen file information */
    if (hand->output) {
        fprintf(hand->output, "Using specified files:\n");
        fprintf(hand->output, " - HDF5 file:           %s\n", hand->h5_file_path);
        fprintf(hand->output, " - Updater file prefix: %s\n\n", hand->updater_path);
    }
    else { /* Print it to stdout even if verbose output is not enabled */
        fprintf(stdout, "Using specified files:\n");
        fprintf(stdout, " - HDF5 file:           %s\n", hand->h5_file_path);
        fprintf(stdout, " - Updater file prefix: %s\n\n", hand->updater_path);
    }

    for (i = 0;; i++) {
        ret = snprintf(updater_name, sizeof(updater_name), "%s.%d", hand->updater_path, i);

        if (ret < 0) {
            fprintf(stderr, "Internal snprintf error while formatting updater_name\n");
            goto error;
        }
        if (ret >= (int)sizeof(updater_name)) {
            fprintf(stderr, "Updater file path too long: %s.%d\n", hand->updater_path, i);
            goto error;
        }

        /* Only process if the file exists */
        if (access(updater_name, F_OK) != 0) {
            /* Can't find the first updater file */
            if (i == 0) {
                fprintf(stderr, "No updater files found at path: %s.0\n", hand->updater_path);
                goto error;
            }

            /* No more updater files to process -- normal termination */
            if (hand->output) {
                fprintf(hand->output, "No more updater files found. Stopping.\n");
            }
            break;
        }

        /* Apply the updater file */
        if (!apply_updater_recovery_only(updater_name, hand)) {
            goto error;
        }
    }

    /* If the is_posix flag is true, then the HDF5 file should still be open.
     * Close it now. */
    if (hand->is_posix) {

        assert(hand->h5_fd >= 0); /* verify that the HDF5 file is open */
        if (0 != close(hand->h5_fd)) {
            fprintf(stderr, "failed to close the HDF5 file.  errno = %d (%s)\n", errno, strerror(errno));
            goto error;
        }
        hand->h5_fd = -1; /* to mark the HDF5 file as close in hand */
    }
    else {

        assert(-1 == hand->h5_fd); /* verify that the HDF5 file is closed */
    }

    return 0;

error:
    return -1;
} /* apply_all_updater_files() */

/*-------------------------------------------------------------------------
 * Function: run_command_with_catch
 *
 * Purpose:  Run a command with the given arguments, redirecting its output
 *    (stdout and stderr) to a file, and capturing its exit status and PID
 *    into separate files.
 *
 * Description: This function forks a child process to run the command. The
 *    child process redirects its output (stdout and stderr) to a file named
 *    "<outbase>.out". The parent process writes the child's PID to
 *    "<outbase>.pid", then waits for the child to finish and captures its
 *    exit status in "<outbase>.rc".
 *
 * Return:   Success:       exit status of the command
 *           Failure:      -1
 *
 * Cody Sloan -- 6/10/2025
 */
static int
run_command_with_catch(const char *outbase, char *const cmd_argv[])
{
    pid_t pid = fork();
    if (pid < 0) {
        perror("fork failed");
        goto error;
    }
    else if (pid == 0) { /* Child process */

        /* Prepare output file names */
        char out_file[FILE_NAME_LEN];
        snprintf(out_file, sizeof(out_file), "%s.out", outbase);

        /* Open output file */
        int out_fd = open(out_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (out_fd < 0) {
            perror("open output file failed");
            exit(EXIT_FAILURE);
        }

        /* Redirect stdout and stderr to the output file */
        if (dup2(out_fd, STDOUT_FILENO) < 0 || dup2(out_fd, STDERR_FILENO) < 0) {
            perror("dup2 failed");
            close(out_fd);
            exit(EXIT_FAILURE);
        }

        close(out_fd); /* Close the file descriptor after duplicating */

        /* Execute the command */
        execvp(cmd_argv[0], cmd_argv);

        /* If execvp returns, there was an error */
        perror("execvp failed");
        exit(EXIT_FAILURE);
    }
    else { /* Parent process */
        /* Wait for the child process to finish */
        int status;
        if (waitpid(pid, &status, 0) == -1) {
            perror("waitpid failed");
            goto error;
        }

        /* Check if the child exited normally */
        if (WIFEXITED(status)) {
            int exit_status = WEXITSTATUS(status);

            /* Write the exit status to a file */
            char rc_file[FILE_NAME_LEN];
            snprintf(rc_file, sizeof(rc_file), "%s.rc", outbase);
            FILE *rc_fp = fopen(rc_file, "w");
            if (rc_fp) {
                fprintf(rc_fp, "%d\n", exit_status);
                fclose(rc_fp);
            }
            else {
                perror("fopen rc file failed");
                goto error;
            }

            return exit_status; /* Return the exit status of the command */
        }
        else {
            fprintf(stderr, "Command did not exit normally\n");
            goto error;
        }
    }

error:
    return -1; /* Indicate failure */
} /* run_command_with_catch() */

/*-------------------------------------------------------------------------
 * Function: check_h5clear_path
 *
 * Purpose: Check if the h5clear utility is available in the system PATH or
 *          specified by the H5CLEAR_PATH environment variable.
 *
 * Return:   Success:    0
 *           Failure:    -1
 */
static int
check_h5clear_path(handler_t *hand)
{
    char path_buf[FILE_NAME_LEN];

    /* Check for h5clear path in environment variable H5CLEAR_PATH */
    char *h5clear_env = getenv("H5CLEAR_PATH");
    if (h5clear_env != NULL && strlen(h5clear_env) > 0) {
        strncpy(path_buf, h5clear_env, sizeof(path_buf) - 1);
        path_buf[sizeof(path_buf) - 1] = '\0'; /* Ensure null-termination */

        /* Check that path ends with "h5clear" */
        size_t len = strlen(path_buf);
        if (len < 7 || strcmp(path_buf + len - 7, "h5clear") != 0) {
            fprintf(stderr, "ERROR: H5CLEAR_PATH does not end with 'h5clear'\n");
            return -1;
        }
    }
    else {
        /* Check for h5clear in PATH */
        bool  found    = false;
        char *path_env = strdup(getenv("PATH"));
        char *s        = path_env;
        char *p        = NULL;

        /* Loop through PATH to find h5clear */
        do {
            p = strchr(s, ':');
            if (p != NULL) {
                *p = '\0'; /* Temporarily null-terminate */
            }

            snprintf(path_buf, sizeof(path_buf), "%s/h5clear", s);
            if (access(path_buf, X_OK) == 0) {
                found = true;
                break;
            }

            s = p + 1;
        } while (p != NULL);

        free(path_env);

        if (!found) {
            fprintf(stderr,
                    "ERROR: h5clear command not found in PATH or H5CLEAR_PATH environment variable\n");
            return -1;
        }
    }

    /* Store the h5clear path in the handler */
    hand->h5clear_path = strdup(path_buf);
    if (hand->h5clear_path == NULL) {
        fprintf(stderr, "Memory allocation failed for h5clear path\n");
        return -1;
    }
    return 0;
}

/*-------------------------------------------------------------------------
 * Function: reset_status_flags
 *
 * Purpose: Reset the status flags in the superblock of the h5 file so that
 *         the file can be opened again.
 *
 * Description: runs the h5clear command with the -s option to reset the
 *              status flags in the superblock of the h5 file.
 *              Path to h5clear utility MUST be defined in PATH or
 *              H5CLEAR_PATH environment variable.
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *
 * Cody Sloan -- 6/10/2025
 *
 * Changes:     Added the "--increment" option to h5clear_opts[].  This
 *              option should set the eof of the target file to the
 *              maximum of EOA and EOF.  This should prevent recovery
 *              failures caused by apparent HDF5 file truncation.
 *
 *                                              JRM -- 5/9/26
 *
 *-------------------------------------------------------------------------
 */
static int
reset_status_flags(handler_t *hand)
{
    /* Check handler*/
    if (!hand || !hand->h5_file_path || !hand->h5clear_path) {
        fprintf(stderr, "Invalid handler\n");
        return -1;
    }

    if (hand->output) {
        fprintf(hand->output, "Resetting status flags in the HDF5 file: %s\n", hand->h5_file_path);
    }

    /* Add -s option to command: specifies we want h5clear to clear status flags */
    // char h5clear_opts3] = "-s";
    // char h5clear_opts[] = "-s --increment";
    char h5clear_opts[] = "--status";
    // char h5clear_opts[] = "--status --increment";
    /* Execute h5clear command*/
    char *cmd_argv[] = {hand->h5clear_path, h5clear_opts, hand->h5_file_path, NULL};
    int   ret        = run_command_with_catch("h5clear_post", cmd_argv);

    if (ret < 0) {
        fprintf(stderr, "error: Failed to run h5clear command\n");
        return -1;
    }
    if (hand->output) {
        fprintf(hand->output, "Successfully reset status flags in the HDF5 file: %s\n", hand->h5_file_path);
    }
    return 0;
} /* reset_status_flags() */

/*----------------------------------------------------------------------------
 * Function: release_resources
 *
 * Purpose:  Free some memory
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *
 * Found in aux_process, but modified for recovery_tool resources.
 *----------------------------------------------------------------------------
 */
static int
release_resources(handler_t *hand)
{
    if (hand->log_file_path) {
        free(hand->log_file_path);

        if (fclose(hand->log_file) == EOF) {
            fprintf(stderr, "log file close failed\n");
            goto error;
        }
    }
    if (hand->h5_file_path)
        free(hand->h5_file_path);

    if (hand->updater_path)
        free(hand->updater_path);

    return 0;

error:
    return -1;

} /* release_resources() */

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose:  Show command usage
 *
 * Found in aux_process, but modified for recovery_tool usage.
 *-------------------------------------------------------------------------
 */
static void
usage(void)
{
    printf("Usage: recovery_tool [options] <h5_file> <ud_file>\n");
    printf("    Where:\n");
    printf("      <h5_file>: the path to the HDF5 file.\n");
    printf("      <ud_file>: the path to one of the updater files (doesn't matter\n");
    printf("                 which, and can accept basename).\n");
    printf("    Options:\n");
    printf("      [-h --help]:                Prints this help page.\n");
    printf("      [-p --posix]:               Indicate that the HDF5 file is on a POSIX \n");
    printf("                                  file system.\n");
    printf("      [-v --verbose]:             Write log entries to stdout.\n");
    printf("      [-l --log_file] <log_file>: Specify path of a log file for log entries.\n");
    printf("                                  (Will ignore verbose option)\n");
    printf("\nNote: The h5clear utility must be available in PATH or specified\n");
    printf("      through the H5CLEAR_PATH environment variable.\n");
    printf("\n");
} /* usage() */

/*-------------------------------------------------------------------------
 * Function: parse_command_line
 *
 * Purpose:  Parse the options that a user specifies
 *
 * Return:   Success:    0
 *
 *           Failure:    -1
 *
 * Found in aux_process, but modified to handle recovery_tool inputs.
 *-------------------------------------------------------------------------
 */
static int
parse_command_line(int argc, char *argv[], handler_t *hand)
{
    int              opt;
    aux_long_options long_options[] = {{"help", no_arg, 'h'},
                                       {"posix", no_arg, 'p'},
                                       {"verbose", no_arg, 'v'},
                                       {"log_file", require_arg, 'l'},
                                       {NULL, 0, 0}};

    /* Initialize the command line options */
    hand->verbose       = false;
    hand->log_file_path = NULL;
    hand->log_file      = NULL;
    hand->is_posix      = false;
    hand->output        = NULL;
    hand->updater_path  = NULL;
    hand->h5_file_path  = NULL;
    hand->h5_fd         = -1;

    /*
     * aux_get_options supports both POSIX and Windows
     */
    while ((opt = aux_get_options(argc, argv, "hvpl:", long_options)) != EOF) {
        switch (opt) {
            case 'h':
                usage();

                exit(0);

                break;
            case 'p':
                /* Whether the file system is POSIX */
                hand->is_posix = true;
                break;
            case 'v':
                /* Whether to write log entries to stdout */
                fprintf(stdout, "Whether to write log entries to stdout:\t\t\ttrue\n");
                hand->verbose = true;
                break;
            case 'l':
                /* The log file */
                if (aux_optarg) {
                    fprintf(stdout, "Printing to log file:\t\t\t\t\t%s\n", aux_optarg);
                    hand->log_file_path = strdup(aux_optarg);
                }
                else
                    fprintf(stderr, "aux_optarg is null\n");
                break;
            case ':':
                fprintf(stderr, "option needs a value\n");
                break;
            case '?':
                fprintf(stderr, "unknown option: %c\n", opt);
                break;
        }
    }

    if ((argc - aux_optind) >= 2) {
        hand->h5_file_path = strdup(argv[aux_optind++]);
        hand->updater_path = strdup(argv[aux_optind++]);
    }
    else {
        fprintf(stderr, "Missing required arguments: <h5_file> <ud_file>\n");
        goto error;
    }

    if (hand->log_file_path) {
        if (!(hand->log_file = fopen(hand->log_file_path, "w"))) {
            fprintf(stderr, "failed to create the log file: %s\n", hand->log_file_path);
            goto error;
        }

        hand->output = hand->log_file;
    }
    else if (hand->verbose) {
        hand->output = stdout;
    }

    /* Print whether assuming POSIX or not */
    if (hand->is_posix) {
        if (hand->output) {
            fprintf(hand->output, "Assuming POSIX semantics for file system containing HDF5 file. \n");
        }
        else { /* Print it to stdout even if verbose output is not enabled */
            fprintf(stdout, "Assuming POSIX semantics for file system containing HDF5 file. \n");
        }
    }

    return 0;

error:
    return -1;
} /* parse_command_line() */

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose:  This program (recovery tool) applies the updater files to
 *           the HDF5 file to try to recover from crashes when they happen.
 *
 * Return:   Success:    0
 *
 *           Failure:    1
 *-------------------------------------------------------------------------
 */
int
main(int argc, char **argv)
{
    handler_t hand;

    if (parse_command_line(argc, argv, &hand) < 0)
        goto error;

    if (check_h5clear_path(&hand) < 0)
        goto error;

    if (apply_all_updater_files(&hand) < 0)
        goto error;

    if (reset_status_flags(&hand) < 0) {
        fprintf(stderr, "failed to reset the status flags in the HDF5 file\n");
        goto error;
    }
    if (release_resources(&hand) < 0)
        goto error;

    /* Print completion message */
    if (hand.output) {
        fprintf(hand.output, "Recovery completed successfully.\n");
    }
    else { /* Print it to stdout even if verbose output is not enabled */
        fprintf(stdout, "Recovery completed successfully.\n");
    }

    return EXIT_SUCCESS;

error:
    return EXIT_FAILURE;
}

/*****************************************************************************/
/******** End functions specific to recovery_tool.c **************************/
/*****************************************************************************/
