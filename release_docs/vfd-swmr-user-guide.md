# VFD SWMR User's Guide 

**This document describes VFD SWMR prototype that is currently 
available in the HDF5 GitHub repository [feature branch](https://github.com/LifeboatLLC/hdf5_swmr.git).**

SWMR, which stands for Single Writer/Multiple Reader access mode, is a feature
of the HDF5 library that lets a process write data to an HDF5 file
while one or more processes read the file.  Use cases range from
monitoring data collection and/or steering experiments in progress
to financial applications.

The following diagram illustrates the SWMR concept.

<img src = SWMRdataflow.png width=400 />

The original version of SWMR introduced in HDF5 1.10.0 release, 
functions by ordering metadata writes to
the HDF5 file so as to always maintain a consistent view of metadata
in the HDF5 file -- which requires SWMR specific modifications to 
all code that maintains on disk metadata. For more information, and specifically, 
for the limitations of the original SWMR, see
 [Introduction to Single-Writer/Multiple-Reader (SWMR)](https://https://support.hdfgroup.org/documentation/hdf5/latest/_s_w_m_r_t_n.html)

VFD SWMR is designed to address the limitations of the initial SWMR implementation and 
be a more maintainable.  It functions by taking 
regular snapshots of HDF5 file metadata on the writer side, and using 
a specialized virtual file driver (VFD) on the reader side to 
intercept metadata read requests and satisfy them from the 
snapshots where appropriate -- thus assuring that the readers 
see a consistent view of HDF5 file metadata.

This design allowed us to implement VFD SWMR with only minor 
modifications to the HDF5 library above metadata cache and page 
buffer.  As a result, not only is VFD SWMR more modular and 
easier to maintain, it is also almost "full SWMR" -- that is it 
allows use of almost all HDF5 capabilities by VFD SWMR writers,
with results that become visible to the VFD SWMR readers.

In particular, VFD SWMR allows the writer to create and delete 
both groups and datasets, and to create and delete attributes on 
both groups and datasets while operating in VFD SWMR mode -- 
which is not possible using the original SWMR implementation.  

We say that VFD SWMR is almost "full SWMR" because there are a 
few limitations -- most notably:

* The current implementation of variable length data in datasets
  is fundamentally incompatible with VFD SWMR, as it stores variable 
  length data as metadata. This limitation will be lifted when reimplemnttaion of
  variable length data storage is available.

* At present the Virtual Data Set (VDS) feature is not 
  well integrated with VFD SWMR. Please see 
[Using virtial datasets (VDS)](/doc/vfd-swmr-user-guide.md#using-virtual-datasets-vds) 
for how to use VFD SWMR and VDS features.

* VFD SWMR is only tested with, and should only be used with 
  the latest HDF5 file format.  Theoretically, there is no functional
  reason why it will not work with earlier versions of the file format.  
  However, it is possible to construct very large pieces of metadata 
  in early versions of the HDF5 file format, which has the potential to 
  cause major performance issues.

Due to its regular snapshots of metadata, VFD SWMR provides guarantees 
on the maximum time from write to visibility to the readers -- with 
the provisos that the underlying file system is fast enough, that 
the writer makes HDF5 library API calls with sufficient regularity, and 
that both reader and writer avoid long running HDF5 API calls.

For further details on VFD SWMR design and implementation, see 
[RFC: VFD SWMR](https://support.hdfgroup.org/releases/hdf5/documentation/rfc/VFD_SWMR_RFC_220519.pdf).

# Quick start

Follow these instructions to download, configure, and build the
VFD SWMR project, then install the HDF5 library and
utilities built by the VFD SWMR project.

## Download

Clone the HDF5 repository in a new directory, then switch to the 
`feature/vfd_swmr` branch as follows:

```
% git clone https://github.com/HDFGroup/hdf5 swmr
% cd swmr
% git checkout feature/vfd_swmr
```

## Build

There are no special instructions for building VFD SWMR. Simply follow
the usual build procedure for CMake or the Autotools using the guides
in the `release_docs` directory.

IMPORTANT:

The VFD SWMR branches are maintenance branches and will default to a debug
build. They also do not come with generated files, so Perl will be required
when building with CMake and Perl and the Autotools (autoconf, etc.) will
be required when building with the Autotools.

Run `autogen.sh` script in the top
directory if building with Autotools to generate required files. 

Some notes:

- The VFD SWMR tests can take some time to run.
- The VFD SWMR acceptance tests will typically emit some output about "expected errors" that you can ignore. Real errors are clearly flagged.
- If the tests do not pass on your system, please let the developers know via the email address given at the end of this document.

# Sample programs

## Extensible datasets

For an example of a program that uses VFD SWMR to write/read many
extensible datasets, have a look at `test/vfd_swmr_bigset_writer.c`, the
"bigset" test.  We compile two binaries from that source file, one that
operates in write mode, and a second that operates in read mode.

In write mode, "bigset" creates an HDF5 file containing one or more
datasets that are extensible in either one dimension or two.  Then it
runs for several steps, increasing the size of each dataset in each
dimension once every step.  The dimensions, number of datasets, the
step increase in dataset size, and the number of steps are configurable
using command-line options -d, -s, -r and -c, and -n, respectively---use
the -h option to get a usage message.  Each dataset is written with a
predictable pattern.

In read mode, "bigset" reads each dataset from an HDF5 file created
by a "bigset" writer and verifies the patterns.  It takes the same
command-line parameters as the "bigset" writer.  The reader and writer
may run concurrently; the reader "polls" the content until it is just
shy of complete, given the number of steps expected.

To run a bigset test, open a couple of terminal windows, one for the
reader and one for the writer.  cd to the `test` directory under
my build directory, and run the writer in one window:

```
% ./vfd_swmr_bigset_writer -n 50 -d 2
```

and in the other window, run the reader:

```
% ./vfd_swmr_bigset_reader -n 50 -d 2 -W
```

The writer will wait for a signal before it quits.  You can tap CTRL-C to make
it quit.

The reader and writer programs support several command-line options:

```
usage: vfd_swmr_bigset_writer [-F] [-M] [-S] [-V] [-W] [-a steps] [-b] [-c cols]
    [-d dims]
    [-n iterations] [-r rows] [-s datasets]
    [-u milliseconds]

-F:                   fixed maximal dimension for the chunked datasets
-M:                   use virtual datasets and many source files
-S:                   do not use VFD SWMR
-V:                   use virtual datasets and a single source file
-W:                   do not wait for a signal before exiting
-a steps:             `steps` between adding attributes
-b:                   write data in big-endian byte order
-c cols:              `cols` columns per chunk
-d 1|one|2|two|both:  select dataset expansion in one or
                      both dimensions
-n iterations:        how many times to expand each dataset
-r rows:              `rows` rows per chunk
-s datasets:          number of datasets to create
-u ms:                milliseconds interval between updates
                      to vfd_swmr_bigset_writer.h5
```

## The VFD SWMR demos

The VFD SWMR demos are located in the `examples` directory of this source
tree. Instructions for building the example programs are given in the README
file in that directory. These programs are NOT installed via `make install`
and have to built by hand with h5cc as described in the README.

Two Gaussian programs are built, `wgaussians` and `rgaussians`.  If you start
both from the same directory in different terminals, you should see the
"bouncing 2-D Gaussian distributions" in the `rgaussians` terminal.  This demo
uses curses, so you may need to install the curses developers library to build
(and this is probably not going to be easy to build on Windows).

The creation-deletion (`credel`) demo is also run in two terminals.
The two command lines are given in the README. You need to use the `h5ls`
installed from the VFD SWMR branch, since only that version has the `--poll`
option. Be careful to not use a non-VFD-SWMR system h5ls here.

# Developer tips

## Configuring VFD SWMR

VFD SWMR may be configured either using the configuration parser or directly through the HDF5 property list API. The configuration parser approach is recommended for most applications because it automatically applies the required HDF5 property list settings in addition to the VFD SWMR configuration parameters.

### File-creation properties

VFD SWMR requires HDF5 files to be created using a paged allocation strategy. This requirement may be satisfied automatically using a VFD SWMR configuration file (recommended) or configured manually through the file creation property list (FCPL).

When configuring manually, the paged allocation strategy is enabled with:

```
ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1);
```

Allocated storage that is smaller than the page size will not overlap a page boundary, and allocated storage that is one page or greater in size will start on a page boundary. VFD SWMR relies on this allocation strategy.

### File-access properties

VFD SWMR also requires several file access properties to be configured before opening or creating an HDF5 file. These properties may be configured automatically using the VFD SWMR configuration parser (recommended) or manually through the file access property list (FAPL).

When configuring manually, the application must:

1. Create a file access property list using `H5Pcreate(H5P_FILE_ACCESS)`.
2. Set the latest file format using `H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST)`. 
3. Enable page buffering using `H5Pset_page_buffer_size()`.
4. Set any VFD SWMR configuration properties using `H5Pset_vfd_swmr_config()`. 

The following sections describe the available VFD SWMR configuration parameters. 
These parameters may be specified either in a VFD SWMR configuration file
or programmatically through the `H5F_vfd_swmr_config_t` structure, which is documented in 
[H5Fpublic.h](https://github.com/LifeboatLLC/hdf5_swmr/blob/feature/vfd_swmr/src/H5Fpublic.h). 

VFD SWMR relies on metadata reads and writes to go through the
page buffer.  Note that the default page size is 4096 bytes. Finding good
values for `buf_size` may take some experimentation. We use 4096 (giving a
single page buffer) for `buf_size` in our test code.

*Note*: when VFD SWMR is enabled, the meta-/raw-data pages proportion 
set by `H5Pset_page_buffer_size()` does not actually control the
pages reserved for raw data.  *All* pages are dedicated to buffering
metadata.

### Using the VFD SWMR configuration parser (recommended)

The recommended method for configuring VFD SWMR is through the VFD SWMR configuration parser. The parser reads VFD SWMR configuration settings from a configuration string or file and automatically applies the required file creation properties, file access properties, and VFD SWMR configuration to the supplied property lists.

Before calling the configuration parser, the application must create the required property lists using `H5Pcreate()`. The parser configures the supplied property lists but does not create or open HDF5 files. After the parser returns successfully, the configured property lists may be used with the appropriate HDF5 file creation or file opening routines.

The parser accepts configuration expressed in the VFD SWMR configuration language. For a complete description of the configuration syntax, supported directives, and examples, see the [VFD SWMR Configuration Language RFC](https://github.com/LifeboatLLC/HDF5-Encryption/blob/main/design_docs/RFC-VFD-Configuration-Language-2026-05-28.pdf).

All configuration parser interfaces use the same configuration format and apply the same VFD SWMR settings. They differ only in how the configuration is provided:

```c
herr_t H5Fswmr_config_string(const char *config_str,
                             hid_t fapl_id,
                             hid_t fcpl_id,
                             hbool_t writer,
                             hbool_t create_file);

herr_t H5Fswmr_config_file(const char *file_path,
                           hid_t fapl_id,
                           hid_t fcpl_id,
                           hbool_t writer,
                           hbool_t create_file);

herr_t H5Fswmr_config_env(hid_t fapl_id,
                         hid_t fcpl_id,
                         hbool_t writer,
                         hbool_t create_file,
                         const char *env_var_name);
```
* `H5Fswmr_config_string()`
Uses the configuration provided in config_str.
* `H5Fswmr_config_file()`
Loads configuration from the file specified by file_path.
* `H5Fswmr_config_env()`
Loads configuration from a file path stored in an environment variable.  
If `env_var_name` is `NULL`, the default environment variable `HDF5_VFD_SWMR_CONFIG` is used. Otherwise, the value of `env_var_name` is used as the environment variable containing the configuration file path.

#### Common parameters

All configuration parser interfaces accept the following parameters:

- `fapl_id`  
  The `hid_t` identifier of a valid file access property list.  
  The parser updates this property list with the VFD SWMR file access configuration.

- `fcpl_id`  
  The `hid_t` identifier of a valid file creation property list.  
  The parser updates this property list with the VFD SWMR file creation configuration when `create_file` is `true`.  
  This parameter is ignored when `create_file` is `false`, in which case `H5I_INVALID_HID` can be used.

- `writer`  
  Specifies whether the configuration is applied for a writer (`true`) or reader (`false`).

- `create_file`  
  Indicates whether the configuration is being applied in the context of file creation.  
  When `true`, both file creation and file access properties are configured.  
  When `false`, only file access properties are configured.


*Note:* An error will occur if `create_file` is `true` while `writer` is `false`.

#### Example configuration:
The following example configuration is used by the `credel` demo program and is included in [credel_swmr_config.txt](https://github.com/LifeboatLLC/hdf5_swmr/blob/feature/vfd_swmr/examples/credel_swmr_config.txt). All three parser interfaces accept this same configuration format. 

Comments have been added to explain field meanings and the use of boolean values (`0` and `1`). These comments are valid configuration syntax, but are included here for documentation and are not required for correct configuration.

```
( vfd_swmr_config_data
  (
    ( H5F_vfd_swmr_config                   
      (
        ( version 1 )                       /* Only valid version number currently */
        ( tick_len 4 )
        ( max_lag 7 )
        ( presume_posix_semantics 1 )       /* boolean (0/1) */
        ( maintain_metadata_file 1 )        /* boolean (0/1) */
        ( generate_updater_files 0 )        /* boolean (0/1) */
        ( flush_raw_data 1 )                /* boolean (0/1) */
        ( md_pages_reserved 128 )
        ( md_file_path "./" )
        ( md_file_name "credel_md_file" )'
        ( log_file_path "" )                /* Empty string or omit this field to disable logging */
        ( pb_expansion_threshold 50 )
      )
    )
    ( page_buffer_config
      (
        ( page_buf_size 4096 )
        ( metadata_pages_only 1 )           /* boolean (0/1) */
      )
    )
    ( file_space_strategy_config
      (
        ( persist 0 )                       /* boolean (0/1) */
      )
    )
    ( file_space_page_size
      (
        ( page_size 4096 )
      ) 
    )
  )
)
```

#### Example: Configuring writer and reader
The following examples demonstrate how to configure VFD SWMR for both a writer and a reader using a configuration parser interface.

Example code: Setting VFD SWMR for writer process:
```c
hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);
hid_t fcpl = H5Pcreate(H5P_FILE_CREATE);

H5Fswmr_config_file("/path/to/configuration_file", 
                    fapl, 
                    fcpl, 
                    true,  /* writer */
                    true); /* creating a file */
```

Example code: Setting VFD SWMR for reader process:
```c
hid_t fapl = H5Pcreate(H5P_FILE_ACCESS);

H5Fswmr_config_file("/path/to/configuration_file", 
                    fapl, 
                    H5I_INVALID_HID, /* No FCPL needed */ 
                    false,           /* reader */
                    false);          /* open an existing file */
```
After successful configuration, the property lists can be passed to `H5Fcreate()` or `H5Fopen()` to create or open the HDF5 file.

### Configuring through the HDF5 API

Below are examples of how to configure SWMR access mode for both writer and reader programmatically. 

Example code: Setting VFD SWMR for writer process:

```c
    fapl = H5Pcreate(H5P_FILE_ACCESS);
    H5Pset_page_buffer_size(fapl, 4096, 100, 0);
    H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1);

    memset(&config, 0, sizeof(config));
    H5Pget_vfd_swmr_config(fapl, &config);

    config.version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config.tick_len = 4;
    config.max_lag = 7;
    config.writer = true;
    config.maintain_metadata_file = true;
    config.flush_raw_data = true;
    config.md_pages_reserved = 128;
    strlcpy(config.md_file_path, "./my_md_file", sizeof(config.md_file_path));

    H5Pset_vfd_swmr_config(fapl, &config);
```

Example code: Setting VFD SWMR for reader process

```c
    fapl = H5Pcreate(H5P_FILE_ACCESS);

    memset(&config, 0, sizeof(config));
    H5Pget_vfd_swmr_config(fapl, &config);

    config.version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config.tick_len = 4;
    config.max_lag = 7;
    strlcpy(config.md_file_path, "./my_md_file", sizeof(config.md_file_path));

    H5Pset_vfd_swmr_config(fapl, &config);
```

### `H5F_vfd_swmr_config_t` fields discussion

When VFD SWMR is enabled, changes to the HDF5 metadata accumulate in
RAM until a configurable unit of time known as a *tick* has passed.
At the end of each tick, a snapshot of the metadata at the end of
an

When a reader first enters the API, it starts to use, or "selects,"
the metadata in the newest snapshot, and on every subsequent API
entry, if a tick has passed since the last selection, and if new
snapshots are available, then the reader selects the latest.

If a reader spends longer than `max_lag - 1` ticks (2400ms with
the example configuration) inside the HDF5 API, then its snapshot
may expire, resulting in undefined behavior.  When a snapshot
expires while the reader is using it, we say that the writer has
"overrun" the reader.  The writer cannot detect overruns.
Frequently the reader will detect an overrun and force the program
to exit with a diagnostic assertion failure.

The application tells VFD SWMR whether or not to configure for
reading or writing a file by setting the `writer` parameter to
`true` for writing or `false` for reading.

VFD SWMR snapshots are stored in a "metadata file" that is shared
between writer and readers.  On a POSIX system, the metadata file
may be placed on any *local* filesystem that the reader and writer
share.  The `md_file_path` parameter tells where to put the metadata
file.

The `md_pages_reserved` parameter tells how many pages to reserve
at the beginning of the metadata file for the metadata-file header
and the metadata index.  The header has an entire page to itself.
The remaining `md_pages_reserved - 1` pages are reserved for the
metadata index.  If the index grows larger than its initial
allocation, then it will move to a new location in the metadata file,
and the initial allocation will be reclaimed.  `md_pages_reserved`
must be at least 2.

The `version` parameter tells what version of VFD SWMR configuration
the parameter struct `config` contains.  For now, it should be
initialized to `H5F__CURR_VFD_SWMR_CONFIG_VERSION`.

**Important:** Writer and reader processes must use compatible VFD SWMR configuration parameters. In particular, the tick_len and max_lag values must be identical for both processes. Using the same configuration file for both the writer and reader is the recommended way to ensure the settings remain synchronized.

## Using virtual datasets (VDS)

An application may want to use VFD SWMR to create, read, or write
a virtual dataset.  Unfortunately, VDS does not work properly with
VFD SWMR at this time.  In this section, we describe some workarounds
that can be used with great care to make VDS and VFD SWMR cooperate.

A virtual dataset, when it is read or written, will open files on
an application's behalf in order to access the source datasets
inside.  If a virtual dataset resides on file `v.h5`, and one of
its source datasets resides on a second file, `s1.h5`, then the
virtual dataset will try to open `s1.h5` using the same file-access
properties as `v.h5`.  Thus, if `v.h5` is open with VFD SWMR with
metadata file `v.shadow`, then the virtual dataset will try to open
`s1.h5` with the same metadata file, which will fail.

Suppose that `v.h5` is *not* open with VFD SWMR, but it was opened
with default file-access properties.  Then the virtual dataset will
open the source dataset on `s1.h5` with default file-access
properties, too.  This default virtual-dataset behavior is not
helpful to the application that wants to use VFD SWMR to read or
write source datasets.

To use VFD SWMR with VDS, an application should *pre-open* each file
using its preferred file-access properties, including independent metadata
filenames for each source file.  As long as the virtual dataset remains
in use, the application should leave each of the pre-opened files open.
In this way the library, when it tries to open the source files, will
always find them already open and reuse the already-open files with the
file-access properties established on first open.

## Pushing HDF5 raw data to reader visibility

At present, VFD SWMR is hard coded to flush raw data at the end of 
each tick.  While this imposes additional overhead, it simplifies testing, 
and is probably desirable for applications that do not require the best
possible raw data throughput.  We plan to upgrade our tests and make this 
user configurable in the first production release.

With the currently hard coded flush of raw data at the end of each tick, 
it should not be necessary to call H5Fflush().  In fact, when VFD SWMR is 
active, H5Fflush() may require up to `max_lag` ticks to complete due to 
metadata consistency issues.

Instead, a writer can make its last changes to HDF5 file visible to all
readers immediately using the new call, `H5Fvfd_swmr_end_tick()`.  Note
that this call should be used sparingly, as it terminates the current 
tick early, thus effectively reducing `max_lag`.  Repeated calls in 
quick succession can force a reader to overrun `max_lag`, and 
read stale metadata.

When the flush of raw data at end of tick is disabled (not possible at present), 
the `H5Fvfd_swmr_end_tick()` call will make the writers current view of metadata
visible to the reader -- which may refer to raw data that hasn't been written to 
the HDF5 file yet.

## Reading up-to-date content

One expected use case for VFD SWMR involves an experiment in which instruments 
continuously generate 2-dimensional data frames.  These data frames are recorded 
in datasets in a HDF5 file that has been opened in VFD SWMR writer mode.  In this 
use case, the HDF5 file is opened in VFD SWMR reader mode by a second program 
that generates a real time display of the data as it is being collected -- thus 
allowing the experimenters to steer the experiment.

THG developed a demonstration program for class of application,
and we have some advice based on that experience. 

The writer typically will increase a dataset's dimensions by a
frame, using `H5Dset_extent()`, before it writes the data of that
frame with `H5Dwrite()`.  It's possible that a snapshot of the HDF5
file will propagate to the reader between the `H5Dset_extent()`
call and the `H5Dwrite()`.  Values `H5Dread()` from the last frame
at that juncture will not reflect the actual experimental data.
Instead, the reader will see arbitrary values or the fill value.
To display those values would be distracting and misleading to
the experimenter. 

On the reader, a strategy for displaying the most current, bonafide application
data is to read the dimensions of the frames dataset, `d`, compute
the number `n` of full frames contained in `d`, and read the
next-to-last frame, `n - 2`.  THG uses a variant of this strategy
in its `gaussians` demo.

On the writer, a strategy for protecting against snapshots between
the `H5Dset_extent()` and `H5Dwrite()` calls is to suspend VFD
SWMR's clock across both of the calls.  The
`H5Fvfd_swmr_disable_end_of_tick()` call takes a file identifier
and stops new snapshots from being taken on the given file until
`H5Fvfd_swmr_enable_end_of_tick()` is called on the same file.
Needless to say, end of tick processing should only be disabled
briefly.

# VFD SWMR utilities

The VFD SWMR distribution includes auxiliary utilities for supporting metadata consistency and recovery in environments such as NFS-mounted filesystems or interrupted executions. The utilities are located in the `utils/vfd_swmr` directory in this source tree. More detailed information, including command-line usage, can be found in the accompanying `README.md` in the same directory.

These utilities operate on updater files generated during VFD SWMR writer execution. Depending on the tool, these updater files are either used to build a consistent external metadata view, or to rebuild the internal metadata state of an HDF5 file after interruption.

## aux_process

The `aux_process` utility is used in networked or NFS-based environments to maintain a local, up-to-date metadata view of a VFD SWMR dataset. Because metadata updates from the writer may appear late or inconsistently due to filesystem behavior, `aux_process` provides a local metadata copy on the reader system.

It works by processing a sequence of updater files created during writer execution. These files contain step-by-step metadata updates and rely on filesystem rename behavior to ensure they are seen in the correct order. The resulting local metadata file reflects the latest state that can be built from the updater files that are available.

## recovery_tool

The `recovery_tool` is used to restore a consistent HDF5 metadata state after an unexpected interruption, such as a writer crash. It applies the same sequence of updater files used during normal execution, but instead rebuilds the metadata directly inside the HDF5 file so it can be safely reopened.

## crasher (testing utility)

The `crasher` utility is used only for testing recovery behavior. It simulates an abrupt termination of a running writer process so that interrupted SWMR sessions can be validated using the `recovery_tool`.


# Known issues

## Variable-length data

A VFD SWMR reader cannot reliably read back a variable-length dataset
written by VFD SWMR.  For example, a variable-length string
created and written as follows

```
    hid_t dset, space, type;
    char data[] = "content";

    type = H5Tcopy(H5T_C_S1);

    H5Tset_size(type, H5T_VARIABLE);

    space = H5Screate(H5S_SCALAR);

    dset = H5Dcreate2(..., "string", type, space, H5P_DEFAULT, H5P_DEFAULT,
        H5P_DEFAULT);

    H5Dwrite(dset, type, space, space, H5P_DEFAULT, &data);
```

and read back like this,

```
    char *data;
    herr_t ret;

    ret = H5Dread(..., ..., H5S_ALL, H5S_ALL, H5P_DEFAULT, &data);
```

may produce either an error return from `H5Dread` (`ret < 0`) or
a `NULL` pointer (`data == NULL`).

As discussed above, this is caused by a fundamental incompatibility 
between the current variable length data implementation in HDF5, which 
stores variable length data as metadata.  It is possible we may be able 
to mitigate the issue, but the most likely solution is the planned 
re-implementation of variable length data that is currently in the planning
stage.  Unfortunately, we have no ETA for this re-implementation.

## Iteration

An application that reads in VFD SWMR mode should take care to avoid
HDF5 iteration APIs, especially when iterating large numbers of objects
or using long-running application callbacks.  While the library is in an
iteration routine, it cannot track changes made by the writer.  If the
library spends more than `max_lag` ticks in the routine, then its view
of the HDF5 file will become stale.  Under those circumstances, HDF5
content could be mis-read, or the library could crash with a diagnostic
assertion.

## Object handles

At the present level of development, the writer cannot invalidate
a reader's HDF5 object handles (`hid_t`s).  If a reader holds an
object open---that is, it has a valid handle (`hid_t`) for the
object---while the writer deletes it, then reading content through
the handle may yield corrupted data or the data from some other
object, or the library may crash.

## Supported filesystems

A VFD SWMR writer and readers share a couple of files, the HDF5 (`.h5`)
file and the metadata file -- which is used to communicate snapshots of 
the HDF5 file metadata from the writer to the readers.  VFD SWMR relies 
on writes to the metadata file to take effect in the order described in 
the POSIX documentation for `read(2)` and `write(2)` system calls.  If 
the VFD SWMR readers and the writer run on the same POSIX host, this 
ordering should take effect, regardless of the underlying filesystem. 

If the VFD SWMR reader and the writer run on *different* hosts, then
the write-ordering rules depend on the shared filesystem.  VFD SWMR is
not generally expected to work with NFS at this time.  Parallel file systems
like GPFS and Lustre should order writes according to POSIX convention, so we
expect VFD SWMR to work on those file systems but we have not tested this.

The HDF Group plans to add support for networked file systems like NFS and
Windows SMB to VFD SWMR in the future.

## Microsoft Windows 

VFD SWMR is not officially supported on Microsoft Windows at this time.  The
feature should in theory work on Windows and NTFS, however it has not been
tested as the existing VFD SWMR tests rely on shell scripts.  Note that Windows
file shares are not supported as there is no write ordering guarantee (as with
NFS, et al.).

## File-opening order

If an application tries to open a file in VFD SWMR reader mode, and the
file is not already open by a VFD SWMR writer, then the application will
sleep in the `H5Fopen()` call until either the writer opens the same
file (using the same shadow file) or the reader times out after several
seconds.

# Reporting bugs

VFD SWMR is still under development, so it is possible that you will encounter 
bugs.  Please report them, along with any performance or design issues you 
encounter.

To contact the VFD SWMR developers, email vfdswmr@hdfgroup.org.
