# HDF5 Examples (VFD SWMR Extensions)

This directory contains VFD SWMR demo programs and related recovery/testing utilities.

> ⚠️ The original HDF5 “run-all-ex.sh” example system in this directory is outdated and should not be used.
> See the “Legacy HDF5 Examples” section at the bottom for reference only.

---

## 1. Building the programs

### credel
```bash
../install/bin/h5cc -o credel credel.c nbcompat.c
```

### gaussians
```bash
../install/bin/h5cc -o gaussians gaussians.c nbcompat.c -lcurses
```

Create Aliases:
```bash
    ln -s gaussians wgaussians
    ln -s gaussians rgaussians
```

## 2. Running the Demos

Each demo is intended to be run using multiple terminal windows. One process performs file creation or updates, while additional processes inspect the resulting HDF5 file. Some configurations also include an auxiliary process that generates updater metadata during execution.

Use CTRL-C to stop the demo programs, which run indefinitely.

### Running credel demo:
---
#### Normal Run:
```bash
# In one terminal window:
HDF5_VFD_SWMR_CONFIG=credel_swmr_config.txt ./credel -v 

# In another terminal window:
../install/bin/h5ls --swmr-config-file=credel_swmr_config.txt --poll=100 -r -d ./credel.h5 
```

#### With auxiliary Process:

```bash
# In one terminal window:
HDF5_VFD_SWMR_CONFIG=credel_swmr_config.txt ./credel -v

# In another terminal window:
../utils/vfd_swmr/aux_process credel_md_file ./credel_updater_file

# In a third terminal window:
../install/bin/h5ls --swmr-config-file=credel_swmr_gen_updater_config.txt --poll=100 -r -d ./credel.h5
```


### Running gaussians demo:
---
#### Standalone Run:
```bash
HDF5_VFD_SWMR_CONFIG=gaussians_swmr_config.txt ./gaussians
```

#### To run as writer and reader:
```bash
# In one terminal window:
HDF5_VFD_SWMR_CONFIG=gaussians_swmr_config.txt ./wgaussians

# In another terminal window:
HDF5_VFD_SWMR_CONFIG=gaussians_swmr_config.txt ./rgaussians
```

## 3. Using h5dump with demo VFD SWMR configurations

`h5dump` supports the `--swmr-config-file` option, which allows a VFD SWMR configuration file to be loaded before opening an HDF5 file. The configuration is applied to the file access property list (FAPL) used by h5dump, enabling the tool to access files that require specific VFD SWMR settings.

>**Note:** This functionality has only been verified with `credel.h5` and `credel_swmr_config.txt`. Using `h5dump` with `gaussians.h5` and `gaussians_swmr_config.txt` currently results in errors.

### Running h5dump on credel.h5

After running the credel demo program:
```bash
../install/bin/h5dump --swmr-config-file=credel_swmr_config.txt ./credel.h5
```

<!-- ### Running h5dump on gaussians.h5

After running the gaussians demo program:
```bash
../install/bin/h5dump --swmr-config-file=gaussians_swmr_config.txt ./gaussians.h5
``` -->

## 4. Testing recovery_tool with demos
These examples demonstrate recovering an HDF5 file after an interrupted or incomplete write by applying updater files generated during execution.

### credel recovery test
Start the demo:
```bash
HDF5_VFD_SWMR_CONFIG=credel_swmr_gen_updater_config.txt ./credel -v
```

While it is running, terminate the process (in another terminal):
```bash
# Find the PID first:
pgrep credel

# Then kill the process:
kill -9 <pid_of_credel>
```

After termination, verify the file is not readable (it should error trying to open the file):
```bash
../install/bin/h5ls ./credel.h5
```

Attempt recovery of the file:
```bash
H5CLEAR_PATH=../install/bin/h5clear ../utils/vfd_swmr/recovery_tool ./credel.h5 ./credel_updater_file
```

Verify the file is readable now (it should list the two last created groups before the writer crashed):
```bash
../install/bin/h5ls ./credel.h5
```

### gaussians recovery test
Start the demo (writer only):
```bash
HDF5_VFD_SWMR_CONFIG=gaussians_swmr_gen_updater_config.txt ./wgaussians
```

While it is running, terminate the process (in another terminal):
```bash
# Find the PID first:
pgrep wgaussians

# Then kill the process:
kill -9 <pid_of_wgaussians>
```

Run this in the same terminal that `./wgaussians` was run in to undo any ncurses terminal state left behind by the intentional crash.
```bash
reset
```

After termination, verify the file is not readable (it should error trying to open the file):
```bash
../install/bin/h5ls ./gaussians.h5
```

Attempt recovery of the file:
```bash
H5CLEAR_PATH=../install/bin/h5clear ../utils/vfd_swmr/recovery_tool ./gaussians.h5 ./gaussians_updater_file
```

Verify the file is readable now (it should list group-0):
```bash
../install/bin/h5ls ./gaussians.h5
```

## 4. Testing demos over NFS

The `aux_process` program allows the VFD SWMR demos to be run across an NFS mount. In this configuration, the writer and reader processes may run on separate systems while accessing the same HDF5 file through a shared NFS filesystem.

The following requirements must be met:

* `aux_process` must run on the same system as the reader process.
* The metadata file generated by `aux_process` must be stored on a local POSIX-compliant filesystem.
* The reader configuration must reference the correct metadata file generated by `aux_process`.

>**Note:** When running over NFS, users must ensure that all program and file paths are valid on the system where each process is executed. In particular, the `--swmr-config-file` argument passed to `h5ls` must use an absolute path to the configuration file.

### Running credel over NFS mount
**Note:** When running over NFS, `aux_process` may detect and apply updater files in bursts rather than continuously. While `aux_process` is waiting for additional updater files to become visible, `h5ls` may repeatedly display the same groups and datasets. This behavior is expected and does not indicate that either process has stalled.

**Terminal 1 (writer system):**

Run the demo from the NFS-mounted directory:
```bash
HDF5_VFD_SWMR_CONFIG=/path/to/credel_swmr_gen_updater_config.txt /path/to/credel -v
```

**Terminal 2 (reader system - setup):**
Before starting the auxiliary process, update the SWMR configuration file to set the metadata file location to a local non-NFS mounted directory:
```bash
sed -i 's|( md_file_path "./" )|( md_file_path "/absolute/path/to/metadata_directory/" )|' credel_swmr_gen_updater_config.txt
```

**Terminal 2 (reader system - auxiliary process):**
Start the auxiliary process after the writer has started:
```bash
/path/to/aux_process -v /path/to/metadata_directory/credel_md_file /path/to/NFS/mounted/directory/credel_updater_file
```

**Terminal 3 (reader system - reader process):**
Start `h5ls` after the auxiliary process has been started:
```bash
<path_to_hdf5>/install/bin/h5ls --swmr-config-file=/absolute/path/to/credel_swmr_gen_updater_config.txt \
--poll=100 -r -d /path/to/NFS/mounted/directory/credel.h5
```

### Running gaussians over NFS mount
Not supported at this time.