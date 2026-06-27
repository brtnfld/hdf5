#!/usr/bin/env python3
"""Print only conflict hunks whose content doesn't match already-validated
known-safe patterns, for files passed on stdin. Helps focus manual review
on genuinely new patterns instead of re-reading familiar ones."""
import re
import sys

KNOWN_SAFE = re.compile(
    r"FUNC_ENTER_(PACKAGE|STATIC)|H5FDperform_init|_VALUE\b|read_vector|write_vector|"
    r"read_selection|write_selection|_ctl,|H5FD__\w+_ctl|bad VFL driver info|"
    r"get_default_config|H5P_set_driver|\(void \*\)|ignore_disabled_file_locks|"
    r"lock_env_var|HDF5_USE_FILE_LOCKING|populate_config|H5_VFD_|getenv\(HDF5_DRIVER|"
    r"GCC.*clang|particular GCC|H5_PKG_INIT_VAR|term_f[12]|cls->get_(rank|size|comm)|"
    r"H5FD_class_mpi_t|FUNC_ENTER_\w+_NOERR"
)


def parse_hunks(lines):
    segs = []
    i = 0
    n = len(lines)
    while i < n:
        if lines[i].startswith("<<<<<<< HEAD"):
            ours = []
            i += 1
            while i < n and not lines[i].startswith("======="):
                ours.append(lines[i])
                i += 1
            i += 1
            theirs = []
            while i < n and not lines[i].startswith(">>>>>>> "):
                theirs.append(lines[i])
                i += 1
            i += 1
            segs.append((ours, theirs))
        else:
            i += 1
    return segs


def main():
    for path in [l.strip() for l in sys.stdin if l.strip()]:
        with open(path, errors="surrogateescape") as f:
            content = f.read()
        if "<<<<<<< HEAD" not in content:
            continue
        segs = parse_hunks(content.split("\n"))
        unknown = []
        for ours, theirs in segs:
            blob = "\n".join(ours) + "\n" + "\n".join(theirs)
            if not KNOWN_SAFE.search(blob):
                unknown.append((ours, theirs))
        print(f"=== {path}: {len(segs)} hunks, {len(unknown)} UNRECOGNIZED ===")
        for ours, theirs in unknown:
            print("--- ours ---")
            print("\n".join(ours[:15]))
            print("--- theirs ---")
            print("\n".join(theirs[:15]))
            print()


if __name__ == "__main__":
    main()
