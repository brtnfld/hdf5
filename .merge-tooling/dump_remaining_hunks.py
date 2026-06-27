#!/usr/bin/env python3
"""Dump remaining conflict hunks (with a few lines of context) for review.
Usage: dump_remaining_hunks.py <file1> [file2 ...]
"""
import sys

CONTEXT = 2


def dump(path):
    with open(path, "r", errors="surrogateescape") as f:
        lines = f.read().split("\n")
    n = len(lines)
    i = 0
    hunk_no = 0
    while i < n:
        if lines[i].startswith("<<<<<<< HEAD"):
            hunk_no += 1
            start_ctx = max(0, i - CONTEXT)
            ours_start = i + 1
            j = ours_start
            while j < n and not lines[j].startswith("======="):
                j += 1
            ours = lines[ours_start:j]
            theirs_start = j + 1
            k = theirs_start
            while k < n and not lines[k].startswith(">>>>>>> "):
                k += 1
            theirs = lines[theirs_start:k]
            end_ctx = min(n, k + 1 + CONTEXT)
            print(f"\n=== {path} :: hunk {hunk_no} (line {i+1}) ===")
            print("--- context before ---")
            print("\n".join(lines[start_ctx:i]))
            print("--- OURS ---")
            print("\n".join(ours))
            print("--- THEIRS ---")
            print("\n".join(theirs))
            print("--- context after ---")
            print("\n".join(lines[k + 1:end_ctx]))
            i = k + 1
        else:
            i += 1


def main():
    for path in sys.argv[1:]:
        dump(path)


if __name__ == "__main__":
    main()
