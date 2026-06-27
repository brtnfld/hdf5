#!/usr/bin/env python3
"""Final-pass resolver for peripheral (non-vfd_swmr) files whose conflicts
could not be auto-classified by resolve_hunks.py/partial_resolve.py.

Policy (class 3 of MERGE_NOTES.md: "pure modernization -> take develop's
form"), refined by path/content:

  - Autotools-only files (*.am, configure.ac, autogen.sh, *.m4, Makefile.in,
    aclocal.m4) -> take ours. These are wholesale deleted by develop in a
    later milestone (CMake-only); content is immaterial until then.
  - Everything else -> take theirs (develop's modern form), UNLESS the ours
    side contains a feature-specific VFD/build-registration token
    (ros3, hdfs, mirror, splitter, core_paged, H5_CREATE_VFD_DIR, VFD_LIST)
    that theirs lacks -- those are real functionality, not style, so keep
    ours.

Operates on files that already have live `<<<<<<< HEAD` markers (i.e. after
partial_resolve.py ran). Resolves ALL remaining hunks per-file (no None left)
and reports the decision for audit.
"""
import re
import sys

AUTOTOOLS_RE = re.compile(r"(\.am$|configure\.ac$|autogen\.sh$|\.m4$|Makefile\.in$)")
FEATURE_TOKEN_RE = re.compile(
    r"ros3|hdfs|mirror|splitter|core_paged|H5_CREATE_VFD_DIR|VFD_LIST", re.IGNORECASE
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
            if i >= n:
                return None
            i += 1
            theirs = []
            while i < n and not lines[i].startswith(">>>>>>> "):
                theirs.append(lines[i])
                i += 1
            if i >= n:
                return None
            i += 1
            segs.append(("hunk", ours, theirs))
        else:
            text = []
            while i < n and not lines[i].startswith("<<<<<<< HEAD"):
                text.append(lines[i])
                i += 1
            segs.append(("text", text))
    return segs


def decide(path, ours, theirs):
    ours_txt, theirs_txt = "\n".join(ours), "\n".join(theirs)
    if AUTOTOOLS_RE.search(path):
        return "ours", "autotools-immaterial"
    ours_has_feat = bool(FEATURE_TOKEN_RE.search(ours_txt))
    theirs_has_feat = bool(FEATURE_TOKEN_RE.search(theirs_txt))
    if ours_has_feat and not theirs_has_feat:
        return "ours", "feature-token-present"
    return "theirs", "modernization-default"


def process(path):
    with open(path, "r", errors="surrogateescape") as f:
        content = f.read()
    if "<<<<<<< HEAD" not in content:
        return None
    lines = content.split("\n")
    segs = parse_hunks(lines)
    if segs is None:
        print(f"UNPARSEABLE\t{path}")
        return None

    out_lines = []
    reasons = []
    for seg in segs:
        if seg[0] == "text":
            out_lines.extend(seg[1])
            continue
        _, ours, theirs = seg
        choice, reason = decide(path, ours, theirs)
        out_lines.extend(ours if choice == "ours" else theirs)
        reasons.append((choice, reason))
    new_content = "\n".join(out_lines)
    with open(path, "w", errors="surrogateescape") as f:
        f.write(new_content)
    return reasons


def main():
    paths = [l.strip() for l in sys.stdin if l.strip()]
    for p in paths:
        reasons = process(p)
        if reasons is not None:
            print(f"{p}\t{reasons}")


if __name__ == "__main__":
    main()
