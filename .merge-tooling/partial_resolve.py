#!/usr/bin/env python3
"""Apply resolve_hunks.py's per-hunk heuristic to every hunk in a file, but
unlike resolve_hunks.py, write the file even when some hunks are undecided --
undecided hunks are left as live conflict markers. Then print the undecided
hunks (with file path + surrounding context) for manual review.

Usage: partial_resolve.py < file_list.txt
"""
import re
import subprocess
import sys

VFD_TOKEN_RE = re.compile(r"vfd_swmr|swmr|H5PB|H5MV|tick_num|delayed.?write", re.IGNORECASE)


def norm(s):
    return re.sub(r"\s+", " ", s).strip()


def is_line_subset(sub_lines, super_lines):
    super_set = {norm(l) for l in super_lines}
    substantive = [norm(l) for l in sub_lines if len(norm(l)) >= 3]
    if not substantive:
        return False
    return all(l in super_set for l in substantive)


def git_show(ref, path):
    r = subprocess.run(["git", "show", f"{ref}:{path}"], capture_output=True, text=True)
    return r.stdout if r.returncode == 0 else None


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
            marker = lines[i]
            i += 1
            segs.append(("hunk", ours, theirs, marker))
        else:
            text = []
            while i < n and not lines[i].startswith("<<<<<<< HEAD"):
                text.append(lines[i])
                i += 1
            segs.append(("text", text))
    return segs


def decide(ours, theirs, dev_final_norm):
    ours_txt, theirs_txt = "\n".join(ours), "\n".join(theirs)
    ours_n, theirs_n = norm(ours_txt), norm(theirs_txt)
    if not theirs_n and ours_n:
        return "ours"
    if not ours_n and theirs_n and not VFD_TOKEN_RE.search(ours_txt):
        return "theirs"
    if dev_final_norm is not None and ours_n in dev_final_norm:
        return "ours"
    if (
        dev_final_norm is not None
        and theirs_n
        and theirs_n in dev_final_norm
        and ours_n not in dev_final_norm
        and not VFD_TOKEN_RE.search(ours_txt)
    ):
        return "theirs"
    if is_line_subset(theirs, ours):
        return "ours"
    if is_line_subset(ours, theirs) and not VFD_TOKEN_RE.search(ours_txt):
        return "theirs"
    return None


def process(path, develop_head_ref="upstream/develop"):
    with open(path, "r", errors="surrogateescape") as f:
        content = f.read()
    if "<<<<<<< HEAD" not in content:
        return 0, 0
    lines = content.split("\n")
    segs = parse_hunks(lines)
    if segs is None:
        print(f"UNPARSEABLE\t{path}")
        return 0, -1

    dev_final = git_show(develop_head_ref, path)
    dev_final_norm = norm(dev_final) if dev_final is not None else None

    out_lines = []
    n_resolved = 0
    n_manual = 0
    for seg in segs:
        if seg[0] == "text":
            out_lines.extend(seg[1])
            continue
        _, ours, theirs, marker = seg
        d = decide(ours, theirs, dev_final_norm)
        if d == "ours":
            out_lines.extend(ours)
            n_resolved += 1
        elif d == "theirs":
            out_lines.extend(theirs)
            n_resolved += 1
        else:
            out_lines.append("<<<<<<< HEAD")
            out_lines.extend(ours)
            out_lines.append("=======")
            out_lines.extend(theirs)
            out_lines.append(marker)
            n_manual += 1
    new_content = "\n".join(out_lines)
    with open(path, "w", errors="surrogateescape") as f:
        f.write(new_content)
    return n_resolved, n_manual


def main():
    paths = [l.strip() for l in sys.stdin if l.strip()]
    for p in paths:
        resolved, manual = process(p)
        print(f"{p}\tresolved={resolved}\tmanual={manual}")


if __name__ == "__main__":
    main()
