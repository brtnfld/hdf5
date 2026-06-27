#!/usr/bin/env python3
"""Per-hunk conflict resolver validated against current develop HEAD.

For each conflict hunk in a file: extract ours/theirs text. If ours text
(whitespace-normalized) is found verbatim in current upstream/develop HEAD's
version of the file, the hunk resolves to ours (feature already anticipated
this). If theirs matches and ours does not, AND ours has no vfd/swmr tokens,
resolves to theirs. Otherwise the hunk is left unresolved (manual).

A file is only auto-resolved if ALL of its hunks resolve unambiguously.
"""
import re
import subprocess
import sys

VFD_TOKEN_RE = re.compile(r"vfd_swmr|swmr|H5PB|H5MV|tick_num|delayed.?write", re.IGNORECASE)


def norm(s):
    return re.sub(r"\s+", " ", s).strip()


def is_line_subset(sub_lines, super_lines):
    """True if every substantive (len>=3 after norm) line of sub_lines appears
    verbatim (normalized) somewhere in super_lines. Trivial lines (braces,
    blank, short tokens) don't count -- avoids false positives on boilerplate."""
    super_set = {norm(l) for l in super_lines}
    substantive = [norm(l) for l in sub_lines if len(norm(l)) >= 3]
    if not substantive:
        return False
    return all(l in super_set for l in substantive)


def git_show(ref, path):
    r = subprocess.run(["git", "show", f"{ref}:{path}"], capture_output=True, text=True)
    return r.stdout if r.returncode == 0 else None


def parse_hunks(lines):
    """Line-based parser. Returns list of segments: either
    ('text', [lines]) or ('hunk', ours_lines, theirs_lines)."""
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
                return None  # malformed
            i += 1  # skip =======
            theirs = []
            while i < n and not lines[i].startswith(">>>>>>> "):
                theirs.append(lines[i])
                i += 1
            if i >= n:
                return None  # malformed
            i += 1  # skip >>>>>>> line
            segs.append(("hunk", ours, theirs))
        else:
            text = []
            while i < n and not lines[i].startswith("<<<<<<< HEAD"):
                text.append(lines[i])
                i += 1
            segs.append(("text", text))
    return segs


def resolve_file(path, develop_head_ref="upstream/develop"):
    try:
        with open(path, "r", errors="surrogateescape") as f:
            content = f.read()
    except Exception as e:
        return ("ERROR", str(e))

    if "<<<<<<< HEAD" not in content:
        return ("NO_CONFLICT", None)

    lines = content.split("\n")
    segs = parse_hunks(lines)
    if segs is None:
        return ("UNPARSEABLE", None)

    dev_final = git_show(develop_head_ref, path)
    dev_final_norm = norm(dev_final) if dev_final is not None else None

    decisions = []
    for seg in segs:
        if seg[0] != "hunk":
            continue
        _, ours, theirs = seg
        ours_txt, theirs_txt = "\n".join(ours), "\n".join(theirs)
        ours_n, theirs_n = norm(ours_txt), norm(theirs_txt)
        if not theirs_n and ours_n:
            # develop has nothing here at all (pure one-sided addition) -- no
            # information loss either way; keep the addition.
            decisions.append("ours")
        elif not ours_n and theirs_n and not VFD_TOKEN_RE.search(ours_txt):
            decisions.append("theirs")
        elif dev_final_norm is not None and ours_n in dev_final_norm:
            decisions.append("ours")
        elif (
            dev_final_norm is not None
            and theirs_n
            and theirs_n in dev_final_norm
            and ours_n not in dev_final_norm
            and not VFD_TOKEN_RE.search(ours_txt)
        ):
            decisions.append("theirs")
        elif is_line_subset(theirs, ours):
            # every substantive line of theirs already appears in ours --
            # ours is a strict superset, safe regardless of vfd content.
            decisions.append("ours")
        elif is_line_subset(ours, theirs) and not VFD_TOKEN_RE.search(ours_txt):
            decisions.append("theirs")
        else:
            decisions.append(None)

    if not decisions or any(d is None for d in decisions):
        return ("MANUAL", decisions)

    out_lines = []
    di = 0
    for seg in segs:
        if seg[0] == "text":
            out_lines.extend(seg[1])
        else:
            choice = decisions[di]
            di += 1
            out_lines.extend(seg[1] if choice == "ours" else seg[2])
    new_content = "\n".join(out_lines)

    with open(path, "w", errors="surrogateescape") as f:
        f.write(new_content)
    return ("RESOLVED", decisions)


def main():
    paths = [l.strip() for l in sys.stdin if l.strip()]
    summary = {}
    for p in paths:
        status, detail = resolve_file(p)
        summary.setdefault(status, []).append(p)
        print(f"{status}\t{p}\t{detail}")
    sys.stderr.write("\n=== SUMMARY ===\n")
    for k, v in summary.items():
        sys.stderr.write(f"{k}: {len(v)}\n")


if __name__ == "__main__":
    main()
