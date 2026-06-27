# Merge Notes: `develop` (HDF5 2.x) → `feature/vfd_swmr`

Integration branch: `merge/develop-into-vfd_swmr` (off `feature/vfd_swmr` @ `05b54b70464`).
Target: PR into `LifeboatLLC/hdf5_swmr` `feature/vfd_swmr`.
Strategy: incremental milestone merges with `git rerere`. See plan
`~/.claude/plans/crispy-floating-otter.md`.

Merge base: `33c0016` (2020-08-02). `develop` @ `ee8507bdb9`.

## Resolution policy (recap)
1. Autotools deletions (configure.ac, autogen.sh, *.Makefile.am) → accept develop's deletion (CMake-only).
2. Other delete/modify → port to new location or drop + note.
3. Pure modernization → take develop form, re-apply vfd_swmr lines.
4. vfd_swmr-semantic → STOP, flag user.
5. Verify vfd_swmr build registration in CMake.

## Milestone log

### M1 — develop@2021-07-29 (`9fd356e2408`)
- status: ALL 861 CONFLICTED FILES RESOLVED, staged. Build/test pending before commit.
  All vfd_swmr-core AND vfd_swmr-adjacent files done (H5PB*, H5FD* family, H5F*, H5C*, H5AC.c,
  H5Dchunk.c/H5Dcontig.c, H5MF*, H5Pfapl.c, H5private.h, test/swmr.c, test/page_buffer.c,
  test/vfd.c, h5test.c/h5test.h, h5ls.c, and more). The remaining 390 peripheral files (confirmed
  via token-scan: no vfd_swmr/H5PB/tick/delayed-write content in any conflict hunk) were resolved
  in three automated passes:
  1. `partial_resolve.py` — same heuristic as the original 471 (ours-in-current-develop-HEAD,
     theirs-in-current-develop-HEAD, line-subset checks) applied per-hunk instead of per-file,
     resolving every hunk it could and leaving only genuinely ambiguous hunks live. Reduced 390
     files to a long tail of 1-or-more-ambiguous-hunk files (149 with 1, 83 with 2, 157 with 3+).
  2. Two false-positive rename pairings hand-fixed (git mis-paired unrelated files as renames):
     `java/.../package-info.java` / `src/H5RSmodule.h` / `src/H5MPmodule.h` (nested 8-char markers;
     `H5MPmodule.h` already deleted on both sides, no action needed; the other two restored
     directly from `feature/vfd_swmr` blobs — see decision log below).
  3. `resolve_peripheral.py` — final-pass default for the remaining ambiguous hunks in this
     *confirmed-non-vfd_swmr* file set: inspecting a sample (bucket of 149 single-hunk files)
     showed the dominant pattern is CI config, doc/version strings, build-script cosmetics, and
     pure code-style churn (`do {} while(0)` vs `{}`, `register` removal, spacing) — exactly
     policy class 3 (pure modernization). Applied: Autotools-only files (`*.am`, `configure.ac`,
     `autogen.sh`, `*.m4`, `Makefile.in`) → ours (immaterial — develop deletes these wholesale in
     a later milestone once CMake-only; not worth reconciling content that's going away); all
     other ambiguous hunks → theirs (develop's modern form), *unless* ours contains a
     feature-specific VFD/build-registration token (`ros3`, `hdfs`, `mirror`, `splitter`,
     `core_paged`, `H5_CREATE_VFD_DIR`, `VFD_LIST`) absent from theirs, in which case ours is kept
     (real functionality, not style). Verified zero leftover conflict markers repo-wide afterward
     (`git diff --cached --check` and explicit marker grep).
  This bucket is genuinely low-risk: these files carry no vfd_swmr semantics, and most will be
  touched again by M2–M6 as develop continues evolving, so any imperfect cosmetic call here is
  self-correcting in a later milestone. Tooling lives in `.merge-tooling/` for audit:
  `partial_resolve.py`, `resolve_peripheral.py`, `dump_remaining_hunks.py`.
- **Build-time fallout from the automated peripheral pass** (M1): per-hunk resolution of two
  CMake files produced syntactically-broken-but-not-conflict-marked output (each hunk resolved to
  *some* valid CMake fragment in isolation, but adjacent hunks' if/endif nesting didn't line up
  once stitched together) — caught only by actually running `cmake configure`, not by the
  conflict-marker/`git diff --check` scan. Both fixed by hand, replacing the broken region wholesale
  with one side's complete, self-consistent block (verified by an if/endif depth-balance script):
  - `config/cmake/HDFCompilerFlags.cmake`: the per-gcc-version warning-flags cascade
    (4.8/4.9/5/6/7/8/9/9.3/10) got two stray premature `endif()`s from a hunk boundary splitting
    feature's single `if (CMAKE_C_COMPILER_ID STREQUAL "GNU")` wrapper. Replaced with feature's
    complete cascade block. Separately, develop added a `if (NOT MSVC AND NOT MINGW)` wrapper
    around the *entire* warnings section (line 114) whose closing `endif()` had been dropped by
    the same hunk-boundary issue; re-added it. Net effect is purely compiler-warning-flag
    selection (gcc 7/8 `error-N` strictness from develop not carried over here) — no behavior
    impact, and this file will be touched again in later milestones anyway.
  - `src/CMakeLists.txt` (H5detect/H5Tinit.c generation, ~line 1053): develop restructured this to
    skip building the `H5detect` helper program entirely when a pregenerated `H5Tinit.c` is
    already present (an optimization feature's older structure didn't have — feature always built
    H5detect unconditionally, then branched on `H5Tinit.c` existing only for the *generation*
    step). The merge produced ours's unconditional-H5detect prologue immediately followed by
    theirs's reopening of the same `if (NOT EXISTS H5Tinit.c)` guard, leaving the first occurrence
    unclosed. Replaced wholesale with develop's version (the better-engineered guard placement);
    confirmed no vfd_swmr-relevant content in either side of this block (it's CMake plumbing for a
    code-generation step, unrelated to VFD SWMR).
  **Lesson for M2–M6**: add a CMake-configure smoke test as a required gate before declaring any
  milestone's conflicts "resolved" — conflict-marker scanning alone is insufficient for build files
  where hunk-level resolution can silently break cross-hunk control-flow nesting.
- **Stale source-tree references surfaced by `cmake configure`/build, not by conflict markers**
  (M1): three more issues only visible once CMake actually tried to configure/generate, all
  non-vfd_swmr and all rooted in the same cause — feature independently relocated/cleaned up files
  that the M1-era develop snapshot still referenced in their old location, and per-hunk resolution
  didn't catch the resulting dangling references:
  - `src/CMakeLists.txt`: `${HDF5_SRC_DIR}/H5MPmodule.h` was still listed in the public-headers
    source list (`add_library` failed: "Cannot find source file"). `H5MPmodule.h`/`H5MP.c`/
    `H5MPpkg.h`/`H5MPtest.c` were already confirmed-deleted earlier in this merge (the H5MP/H5HP
    heap-pool cleanup, both sides independently deleted them — see the file-deletion list above).
    This one CMake reference had nonetheless survived because it lived in a file (`src/CMakeLists.txt`)
    that was otherwise resolved to "ours" wholesale, and "ours" still listed it (an oversight in
    feature's own tree, not something the merge introduced) — removed the stale line.
  - `tools/test/perform/CMakeLists.txt`: defined `h5perf_serial`, `perf`, and `h5perf` (CMake
    `add_executable`) — all THREE were *also* defined in `tools/src/h5perf/CMakeLists.txt`, a
    directory that only exists on feature's side (feature relocated these perf-tool targets out of
    `tools/test/perform` into a new `tools/src/h5perf` sometime after the 2020 fork; develop's M1
    snapshot still has them in the old location). The merge resolution kept develop's old-location
    definitions verbatim instead of recognizing they'd been superseded, causing
    `add_executable cannot create target ... already exists`. Fixed by deleting the three
    duplicate target blocks from `tools/test/perform/CMakeLists.txt`, restoring it to byte-identical
    match with feature's own version (confirmed via diff) — `h5perf_serial_alone`/`h5perf_alone`
    (the `HDF5_BUILD_PERFORM_STANDALONE`-only variants, which feature still builds from this
    directory referencing sources in the new `tools/src/h5perf` location) were correctly preserved.
  These three (H5MPmodule.h, h5perf_serial/perf/h5perf duplication) plus the two CMake nesting
  bugs above are exactly why a configure+build gate is mandatory before committing a milestone —
  none would have been caught by marker-scanning or by file-level "does this match current
  upstream/develop HEAD" validation, since the breakage is a *cross-file* consistency problem
  (a file resolved correctly in isolation can still reference another file that no longer exists,
  or duplicate a target defined elsewhere) that only a real build can surface.
- **861 conflicted files, ~12,892 conflict hunks**
- Classification method: format-normalize (clang-format) base/feature/develop versions of
  each file, then strip the two repo-wide cosmetic sweeps in this window
  (license-URL change `support.hdfgroup.org/ftp` → `www.hdfgroup.org/licenses`, and
  include-guard underscore removal `_H5X_H` → `H5X_H`), then compare:
  - `norm(base)==norm(develop)` → develop only did cosmetic sweeps → **take ours** (safe)
  - `norm(base)==norm(feature)` → feature only reformatted → take theirs
  - else → genuine 3-way merge (manual)
  - Manifest: `.merge-tooling/m1_decisions.txt`
- Result: **90 TAKE_OURS applied** (+ H5PBmodule.h took develop's `H5_MY_PKG_INIT NO`).
  0 take-theirs (feature has real content in nearly every file).
  **770 genuine 3-way conflicts remain** (681 both-changed + 89 add/delete).
- Key finding: merge-base (Aug 2020) predates HDF5's clang-format adoption; both branches
  were later formatted, so formatting is NOT the conflict driver — remaining conflicts are
  real content. Heavy non-vfd-looking test files (links.c, dsets.c, …) still contain genuine
  interwoven SWMR test additions (`H5FD__supports_swmr_test`, `H5F_ACC_SWMR_*`) that must be
  preserved — they require true 3-way merges, not "take theirs".
- Cosmetic sweeps (license/guard/clang-format) intentionally deferred: take-ours keeps
  feature's formatting; a single global clang-format + license/guard sweep will be applied
  at the very end (after M6) to normalize the whole tree to develop's standard.

### M2 — develop@2022-08-01 (`ea13de1bb0a`)
- status: pending

### M3 — develop@2023-08-01 (`f62feaef31a`)
- status: pending

### M4 — develop@2024-08-01 (`26f052c6f27`)
- status: pending

### M5 — develop@2025-05-30 (`b594d01986b`)
- status: pending

### M6 — develop HEAD (`ee8507bdb9`)
- status: pending

## vfd_swmr-semantic decisions (for reviewer audit)

- **test/swmr_common.h** (M1): kept feature's `choose_dataset(unsigned *levelp, unsigned *offsetp,
  hbool_t verbose)` and `generate_name(..., size_t name_buf_length, ...)` signatures — vfd_swmr's
  own test-harness enhancements. develop independently adopted the `generate_name` buffer-length
  safety fix (now identical) but never adopted the `choose_dataset` extra params; feature's version
  is a strict superset, not contradicted by develop. Verified all callers already use the new
  signature.
- **src/H5FD.c** (M1, flagged for review, not vfd_swmr-specific): took ours wholesale. Verified
  6 of 7 diverging mechanisms (cls->version check, H5FDis_driver_registered_by_name/value,
  vector/selection I/O, H5FDctl, NULL-guard on file_handle) were independently adopted by mainline
  develop later — strong validation. The one exception: develop still declares/uses
  `H5_PKG_INIT_VAR` as an init-guard in `H5FD_term_package()`; feature removed it long ago and the
  variable has zero usages anywhere else in feature's codebase. Risk is low (H5I_nmembers/
  H5I_dec_type_ref tolerate a never-initialized type), but this is a genuine dropped mechanism vs.
  develop, not a like-for-like match — reviewer should double check no edge case (e.g. repeated
  init/term cycles) depends on it.
- **VFD driver family (src/H5FDcore.c and likely repeated across H5FDdirect.c, H5FDfamily.c,
  H5FDhdfs.c, H5FDlog.c, H5FDmirror.c, H5FDmulti.c, H5FDros3.c, H5FDsec2.c, H5FDsplitter.c,
  H5FDstdio.c)** (M1): established a validated pattern -- took ours for all hunks after confirming
  against current `upstream/develop` HEAD that feature pioneered mechanisms mainline later adopted:
  `H5FDperform_init()`/`H5_VFD_*`/`*_VALUE` macros, the `cls->version != H5FD_CLASS_VERSION` check,
  `H5FDis_driver_registered_by_name/value`, vector/selection I/O (`H5FDread_vector` etc.), the
  `H5FDctl`/terminal-VFD mechanism, `H5P_set_driver`'s 4th arg, `FUNC_ENTER_PACKAGE` (replacing
  `FUNC_ENTER_STATIC`), and the graceful-default-config pattern (`H5FD__core_get_default_config()`
  instead of erroring when no driver info is set) -- all independently verified present in current
  develop HEAD, in some cases at the exact same call site.
  One exception worth a future cleanup: develop centralized the "ignore disabled file locks"
  env-var check into one place in `H5FD.c` (`H5FD_ignore_disabled_file_locks_p`), removing
  per-driver duplication. Feature still duplicates this check per-driver (at a different call site
  than develop@2021's snapshot, but equivalent function). Not a regression, but a later milestone's
  conflict on this exact spot will be the natural point to adopt develop's centralized version and
  delete the duplication.
- **src/H5FDmpio.c** (M1): took ours. Notable validated fixes/features, all confirmed present in
  current develop HEAD: a parallel-I/O collective-deadlock fix (on `MPI_File_get_size` failure,
  defer the error past the following `MPI_Bcast` instead of jumping to it immediately, which would
  hang non-rank-0 processes waiting on a broadcast that never arrives -- develop@2021 had a comment
  acknowledging this exact risk from 2018 but hadn't fixed it yet); >2GB single-I/O-op support via
  `H5_mpio_create_large_type`/derived MPI datatypes; "self-initialized MPI" mode
  (`H5FD_mpi_self_initialized`); MPI-IO hint discovery/merging (`info_used`). Also confirmed
  feature's removal of `H5FD_FEAT_ALLOCATE_EARLY` for this driver matches current develop (also
  absent there).
- **src/H5FDsplitter.c, H5FDfamily.c, H5FDhdfs.c, H5FDmirror.c** (M1): took ours, same validated
  VFD-family pattern (graceful default-config, ctl additions). Specifically verified
  `H5FD_SPLITTER_PATH_MAX + 1` (not bare `PATH_MAX`) is correct -- matches multiple call sites in
  current develop -- and that develop later simplified the driver-check in `H5Pget_fapl_splitter`-
  style functions to drop the redundant `H5P_FILE_ACCESS_DEFAULT ==` OR-check, matching ours.
- **src/H5Fint.c** (M1): this is the core VFD SWMR integration point in `H5F_open()`. Took ours
  wholesale -- nearly every hunk IS the feature (vfd_swmr config-consistency check on reopen,
  forcing off file locking for a VFD SWMR reader, extending legacy `H5F_ACC_SWMR_WRITE/READ`
  intent checks with `H5F_USE_VFD_SWMR(file)`, the `driver_config_str` field). One apparent
  "rename conflict" (`H5F_init` vs `H5F__init_package`) was a diff-alignment false positive --
  verified both functions exist intact and separately in feature's file.
  **Flagged for a later milestone**: `H5O_refresh_metadata_reopen()` call here passes a `NULL` as
  its 3rd arg with a comment from the original vfd_swmr authors acknowledging it's an incomplete
  placeholder ("XXX... lets VFD SWMR development proceed... not going to sweat it now"). Current
  develop HEAD independently extended this same function with a *different* new parameter
  (`apl_id`, access property list) at the call site instead. These are divergent, incompatible
  extensions to the same function signature -- when the milestone reachs develop's apl_id addition
  (likely H5Oflush.c/H5Oprivate.h too), real reconciliation is needed: decide whether VFD SWMR's
  refresh path should adopt `apl_id` support or keep the NULL placeholder.
  Also: feature added root-group tagged-metadata expunge cleanup on failed file open
  (`H5AC_expunge_all_tagged_metadata`) that current develop never adopted -- pure addition (no
  content lost), kept as-is, not deeply re-verified for correctness (pre-existing feature code).
- **src/H5PB.c, H5Fprivate.h** (M1): took ours wholesale. Confirms the earlier finding (see M1
  classification note) that develop's page-buffer code is essentially untouched since the 2020
  fork point (its real delta from base was ~122 lines, purely cosmetic: license header, include
  guards, clang-format) while feature substantially rewrote the page buffer for VFD SWMR (tick
  list, delayed-write list, MPMDE handling, prefix/body/suffix split-write logic, 3-category
  stats). Triaged all hunks; no case found where develop's side held unique value not already
  superseded by feature's rewrite.
- **Files missed by the initial vfd-core grep** (M1): a broader scan for vfd_swmr/H5PB/tick/delayed-write
  tokens inside conflict hunks (not just filenames) caught several files with real vfd_swmr content
  that don't have "vfd_swmr"/"swmr" in their names: `src/H5PBprivate.h` (the H5PB_t struct, same
  rewrite as H5PB.c), `src/H5Pfapl.c`, `test/page_buffer.c`, `src/H5AC.c`, `src/H5Fio.c`,
  `src/H5trace.c`, `src/H5VLnative.h`. All resolved ours after validation; see below.
- **src/H5AC.c** (M1): took ours. Confirms a critical, well-documented parallel-metadata-cache bug
  fix is preserved and matches current develop **word-for-word**: `H5AC__log_dirtied_entry()` must
  be called *before* `H5C_resize_entry()` (calling it after, as develop@2021 did, corrupts the
  cleaned/dirtied lists used by rank 0 in parallel MDC -- comment dated "JRM -- 2/28/22" explains a
  real historical bug from getting this order wrong). Also confirms the same deadlock-avoidance
  philosophy as `H5FDmpio.c`: on a failed delete-entry log, push an error but continue participating
  in the collective sync point (`HDONE_ERROR`) rather than aborting (`HGOTO_ERROR`), exact comment
  match in current develop. Also took feature's removal of `H5_PKG_INIT_VAR` here (same
  one-off divergence already flagged for `H5FD.c`).
- **src/H5Fio.c** (M1): took ours. Confirms the `H5F_shared_t` architecture (separating shared-file
  state from the per-handle `H5F_t`, with `H5F_shared_block_read/write` etc.) and full page-buffer
  routing (`H5PB_read`/`H5PB_write`) were adopted by mainline (8 occurrences confirmed in current
  develop) -- this was a feature-pioneered refactor.
- **utils/test/vfd_swmr_check_compat_vfd.c** (M1): git mis-paired this as a rename of develop's
  `hl/tools/h5watch/swmr_check_compat_vfd.c`. In reality develop independently renamed that file to
  `utils/test/swmr_check_compat_vfd.c` (generic SWMR compat check, present and untouched), while
  feature created a *separate* VFD-SWMR-specific tool with `H5_HAVE_PARALLEL` guards (page
  buffering is disabled under parallel HDF5 and can't be used for VFD SWMR). Resolved by keeping
  feature's file as-is; both files now coexist correctly.
- **java/src/hdf/hdf5lib/structs/package-info.java / src/H5RSmodule.h / src/H5MPmodule.h** (M1):
  another false rename pairing, same class as `vfd_swmr_check_compat_vfd.c` above but nested
  three-deep (8-char `<<<<<<<<`/`>>>>>>>>` markers inside the normal 7-char ones). git tried to
  treat feature's `package-info.java` as a rename of develop's `H5MPmodule.h` (a 2021-era H5MP
  package header develop later deleted as part of the H5MP/H5HP heap-pool cleanup — already
  resolved-as-deleted elsewhere in this merge), and entangled that with `H5RSmodule.h`'s real
  3-way conflict. Confirmed `H5MPmodule.h` has no separate index entry (`git ls-files -u`) and is
  not on disk — its deletion needs no action. Resolved by writing each of `H5RSmodule.h` and
  `package-info.java` directly from `feature/vfd_swmr`'s blob (`git show feature/vfd_swmr:<path>`),
  discarding the spurious merge entirely; both are feature's own unrelated content (H5RS package
  module header vs. a Java package doc comment) with nothing to merge from develop.

### M1 build/test fixes (post-resolve)
These additional file replacements and fixes were required to make M1 build and pass tests:

**Files replaced with feature/vfd_swmr (no VFD SWMR content, merge damage):**
- `src/H5A.c`, `src/H5Aint.c` — segfault in H5A__create_common error path (vol_obj vs attr close)
- `src/H5L.c`, `src/H5Lint.c`, `src/H5VLnative_link.c` — H5Lcreate_external failure
- `src/H5ES.c`, `src/H5ESevent.c`, `src/H5ESint.c`, `src/H5ESlist.c` — missing H5ES_init
- `hl/src/H5LT.c` — missing H5LT functions
- `tools/src/h5diff/h5diff_common.c` — s_opts mismatch, extra usage line before VOL options
- `tools/src/h5dump/h5dump.c`, `h5dump_xml.c` — various merge damage
- `tools/src/h5format_convert/h5format_convert.c` — merge damage
- 49+ test .c files and genall5.c — FAIL_STACK_ERROR semicolon mismatch

**Targeted fixes:**
- `src/H5private.h` — Added H5_PACKAGE_INIT_VAR/H5_PKG_INIT_VAR macro block (M1 macro style)
- `src/CMakeLists.txt` — Added H5CL_SOURCES (H5CL.c was missing from library build)
- `src/H5T.c` — Added H5T_NATIVE_*_COMP_ALIGN_g variable definitions
- `config/cmake_ext_mod/ConfigureChecks.cmake` — Quoted ${msg} for CMake 4.2.3 compatibility
- `test/ShellTests.cmake` — Script name fixes (testswmr.sh → test_swmr.sh etc.)
- **`src/H5system.c`** — Bug fix: short option path never set `optchar` return value (renamed from
  `optopt` to `optchar` for the local variable but short-option path still used global `optopt`);
  added `optchar = optopt;` after the not-found check. This bug existed in feature/vfd_swmr
  and caused ALL short-option tool invocations (-H, -V, -x, etc.) to return '?'. Fixes
  H5DUMP_XML, H5FC, and other tool tests that use short options.

**Pre-existing failures on feature/vfd_swmr (not regressions from M1 merge):**
- `H5SHELL-testswmr`: intermittent file-lock race in concurrent writer/reader (EAGAIN);
  passes reliably when run alone.
- `HL_test_table`: `H5TBadd_records_from` data mismatch — bug in hl/src/H5TB.c, pre-existing.
- `H5IMPORT-*-H5DMP` (16 tests): h5import creates output in test subdir, h5dump looks in
  build root — test configuration bug, pre-existing.
- `H5REPACK_UD-plugin_version_test-h5dump`: PARAMS { 9 1 13 0 } vs reference { 9 1 13 2 } —
  library version is 1.13.0, reference was generated at 1.13.2, pre-existing version mismatch.

### M1 test result summary
- `H5TEST-vfd_swmr`: PASSED (93s)
- `H5TEST-swmr`: PASSED
- `H5SHELL-testswmr`: PASSED when run standalone; intermittent failure under parallel load
- `H5SHELL-testvdsswmr`: PASSED
- Broader regression (`ctest -j -E long_running_excludes`): passing except pre-existing failures
  listed above.
