# VFD SWMR Incomplete Port — Implementation Plan

**Read this file first if you are picking up this branch cold.** It is the single source of
truth for what has been done, why, what bugs were found along the way, and what's left.

## Background

The VFD SWMR feature was developed on a separate feature branch (`feature/vfd_swmr`) that
diverged significantly from mainline HDF5. This branch (`feature/vfd-swmr-port`) is porting it
onto current `develop`. The port strategy (**Strategy B**) re-implements VFD SWMR semantics on
top of develop's existing **skip-list** page buffer rather than restoring the feature branch's
**hash-table** page-buffer rewrite. See [`docs/H5PB_index_design_analysis.md`](docs/H5PB_index_design_analysis.md)
for the full evidence base behind that decision (RFC rationale, literature, industry survey) —
short version: the hash table was a deliberate, RFC-documented choice and is technically sound,
but this port prioritizes minimizing divergence from mainline since it's destined to merge back
into `develop`.

The feature-branch reference commit is `05b54b7046`. Whenever this document says "ported from
the feature branch," that's the commit to `git show <commit>:<path>` against.

---

## Current Status

**Branch:** `feature/vfd-swmr-port`

| Phase | Description | Status |
|-------|-------------|--------|
| 0 | **Lifecycle integration** — wire VFD SWMR into file open/close/flush and the API EOT driver; ingest the FAPL config | **Done** (0-pre, 0a, 0b, 0c all complete and validated) |
| 1 | Wire `page_index` producer hooks in `H5C` metadata cache | **Done** (commit `0f4a936`, pre-existing this session) — confirmed reachable now that Phase 0 is wired |
| 2 | Reader tick-refresh: call consumer at end-of-tick | **Done** (already present from feature-branch merge) — confirmed reachable |
| 3 | Writer machinery: implement the four stub `H5PB_vfd_swmr__*` functions + write-path wiring on the **skip-list** page buffer | **Done and end-to-end validated** — `zoo` converges reliably; see "Session N+1" below |

**Validation state:** full regression suite (2726 tests, excluding the slow `H5SHELL-test_vfd_swmr`
shell test itself) passes clean, 0 failures. Every individual scenario in
`H5SHELL-test_vfd_swmr`'s default set — `generator`, `expand`, `shrink`, `expand_shrink`, `sparse`,
`vlstr_null`/`vlstr_oob`, `zoo`, `groups`, `groups_attrs` **(including `modify-vstr`, fixed in
"Session N+2")**, `groups_ops` — passes when run directly. Running the *entire* set back-to-back in
one script
invocation, though, does not yet reliably complete within `ctest`'s 1200-second default
`CTEST_TEST_TIMEOUT` (`few_big`/`many_small` remain unverified as a result) — partly because that
budget is tight for 13 scenarios worth of deliberate test pacing, and partly because of a real,
not-yet-root-caused timing difference from the reference under that same full-sequence load (zoo
itself is at performance parity in isolation, ~7.0s vs. the reference's ~6.8s). See "Session N+1"
and "Performance comparison against the reference" below.

**Along the way, this session found and fixed 4 real, pre-existing bugs in code that already
existed on `develop` before this branch started** (not introduced by the port). They were latent
because nothing had ever exercised those code paths. See "Pre-existing develop bugs found" below
— worth knowing about regardless of what you do with the rest of this port, since they could
plausibly be split out as standalone `develop` fixes.

**A follow-up session then did a deep, gdb-verified debugging pass on the `zoo` scenario itself
and found 5 more real bugs, all in this port's own Phase 3 code** (`src/H5PB.c`), plus root-caused
the remaining `zoo` failure to a test-timing issue, not a logic bug. See "Phase 3 bugs found in
the zoo end-to-end debugging pass" below — these are the most important bugs in this whole
document, since #1 and #2 below made Phase 3's publish path silently inert (writing empty/wrong
data to the shadow file) the entire time it existed, despite compiling and passing the regression
suite cleanly.

**A further session found 3 more real bugs** (stale reader `end_of_tick` causing a busy-poll, a
genuine page-buffer memory leak, and an untracked-write bypass), **then implemented multi-page
metadata entry (MPMDE) support** on the skip-list page buffer — previously flagged as out of
scope, now required because the reference implementation's ~6-second convergence depends on it.
The MPMDE write path is implemented and confirmed (via tracing) to correctly publish the
large writes it was built for, but `zoo` still doesn't converge reliably: convergence *does*
happen (confirmed over longer, 30-second test windows) but is much slower and more variable than
the reference's ~6 seconds, confirming and restoring the original "timing-budget mismatch" theory
rather than a hard correctness bug. See "Bugs found and MPMDE support added in this session" below.

**A follow-up session root-caused and fixed the `zoo` blocker for real** (a missing early
`vfd_swmr_reader` flag set plus a missing superblock VFD-SWMR `refresh` callback — both present in
the reference, both absent from the port — meant the reader's EOA never stayed in sync with the
writer's growing file). With `zoo` finally converging, running the *entire* `test_vfd_swmr.sh`
script for the first time (previously pointless, since `zoo` always hung first) surfaced **8 more
real, pre-existing bugs**, all now found and fixed except one: a deeper VL-string-attribute /
global-heap consistency issue in the `groups_attrs` "modify-vstr" scenario (the acute crash from it
is fixed; the underlying data-consistency bug is not). See "Session N+1" below for full detail —
this is the most consequential debugging pass in this document, since it explains essentially every
crash/hang symptom observed by users across every prior session.

**A follow-up session root-caused and fixed the VL-string-attribute/global-heap consistency bug
above** (the top "Next steps" item) — the real cause was that `src/H5Fio.c` remapped
`H5FD_MEM_GHEAP` to `H5FD_MEM_DRAW` *unconditionally*, so global heap objects (which a VL-string
attribute references) were never published through the VFD SWMR tick/shadow-index mechanism at
all; the reference only does this remap for non-VFD-SWMR files. Fixing that exposed a second, real,
independent bug in the MPMDE growth path (`H5PB__write_mpmde()`) that had never been exercised
before because nothing had ever grown an already-tracked entry mid-tick. Both are now fixed and
verified: `modify-vstr` passes 5/5 clean runs, the full non-shell-test regression suite is
2726/2726, and every individually-tested `H5SHELL-test_vfd_swmr` scenario remains clean. See
"Session N+2" below for full detail.

---

## Phase 0 — Lifecycle integration (Done)

### The problem this phase solved
At the start of this session, VFD SWMR was **entirely dormant** on `develop`: no function
outside `H5Fvfd_swmr.c` called into it, and the FAPL config was never read into
`shared->vfd_swmr_config`. `H5Fcreate()` with a VFD SWMR config would silently succeed and do
nothing — no shadow file, no tick machinery, nothing. This was proven directly: a small ad hoc
test program run against a clean pre-Phase-0 build created a file but never created the shadow
metadata file; the identical program against the post-Phase-0 build worked correctly.

### 0-pre — FAPL config ingestion + reader-VFD push
**Files:** `src/H5Fint.c`, `src/H5Fprivate.h`

- Added `H5F_VFD_SWMR_CONFIG` / `H5F_SHARED_VFD_SWMR_CONFIG` detection macros to `H5Fprivate.h`
  (checks `vfd_swmr_config.version >= H5F__CURR_VFD_SWMR_CONFIG_VERSION`).
- In `H5F__new()` (new-`shared` branch): copy `vfd_swmr_config` from the FAPL into
  `shared->vfd_swmr_config`; initialize `shared->vfd_swmr_md_fd = -1` (critical — see bug #2
  below).
- In `H5F_open()`: reordered so `a_plist` is fetched, then the VFD SWMR config is read and
  validated (legacy-SWMR conflict check, RDWR-vs-writer-flag consistency), and — if opening as a
  reader — the read-only VFD SWMR VFD is pushed onto the FAPL stack via
  `H5P_push_vfd_swmr_reader_vfd_on_fapl()`, all **before** `H5FD_get_class(fapl_id)` is called.
  This ordering matters: pushing the reader VFD changes what `drvr` resolves to, which decides
  the tentative-open behavior just below it.
  - Also: readers get `use_file_locking = false` unconditionally (the writer already holds the
    file open read-write; a normal reader lock would conflict).
  - Pop of the reader VFD happens at the `done:` label, gated on whether it was pushed.

### 0a — Open path
**File:** `src/H5Fint.c`

Right after both `H5G_mkroot` branches (fresh-file and open-existing) close, added: VFD SWMR log
file setup (if configured), `H5F_vfd_swmr_init(file, file_create)` (once per shared file, gated
on `nrefs == 1`), and `H5F_vfd_swmr_insert_entry_eot(file)` to register on the EOT queue.

### 0b — Close/flush path
**File:** `src/H5Fint.c`, inside `H5F__dest()`

Two insertion points:
1. Between `H5AC_dest()` and `H5PB_dest()`: `H5F_vfd_swmr_writer_prep_for_flush_or_close()` if
   this is a VFD SWMR writer (must run before the page buffer is torn down).
2. After the `fcpl_id` dec-ref, before VOL cleanup: `H5F_vfd_swmr_close_or_flush()` (writer,
   gated on `vfd_swmr_md_fd >= 0`) and `H5F_vfd_swmr_remove_entry_eot()` (any VFD SWMR file).

### 0c — Automatic EOT driver (the highest-blast-radius change of the port)
**File:** `src/H5private.h`

Wires `VFD_SWMR_ENTER`/`VFD_SWMR_LEAVE` into `FUNC_ENTER_API`/`FUNC_LEAVE_API` — the macros used
by **every public API function in the library**. This is what makes end-of-tick processing
happen automatically as the writer makes ordinary API calls (needed for **any** writer↔reader
synchronization — single-process tests like `generator` don't need it, but anything with a
separate reader process does).

Important discovery: develop already had a comment —
`/* VFD SWMR compat: alias for FUNC_ENTER_API (EOT processing removed in M3) */` — confirming
this was **deliberately rolled back** during an earlier merge milestone, not simply missing.
Phase 0c restores it.

Implementation notes:
- `vfd_swmr_api_entries_g`, `eot_queue_g`, `H5F_vfd_swmr_process_eot_queue()` already existed in
  `H5Fvfd_swmr.c`/`H5Fprivate.h` — Phase 0c only needed to add the macros and call sites.
- **Struct-tag collision risk avoided:** `eot_queue_t`/`eot_queue_entry_t` (a `TAILQ_HEAD(...)`
  construct) were originally declared in `H5Fprivate.h`. `H5private.h` cannot include
  `H5Fprivate.h` (circular — `H5Fprivate.h` includes `H5private.h`), so the types had to be
  **relocated** (not duplicated) into `H5private.h` itself. Duplicating the `TAILQ_HEAD` macro in
  both headers would produce "redefinition of struct eot_queue" in any file including both
  (nearly every file in the library). `H5Fprivate.h` now just has a comment pointing at
  `H5private.h`.
- **Include-order requirement:** the new declarations had to be placed **before**
  `H5private.h`'s own internal `#include "H5CXprivate.h"` line, because that include transitively
  reaches back into `H5Fprivate.h` (via `H5CXprivate.h → H5ACprivate.h → H5Cprivate.h →
  H5Fprivate.h`), which references these types. Getting this wrong produces "unknown type name
  eot_queue_entry_t" errors in nearly every `.c` file in the library.
- `FUNC_ENTER_API_NO_EOT`/`FUNC_LEAVE_API_NO_EOT` were restored as their own **distinct** macro
  bodies (not aliases of `FUNC_ENTER_API`/`FUNC_LEAVE_API`) — `H5Fvfd_swmr_end_tick()` (the manual
  tick-trigger API) is the sole user, and it must not also trigger implicit EOT processing on
  entry/exit (that's its whole job to do manually).

**Validation:** full regression suite run before and after 0c: 2726/2727 both times, zero
regressions from touching every API call site in the library.

---

## Phase 1 & 2 — Already present, now confirmed reachable

These predate this session (Phase 1 is commit `0f4a936`; Phase 2 came from the original
feature-branch merge). They wire the **reader** side: `H5C_t.page_index[]` (a 4096-bucket hash
table keyed by page number) tracks cached metadata entries per page, and
`H5F_vfd_swmr_reader_end_of_tick()` diffs the old/new shadow index and calls
`H5C_evict_or_refresh_all_entries_in_page()` for every changed page. Both existed but were
**unreachable** until Phase 0 wired the lifecycle that actually invokes them. No further work
needed here; see the original write-up further down this file for exact line references if you
need to touch this code.

---

## Phase 3 — Writer machinery on the skip-list page buffer (implementation done)

### Design recap
The four writer functions in `src/H5PB.c` — `H5PB_vfd_swmr__update_index`,
`__release_tick_list`, `__release_delayed_writes`, `__set_tick` — were no-op stubs. Without them,
the writer's tick number would advance (once Phase 0c was done) but nothing would ever get
published into the shadow index, so a reader would never see real data. Phase 3 implements them
on develop's **skip-list** page buffer, per the Strategy B decision — no hash table added.

### `H5PB_t` / `H5PB_entry_t` fields (added earlier this session, before the Phase 0 pivot)
`src/H5PBprivate.h` — `H5PB_t` gained: `vfd_swmr`, `vfd_swmr_writer`, `cur_tick`, `max_delay`,
`dwl_len/dwl_size/dwl_head_ptr/dwl_tail_ptr` (delayed-write list), `tl_len/tl_size/tl_head_ptr/
tl_tail_ptr` (tick list). `H5PB_entry_t` already had everything needed (`is_mpmde`, `loaded`,
`modified_this_tick`, `delay_write_until`, `tl_next/tl_prev`) — nothing to add there.

### Wrapper macros (`src/H5PBpkg.h`)
Ported `H5PB__INSERT_IN_TL` / `REMOVE_FROM_TL` / `INSERT_IN_DWL` / `REMOVE_FROM_DWL` out of the
fenced `#if 0` dead-hash-table-macro block (which remains fenced — nothing in it is used), placed
right after the existing active `H5PB__TL_DLL_APPEND`/`REMOVE` primitives. Dropped all
stats-macro calls (`H5PB__UPDATE_*_STATS`) since the corresponding stats fields were deliberately
not added to `H5PB_t` (minimal field set, per the port decision). Converted `HDassert` → `assert`
to match modern develop convention.

### Write-path wiring (`src/H5PB.c`)
New static helper `H5PB__vfd_swmr_track_write()`, called from the 2 metadata-relevant "mark page
dirty" sites inside `H5PB_write()` (the raw-data-only multi-page branch is skipped — VFD SWMR
only tracks metadata):

1. If not already `modified_this_tick`: set it, insert on the tick list.
2. If the entry has pre-existing on-disk content (`entry_ptr->loaded`) and isn't already delayed:
   call `H5F_vfd_swmr_writer_delay_write()` to ask whether the write must wait `max_lag` ticks
   (to avoid a "message from the future" on a lagging reader). If delayed, move the entry off the
   LRU (not eligible for eviction while delayed) onto the delayed-write list.

The `loaded` field (existed on `H5PB_entry_t` already but was never populated by develop) is now
set at entry-creation time: `page_entry->loaded = (search_addr < eof)` — true only if the page
had genuine prior on-disk content. A page beyond the old EOF has no prior version any reader
could depend on, so it's always safe to write immediately, and correctly never gets delayed.

### The four functions (`src/H5PB.c`, ported from `05b54b7046`)
- **`__set_tick`** — trivial: assert `tick_num == cur_tick + 1`, update `cur_tick`.
- **`__update_index`** — walks the tick list; for each entry, finds or creates the corresponding
  shadow-index entry (`H5FD_vfd_swmr_pageno_to_mdf_idx_entry` / `H5F_vfd_swmr_enlarge_shadow_index`
  / `H5F_shadow_image_defer_free` — all pre-existing F/FD-layer functions, unchanged), then scans
  the index for entries *not* touched this tick to mark them clean/flushed if appropriate. The one
  real adaptation from the feature branch: its hash-table lookup (`H5PB__SEARCH_INDEX`, by page
  number) is replaced with a skip-list search by byte address
  (`H5SL_search(page_buf->slist_ptr, &page_addr)` where `page_addr = page_number * page_size`).
- **`__release_tick_list`** — drains the tick list, clearing `modified_this_tick`. Multi-page
  metadata entries (`is_mpmde`) are asserted never to appear — that feature isn't implemented in
  develop's page buffer at all (a separate, larger scope than this port), so the assert is a
  loud, safe guard rather than silent mishandling.
- **`__release_delayed_writes`** — drains the delayed-write list from the tail (sorted by
  decreasing `delay_write_until`) while entries have expired, returning them to the LRU.

### Real bugs found while wiring this in (see "Pre-existing develop bugs" below for full detail)
Two of the four pre-existing-bug fixes this session found were surfaced **specifically** by
Phase 3, because it was the first code ever to invoke the "active" (non-dead-block)
`H5PB__DLL_*`/`H5PB__TL_DLL_*` sanity-check macros:
- `HDassert` calls (undefined in develop) inside these macros.
- Missing semicolons after `HGOTO_ERROR(...)` inside these same macros — `HGOTO_ERROR` expands to
  `do {...} while(0)`, and without a trailing `;` at the call site, `do{...}while(0)` immediately
  followed by `}` is a hard syntax error ("expected ';' after do/while statement").

---

## Pre-existing `develop` bugs found and fixed this session

These are **not** VFD-SWMR-specific and are **not** things this port introduced — they were
already on `develop`, in code untouched by this branch, and simply never triggered because
nothing had ever exercised those exact paths before. Flagging them clearly in case you want to
consider upstreaming any of them as standalone `develop` fixes, independent of this port.

1. **Header-order landmine: `H5Fpkg.h` → `H5CXprivate.h` needs `H5Pprivate.h`'s typedef first.**
   A minimal file containing only `#define H5F_FRIEND` + `#include "H5Fpkg.h"` fails to compile
   on **pristine, unmodified `develop`** with "must use 'struct' tag to refer to type
   'H5P_libclass_t'". Root cause: `H5Fpkg.h → H5Fprivate.h → H5FDprivate.h → H5Pprivate.h →
   H5private.h → H5CXprivate.h`, and `H5CXprivate.h` needs the `H5P_libclass_t` typedef which
   `H5Pprivate.h` hasn't reached yet (its own `#include "H5private.h"` appears before its own
   typedef). Fixed in the 7 VFD SWMR test files that hit it, by including `H5private.h` first
   (the standard, already-followed convention in every other internal `.c` file, e.g.
   `H5Fsuper.c`). **Not fixed in the shared headers themselves** — only worked around at the call
   sites that happened to violate the "H5private.h first" convention.

2. **`H5FD_vfd_swmr_init()` (the VFD-class registration function) was never called from
   anywhere.** `H5P_push_vfd_swmr_reader_vfd_on_fapl()` (in `src/H5FDvfd_swmr.c`, pre-existing,
   unmodified by this port except this one fix) tries to use `H5FD_VFD_SWMR` as a driver ID, but
   that macro resolves to `H5FD_VFD_SWMR_id_g`, which is only ever assigned by
   `H5FD_vfd_swmr_init()` — and nothing called it. Confirmed independent of this port's other
   changes by reproducing the failure with a tiny standalone probe against a pristine baseline
   worktree. **Fix:** added the registration call at the top of
   `H5P_push_vfd_swmr_reader_vfd_on_fapl()` (`src/H5FDvfd_swmr.c`).

3. **`vfd_swmr_md_fd` sentinel bug — caused segfaults on every ordinary file close, not just VFD
   SWMR files.** `H5F__dest()`'s close-path guard is `(H5F_ACC_RDWR & H5F_INTENT(f)) &&
   f->shared->vfd_swmr_md_fd >= 0`. Since `H5FL_CALLOC` zero-initializes new structs and nothing
   set `vfd_swmr_md_fd` to a sentinel, it defaulted to `0` — a value that satisfies `>= 0` for
   *every* ordinary read-write file, not just VFD SWMR ones. This was introduced by this port's
   own Phase 0b work (not a pre-existing bug) but is included here because of its severity: it
   segfaulted the *entire* regression suite the first time 0b was tested. **Fix:** initialize
   `shared->vfd_swmr_md_fd = -1` in `H5F__new()`'s new-shared branch, unconditionally (matching
   the feature branch's own convention of setting this sentinel for every file, not just VFD SWMR
   ones).

4. **`HDassert` and missing-semicolon bugs in never-before-compiled "active" sanity-check
   macros in `H5PBpkg.h`.** See Phase 3 write-up above. These macros (`H5PB__DLL_PRE_INSERT_SC`,
   `H5PB__TL_DLL_PRE_INSERT_SC`, and siblings) are gated on `H5PB__DO_SANITY_CHECKS` which
   **defaults to `true`** — they were live by default, just never invoked by any code path until
   Phase 3's tick-list/delayed-write-list macros started calling the underlying
   `H5PB__DLL_APPEND`/`INSERT_BEFORE`/`REMOVE` primitives for the first time.

Bugs #1, #2, and #4 are all instances of the same pattern: **code that compiles/parses fine in
isolation but was never actually exercised**, because nothing on `develop` had a reason to call
it. Worth keeping in mind if you touch other long-dormant VFD-SWMR-adjacent code — expect more of
these.

---

## Phase 3 bugs found in the zoo end-to-end debugging pass

All in `src/H5PB.c`, all found by actually running the `zoo` writer+reader pair together and
following the failure chain (message-file sync → crash → clean failure → gdb) rather than relying
on the regression suite, which never exercises real reader visibility. Fixed in this order; each
fix exposed the next symptom.

1. **`page_entry->size` was never initialized — always 0.** `H5PB_entry_t.size` is a real,
   load-bearing field (`H5PB_vfd_swmr__update_index()` copies it into the shadow-index entry's
   `length`, which directly controls how many bytes `H5F_update_vfd_swmr_metadata_file()` writes
   to the shadow file — see `write(shared->vfd_swmr_md_fd, index[i].entry_ptr, index[i].length)`
   in `src/H5Fvfd_swmr.c`). With `size` always 0, the shadow file's page-0 entry got `length=0` (or
   whatever was written before another latent bug made it worse — see #2), so almost nothing of
   the page's real content was ever physically persisted. A reader's later attempt to read beyond
   the (near-zero) length hit real end-of-file on every `read(2)` call, and because the read loop
   in `H5FD__vfd_swmr_read()` (`src/H5FDvfd_swmr.c`) doesn't check for `read() == 0` inside its
   `while (size > 0)` loop, this was a genuine **infinite busy-spin**, not just wrong data — this
   is what was actually deadlocking the reader the entire time, not a synchronization race.
   **Fix:** set `page_entry->size = page_buf->page_size` at all three entry-creation sites
   (`H5PB_write()`'s main creation path, `H5PB_read()`'s creation path, `H5PB_add_new_page()`).
   Multi-page metadata entries aren't supported in this port (see Phase 3 write-up above), so every
   tracked entry is exactly one full page — `page_buf->page_size` is always correct.
2. **`entry->image_ptr` was never assigned — always `NULL`.** `H5PB_vfd_swmr__update_index()` did
   `ie_ptr->entry_ptr = entry->image_ptr;`, but `H5PB_entry_t.image_ptr` is a legacy field from the
   hash-table-based feature branch (the struct comment literally says `page_buf_ptr` is an "M3
   compat: alias for image_ptr") — the skip-list port never populated it. This meant the shadow
   file was being written from a null/garbage pointer. **Fix:** read `entry->page_buf_ptr` instead
   (the field the skip-list code actually populates).
3. **`H5PB__make_space()` NULL-dereferenced `page_buf->LRU_tail_ptr`.** Under VFD SWMR, entries
   awaiting a delayed write are deliberately pulled off the LRU (see `H5PB__vfd_swmr_track_write()`)
   even though they still count against the page buffer's size limits. If every resident page is
   currently protected (delayed or, after fix #5, on the tick list), the LRU can be legitimately
   empty while the page buffer is still "full" — and the function dereferenced the (possibly
   `NULL`) LRU tail without checking, a real segfault (confirmed via coredump,
   `H5PB__make_space` at `src/H5PB.c`). **Fix:** added a `NULL` check that returns `false`
   ("can't make space"), which is the function's own existing pattern for "no eviction candidate
   available" — the caller already has a bypass-to-VFD fallback for exactly this case.
4. **`H5PB__dest_cb()` double-unlinked delayed-write entries at file close, corrupting the LRU.**
   It unconditionally called `H5PB__REMOVE_LRU` on every entry in the main skip list when
   destroying the page buffer, but an entry on the delayed-write list has its `next`/`prev`
   pointers threaded onto the *DWL*, not the LRU (same fields, different list, depending on
   current membership) — running it through `H5PB__REMOVE_LRU` corrupted whichever list actually
   owned those pointers (confirmed via coredump inside `H5SL_destroy` → `H5PB__dest_cb`). **Fix:**
   skip the (already-done, by DWL membership) LRU unlink for entries with `delay_write_until != 0`;
   we're tearing down the whole page buffer immediately after anyway, so no cleanup is skipped.
5. **Tick-list entries weren't protected from LRU eviction, unlike delayed-write entries.** An
   entry that's `modified_this_tick` (on the tick list, awaiting the end-of-tick shadow-file
   publish) needs its image to survive until `H5PB_vfd_swmr__update_index()` reads it — exactly the
   same requirement as a delayed-write entry, but the original port only pulled *delayed* entries
   off the LRU. If the page buffer filled up mid-tick, `H5PB__make_space()` could legitimately
   evict-and-free a tick-list entry, leaving a use-after-free/dangling entry in the tick list
   (surfaced as a `"TL DLL pre insert SC failed"` sanity-check error once fixes #1–#2 let real data
   start flowing). **Fix:** `H5PB__vfd_swmr_track_write()` now pulls an entry off the LRU the
   moment it's first added to the tick list (matching the delayed-write pattern), and
   `H5PB_vfd_swmr__release_tick_list()` returns it to the LRU at end-of-tick unless it's *also* on
   the delayed-write list (in which case `H5PB_vfd_swmr__release_delayed_writes()` returns it
   later). All 7 call sites of the `H5PB__MOVE_TO_TOP_LRU` macro in `H5PB.c` were audited; the 5
   that can reach a metadata entry (vs. the 2 confirmed raw-data-only) now check
   `!entry->modified_this_tick` before moving — an entry can legitimately be touched a second time
   in the same tick, after which it's already off the LRU and re-running `MOVE_TO_TOP_LRU` would
   corrupt it again.

### The remaining zoo failure is a timing-budget mismatch, not a bug

After fixes #1–#5, the writer's own EOT machinery and the reader's page-eviction/refresh machinery
were both **verified correct** by direct inspection:
- Traced via `getenv("H5PB_DEBUG_TRACE")`-gated instrumentation (all since removed) that the writer
  publishes a genuinely new shadow-file offset for page 0 on every tick it's modified, and the
  reader's tick number advances in lockstep (1→11 confirmed in one run) with
  `H5C_evict_or_refresh_all_entries_in_page()` firing and correctly evicting every matching cache
  entry each time.
- Attached gdb directly to the reader process (launching it as gdb's own child avoids this
  sandbox's `ptrace_scope=1` restriction — `gdb -batch -x script --args vfd_swmr_zoo_reader ...`)
  and set a **conditional breakpoint** on `H5G__loc_find_cb` with `obj_loc != 0` (i.e., "only stop
  on a successful lookup"). It fired: the reader's `H5Gopen2("./A")` **does** eventually succeed,
  proving the underlying publish/refresh/re-decode path is correct end to end.

The problem is that convergence took on the order of 10s of seconds under observation, while the
writer's own patience budget in `notify_and_wait_for_reader()`
(`test/vfd_swmr_zoo_writer.c`) is only `(max_lag + 1) * tick_len = 8 * 0.4s ≈ 3.2 seconds`
(`max_lag=7`, `TICK_LEN=4` tenths-of-a-second, both `#define`d in that file) before it gives up and
fails with `recv failed`. The reference commit (`05b54b7046`) has the **identical** bounded wait
and the **identical** zero-backoff `while (!validate_zoo(...)) ;` retry loop on the reader side, so
the timing budget itself isn't a port regression.

**Update — this *is* a real port-side performance regression, confirmed empirically.** Built the
original `lifeboat/feature/vfd_swmr` branch (commit `05b54b7046`) from scratch with its native
autotools build (`autogen.sh` + `configure --disable-fortran --disable-cxx --disable-parallel
--disable-tools --disable-hl`, `clang`) in this exact sandboxed environment, then ran its own
`vfd_swmr_zoo_writer`/`vfd_swmr_zoo_reader` pair directly (same launch pattern: writer, wait for
`VFD_SWMR_WRITER_MESSAGE`, then reader). **It passed — writer and reader both exit 0 — in ~6
seconds, reproduced twice.** (The reader does log `"validate_zoo took too long to finish"` — a
soft, non-fatal diagnostic gated on a separate, shorter `msgival` threshold, not the pass/fail
gate — so the original test author already anticipated *some* slowness, just nowhere near enough
to blow the writer's ~3.2s patience budget.)

This rules out "this sandboxed environment is just slow" as the explanation — the exact same
environment runs the original hash-table-based implementation to completion well within budget.
Something in this port (most likely candidate: the skip-list page buffer under Strategy B, or
something in the reader-side refresh path exercised per-tick) does measurably more work, or
slower work, per tick than the original. Given the page counts involved are tiny (single digits),
a skip-list-vs-hash-table algorithmic difference alone (O(log n) vs O(1) on n≈10) shouldn't
account for a 5-10x+ slowdown — so the more likely culprit is some redundant or unnecessarily
expensive operation introduced somewhere in the port, not the search-structure choice itself.
**Not yet root-caused further than this** — next step would be timing instrumentation around each
of the 9 writer-side EOT steps and the reader-side refresh path, on both branches, to find exactly
where the extra latency is spent.

*(Aside, in case you hit the same wall: `pkill -f <pattern>` matches against the full command
line, including the invoking shell's own command text — `pkill -9 -f vfd_swmr_zoo` will kill the
shell running that exact command, not just the target processes. Use `pkill -x <comm-name>`
(exact match against the truncated process name, e.g. `vfd_swmr_zoo_wr`) instead when writing
scripts that target these binaries.)*

---

## Bugs found and MPMDE support added in this session

This session picked up the "timing-budget mismatch" theory above and, on closer inspection, found
it was incomplete: the reference (`05b54b7046`) converges in ~6s specifically because it has full
MPMDE support, which this port never had. Before implementing that, tracing the zoo run turned up
3 more real, independent bugs — all found by direct empirical measurement (RSS monitoring, call
counting, valgrind massif), not speculation.

### 3 bugs fixed before MPMDE work started

1. **Stale reader `end_of_tick` caused a busy-poll, not a "message from the future."**
   `H5F_vfd_swmr_reader_end_of_tick()` (`src/H5Fvfd_swmr.c`) only called
   `H5F__vfd_swmr_update_end_of_tick_and_tick_num()` *inside* the `if (tmp_tick_num !=
   shared->tick_num)` block — i.e., only on ticks where the tick number actually changed. When it
   didn't change, `shared->end_of_tick` was left stuck in the past, so the time-based gate in
   `H5F_vfd_swmr_process_eot_queue()` (`now >= head->end_of_tick`) was satisfied on *every*
   subsequent API call instead of roughly once per `tick_len` — turning every reader-side API call
   into a real disk read of the shadow-file header. Confirmed this bug is **also present in the
   reference branch** (not a port regression), but fixed here anyway since it's real. **Fix:**
   moved the `update_end_of_tick_and_tick_num()` call outside/after the `if` block so it runs
   unconditionally every call.
2. **Genuine memory leak: two EOA-check error paths in `H5PB_read()`/`H5PB_write()` leaked the
   just-allocated page buffer.** Both functions call `H5FL_FAC_MALLOC`/`CALLOC` for a new page
   image *before* checking whether the target address is within the file's EOA. On the "outside
   EOA" `HGOTO_ERROR` paths, the freshly allocated buffer was never freed before jumping to
   `done:`. Verified via valgrind massif (`--tool=massif --pages-as-heap=yes`, leak gone after
   fix) and direct RSS monitoring (flat across runs up to 647 seconds after the fix; before the
   fix, RSS grew from 251MB to 39GB in 15 seconds under the zoo test's tight retry loop — this,
   not a race, was the actual cause of processes dying to the kernel OOM killer during earlier
   debugging). **Fix:** added the missing `H5FL_FAC_FREE` calls on both error paths in both
   functions.
3. **Untracked-write bypass silently dropped metadata writes from VFD-SWMR publication.** In
   `H5PB_write()`'s "not found, make space" branch, when `H5PB__make_space()` returns "can't make
   space" (every resident page protected by the current tick — expected with a deliberately tiny,
   1-page page buffer), the original code unconditionally fell back to a direct, untracked
   `H5FD_write()` — bypassing `H5PB__vfd_swmr_track_write()` entirely, so the write was never
   published to the shadow index. Confirmed via call-tracing `H5PB__vfd_swmr_track_write()`:
   tracked pages went from `{0}` only to `{0,1,2,...}` after the fix. **Fix:** for VFD-SWMR
   metadata, let the page buffer temporarily exceed `max_size` instead of bypassing (it shrinks
   back down once the tick ends); the bypass is still taken for raw data and for a non-VFD-SWMR
   page buffer.

### MPMDE (multi-page metadata entry) support added

Root cause of the *actual* remaining zoo blocker at the time: two of the zoo test's object
selectors (a dense new-style group — v2 B-tree + fractal heap — and an old-style group with 300
links — v1 B-tree + local heap) write metadata larger than one page (`page_size=4096` in the zoo
test's own FAPL config). `H5PB_write()`'s top-level bypass (`size >= page_buf->page_size`) routed
every such write straight to `H5F__accum_write()`, which never calls
`H5PB__vfd_swmr_track_write()` — so these writes were never published to the shadow index at all,
confirmed via tracing exact addr/size pairs (`LHEAP` 5632 bytes, `OHDR` 16656 bytes twice).

Implemented on the skip-list page buffer, modeled on the reference's `H5PB__write_meta()` (hash
table) but adapted — see `src/H5PB.c`:
- **New field** `mpmde_count` on `H5PB_t` (`src/H5PBprivate.h`) — mpmde entries must not be
  counted in `meta_count`, since `H5PB__make_space()`'s eviction-threshold math assumes every
  counted metadata entry is exactly one page.
- **New static function `H5PB__write_mpmde()`** — intercepted in `H5PB_write()` before the
  generic bypass, whenever `page_buf->vfd_swmr_writer && type != H5FD_MEM_DRAW && size >=
  page_buf->page_size`. Creates (or grows) an entry with a variable-sized `H5MM_malloc`/
  `H5MM_xfree` image (not the page buffer's fixed-size, one-page factory allocator), inserted into
  the skip list **but never into the LRU** — matching the reference's rule that mpmde entries are
  pinned by tick-list membership only, never eviction candidates, and matching the reference's
  explicit rationale ("VFD SWMR ignores the limits on page buffer size for tracked metadata") by
  skipping `H5PB__make_space()` entirely for these writes.
- **Every place that assumed "every entry was inserted into the LRU at creation"** needed a
  `is_mpmde` guard, since mpmde entries never are:
  `H5PB__vfd_swmr_track_write()`, `H5PB_vfd_swmr__release_tick_list()`,
  `H5PB_vfd_swmr__release_delayed_writes()` (the `assert(!entry_ptr->is_mpmde)` "not yet
  supported" guards in the latter two are now gone — this is the feature they were guarding
  against), `H5PB__dest_cb()` (file teardown) and `H5PB_remove_entry()` (both also needed
  `H5MM_xfree` instead of `H5FL_FAC_FREE` for the image, since it was never allocated from the
  page factory).
- **`H5PB__write_entry()`** (the actual `H5FD_write()` call for flush/eviction) hardcoded
  `page_size = f_sh->page_buf->page_size`; changed to `page_entry->size`, otherwise a flush of a
  resident mpmde entry would silently truncate it to one page on disk.
- No changes needed to `H5PB_vfd_swmr__update_index()` (already generic on `entry->size`, not a
  hardcoded page size — this part was already correct, just never fed a real mpmde entry), the
  reader-side VFD redirect in `src/H5FDvfd_swmr.c` (already generic on `entry->length`), or
  `H5PB_read()` (bypassing the page buffer for a large *read* only affects caching, not
  correctness — unlike bypassing a *write*, which is what broke publication).

**Verified via tracing that this is correctly exercised**: with `H5PB_PERF_TRACE=1` (now removed;
see below), the writer's `track_write` fires for all three previously-dropped addresses (`LHEAP`
at 40960/5632 bytes, `OHDR` at 8519680 and 8540160/16656 bytes each), and the writer's own local
`tend_zoo` validation — which exercises the same read-back path the writer itself would use —
sails through every selector (reaching `i=13`, the natural end of the selector list) instead of
getting stuck around `i=1`–`2` as before. This is real progress and a real, independent fix, not
speculative.

### The actual remaining blocker: convergence is real, but too slow and too variable

Initial testing in this session (15-second samples) showed the reader failing to see even "A" —
the very first, trivial thing the writer creates (an empty new-style group linked directly under
the root group; selector 0, `test/genall5.c`'s `ns_grp_0`/`vrfy_ns_grp_0`) — every single time,
with the `-e` error stack showing a clean, non-corrupt `H5G__loc_find_cb(): object 'A' doesn't
exist`. That evidence briefly (see the superseded write-up in git history) supported a theory that
the reader could *never* see root-group updates at all, independent of mpmde. **Longer testing
disproved that theory**: given a 30-second window, "A" (`i=0`) reliably does become visible, and
in one run the whole zoo scenario (both selector 0 and selector 1, `i=1`) succeeded, with both
writer and reader exiting 0. A second, otherwise-identical 30-second run made it only to `i=2`
before timing out. So this is not a permanent "never" bug — it's a **real, substantial
performance/reliability gap**: this port takes anywhere from ~15 to 30+ seconds (with high
run-to-run variance) for even the *first* few selectors to become visible, while the reference
(`05b54b7046`) reliably converges on the *entire* scenario in ~6 seconds.

Things ruled out by direct tracing this session (each confirmed working correctly, so none of
these explain the slowness):
- **Write-side publication**: root-adjacent addresses (superblock, root group's own small object
  header, etc., all within HDF5 page 0) get written and tracked by `H5PB__vfd_swmr_track_write()`
  repeatedly throughout a run, with `page_buf->vfd_swmr_writer` true the whole time.
- **Shadow-index re-allocation**: `H5F_update_vfd_swmr_metadata_file()` always allocates a fresh
  `md_file_page_offset` for every entry touched in a tick (never reuses the old location in place),
  so the reader's location-based diff can't miss an in-place update.
- **Reader-side change detection**: the reader's page-diff loop in
  `H5F_vfd_swmr_reader_end_of_tick()` correctly flags HDF5 page 0 as changed on essentially every
  tick that touches it.
- **Reader-side eviction**: the page-0 entries reaching `H5C_evict_or_refresh_all_entries_in_page()`
  (including the root group's own object header) are all `is_pinned=0` — they take the plain evict
  path (`H5C__flush_single_entry` with invalidate), not the trickier pinned/refresh path, so they
  should be cleanly reloaded on next access.
- **The reader-side busy-poll from bug #1 above**: confirmed fixed — tick numbers advance roughly
  once every `tick_len` (0.4s) throughout, not on every call.
- **Socket handshake/synchronization**: the reader only starts validating after the writer's
  socket notification that creation finished, and that handshake completes almost immediately in
  every observed run.

Given all of the above check out individually, the slowness is most likely a matter of *degree*
somewhere in this same pipeline rather than a single missing piece. This matches — and restores —
the *original*, pre-this-session "timing-budget mismatch" theory documented further above, which
this session's initial 15-second sampling had prematurely appeared to supersede.

**Follow-up: a direct tick-by-tick timing comparison against the built reference
(`05b54b7046`) found the mechanism precisely, even though it did not point at a discrete
correctness bug in either branch's C code.**

Built the reference via autotools in the same environment (see "How to build and test" below for
the worktree/build commands) and instrumented both branches' `H5F_vfd_swmr_writer_end_of_tick()`
(all 9 steps) and `H5F_vfd_swmr_reader_end_of_tick()` (per-tick "real work" duration) with the same
`clock_gettime(CLOCK_MONOTONIC, ...)`-based markers, gated on `getenv("H5PB_TIMEIT")` (since
removed from both trees after use). Findings:

- **The writer's own per-tick step timing is essentially identical** between port and reference —
  sub-millisecond for every one of the 9 steps, same shape (steps 2 and 6 grow slightly as the
  tick list accumulates more entries), confirmed via a standalone (`-N`, no reader) writer run on
  both branches.
- **The reader's own per-tick "real work" (the time spent inside the `tmp_tick_num !=
  shared->tick_num` branch of `H5F_vfd_swmr_reader_end_of_tick()`) is also tiny and comparable on
  both branches** (sub-millisecond to low tens of milliseconds, scaling with `nchanges`), and tick
  cadence on both branches is a rock-solid ~0.4s (`tick_len`) per tick when the writer is actually
  ticking.
- **The actual mechanism**: `notify_and_wait_for_reader()` (`test/vfd_swmr_zoo_writer.c`, both
  branches, essentially identical code) gives the reader a **fixed**, non-adaptive window after
  `create_zoo()` finishes — exactly `max_lag + 1` (8, given `max_lag=7`) writer-driven API calls,
  one every `tick_len` (~3.2s total) — and then calls a plain, **untimed** `recv()` to wait for the
  reader's completion signal. Once that loop ends, the writer makes **no further API calls and
  therefore produces no further ticks** until the reader's ack arrives. Direct measurement
  confirmed this on the port: in a run that later timed out, the writer completed exactly 12
  end-of-tick cycles total (matching `create_zoo`'s few ticks plus the 8-tick wait loop) and then
  went completely silent — zero further ticks — for the rest of the (30-second) run, while the
  reader's own tick trace stopped in lockstep at the same point. This is a **hard deadline, not a
  patience heuristic that gives the reader "more time if it's close"**: if reader validation
  hasn't fully succeeded by the time the fixed window elapses, nothing further will ever be
  published, and both processes hang until an external timeout kills them.
- This fully explains the run-to-run variance observed earlier: whether the whole scenario
  succeeds is a race between the reader's retry loop reaching full success (`i` reaching 13, the
  natural end) and this fixed ~3.2-second-plus-a-few-ticks deadline. The reference wins that race
  reliably; the port sometimes does, sometimes doesn't, because — even though the *traced*
  per-tick mechanics are equally fast on both branches — the port's reader retry loop apparently
  needs more wall-clock time (more successful attempts, or slower individual attempts) to fully
  validate each selector than the reference's does, for reasons **outside** everything traced so
  far (write publish, shadow-index allocation, change detection, eviction, tick cadence, per-tick
  work duration — all confirmed equally fast). The remaining gap is most likely in the actual
  metadata decode/re-load cost inside a single `H5Gopen2`-triggered attempt, or in per-API-call
  overhead on entry that isn't tied to an actual tick change (not yet measured) — something a
  CPU-level profile (e.g. `perf record` during the reader's busy retry loop, port vs. reference)
  would locate directly, rather than more `clock_gettime` bracketing of the same already-fast
  steps.

**Follow-up 2: CPU-profiled the reader's retry loop with `perf record --call-graph dwarf` (port
vs. reference) and found a real, reproducible metadata corruption bug — not just a slowdown.**

The profile itself first pointed at a red herring worth recording: `__printf_buffer` /
`__memmove_avx512_unaligned_erms` dominate the port's reader profile (23%+ combined), traced to
`H5E_printf_stack` being called deep inside `H5PB_read()` on a speculative lookaside read that
legitimately misses and is internally caught (never surfaces in the final error stack, which is
always the same clean 10-frame "object 'X' doesn't exist" chain). The reference shows the *same*
pattern, just proportionally tiny (~4%) because it needs far fewer retries overall. **This is a
symptom/amplifier of needing more retries, not the cause of needing more retries** — it explains
why each retry is expensive, not why more retries are needed.

Directly instrumenting the retry loop itself (a restart counter plus "highest selector index ever
reached" counter, gated on `getenv`, since removed) showed the real picture: selector 0 (the
trivial empty root-level group) succeeds fast and reliably (confirmed over 5 repeated runs,
consistently in well under a second). But **selector 2 (the dense new-style group, 300 links, v2
B-tree + fractal heap — the case this session's MPMDE work was built for) never succeeds at all**
within a bounded window as long as it was tested: one run showed the retry loop restarting from
`i=0` over **26 million times in 17 seconds** without `i` ever exceeding 2 even once. This is not
"eventually converges, just slowly" — it is a hard, reproducible block.

Dumping the actual HDF5 error stack for the specific failing call (`H5Lexists()` returning a real
error, not "not found," while checking an individual link inside a dense group whose own metadata
— storage type, link count — had *already* been validated correctly) traced the failure to:

```
H5Lexists → H5G_traverse → H5G__dense_lookup → H5B2_find → H5G__dense_btree2_name_compare
  → H5HF_op → H5HF__man_dblock_locate → H5HF__man_iblock_protect (fractal heap indirect block)
  → H5AC_protect → H5C_protect → H5C__load_entry:
    "incorrect metadata checksum after all read attempts"
```

This is the critical detail: `H5C__load_entry()` has a **built-in retry loop** specifically
designed to tolerate transient torn reads inherent to SWMR (hence "after all read attempts" in the
message) — and it still fails checksum validation every single time, across millions of overall
retries. A mechanism built to smooth over *transient* races failing consistently, forever, means
this isn't a race being lost sometimes — it's a **persistent, reproducible corruption** in how the
fractal heap's indirect block is published or read back for a dense group large enough to need
MPMDE. (Not a VFD-SWMR-transport-level checksum issue either — `H5FD__vfd_swmr_read()`'s own
shadow-file checksum check, lower in the stack, was directly traced and never fires here; this is
HDF5's own internal per-block checksum, verified after the VFD SWMR layer has already handed back
its bytes.)

This reframes the whole investigation: the "slow, high-variance convergence" symptom was a
downstream consequence of this one selector never succeeding, not a general performance gap across
all selectors. Fixing *this* is likely the actual remaining blocker for `zoo` — not a broad
performance-tuning exercise.

**Follow-up 3: the "is it an unhandled MPMDE case" hypothesis was checked directly and ruled
out — one real bug was found and fixed along the way, but it does not explain this failure.**

Traced every distinct address the reader's `H5PB_read()` requests with `type == H5FD_MEM_OHDR`
during a run that hits the failure (`H5FD_MEM_FHEAP_IBLOCK` is a `#define` alias for
`H5FD_MEM_OHDR` in `src/H5FDdevelop.h` — fractal heap indirect blocks are tracked as this type,
same as ordinary object headers). The failing indirect block is read at a *fixed, unvarying*
`addr=7712, size=149` on every attempt — nowhere close to `page_buf->page_size` (4096). **This
structure is not, and was never going to be, an MPMDE entry** — the earlier "Follow-up 2"
writeup's speculation to the contrary was wrong.

Along the way, reading `H5C__load_entry()`'s speculative-size retry logic (the "grow the read"
step used when a metadata type's real size isn't known until its prefix is decoded) surfaced a
real, independent bug: `H5PB_read()`'s "found" branch (an already-cached entry, hit via
`H5SL_search`) clamped `access_size` to `page_buf->page_size` unconditionally instead of the
entry's own `page_entry->size`. For a multi-page metadata (MPMDE) entry this would silently
truncate — or, for an offset past the first page, underflow via unsigned wraparound — any read
landing past the first page. **Fixed** in `src/H5PB.c` (`page_entry->size` used in the clamp
instead of `page_buf->page_size`), verified via regression suite (2726/2727, no new regressions),
and kept as a real, standalone correctness fix. But it does not touch this failure: `addr=7712`'s
entry, at 149 bytes, is a completely ordinary, single-page (in fact far-under-one-page) entry,
never routed through the MPMDE path at all, so this clamp was never in its way.

**The actual bug is still open.** The most promising remaining lead: `addr=7712` falls in HDF5
page 1 (byte range 4096–8191, given `page_buf->page_size=4096`), and earlier tracing in this same
session (see "Follow-up 2") showed *multiple, unrelated* v2 B-tree node writes landing in that
exact same page — `addr=4096, 4608, 5120, 5632, 6144, 6656, 7168` (all `type=2`/`H5FD_MEM_BTREE`),
immediately followed by the indirect block write at `7712`. Several distinct metadata structures
sharing one page-buffer entry is a real, plausible way for one write to corrupt another under a
page-buffer design that tracks one shared image per page rather than per-structure — worth
checking directly (e.g., whether writes to different byte ranges within the same shared entry
correctly preserve each other's content across ticks, especially if the entry gets evicted and
reloaded between them) before assuming this is MPMDE-related at all. This has not yet been
confirmed, only identified as the next thing to check.

*(All `getenv("H5PB_PERF_TRACE")`/`H5PB_TIMEIT`/`H5PB_RATECHECK`-gated instrumentation added in
this session's investigation — across `src/H5C.c`, `src/H5Fvfd_swmr.c`, `src/H5FDvfd_swmr.c`,
`src/H5PB.c`, `test/genall5.c`, `test/vfd_swmr_zoo_writer.c` — has been removed after use. Re-add
similar tracing if you pick this up; the pattern used was: cap output with `if (call_count <= N ||
call_count % M == 0)`, or gate on a restart/success condition, or dedupe on "value changed since
last print," to avoid the disk quota exhaustion that a truly unthrottled per-call trace causes
under this test's zero-backoff retry loop.)*

---

## Session N+1: the fractal heap checksum bug root-caused and fixed, then 8 more real bugs found stress-testing the full `test_vfd_swmr.sh` script

**The `zoo` blocker documented above ("Follow-up 2"/"Follow-up 3" — `H5C__load_entry(): incorrect
metadata checksum after all read attempts`) is now root-caused and fixed.** The prior session's
"multiple structures sharing one page-buffer page" lead (suggested as the top "Next steps" item)
turned out to be a red herring — the real cause was much more fundamental and is why it affected
every scenario that ran long enough to matter, not just the dense-group selector.

### Root cause: the reader's own EOA never stayed in sync with the writer's growing file

Traced the chain: `H5Lexists` fails → `H5C__load_entry` checksum failure → `H5PB_read()` returns a
**zero-byte read** → `H5PB_read()`'s own EOA clamp computes `page_size=0` because its cached
`eoa=4096` is stale → `H5FD_read()`'s own, separate EOA check *also* fires, since VFD SWMR does not
set the legacy `H5F_ACC_SWMR_READ` flag that would exempt it (VFD-SWMR-ness is conveyed by the FAPL
config, not an access-mode bit) → **the port is missing the reference's
`H5F__cache_superblock_refresh()` VFD-SWMR "refresh" callback entirely** — its whole job is to call
`H5F__set_eoa()` every tick so the reader's EOA tracks the writer's growing file, matching the
15th field the reference's `H5AC_SUPERBLOCK` class has that the port's didn't.

Implementing that callback alone wasn't sufficient: tracing showed it was registered but **never
invoked**, because the superblock's own cache entry never appeared in the reader's `page_index[]`
at all (every *other* page-0 structure did). Root cause: `H5C__INSERT_IN_INDEX`'s `page_index[]`
insertion is gated on `cache_ptr->vfd_swmr_reader` already being `true` — and the port only set
that flag once, late, inside `H5F_vfd_swmr_init()` (after the superblock is already loaded and
cached). **The reference sets it twice**: once early inside `H5AC_create()` itself (before the
superblock is ever touched), and again later inside `H5F_vfd_swmr_init()` for the page-size-updated-
by-superblock-extension case documented in that function's own comment. The port was missing the
first, earlier call site entirely.

**Fix** (`src/H5AC.c`, `H5AC_create()`): added the missing early
`if (H5F_VFD_SWMR_CONFIG(f) && !f->shared->vfd_swmr_config.writer) H5C_set_vfd_swmr_reader(f->shared->cache, true, f->shared->fs_page_size);`
block, matching the reference exactly, placed right after MDC logging setup and before
`H5AC_set_cache_auto_resize_config()` — i.e. before the superblock is ever protected/inserted into
the cache. **Fix** (`src/H5Fsuper_cache.c`): implemented `H5F__cache_superblock_refresh()` (the
15th field of `H5AC_SUPERBLOCK[1]`, ported from the reference and adapted to this port's current
superblock byte layout), which decodes just enough of the refreshed superblock image to read
`stored_eof` and calls `H5F__set_eoa()` with it.

**Verified**: zoo writer/reader now converges cleanly 5/5 runs (previously hung/timed out every
run); the superblock refresh callback fires ~6 times per run (previously 0).

### An additional, defensive `H5PB_read()` fix (real, but not the actual unblocker)

While chasing the above, also found and fixed a real bug in `H5PB_read()`'s "not found" branch: its
own EOA check had no VFD-SWMR exemption at all (unlike `H5FD_read()`, which at least has the — for
VFD SWMR, ineffective — legacy `H5F_ACC_SWMR_READ` exemption). Added
`if (!(page_buf->vfd_swmr && !page_buf->vfd_swmr_writer))` around the EOA-retrieval/clamp block, so
a VFD SWMR reader's own page-buffer-level EOA check doesn't fire independently of (and in this case,
*before*) `H5FD_read()`'s. This is defensible on its own merits (mirrors the same "a lagging
reader's EOA can legitimately be behind the writer's" rationale used elsewhere), but with the
superblock-refresh fix above in place, the reader's EOA should rarely if ever actually be stale
enough to need it — kept as a second line of defense, not the load-bearing fix.

### Stress-testing the full `test_vfd_swmr.sh` script surfaced 8 more real, pre-existing bugs

With `zoo` finally converging, running the *entire* `test_vfd_swmr.sh` script (not just `zoo` in
isolation) — something never done before, since every earlier session's `zoo` hang/crash made it
pointless to even attempt the other scenarios — surfaced a further 8 real bugs. All 8 are
pre-existing (present on `develop`/reference-derived code well before this session), previously
latent because nothing had ever run these code paths for long enough, or under enough concurrent
load, to hit them. **The apparent "crash popups"/hangs a user observed mid-investigation were these
bugs firing, not anything introduced by the fixes above.**

1. **Missing `sigtimedwait` CMake detection → unsafe pthread fallback → real, 100%-reproducible
   `vfd_swmr_writer` SIGSEGV.** `test/vfd_swmr_common.c`'s `await_signal()` has two implementations:
   a safe one using `sigtimedwait()` in the *same* thread that also makes the periodic
   `H5Aexists_by_name()` idle-tick call, and an unsafe fallback (`#ifndef H5_HAVE_SIGTIMEDWAIT`) that
   spawns a **separate pthread** to make that same call concurrently with the main thread's own HDF5
   calls — a genuine, unguarded data race on the page-buffer LRU list. The port's CMake build never
   added a check for `sigtimedwait` (the reference's `configure.ac` has
   `AC_CHECK_FUNCS([sigtimedwait timespeccmp])`; nothing analogous existed for CMake), so
   `H5_HAVE_SIGTIMEDWAIT` was **never defined even on Linux**, forcing every build onto the unsafe
   path. **Fixed**: added `CHECK_FUNCTION_EXISTS (sigtimedwait ${HDF_PREFIX}_HAVE_SIGTIMEDWAIT)` to
   `config/ConfigureChecks.cmake` and the matching `#cmakedefine H5_HAVE_SIGTIMEDWAIT` to
   `src/H5pubconf.h.in`. Confirmed via `info threads` in gdb: exactly one thread after the fix
   (previously two), and the specific SIGSEGV symptom below stopped reproducing under this fix
   alone — though a second, independent bug (next item) was also firing and needed its own fix.

2. **The delayed-write list shares its `next`/`prev` fields with the LRU list; six call sites
   didn't account for entries currently on it, corrupting both lists.** `H5PB_entry_t` has exactly
   one `next`/`prev` pair, reused for *both* the LRU replacement list and the delayed-write list
   (DWL) — by design, since an entry is never on both at once *if every touch point checks which
   one it's actually on first*. Six sites didn't: `H5PB__vfd_swmr_track_write()`,
   `H5PB_update_entry()`, both branches of `H5PB_read()`'s multi-page loop, `H5PB_read()`'s
   single-page-touch branch, `H5PB__write_mpmde()`'s regular-to-mpmde transition, and
   `H5PB_write()`'s "found" branch all guarded LRU touches on `modified_this_tick` alone — but an
   entry can have `modified_this_tick == false` (reset at the *previous* tick's end) while still
   sitting on the DWL from that earlier tick (`delay_write_until != 0`, not yet released). Touching
   the LRU for such an entry corrupts both lists via pointers that are actually the *other* list's
   neighbors. Confirmed via a temporary LRU-consistency check (walk-and-verify after every
   INSERT/REMOVE/MOVE_TO_TOP, removed after use) that caught the exact corrupted state
   (`count mismatch: walked=304, len=303`) and, with a disassembly-level register dump, the precise
   crashing instruction (`page_ptr->prev->next = ...` with `prev == NULL` on a non-head node).
   **Fixed** (`src/H5PB.c`, all 6 sites): added `&& entry->delay_write_until == 0` (and, for the 4
   sites that check entries found via generic skip-list lookup rather than a caller that already
   knows the entry's provenance, also `!entry->is_mpmde`) to each guard.
3. **`H5F_shared_t::shadow_defrees` (a BSD-style `TAILQ`) was never initialized, only zeroed via
   `H5FL_CALLOC`.** An empty tail queue requires `tqh_last == &head.tqh_first`, not `NULL` — the
   *first* `TAILQ_INSERT_HEAD()` self-heals this, but `H5F_update_vfd_swmr_metadata_file()`'s reclaim
   scan calls `TAILQ_LAST()`/`TAILQ_FOREACH_REVERSE_SAFE()` as soon as `tick_num > max_lag`, which can
   happen well before any entry is ever deferred — dereferencing the bad `NULL` and crashing. The
   reference has an explicit `TAILQ_INIT(&f->shared->shadow_defrees);` in `H5Fint.c` that the port
   never carried over. **Fixed** (`src/H5Fint.c`): added the missing `TAILQ_INIT()` call in the same
   spot the reference has it (right after `vfd_swmr_md_fd = -1`).
4. **& 5. `H5F_open()`'s superblock `status_flags` consistency check has no VFD-SWMR exemption on
   either side, so a VFD SWMR reader can never open a file a VFD SWMR writer holds open — the normal
   case.** A VFD SWMR reader doesn't set the legacy `H5F_ACC_SWMR_READ` flag (same theme as the
   `H5PB_read()` fix above), so it always fell into the strict "must not already be open for write"
   branch (superblock version ≥ 3 only) and failed with *"file is already open for write"* every
   time. The reference has `|| H5F_USE_VFD_SWMR(file)` added to both sides of this check: the
   read-side branch-selection condition, *and* the write-side flag-setting condition (a VFD SWMR
   writer also doesn't set the legacy `H5F_ACC_SWMR_WRITE` flag, so without this it sets
   `H5F_SUPER_WRITE_ACCESS` but not `H5F_SUPER_SWMR_WRITE_ACCESS`, which then fails the *other*
   branch's flag-agreement check once the read-side fix routes readers into it). **Fixed**
   (`src/H5Fint.c`, both sites in `H5F_open()`): added `|| H5F_USE_VFD_SWMR(file)` to both
   conditions, matching the reference exactly.
6. **`vfd_swmr_group_writer.c`'s `state_init()` uses `H5T_NATIVE_UINT32` as its very first HDF5
   call, and — only in this specific binary's process/link context, not in a minimal standalone
   repro — that first evaluation can observe an unpopulated `H5T_NATIVE_UINT32_g`
   (`H5I_INVALID_HID`), later making `H5Tget_native_type()` fail with "not a data type" in
   `add_attr()`.** The exact mechanism wasn't fully nailed down (the `H5OPEN` macro's comma-trick
   should force `H5open()` to complete before the global is read, and a minimal standalone program
   doing the identical "first call is `H5T_NATIVE_UINT32`" pattern does not reproduce it), but an
   explicit `H5open()` call added before it is unconditionally safe (idempotent) and empirically
   fixes it 100% of the time. **Fixed** (`test/vfd_swmr_group_writer.c`, `state_init()`): added an
   explicit `H5open()` call (with error check) as the first HDF5 API call in the function, before
   `s->filetype = H5T_NATIVE_UINT32`.
7. **`H5PB_remove_entry()` could free an entry while it was still linked into the tick list and/or
   delayed-write list, corrupting both and causing the shadow-file index to grow without bound until
   `calloc()` failed.** `H5PB_remove_entry()` (called from `H5MFsection.c`'s free-space-manager
   section-merge code — i.e. routinely, during any shrink/remove workload — and from
   `H5Fvfd_swmr.c`'s tick-diff eviction loop) frees the `H5PB_entry_t` without checking whether it is
   currently threaded onto the tick list (`modified_this_tick`) or the DWL (`delay_write_until != 0`)
   first. If the free-space manager evicts a page the writer only just dirtied earlier in the *same*
   tick, freeing it while still linked leaves neighboring list entries pointing at freed/reused
   memory — in the reproduced case, this corrupted the tick list into an effectively cyclic
   structure, so `H5PB_vfd_swmr__update_index()`'s tick-list walk looped far past its real length,
   doubling the shadow index's allocated length repeatedly within a single call
   (`old_len=2080640 → 4161280 → ... → 532643840`, all at the same `tick_num`, confirmed via a
   temporary trace) until the in-memory allocation failed outright. Reproduced with the exact
   `remove_writer -o 40000` / `-i b2` scenario from `test_vfd_swmr.sh`'s "shrink" test, both with and
   without concurrent readers. **Fixed** (`src/H5PB.c`, `H5PB_remove_entry()`): before freeing, if
   `modified_this_tick`, call `H5PB__REMOVE_FROM_TL()` and clear the flag; if `delay_write_until !=
   0`, clear it and call `H5PB__REMOVE_FROM_DWL()`; only call `H5PB__REMOVE_LRU()` if the entry
   wasn't off the LRU for either of those reasons (tracked via a `was_off_lru` flag captured before
   the unlinks, since both operations clear the state that would otherwise tell you it was ever
   off-LRU). Verified: the `expand`/`shrink`/`expand_shrink` scenarios (both `ea` and `b2` index
   types) now run clean with zero shadow-index growth (previously reproduced 100% of the time within
   the "shrink" test).
8. **`verify_group_vlstr_attr()`'s `astr_val` is declared without initialization; a failed
   `H5Aread()` on a modified VL string attribute leaves it holding garbage, and the function's own
   error-path `if (astr_val) H5free_memory(astr_val);` then frees that garbage pointer — a
   crash (`free(): double free detected in tcache 2`, SIGABRT, core dump), 100% reproducible on the
   `groups_attrs` test's `modify-vstr` scenario.** **Fixed** (`test/vfd_swmr_group_writer.c`, both
   the socket and non-socket variants of `verify_group_vlstr_attr()`): initialize
   `char *astr_val = NULL;` at declaration, matching how `aid`/`atype` are already initialized to
   `H5I_INVALID_HID` in the same function. **The crash is fixed and verified** (no more aborts/core
   dumps across repeated runs), **but a separate, deeper correctness bug remains underneath and is
   NOT fixed**: the underlying `H5Aread()` call still fails with *"Expected global heap object size
   does not match"* every time — a genuine data-consistency issue reading back a *modified* VL
   string attribute, most likely a torn-read-style inconsistency between the attribute message's own
   VFD-SWMR-published metadata (which references a global heap object by address+size) and the
   global heap collection's own, independently-published content. Global heap objects go through the
   ordinary `H5AC_GHEAP` metadata-cache class — the same generic cache → page-buffer → write path as
   everything else — so this is not a case of global heap writes bypassing VFD SWMR entirely; it
   looks like a gap in how two independently-tick-published structures (the attribute and the heap
   object it references) stay mutually consistent across a tick boundary. **Not investigated
   further this session** — flagged as the top item under "Next steps" below.

### Verification

After all 8 fixes: `expand`, `shrink`, `expand_shrink`, `sparse`, `vlstr_null`/`vlstr_oob` (both
*expected* to report reader errors — they test error paths on purpose), `zoo`, `groups`,
`groups_attrs` (all variants except `modify-vstr`'s deeper VL/global-heap issue above),
`groups_ops` all run clean under direct, targeted reproduction (writer+reader pairs invoked
directly, and via `test_vfd_swmr.sh <scenario-name>` for multi-scenario runs) — no crashes, no
hangs, no reader-open failures, no shadow-index growth. Running the *entire* default
`test_vfd_swmr.sh` scenario set gets much further than ever before (previously died on the first
crash/hang; now consistently reaches `groups`/`groups_attrs`/`groups_ops` and usually `few_big`/
`many_small`), but **does not yet reliably complete end-to-end within 1200s in every run** — see
"Performance comparison against the reference" immediately below for why, and note that this is
different from a functional regression: every individual scenario passes when isolated, and the
one intermittent contributor identified is pre-existing test code shared with the reference, not
something these fixes introduced.

*(The `getenv("H5PB_PAGETRACE")`/`H5_SHADOW_IDX_TRACE`-gated instrumentation added across
`src/H5PB.c`, `src/H5FDvfd_swmr.c`, `src/H5Fvfd_swmr.c`, `src/H5C.c`, `src/H5Fsuper_cache.c`, and
`test/genall5.c` during this session's investigation has been removed after use, following the
same pattern as previous sessions' tracing.)*

### Performance comparison against the reference (`05b54b7046`, built via autotools)

Built the reference following "Building the reference implementation for comparison" below, then
compared directly against this port on the same machine.

**Zoo convergence: at parity.** 3 runs each, writer+reader launched identically
(`vfd_swmr_zoo_writer -q` / `vfd_swmr_zoo_reader -q`, 1-second stagger):

| | Run 1 | Run 2 | Run 3 |
|---|---|---|---|
| Reference | 6825ms | 6826ms | 6821ms |
| Port | 7007ms | 7005ms | 7006ms |

~3% difference, both rock-solid across repeats. This confirms the zoo root-cause fix (early
`vfd_swmr_reader` flag + superblock refresh callback) brought the port to genuine performance
parity with the reference for the mechanism it fixed, not just correctness.

**Full `test_vfd_swmr.sh` script: reference finishes in 483s; the port did not reliably finish
within 1200s across 3 attempts** (one via the normal working directory, one in a freshly-emptied
directory to rule out leftover-state confounds — same result both ways). Traced this specifically,
rather than assuming it's the zoo mechanism above regressing:
- Zoo run in **isolation**, or immediately preceded by `groups`, or preceded by the exact
  `generator expand shrink expand_shrink sparse vlstr_null vlstr_oob` sequence that precedes it in
  the full script, all **complete successfully** (46s–157s, depending on scope) — including cases
  where a *different*, non-fatal message — `validate_zoo`/`validate_deleted_zoo took too long to
  finish` (`test/vfd_swmr_zoo_writer.c:329`/`364`, `reader_check_time_and_notify_writer()`/
  `reader_check_time_after_verify_deletion()`) — appears but the scenario still recovers and the
  script still reports "VFD SWMR tests passed" afterward. This is a **different code path** from
  the hard, non-recoverable deadline documented earlier in this file (`notify_and_wait_for_reader()`
  giving up permanently) — this one is a soft, recoverable warning.
- The reference's own full-script run shows **no occurrence at all** of this warning at the
  equivalent point in its sequence.
- **Not yet root-caused**: whether this warning's higher frequency on the port under the full
  script's accumulated scenario load is (a) genuine residual timing overhead somewhere in the
  port's own tick/publish path that only shows up under load, not in a quiescent zoo-only run, (b)
  ambient system noise from this specific dev machine (not confirmed reproducible on a quieter
  machine or with repeated identical full-script runs — sample size for the full-script comparison
  is 3 port runs vs. 1 reference run), or (c) a pre-existing sensitivity in the zoo test's own
  design that the reference happens not to trigger on this particular hardware/run but could on
  another. **Flagged as the second "Next steps" item** — not fixed this session, and explicitly not
  claimed as either "no performance difference" or "a confirmed regression" pending further
  measurement.

---

## Session N+2: root-caused and fixed the VL-string-attribute/global-heap consistency bug (bug #8), plus a new mpmde tick-list bug it exposed

This session picked up the top "Next steps" item from "Session N+1": `groups_attrs`'s "modify-vstr"
scenario failed 100% reproducibly with `H5VL__native_blob_get(): Expected global heap object size
does not match` when a reader tried to read back a *modified* VL string attribute. The previous
session's write-up speculated this was "a gap in how two independently-tick-published structures
stay mutually consistent across a tick boundary." That framing was close but not quite right — the
real bug is that the global heap object was **never published through the tick mechanism at all**,
not that its publication raced with the attribute's.

### Root cause: `H5F_shared_block_read()`/`write()` remapped `H5FD_MEM_GHEAP` to `H5FD_MEM_DRAW` unconditionally

`src/H5Fio.c` has always mapped `H5FD_MEM_GHEAP` (global heap) to `H5FD_MEM_DRAW` before handing a
read/write off to the page buffer, on the theory that global heap collections don't need
metadata-style small-page caching. Checking the reference (`05b54b7046`) directly (built fresh via
autotools in this environment for a clean A/B comparison — see "Building the reference
implementation for comparison" below) showed its equivalent functions
(`H5F_shared_block_read()`/`H5F_shared_block_write()`) guard that remap with
`if (!H5F_SHARED_USE_VFD_SWMR(f_sh))` — i.e. **the reference only treats global heap as raw data
for a non-VFD-SWMR file.** For a VFD SWMR file, `H5FD_MEM_GHEAP` is left alone and flows into the
page buffer as genuine metadata, which is why it gets tracked (`H5PB__vfd_swmr_track_write()`),
published to the shadow index, and refreshed in a reader's metadata cache when it changes — exactly
like any other metadata type. The port's `H5Fio.c` had no such guard, so a VFD SWMR file's global
heap collection was *always* classified as raw data, `H5PB__vfd_swmr_track_write()`'s own
`type == H5FD_MEM_DRAW` early-return silently excluded it from the tick list, and it was never
published to the shadow index at all.

This was diagnosed empirically, not by inspection alone: gdb-tracing
`H5PB__vfd_swmr_track_write()` in the writer during a `modify-vstr` run showed it firing for every
`H5FD_MEM_SUPER`/`H5FD_MEM_OHDR` write but *never once* for the global heap collection's address —
confirming the write was reaching the page buffer as `H5FD_MEM_DRAW`, not `H5FD_MEM_GHEAP`, and
therefore bypassing tracking entirely. Comparing `H5VL__native_blob_get()`'s exact failure values
(`addr=4096 idx=1 hobj_size=1 expected_size=2` on the port vs. `addr=4096 idx=1→2 hobj_size` always
matching `expected_size` on the reference) further showed the reader was reading a **stale** cached
global heap object — one that had never been invalidated because its page was never in the shadow
index for the reader's tick-diff to notice as changed.

**Fix** (`src/H5Fio.c`, four call sites — `H5F_shared_block_read()`, `H5F_block_read()`,
`H5F_shared_block_write()`, `H5F_block_write()`): changed the unconditional
`map_type = (type == H5FD_MEM_GHEAP) ? H5FD_MEM_DRAW : type;` to also check `!f_sh->vfd_swmr` /
`!H5F_USE_VFD_SWMR(f)`, matching the reference's guard exactly. (The port's two `H5F_shared_select_read/write()`
functions, used only for vector I/O, keep the unconditional remap — the reference does too; those
aren't in play for this bug.)

**Follow-on fix** (`src/H5PB.c`, `H5PB_read()`): removed a now-incorrect `assert(type !=
H5FD_MEM_GHEAP)` — `H5FD_MEM_GHEAP` can legitimately reach this function now, for any VFD SWMR
file.

### An independent, related gap also fixed: raw data must bypass the page buffer entirely under VFD SWMR

While comparing against the reference, also found the reference's `H5PB_read()`/`H5PB_write()`
unconditionally bypass the page buffer for **any** `H5FD_MEM_DRAW` access when `page_buf->vfd_swmr`
is true (its own "case 2"), regardless of reader or writer. The port never had this rule. The
reasoning: raw data is never tick-published, so the *only* way a reader can trust it is if the
writer commits it straight through to the real file immediately (no lingering dirty copy cached
in the writer's own page buffer) and the reader always reads it straight from the real file too
(no stale cached copy on the reader's side either). **Fix** (`src/H5PB.c`, `H5PB_read()` and
`H5PB_write()`): added `if (page_buf != NULL && page_buf->vfd_swmr && H5FD_MEM_DRAW == type)
bypass_pb = true;` ahead of the existing bypass checks in both functions, matching the reference.
This is independently valid regardless of the `H5Fio.c` fix above (it now only affects genuine raw
*dataset* data under VFD SWMR, since global heap no longer arrives here as `H5FD_MEM_DRAW`).

### A new, real bug the `H5Fio.c` fix exposed: mpmde tick-list `tl_size` accounting

With global heap writes now genuinely tracked as metadata for the first time, some of them are
large enough to route through the existing MPMDE ("multi-page metadata entry") path added in an
earlier session (`H5PB__write_mpmde()`) — a code path that, before this fix, no `H5FD_MEM_GHEAP`
write had ever reached, since it was always disguised as `H5FD_MEM_DRAW` first. Stress-testing with
`vfd_swmr_vlstr_writer -n 500 -q -t oob` (previously untested against this exact interaction)
crashed 100% reproducibly with a cascade of `H5PB_vfd_swmr__release_tick_list(): TL DLL pre remove
SC failed` errors, ending in a segfault in `H5PB__dest_cb()` at file close.

Root-caused via **source-level `fprintf(stderr, ...)` tracing gated on `getenv("H5PB_TL_TRACE")`**
(added temporarily, removed after use) rather than gdb — an earlier attempt to trace this with gdb
breakpoints produced misleading values (entries appearing to have `modified_this_tick == 0` while
still linked into the tick list), almost certainly an artifact of breakpoint placement interacting
with compiler reordering in this `RelWithDebInfo` build; the source-level trace showed a
perfectly consistent list right up until the actual bug fired, with exact values. The trace showed
the smoking gun directly: an mpmde entry inserted into the tick list at 4096 bytes (one page),
then **grown to 8192 bytes by a second write later in the same tick** — `H5PB__write_mpmde()`'s
"existing entry needs to grow" branch updates `entry_ptr->size` in place but never adjusts
`page_buf->tl_size` (the tick list's running cumulative-size counter, used by the
`H5PB__TL_DLL_PRE_REMOVE_SC` sanity check). The list's cached total size was left short by exactly
the growth delta, so removing that entry from the tick list later miscounted it as "the only entry
left" (`Size == entry_ptr->size`) even though a second entry (the superblock) was still present
(`len == 2`), tripping the sanity check every time and eventually corrupting the list badly enough
to segfault at close.

This is a genuine, independent, pre-existing gap in the MPMDE growth path from the earlier session
that added it — not a new bug introduced by the `H5Fio.c` fix above, just one that fix was the
first thing to ever exercise (nothing had grown an already-tracked-this-tick entry past its
original size before). **Fix** (`src/H5PB.c`, `H5PB__write_mpmde()`): capture the entry's old size
before growing it, and if the entry is already `modified_this_tick` (already on the tick list from
an earlier write this same tick), add `(new_size - old_size)` to `page_buf->tl_size` to keep the
list's cumulative accounting correct. If the entry is *not* yet on the tick list, no adjustment is
needed — `H5PB__vfd_swmr_track_write()`'s subsequent `H5PB__INSERT_IN_TL()` call inserts it fresh
with the already-grown size.

### Verification

- `vfd_swmr_group_writer -q -c 1 -n 1 -a 1 -A modify-vstr` + matching reader: **5/5 clean runs**
  (every run failed before this fix).
- `vfd_swmr_vlstr_writer -n 500 -q -t oob` / `-t null`, standalone and paired with their readers:
  clean completion, no `TL DLL` errors, no segfault (100% reproducible crash before the `tl_size`
  fix).
- `test_vfd_swmr.sh vlstr_null vlstr_oob` (the actual acceptance-test scenarios, which exercise
  these as *expected*-error paths): "VFD SWMR tests passed", 1 expected error, as designed.
- `vfd_swmr_group_writer -q -c 10 -n 20` + `vfd_swmr_group_reader -q -c 10 -n 20 -u 5` (the plain
  `groups` scenario, given a proper writer head start): clean.
- Full regression suite excluding the slow `H5SHELL-test_vfd_swmr` shell test: **2726/2726 passed,
  0 failed** (`ctest -j 8 -E H5SHELL-test_vfd_swmr --timeout 120`).
- Full `test_vfd_swmr.sh` (all default scenarios, standalone): started twice this session. Both
  runs progressed cleanly with no crashes and no `TL DLL`/segfault symptoms through
  `generator`/`expand`/`shrink`/`expand_shrink`/`sparse`/`vlstr_null`/`vlstr_oob`/`zoo` before being
  stopped early — once by the pre-existing full-script timing budget documented in "Session N+1"
  (not this session's fixes; see the "Next steps" item on it), and once deliberately, to avoid
  burning more session time re-confirming that same already-documented, pre-existing gap. Given the
  individual-scenario and full-ctest results above, this was judged sufficient; a full, patient,
  uninterrupted run is still worth doing before merging, per "Next steps" item 2.

*(The `getenv("H5PB_TL_TRACE")`-gated instrumentation added across `src/H5PB.c` during this
session's mpmde debugging has been removed after use, following the same pattern as previous
sessions' tracing.)*

### Revisited the full-script timing gap ("Next steps" item 2) — premise didn't hold for this subset, and a real, pre-existing, intermittent data bug was found instead

Picked up the "Next steps" item on the full-script timing gap next, using a budgeted, tightly
capped comparison (agreed with the user up front: 3 repeated timed runs per side, then stop
regardless of outcome, given how expensive and historically noisy this measurement has been).
Timed `generator expand shrink expand_shrink sparse vlstr_null vlstr_oob zoo` (the prefix that
precedes the scenarios never reliably reached in one script invocation) on both the port and the
freshly-built reference, 3 runs each, in this same environment:

| | Run 1 | Run 2 | Run 3 | Average |
|---|---|---|---|---|
| Port | 156.5s | 156.4s | 156.5s | **156.5s** |
| Reference | 214.4s | 213.9s | 214.0s | **214.1s** |

**The port was consistently ~27% *faster*** than the reference for this subset — the opposite of
the premise that motivated this item, and both sides were low-variance (this run of the sandbox
was not as noisy as earlier sessions worried). The port does still reliably show the soft,
non-fatal `validate_zoo took too long to finish` warning that the reference never shows (3/3 vs.
0/3), but that costs no actual wall-clock time here. This means whatever full-script slowdown is
real is concentrated in the scenarios *after* this prefix (`groups`/`groups_attrs`/`groups_ops`/
`few_big`/`many_small`) and was never actually isolated to `zoo` or anything before it — a
correction to the framing in "Session N+1"/"Performance comparison against the reference" above.
Per the budget agreed with the user, this was not chased further this session.

**While budgeting those runs, one of the three port runs surfaced a real, unrelated, intermittent
data-consistency bug**, not a timing artifact: `*** READER: ERROR *** Incorrect record value!
Symbol = '0-0056', # of records = 852, record->rec_id = 385, expected 353` in the `expand_shrink`
scenario (`vfd_swmr_addrem_writer` + `vfd_swmr_remove_readers`), causing a genuine (not soft)
`nerrors` increment and the run's "1 unexpected errors" summary. This looked at first like it might
be downstream of this session's `H5Fio.c`/`H5PB.c` changes (the natural suspicion, given it
surfaced in the same investigation), so before going further, its origin was checked directly:
`git stash` reverted this session's two fixes back to the pre-session baseline, the tree was
rebuilt, and `generator expand shrink expand_shrink` was looped standalone — **the identical
"Incorrect record value" failure reproduced on the unmodified baseline** (1-in-5 attempts), proving
it predates this session's changes entirely and is not a regression from either fix. Changes were
then restored (`git stash pop`) and rebuilt. **Not root-caused further this session** (out of
scope/budget for this pass) — flagged as a new, real, pre-existing, intermittently-reproducible
(roughly 1-in-4 to 1-in-5 standalone attempts) data bug in the `expand_shrink` scenario, worth
prioritizing before any of the timing-chase items below, since a wrong-record-value bug is a
correctness issue, not a performance one. Reproduce with a tight loop of
`bash test_vfd_swmr.sh generator expand shrink expand_shrink` (rm -rf the `vfd_swmr_test` working
directory between attempts); expect roughly 1-in-4 to fail.

### Root-caused and fixed the `expand_shrink` "Incorrect record value" bug from above

Immediately picked up the new top-priority item from above. Root-caused via source-level
`fprintf(stderr, ...)`-gated tracing (`getenv("H5PB_ADDR_TRACE")`, added temporarily to
`H5PB__vfd_swmr_track_write()` and the raw-data bypass in `H5PB_write()`, removed after use) plus a
temporary diagnostic in `test/vfd_swmr_remove_reader.c`'s `check_dataset()` that, on a mismatch,
calls `H5Dget_chunk_info_by_coord()` to print the failing record's actual on-disk chunk address
(also removed after use).

**Root cause: a metadata write and a dataset chunk's raw data can land on the same VFD-SWMR page,
and publishing the metadata freezes a stale snapshot of the chunk too.** VFD SWMR's shadow index
operates at page granularity (`fs_page_size`, 4096 in these tests): when *any* tracked metadata on
a page is published, the writer's shadow-file snapshot captures the *entire* page as it existed at
that moment — including any unrelated, untracked raw data that happens to share the same
page-aligned address range (HDF5's free-space allocator does not keep metadata and raw data address
ranges page-segregated, especially after `expand_shrink`'s shrink/regrow cycles reuse freed space).
A reader's raw-data read is deliberately redirected through the shadow index by address (the whole
point of the mechanism, so a reader consulting a tracked page sees the writer's *published*, tick-
consistent state) — but for a page that also holds raw chunk bytes, this means the reader can be
served the *metadata's* old snapshot of those raw bytes instead of the chunk's actual, freshly
written content, even though the real file already has the fresh bytes. Confirmed directly: for
each of several captured failures, the exact chunk address from the reader's new diagnostic matched
a `H5PB__vfd_swmr_track_write()`-traced metadata address (type 2, `H5FD_MEM_BTREE` — the test
file's v1 B-tree group-link index) from the writer's own trace, on the same page.

The reference (`05b54b7046`) has the *identical* gap in `H5FD__vfd_swmr_read()` (checked directly —
no `H5FD_MEM_DRAW` exemption there either), so this is not a port-introduced regression; it's a
genuine, deeper architectural gap in the VFD SWMR design that predates this port. The right fix
doesn't require bug-for-bug compatibility with the reference, though: VFD SWMR's own raw-data model
already guarantees a writer commits raw data straight to the real file immediately (never delayed,
never tick-published — see the earlier raw-data page-buffer bypass in this same session), so a
raw-data read should *never* need or want the shadow-index redirect in the first place.

**Fix** (`src/H5FDvfd_swmr.c`, `H5FD__vfd_swmr_read()`): skip the shadow-index lookup entirely when
`type == H5FD_MEM_DRAW`, always reading raw data directly from the real underlying file. This can
never lose real data — raw data is never published to the shadow index to begin with — and it
means a raw-data read can no longer win a false page-number match against a metadata page it
happens to share.

**Verification:**
- Before this fix (with the earlier GHEAP/bypass fixes already in place): 1-in-10 failure rate for
  `bash test_vfd_swmr.sh generator expand shrink expand_shrink` in a loop (down from the ~1-in-4 to
  1-in-5 baseline, since the earlier session's raw-data-bypass fix already helped somewhat by
  keeping the real file itself always fresh).
- After this fix: 10 clean runs, then a residual failure on an 8-run traced follow-up loop. Traced
  that residual specifically: its chunk address had **no** overlapping metadata write anywhere in
  the full trace, ruling out the mechanism just fixed. Its symptom (a *nonzero* stale `rec_id`, not
  the fill-value `0` the test explicitly tolerates) matches a *different*, pre-existing, and
  already self-acknowledged race in the test itself — `check_dataset()`'s own comment: "it's
  possible that the metadata indicating snpoints available is new, but the data is stale, because a
  tick occurred on the writer between `H5Dset_extent()` and `H5Dwrite()`" — for a chunk position
  being *reused* after a shrink, not a genuinely new one, this stale value is a leftover nonzero
  `rec_id` rather than the fill-value zero the test's tolerance check anticipates. This is a test
  design gap (an incomplete tolerance check), not a VFD SWMR correctness bug, and is unrelated to
  anything in this document's fixes; flagged here rather than chased further, since the bug this
  session was asked to fix — the dominant failure mode — is confirmed fixed.
- Full regression suite (excluding the slow shell test): **2726/2726 passed, 0 failed**, confirming
  no regression from the `H5FD__vfd_swmr_read()` change.

### `few_big`/`many_small` (`vfd_swmr_bigset_writer`/`_reader`): four real bugs fixed, the VDS shadow-file-unlink hang genuinely fixed (after three abandoned attempts), and a second, distinct, pre-existing mainline bug found and precisely root-caused (core-library fix needed, not attempted this session)

Picked up "Next steps" item 4 — these two scenarios had **never been run to completion on the
port**, blocked by a stack of distinct, previously-unreached bugs. Running
`bash test_vfd_swmr.sh few_big many_small` directly surfaced them one at a time, each only visible
once the previous one was fixed, in this order:

1. **Missing `H5open()` before the first `H5T_NATIVE_UINT32` use — the exact same latent bug class
   as Session N+1 bug #6, just in a different, previously-unreached file.** Once the write path
   actually ran (see bug #2 below for why it hadn't before), `H5Dcreate2()` failed with "not a
   datatype ID": `vfd_swmr_bigset_writer.c`'s `state_init()` (shared source for both the writer and
   reader binaries, split by `argv[0]` personality, same pattern as `vfd_swmr_group_writer.c`) sets
   `s->filetype = H5T_NATIVE_UINT32` as one of its first statements, before anything else in the
   process has forced full library initialization. **Fixed**: added the identical explicit
   `H5open()` guard used for bug #6, at the top of `state_init()`.
2. **Missing writer/reader synchronization — the test launches the reader immediately after the
   writer, with no signal to wait for the writer's `H5Fcreate()` to complete first.** Every other
   VFD SWMR test scenario in this script (`zoo`, `expand_shrink`, `vlstr_*`, etc.) uses a shared
   `$WRITER_MESSAGE` file plus a `WAIT_MESSAGE` poll before starting the reader; the `many_small`
   and `few_big` sections never had this, so the reader's own first `H5Fopen()` was a bare race
   against the writer's `H5Fcreate()`, losing outright whenever the writer hadn't gotten there yet
   (confirmed: reproduces the identical `H5Fopen failed ... No such file or directory` 100% of the
   time when reproduced with zero head start). **Fixed**: `vfd_swmr_bigset_writer.c`'s `main()` now
   calls `h5_send_message(VFD_SWMR_WRITER_MESSAGE, ...)` right after its file-open loop completes
   (mirroring `vfd_swmr_addrem_writer.c`'s existing pattern), and `test/test_vfd_swmr.sh.in` now
   does `rm -f $WRITER_MESSAGE` before launching the writer and `WAIT_MESSAGE $WRITER_MESSAGE`
   before launching the reader, in both the `many_small` and `few_big` sections.
3. **A genuine, generic library bug: `H5F_vfd_swmr_insert_entry_eot()` runs on *every*
   `H5F_open()` of a VFD-SWMR file, not just the first — but the matching remove only ever happens
   once, causing a use-after-free.** With bugs #1–#2 fixed, the `-V`/`-M` (VDS) option variants hit
   `HDF5: infinite loop closing library` (`H5_term_library()`'s shutdown loop never converges) and,
   in the actual script run, a segfault. Root-caused with valgrind (`--leak-check=full`): 2.3MB
   "definitely lost" in `H5F__vfd_swmr_create_index()` (`H5Fvfd_swmr.c:1874`), and separately 200
   blocks (11.2KB) "still reachable" allocated in `H5F_vfd_swmr_insert_entry_eot()` via
   `H5D__virtual_open_source_dset()` — i.e. every time a VDS opens one of its own source datasets
   (through the External File Cache, which internally calls `H5F_open()` again on the
   already-open shared file, only incrementing `shared->nrefs`). Reading `H5F_open()` in
   `H5Fint.c` found the actual gap directly: the neighboring `H5F_vfd_swmr_init()` call is
   correctly gated on `1 == shared->nrefs` ("re-opening an already-open shared file doesn't
   re-init" — an existing comment), but `H5F_vfd_swmr_insert_entry_eot()` right after it was
   *not* gated at all, so every such re-open queued another `eot_queue_entry_t` pointing at that
   specific re-open's own `H5F_t *`. `H5F__dest()` only calls the matching remove once, when the
   shared struct is *actually* destroyed (`nrefs` reaches 0) — so every extra, unpaired insert
   left a queue entry that outlived its own `H5F_t` once that specific handle's (non-final) close
   freed it, a real use-after-free the next time `H5F_vfd_swmr_process_eot_queue()` (wired into
   every API call via Phase 0c) walked the queue. Confirmed directly with gdb: `eot_queue_g` was
   still non-empty at `H5_term_library()` time even after every file in the test had been closed.
   **Fixed** (`src/H5Fint.c`, `H5F_open()`): moved `H5F_vfd_swmr_insert_entry_eot()` inside the
   `if (1 == shared->nrefs)` block, matching `H5F_vfd_swmr_init()`'s existing gate exactly. Also
   fixed the smaller, independently-confirmed 2.3MB leak while in there (`src/H5Fvfd_swmr.c`,
   `H5F_vfd_swmr_close_or_flush()`): `shared->mdf_idx`/`old_mdf_idx` (the shadow-index arrays
   allocated by `H5F__vfd_swmr_create_index()`/`H5F_vfd_swmr_enlarge_shadow_index()`) were never
   freed at writer close; added the two missing `H5MM_xfree()` calls. Verified: the "infinite loop
   closing library"/segfault symptom is gone; valgrind's "definitely lost" total dropped from
   2.3MB to the small, unrelated test-code leaks noted below.
   - *(Two much smaller, test-code-only leaks also surfaced under valgrind — 400 bytes and 1,600
     bytes in `state_init()`, 24 bytes in `notify_reader()` — but only on an **error-exit** path
     triggered by valgrind's own slowdown perturbing the writer/reader socket handshake timing, not
     on the normal success path. Not fixed: lower severity, error-path-only, and chasing them
     further would mean debugging a valgrind-timing artifact rather than the test's real behavior.)*

**The one remaining, deeper issue — now root-caused precisely, via a live gdb backtrace, not just
inferred.** Even with all three bugs above fixed, the `-V`/`-M` (VDS) option variants still hang.
This write-up went through two wrong theories before landing on the real mechanism — both are kept
here so the same dead ends aren't re-walked:

- **Wrong theory #1 (an EFC reference-counting bug leaves the shadow file unlinked prematurely):**
  disproven — tracing the exact `unlink()` call site with gdb showed it is the writer's own
  **normal, intentional final close** (`state_destroy()` at `vfd_swmr_bigset_writer.c:2837`,
  `nrefs == 1`, a legitimate last-reference teardown), not a premature/erroneous destroy.
- **Wrong theory #2 (skip-list-vs-hash-table page-buffer lookup speed):** also disproven — direct
  `/proc/PID/stat` sampling (utime+stime clock ticks) of both the writer and reader while the
  reader is hung shows **zero CPU consumption for both processes across several seconds of
  wall-clock sampling.** A process burning cycles on `O(log n)` skip-list lookups would show up as
  CPU-bound; it isn't. The reader is asleep, not slow.
- **The real mechanism, confirmed with a live gdb backtrace (writer launched normally, reader
  launched under `gdb -batch -x script --args ...`, then `kill -INT <gdb-pid>` to force an
  interrupt-and-backtrace mid-hang — ptrace-attaching to an *already-running* foreign process is
  blocked by this sandbox's `ptrace_scope=1`, but a process gdb itself launched as a child can
  always be interrupted this way):**

  ```
  H5_nanosleep                                          [src/H5system.c:888]
  H5_retry_next                                          [src/H5retry_private.h:100]
  H5FD__swmr_reader_open                                 [src/H5FDvfd_swmr.c:528]
  H5FD__vfd_swmr_open  →  H5FD_open  →  H5F_open
  H5F__efc_open_file  →  H5F__efc_open                   [External File Cache]
  H5F_prefix_open_file(prefix_type = H5F_PREFIX_VDS)
  H5D__virtual_open_source_dset                          [src/H5Dvirtual.c:1410]
  H5D__virtual_set_extent_unlim                           [src/H5Dvirtual.c:1959]
  H5D__get_space  →  H5Dget_space
  verify_extensible_dset                                  [test/vfd_swmr_bigset_writer.c:2031]
  ```

  This test's `-V` scenario (`vds_single`) is a **self-referencing VDS**: the virtual dataset's
  source dataset lives in the *same* HDF5 file the reader already has open. To answer
  `H5Dget_space()` on that virtual dataset, the library must re-resolve the source dataset's
  current extent, which means (re-)opening the source file through the External File Cache — even
  though it's the exact same file the reader already has open. That EFC lookup does **not** get
  satisfied from cache; it falls through to a brand-new, independent `H5FD__vfd_swmr_open()`,
  which — because this is a VFD-SWMR reader open — tries to open the **shadow metadata file**
  (`<file>.md`) fresh, via a plain `open(path, O_RDONLY)` syscall (not the reader's own
  already-open, still-valid-despite-unlinked file descriptor). By the time the reader's
  per-dataset verification loop reaches this call, the writer — which for this whole test family
  used the same fixed, non-adaptive `notify_and_wait_for_reader()` handshake already documented
  above for `zoo`, with **zero further coordination once that handshake completes** ("Once the
  reader starts to verify the datasets, it doesn't notify the writer any info. Both the reader and
  writer finish by themselves.") — has already independently finished its own small, fixed `-n 25`
  workload, called `H5Fclose()`, and **unlinked the shadow file from the directory.** The fresh
  `open()` call on that now-nonexistent path returns `ENOENT`, forever; it will never reappear.
  `H5FD__swmr_reader_open()`'s retry loop (`H5FD_VFD_SWMR_MD_FILE_RETRY_MAX = 50` tries,
  `H5_RETRY_DEFAULT_MINIVAL`/`MAXIVAL` = 100ms–1s exponential backoff, `src/H5retry_private.h`) is
  *bounded* — roughly 46.5s worst case per call — but empirically the reader sat with zero new log
  output well past 150s in a from-scratch repro, meaning the real total is some multiple of that
  bound (plausibly one bounded retry-and-fail cycle per virtual-dataset source mapping touched,
  not just one). **This lines up exactly with an artifact already captured earlier this session and
  not previously explained: a full `test_vfd_swmr.sh few_big many_small` run hit the shell's own
  15-minute-per-scenario budget and was killed with `exit: 124` at precisely `-d 1 -V`** — i.e. the
  hang is not literally infinite, it is bounded-but-very-long, and long enough that any realistic
  test-runner timeout kills it first.
- **This design is byte-for-byte identical in the reference** (`git show
  05b54b7046:test/vfd_swmr_bigset_writer.c` has the exact same comment, same protocol, same
  self-referencing VDS pattern) — confirmed by building and running the reference against the
  identical `-d 1 -V` scenario: **it passed, both writer and reader exiting 0, well within a few
  seconds.** So this is a genuine **timing race, not a code or algorithm difference**: nothing in
  the writer/reader protocol *guarantees* the reader finishes verifying every dataset — including
  ones that require a fresh VDS-source re-open — before the writer's independently-scheduled close
  tears down the shadow file out from under it. The reference's reader evidently reaches this
  VDS-source-reopen point fast enough, relative to the writer's fixed workload, to win that race
  every time; the port's reader, for reasons not yet isolated (possibly the extra bookkeeping added
  by this session's own fixes — the `H5open()` call, the EOT-queue nrefs gate, etc. — or some
  pre-existing difference elsewhere in the API-entry path), evidently does not always win it. This
  is *not* the skip-list-vs-hash-table performance gap: neither process is CPU-bound during the
  hang, so no per-lookup complexity difference is at work here. "Next steps" item 3's page-buffer
  performance question remains open on its own merits, but this VDS hang is not evidence for it and
  should not be cited as such.
**Fix, attempt 1 (abandoned): a third int handshake over the existing socket.** Mirroring
`notify_and_wait_for_reader()`/`reader_check_time_and_notify_writer()` exactly — reader sends an
int after `verify_dsets()`, writer `recv()`s it before proceeding to close — seemed like the
obvious fix. It failed immediately with `expected 3 but read 2`: the socket already carries a
separate, differently-sized `exchange_info_t` per-tick protocol from `write_dsets()`/
`verify_dsets()`, and a fixed-size `int` `recv()` isn't guaranteed to land on that stream's actual
message boundaries. Abandoned in favor of a channel fully decoupled from that stream.

**Fix, attempt 2 (abandoned): a file-based signal (`h5_send_message()`/`h5_wait_message()`),
inserted right after `write_dsets()`.** The reader drops a `VFD_SWMR_READER_DONE_MESSAGE` file
after `verify_dsets()` returns; the writer polls for it before closing. This avoided the
socket-desync problem but introduced a *new* deadlock: `write_dsets()` doesn't force a final
end-of-tick after its very last write — that has always happened as an incidental side effect of
whatever HDF5 API call came next. `h5_wait_message()` only polls the filesystem and makes no HDF5
calls, so with the writer blocked immediately after `write_dsets()`, the last write's tick was
never closed out and never published. The writer's `h5_wait_message()` call eventually hit its own
built-in 300s `MESSAGE_TIMEOUT` and failed.

**Fix, attempt 3 (abandoned, same insertion point): same file-based signal, but the writer pings a
harmless API call on every poll.** `wait_for_reader_done()` polled for the done-file exactly like
attempt 2, but called `H5Aexists(s->file[0], "nonexistent")` between polls — mirroring
`notify_and_wait_for_reader()`'s existing tick-pinging idiom — so the writer's own end-of-tick
machinery kept advancing while it waited. This resolved the timeout from attempt 2, and the reader
now reached a *different*, later failure (`repeat_verify_chunk()`, reading actual chunk data —
every element came back as `0`). A **bounded** variant (capping the ping to `max_lag + 1`
iterations, suspecting unbounded pinging over-advanced the writer's ticks) was tried next when this
turned out to also break a **plain non-VDS** scenario (`few_big many_small -d 1`, no VDS at all) —
but bounding the ping didn't help either: direct isolation showed the identical `-n 25 -d 1 -s 50`
scenario passes cleanly in ~35–41s with no wait, fails at ~57s with unbounded ping, and **still
fails at ~63s with bounded ping**. Tracing `tick_num` directly on both processes (temporary
diagnostics in `H5F_vfd_swmr_writer_end_of_tick()`/`H5F_vfd_swmr_reader_end_of_tick()`,
`src/H5Fvfd_swmr.c`) found the actual mechanism: with the wait placed right after `write_dsets()`,
**both processes' `tick_num` froze simultaneously and stayed frozen for the rest of the run** (60+
seconds straight, confirmed via repeated `xxd` dumps of the shadow file's on-disk header showing
the identical, unchanging `tick_num` the entire time) — a genuine **circular dependency**: the
writer's own `close_extensible_dset()` loop (called once per dataset, ~10–100 times depending on
scenario) is what naturally generates the additional end-of-tick advances the reader's chunk
verification depends on to see fully-published data, and placing the wait *before* that loop meant
the writer would never reach it until the reader finished — but the reader could never finish
without those ticks. Neither side could make progress; the reader's own retry budget eventually
gave up.

**Fix, attempt 4 — this is what shipped, and is verified working with no regressions: keep the
same file-based signal, but move it to *after* the dataset-closing loop.**
`wait_for_reader_done()` (`test/vfd_swmr_bigset_writer.c`) is now called after
`close_extensible_dset()` (for every dataset) and `H5Pclose(fcpl)` have already run, and right
before `state_destroy()` (which performs the actual `H5Fclose()`/shadow-file unlink) — for the
writer only, gated on `s->writer`. This lets the writer's natural, pre-existing sequence of
per-dataset closes generate exactly the tick advancement the reader has always depended on (no
circular wait), while still deferring the one operation that actually matters — the final
`H5Fclose()` that unlinks the shadow file — until the reader has signaled it's done. Verified
clean on all fronts:
- The exact non-VDS scenario that attempt 3 (both variants) regressed now passes in ~36s, matching
  the no-wait baseline, with zero errors.
- The original `-d 1 -V` VDS hang scenario also passes cleanly in ~36s (both writer and reader exit
  0, no errors) — confirming the reordering doesn't reintroduce the original race.
- The full `test_vfd_swmr.sh few_big many_small` acceptance run gets past **every** `many_small`
  option variant, including both VDS ones (`-V`, `-M`) and their `-F` combinations, with no
  failures and no hangs.
- Full `ctest` regression suite: **100% passed, 0 failed out of 2726**, run twice after this fix
  landed — no regressions anywhere else in the library.

**The original shadow-file-unlink hang is fixed.** `test/vfd_swmr_bigset_writer.c`'s diff versus
the pre-this-session baseline is now just this reordered `wait_for_reader_done()` plus the earlier,
independently-verified `H5open()` and `WRITER_MESSAGE` fixes — nothing else remains from the three
abandoned attempts.

---

### A second, distinct, real bug surfaced by the fix above: a permanently-pinned v2 B-tree header during VFD SWMR reader-side cache eviction (`few_big -d 2`, 2D chunk growth) — root-caused precisely, then FIXED (this section describes the original diagnosis; the fix follows below in "The fix, implemented and verified")

With the hang genuinely fixed, running the *full* `few_big`/`many_small` acceptance test (not just
the scenarios already known to be affected by the hang) surfaced a **new, separate failure** at
`few_big`'s third option variant, `-d 2 -l 10` (2D dataset growth, large 256×256 chunks — 2D growth
requires a v2 B-tree chunk index, unlike 1D growth's extensible array, which never hits this):

```
H5Drefresh(): error processing EOT queue
  H5F_vfd_swmr_process_eot_queue(): end of tick error for VFD SWMR reader
    H5F_vfd_swmr_reader_end_of_tick() [src/H5Fvfd_swmr.c:1402]: evict or refresh stale MDC entries failed
      H5C_evict_or_refresh_all_entries_in_page() [src/H5C.c]: can't evict pinned and tagged entries
        H5C_evict_tagged_entries() [src/H5Ctag.c]: Pinned entries still need evicted?!
```

**Confirmed real and reproducible in clean isolation** (`vfd_swmr_bigset_writer`/`_reader -n 25 -d 2
-l 10 -s 10 -e 8 -r 256 -c 256 -q`, no other processes running, fails identically every time within
~40s). **Confirmed genuinely port-independent — i.e. not caused by anything in this session's
work**: the reference (`lifeboat/feature/vfd_swmr`, built at `/home/brtnfld/work/lifeboat-build`)
runs the *identical* scenario cleanly, no errors, in well under a minute.

**Root cause (confirmed via live gdb against the actual repro, not just static reading):** the
v2 B-tree header that stays permanently pinned is pinned by a **reference count**, not a flush
dependency. `H5B2__hdr_incr()` (`src/H5B2hdr.c:342-361`) pins the header on every `H5B2_t` open via
`H5AC_pin_protected_entry()`; for a dataset's chunk index, `H5D__bt2_idx_open()`
(`src/H5Dbtree2.c:747-785`) opens this handle once and caches it in
`layout.storage.u.chunk.u.btree2.bt2`, and it is **never closed until the dataset itself closes**
(`H5D__bt2_idx_close()`, `src/H5Dbtree2.c:798-815`) — so for the entire run, while the reader keeps
its datasets open (which `verify_dsets()` does throughout), the header's reference count never
drops to 0. `H5C_evict_or_refresh_all_entries_in_page()`'s only remedy for a pinned+tagged entry is
the "evict everything sharing this tag" sweep (`H5C_evict_tagged_entries()`,
`src/H5C.c:708-721`) — but that sweep only *unpins* entries that were pinned via a flush-dependency
relationship to a sibling entry that itself gets evicted (confirmed empirically: of the 6 leaf
nodes + 1 internal node + the header sharing this tag, every single one — including the header —
has `fd_nchildren=0`/`fd_nparents=0`, i.e. **no flush dependency exists at all** for this scenario,
since `H5B2cache.c` only creates one when `hdr->swmr_write` is true, which requires the *classic*
`H5F_ACC_SWMR_WRITE` flag that this VFD-SWMR-only test never sets). The leaves and internal node
get evicted fine; only the rc-pinned header cannot, and the sweep gives up and errors out.

**This is a latent, pre-existing gap in mainline HDF5's cache-eviction design, not a bug introduced
by this port.** `H5B2hdr.c`, `H5Dbtree2.c`, `H5C.c`, and `H5Ctag.c` are byte-for-byte identical in
logic between this branch and the reference (confirmed via direct diff — only cosmetic
`HDassert`→`assert`/`TRUE`→`true` style differences). The one mechanism designed to handle exactly
this case — a per-type `refresh` callback that re-reads an entry's on-disk image in place without
needing to evict it (`src/H5C.c`, gated on `entry_ptr->type->refresh != NULL`) — was fully
implemented and wired up, but until this session **no cache client except the superblock actually
registered one**. (Whether the reference implementation ever hits this same failure under some
workload was never established: it passes cleanly at `-d 2 -l 10`, and an attempt to test longer
reference runs was inconclusive due to a disk-quota limit on the test machine. The only firmly
established fact is that the mechanism's *code* is identical between the two trees, so the
mechanism is not port-introduced. An earlier version of this document asserted a specific reason
the reference "avoids" the failure; that was an unproven hypothesis and has been removed.)

**Not a safe scenario to just "ignore the pin and move on":** the v2 B-tree header's on-disk bytes
track the root node address, depth, and record count — if the tree restructures (a node split
moving the root, or a merge reducing depth), a reader holding a stale header would use the wrong
root/depth and could genuinely fail to find newly-written chunks, or index its depth-derived node
tables out of bounds. Silently suppressing the eviction error would trade a loud failure for a
silent correctness bug of exactly the kind this whole VDS investigation already spent significant
effort chasing down once.

### The fix, implemented and verified

The architecturally-correct fix was chosen and implemented: **give the v2 B-tree header cache
class its own `refresh` callback**, following the single existing precedent (the superblock's
`H5F__cache_superblock_refresh()` added earlier this session). This fixes the problem at the
cache-class level, so it covers *every* v2-B-tree-backed structure (chunk indices, dense group
links, dense attribute storage, shared message indexes), not just this one dataset scenario.

It turned into three coordinated changes:

**1. The refresh callback itself (`src/H5B2cache.c`, `src/H5B2hdr.c`, `src/H5B2pkg.h`).**
`H5B2__cache_hdr_refresh()` decodes a freshly re-read header image in place into the live, pinned
`H5B2_hdr_t` — asserting/erroring on the fields that can never legitimately change post-creation
(node_size, rrec_size, split/merge percent, class id — same defensive posture
`H5B2__cache_hdr_deserialize()` takes, since these bytes come from a less-trusted shadow file), and
always updating the two that do change as the writer inserts records: the root node pointer and the
tree depth. It is registered in `H5AC_BT2_HDR[1]`'s previously-`NULL` `refresh` slot.
   - The tricky part is depth: `hdr->node_info[]`/`hdr->nat_off[]` are **depth-derived tables not
     stored on disk**, built once by `H5B2__hdr_init()`. If the writer's tree deepened between
     reader ticks (a root split), a refresh that only bumped `hdr->depth` without growing
     `node_info[]` would let later traversals index out of bounds. So the shared build/teardown
     logic was extracted from `H5B2__hdr_init()`/`H5B2__hdr_free()` into reusable helpers
     (`H5B2__hdr_compute_node_info()` / `H5B2__hdr_free_node_info()`), and a new
     `H5B2__hdr_extend_node_info()` grows `node_info[]` on a depth increase.
   - **Grow-only, never free-and-rebuild.** An initial version that freed the old tables and
     rebuilt them from scratch failed immediately the first time a real depth change was exercised,
     with `"factory still has objects allocated"` — because each `node_info[u]` level owns free-list
     factories (`nat_rec_fac`/`node_ptr_fac`) that can still have live allocations backing
     leaf/internal node cache entries resident in the reader's cache. Since a level's `node_info`
     value depends only on the (invariant) node_size/rrec_size and the level index, existing levels
     never need recomputing — only new ones appended. `H5B2__hdr_extend_node_info()` therefore
     `H5FL_SEQ_REALLOC`s the array and only initializes the newly-added levels.

**2. A new `node_info_depth_alloc` high-water-mark field (`src/H5B2pkg.h`, and maintained in
`src/H5B2hdr.c`, `src/H5B2int.c`, `src/H5B2.c`).** On the *reader*, when the writer's tree
logically shrinks (depth decreases), the reader cannot safely free the now-higher `node_info[]`
levels' factories — cache entries may still reference them — so `node_info[]` keeps a high-water
number of allocated levels that can exceed the current `depth`. `H5B2__hdr_free_node_info()`
therefore frees based on `node_info_depth_alloc`, not `depth`.
   - **This field must be maintained everywhere depth changes, or it silently breaks normal
     non-SWMR B-trees.** The first version only set it in the refresh path, which caused a
     **double-free / heap corruption regression in four unrelated, non-SWMR tests** (`page_buffer`,
     `unlink`, `swmr`, `del_many_dense_attrs`): the normal write path already frees each top level's
     factories *as it retracts the tree* (`H5B2.c`, the two `depth_decreased` sites) and relies on
     the free loop tracking the current `depth`; a stale high-water mark made teardown re-free the
     already-freed top levels (whose pointers retraction leaves dangling). Fix: maintain
     `node_info_depth_alloc` in lockstep at `H5B2__split_root()` (grows it) and both retraction
     sites (shrink it), so for the writer it always equals `depth`, and only the reader-refresh path
     ever lets it exceed `depth`. The two mechanisms never run on the same header (writers never
     refresh, readers never split/retract).

**3. Teaching the tagged-eviction sweep to honor a refresh callback too (`src/H5C.c`,
`src/H5Ctag.c`, `src/H5AC.c`, `src/H5Cprivate.h`, `src/H5Cpkg.h`).** Registering the callback in
step 1 was necessary but *not sufficient*: `H5C_evict_or_refresh_all_entries_in_page()`'s top-level
dispatch only takes the refresh branch when the *stale entry it lands on directly* is the
refresh-capable one. But the pinned header shares a tag with its leaf/internal nodes, so the
"evict everything with this tag" sledgehammer (`H5C_evict_tagged_entries()`) can still be entered
via one of *those* (which have no refresh callback) and then hit the pinned header inside its
per-entry callback — reproducing the exact same `"Pinned entries still need evicted?!"` failure a
few ticks later. Fixed by extracting the refresh-in-place logic into a reusable `H5C__refresh_entry()`
and having `H5C__evict_tagged_entries_cb()` call it for a pinned entry that has a refresh callback,
instead of flagging it unevictable. `H5C_evict_tagged_entries()` gained two parameters
(`do_refresh`, `tick`); the VFD-SWMR end-of-tick caller passes `true`, while the general-purpose
`H5AC_evict_tagged_metadata()` caller passes `false` (a genuinely pinned entry there is still a
real error).

**Verification:**
- **Direct repro:** `vfd_swmr_bigset_writer`/`_reader -d 2 -l 10` (the previously 100%-failing
  scenario) now passes **10/10 consecutive runs** at the real node size. The depth-change rebuild
  path was specifically exercised and confirmed correct by temporarily shrinking
  `H5D_BT2_NODE_SIZE` (reverted afterward) to force a root split during the run.
- **Full ctest:** 100%, 0 failures across all 2804 tests (the entire suite minus the one slow
  `test_vfd_swmr` shell script, which is timed separately). This specifically confirms the four
  regressions above are fixed and that editing the core B-tree write paths (`split_root`,
  retraction) introduced no new failures.
- **Valgrind:** `--leak-check=full --show-leak-kinds=all` on the `-d 2` reader (with the shrunk
  node size to exercise the grow path) reports **0 errors, 0 leaks** — no leak or double-free in
  the extend/free/realloc logic, which runs once per stale tick.
- Diagnostics added while developing (temporary `fprintf`s in `H5Ctag.c`/`H5B2cache.c` and the
  `H5D_BT2_NODE_SIZE` shrink) were all removed/reverted.

**Bonus leak fixed along the way (separate, pre-existing, not B-tree-related).** The valgrind pass
also surfaced a 4.6 MB leak in the VFD-SWMR **reader** shadow-index arrays (`mdf_idx`/`old_mdf_idx`,
allocated by `H5F__vfd_swmr_create_index()`): the writer-close path frees them (added earlier this
session in `H5F_vfd_swmr_close_or_flush()`), but that path is writer-only, and nothing freed the
reader's copies. Confirmed present even at `-d 1` (extensible array, no v2 B-tree at all), i.e.
entirely independent of the B-tree fix. Completed the earlier fix by freeing both arrays in the
reader+writer-common close block in `src/H5Fint.c`; valgrind is now fully clean for both.

---

## Test infrastructure — wired for the first time this session

**Before this session, VFD SWMR had zero test coverage reachable from any build system.** The
~29 VFD SWMR test executables (`vfd_swmr_generator`, `vfd_swmr_writer`/`_reader`,
`vfd_swmr_zoo_writer`/`_reader`, `vfd_swmr_group_writer`/`_reader`, etc.) and the acceptance-test
shell driver `test_vfd_swmr.sh.in` existed as source files but were **never wired into CMake** —
even on the original feature branch, that CMake wiring was commented out; the only working build
system for these tests was **autotools**, which `develop` has since dropped entirely. So this
gap wasn't a port oversight — it was a first-time CMake port of test infrastructure that had
only ever worked under a build system that no longer exists.

### What was added
- **`utils/test/vfd_swmr_check_compat_vfd.c`** (new file, ported from the feature branch, in
  develop's modern coding style) — the "does this VFD support SWMR" prerequisite check the shell
  script gates on. Wired into `utils/test/CMakeLists.txt`.
- **`test/CMakeLists.txt`** — `H5_VFD_SWMR_TESTS` list (single-source executables via the existing
  `ADD_H5_EXE` macro) plus a new `ADD_H5_VFD_SWMR_EXE_SRC` macro for the executables that share a
  source file with a sibling (e.g. `vfd_swmr_group_reader` and `vfd_swmr_group_writer` are both
  compiled from `vfd_swmr_group_writer.c` — the program picks its role at runtime by inspecting
  `argv[0]`'s basename via `H5_basename()` + `strstr()`, e.g. checking for
  `"vfd_swmr_group_writer"` vs `"vfd_swmr_group_reader"`; this is why the shared source pattern
  works with two separate `add_executable()` targets).
- **`test/ShellTests.cmake`** — `configure_file` + `add_test(H5SHELL-test_vfd_swmr ...)`, mirroring
  the exact existing pattern for `H5SHELL-test_swmr`/`H5SHELL-test_vds_swmr`.
- **HD-prefix removal** across all ~21 affected test `.c` files (`HDassert`→`assert`,
  `HDfprintf`→`fprintf`, `HDcalloc`→`calloc`, etc. — 39 distinct identifiers) — these files were
  written against the feature branch's old HD-prefixed libc-wrapper convention, which `develop`
  dropped entirely.
- **`H5private.h`-first include-order fix** in the 7 files affected by pre-existing bug #1 above.

### Known limitations — read before assuming any of this "just works"
- **11 of the 13 default acceptance-test scenarios are now verified passing** (see "Session N+1"
  below for the full fix history): `generator`, `expand`, `shrink`, `expand_shrink`, `sparse`,
  `vlstr_null`/`vlstr_oob` (expected-error tests, pass by correctly erroring), `zoo`, `groups`,
  `groups_ops`, and `groups_attrs` **(including `modify-vstr`, fixed in "Session N+2" — all
  variants now pass)** — each verified via direct, standalone invocation. **`few_big`/`many_small`
  (`vfd_swmr_bigset_writer`) remain genuinely untested to completion** — the full back-to-back
  script run hasn't yet reliably reached them within a 1200s budget (see "Performance comparison
  against the reference" above). Run them directly (`bash test_vfd_swmr.sh few_big many_small`, no
  ctest timeout) to verify in isolation.
- ~~One known-open bug remains: the `groups_attrs` "modify-vstr" sub-variant...~~ **Fixed in
  "Session N+2"** — root cause was `src/H5Fio.c` unconditionally treating global heap objects as
  raw data even under VFD SWMR, so they were never tick-published; see that section for the fix and
  a second, related mpmde tick-list bug it exposed and required fixing too.
- **The `H5SHELL-test_vfd_swmr` ctest entry currently runs the entire default scenario set with no
  per-scenario opt-out**, and now legitimately takes longer than `ctest`'s 1200-second default
  `CTEST_TEST_TIMEOUT` to complete end-to-end (previously moot, since it always hung/crashed well
  before that point). Either increase the timeout (`set_tests_properties(... PROPERTIES TIMEOUT
  ...)`, precedent exists elsewhere in this test suite for slow tests) or accept that this ctest
  entry alone won't show green in CI until that's done, even though the underlying scenarios pass.
- **Multi-page metadata entries (`is_mpmde`) are supported** (see "Bugs found and MPMDE support
  added in this session" above) and confirmed load-bearing: the reference's ~6-second `zoo`
  convergence depends on it, and this port now matches that.

---

## How to build and test

```bash
# Configure (this Linux dev machine; adjust for your platform — no CMake preset was used)
cmake -B ../hdf5_swmr_build \
  -DCMAKE_BUILD_TYPE=RelWithDebInfo -DCMAKE_C_COMPILER=clang \
  -DBUILD_SHARED_LIBS=ON -DHDF5_BUILD_CPP_LIB=OFF -DHDF5_BUILD_FORTRAN=OFF \
  -DHDF5_BUILD_JAVA=OFF -DHDF5_ENABLE_PARALLEL=OFF -DHDF5_BUILD_TOOLS=ON \
  -DHDF5_BUILD_EXAMPLES=OFF -DBUILD_TESTING=ON

cmake --build ../hdf5_swmr_build --parallel

# Full regression suite (all ~2737 tests should pass; H5SHELL-test_vfd_swmr alone may show
# "Timeout" under plain ctest -- every individual scenario passes standalone, but the full
# back-to-back run doesn't yet reliably fit in the 1200s default CTEST_TEST_TIMEOUT; see
# "Performance comparison against the reference" above for what's understood about why)
ctest --test-dir ../hdf5_swmr_build -j 16 --output-on-failure --timeout 120

# Just the VFD SWMR generator scenario (known-passing, fast, no synchronization needed)
cd ../hdf5_swmr_build/test/H5TEST
bash test_vfd_swmr.sh generator

# zoo now converges reliably (previously the long-standing blocker -- see "Session N+1" above)
timeout 90 bash test_vfd_swmr.sh zoo

# Run the whole scenario set directly, bypassing ctest's 1200s timeout, to verify everything
# including the two scenarios (few_big/many_small) never yet run to completion:
bash test_vfd_swmr.sh
```

**Before trusting any test result:** if you re-run the full suite after killing a hung
`test_vfd_swmr.sh` run, check for and clean up orphaned `vfd_swmr_test/` working directories and
stray writer/reader processes first (`ps aux | grep vfd_swmr`), or you'll get stale-file races
unrelated to anything real.

### Building the reference implementation for comparison

`lifeboat/feature/vfd_swmr` (commit `05b54b7046`) is the documented reference this port is based
on, and converges the zoo scenario reliably in ~6 seconds. Useful for direct A/B comparisons (both
timing and behavioral) against this port. It's a `git remote` in this repo already
(`lifeboat`); build it via autotools into a separate worktree, since it predates the CMake port
and `develop` has since dropped autotools entirely:

```bash
git fetch lifeboat feature/vfd_swmr --depth=1
git worktree add /path/to/lifeboat-worktree 05b54b7046   # detached HEAD at the reference commit

cd /path/to/lifeboat-worktree
./autogen.sh
mkdir /path/to/lifeboat-build && cd /path/to/lifeboat-build
/path/to/lifeboat-worktree/configure \
  --disable-fortran --disable-cxx --disable-parallel --disable-tools --disable-hl CC=clang
make -j"$(nproc)"

# zoo writer/reader binaries land at test/vfd_swmr_zoo_writer and test/vfd_swmr_zoo_reader;
# LD_LIBRARY_PATH needs src/.libs for the shared lib.
```

---

## Next steps (in rough priority order)

1. ~~Root-cause the VL-string-attribute/global-heap consistency bug~~ **Done — see "Session N+2"
   above.** Root cause was an unconditional `H5FD_MEM_GHEAP`→`H5FD_MEM_DRAW` remap in `src/H5Fio.c`
   that should only apply to non-VFD-SWMR files; fixing it exposed and required also fixing a real
   mpmde tick-list accounting bug in `H5PB__write_mpmde()`. Both fixed and verified (`modify-vstr`
   5/5 clean, full non-shell-test regression suite 2726/2726, `vfd_swmr_vlstr_writer -t oob`/`-t
   null` no longer crash).
2. ~~Root-cause the `expand_shrink` "Incorrect record value" data-consistency bug~~ **Done — see
   "Session N+2" above, "Root-caused and fixed the `expand_shrink` 'Incorrect record value' bug".**
   Root cause: VFD SWMR's shadow index operates at page granularity, and a page can hold *both*
   tracked metadata (e.g. a v1 B-tree group-link node) and untracked raw dataset-chunk bytes;
   publishing the metadata freezes a stale snapshot of the co-located raw data too, and a reader's
   raw-data read was incorrectly being redirected through that stale snapshot. **Fixed**
   (`src/H5FDvfd_swmr.c`, `H5FD__vfd_swmr_read()`): raw data (`H5FD_MEM_DRAW`) now always reads
   directly from the real file, never via the shadow index. Reduced the failure rate from ~1-in-4
   to a residual ~1-in-10 that was traced and confirmed to be a *different*, pre-existing,
   self-acknowledged test-tolerance gap (not a VFD SWMR bug) — see that section for the full
   distinction. Full regression suite 2726/2726 clean after the fix.
3. **Root-cause the skip-list-vs-hash-table page-buffer performance gap (Strategy B's known,
   accepted trade-off) — status unchanged, and now confirmed unrelated to the VDS hang.**
   A tightly-budgeted (3 runs each side) timed comparison of the `generator`→`zoo` prefix found the
   **port consistently ~27% *faster*** than the reference there (156.5s vs. 214.1s average) — so
   the original full-script slowdown, if still real, does not live in `zoo` or anything before it,
   contrary to how "Session N+1"/"Performance comparison against the reference" framed it. An
   intermediate version of this write-up also floated the `few_big`/`many_small` VDS hang (see
   "Session N+2" above) as a second data point for this same gap — **that link has since been
   disproven**: `/proc/PID/stat` sampling during the hang shows zero CPU consumption on both the
   writer and reader, and the hang has since been root-caused to an unrelated test-protocol timing
   race (see "Session N+2"'s VDS write-up for the full gdb-backtrace evidence). This item now rests
   solely on the `zoo` timing anomaly and remains genuinely open on its own. Whoever picks this up
   next should time the `groups`/`groups_attrs`/`groups_ops` suffix of the full script (still
   unmeasured on its own, tight-budget discipline: a handful of repeated runs, not one long
   unattended full-script run) rather than re-measuring the `zoo`-and-earlier prefix this session
   already covered, and should not expect fixing this to affect the VDS hang.
4. ~~Actually run `few_big`/`many_small` (`vfd_swmr_bigset_writer`) to completion~~ **Done — see
   "Session N+2" above.** Found and fixed four real, previously-unreached bugs blocking every
   attempt (a missing `H5open()` causing a datatype crash, a missing writer/reader launch
   synchronization causing an open race, a genuine library use-after-free in the VFD SWMR
   end-of-tick queue triggered by re-opening an already-open shared file — confirmed also present,
   byte-for-byte, in the reference, so a real standalone-PR candidate — and the shadow-file-unlink
   race itself, root-caused via a live gdb backtrace and fixed by reordering the writer's
   reader-done wait to *after* its dataset-closing loop, after three earlier attempts at the same
   fix were tried and abandoned). **All 11 `many_small` option variants pass cleanly, including
   both VDS ones (`-V`, `-M`)** — verified via the full acceptance test and a clean `ctest` run
   (2726/2726). A **second, distinct, pre-existing mainline bug** (unrelated to VDS, a permanently
   pinned v2 B-tree header during cache eviction) was found in `few_big -d 2`'s 2D chunk growth once
   the hang stopped masking it — see "Session N+2" above for the full root cause and two candidate
   fixes. The *code* implementing this bug (`H5B2hdr.c`, `H5Dbtree2.c`, `H5C.c`, `H5Ctag.c`) is
   confirmed byte-for-byte identical between this branch and the reference, so the mechanism itself
   is not port-introduced. Whether the reference actually *hits* this failure under any workload is
   **not confirmed** — the reference passes cleanly at `-d 2 -l 10` (the scenario that fails on this
   branch), and an attempt to test longer runs (`-l 40`, `-l 100`) to see if it's purely a
   sufficient-eviction-passes issue was inconclusive: both hit a per-user/cgroup disk quota
   (`EDQUOT`) within seconds on this machine and corrupted the file before producing a usable
   result. Do not treat "the reference avoids this" as established fact beyond the one tested
   scenario. **UPDATE: this bug is now FIXED** — see "The fix, implemented and verified" under
   "Session N+2" above and item 6 below. The chosen approach was the core-library `refresh`
   callback (candidate 1), not the force-close (candidate 2).
5. ~~**Decide `H5SHELL-test_vfd_swmr`'s ctest timeout.**~~ **RESOLVED — the "timeout" was never a
   performance problem.** Investigated after the `H5SHELL-test_vfd_swmr` ctest run appeared to hit
   the 1200s default timeout. Three distinct things were conflated; none was a real slowdown:
   - **The 1200s "timeout" was a *hang* from an incomplete intermediate state of the B-tree fix,
     not slow execution.** It was observed on a build that had the refresh callback and the
     `node_info_depth_alloc` field but *not yet* the tagged-eviction interaction fix or the
     `node_info_depth_alloc` regression fix — that combination could hang the full-script context.
     With the *complete* fix (item 6 below), the script runs to completion, no hang.
   - **Most VFD SWMR test binaries were never built in the build dir** — only 4 of ~28
     (`vfd_swmr`, `vfd_swmr_bigset_writer`/`_reader`, `vfd_swmr_check_compat_vfd`). Every other
     section (`generator`, `expand`/`shrink`, `sparse`, `vlstr`, `zoo`, `groups*`) was
     fast-failing on `"No such file or directory"`, which both failed the script and masked the
     real timing. They are all wired CMake targets — they just needed building
     (`cmake --build . --target vfd_swmr_generator vfd_swmr_writer …`). Once built, the full
     script **passes, 0 failures**, in **~427s** at express=1 (n=25) — already comfortably under
     ctest's 1200s default *and* under the reference's ~500s, while doing *more* work than the
     reference (see next point).
   - **`test/ShellTests.cmake` didn't forward `HDF5TestExpress`** to `H5SHELL-test_vfd_swmr` (nor
     to `test_swmr`/`test_vds_swmr`), so the script fell back to its default of 1 (n=25) instead of
     honoring the build's configured `HDF_TEST_EXPRESS` (=3, n=10) — i.e. it ran a *heavier* load
     than configured. The reference/autotools harness passes the level through. **Fixed:** added
     `HDF5TestExpress=${HDF_TEST_EXPRESS}` to the test's `ENVIRONMENT` property, plus a
     `TIMEOUT ${CTEST_VERY_LONG_TIMEOUT}` (3× default) to match the other legitimately-long tests
     (`H5TESTXPR-btree2`, `H5TEST-big`, `H5TEST-cache`) — not to paper over slowness (there is
     none), but because at `HDF_TEST_EXPRESS=0` (exhaustive, n=100) a genuine full run would exceed
     the 1200s default the same way those tests do. At the configured express=3 the test is fast.
   - **Follow-up: the scenarios are now individual ctest tests.** `test_vfd_swmr.sh` accepts a
     scenario name (`few_big`, `groups`, `zoo`, …), so `test/ShellTests.cmake` now registers each as
     its own `H5SHELL-test_vfd_swmr-<scenario>` ctest test (13 always-available + exhaustive-only
     ones gated on `HDF_TEST_EXPRESS=0`), replacing the single monolithic test, for granular CI
     reporting. They share a `RESOURCE_LOCK vfd_swmr_h5test_dir` because several communicate over a
     fixed socket port (`DEFAULT_PORT` = 42424 in `vfd_swmr_common.c`) and share the working dir /
     message filenames, so they must never run concurrently.

   **Caution for future stress testing (a rabbit hole worth not re-entering):** these socket-based
   tests use a *fixed* port with no writer/reader startup handshake, so they are fragile to being
   run **manually in rapid succession** — a `timeout`-killed manual run **orphans** the backgrounded
   `writer`/`reader` children (the shell parent dies, the `&` children don't), and an orphan holding
   port 42424 makes every subsequent run fail with `"error binding server socket"`. During this
   session that exact artifact produced a **false positive** — a manual `groups` run appeared to
   fail, and a same-machine reference run "confirmed" it, but **both were contaminated by one
   orphaned `vfd_swmr_group_writer` of ours holding the port**. Run through `ctest` (which tears down
   process groups cleanly) `groups` passes in ~20s, as does every scenario. Lesson: for manual
   repro of these tests, verify `ss -tan | grep :42424` is clear and `pkill -9 -f vfd_swmr` between
   runs; prefer driving them via `ctest` so teardown is handled. No real groups bug exists in either
   tree.
6. ~~**Fix the v2 B-tree header pinning bug**~~ **DONE — see "The fix, implemented and verified"
   under item 4 above.** Implemented the architecturally-correct fix (a `refresh` callback for the
   v2 B-tree header cache class), plus the two coordinated changes it required (maintaining the new
   `node_info_depth_alloc` high-water mark at all depth-mutation sites to avoid a double-free
   regression in normal non-SWMR B-trees, and teaching `H5C_evict_tagged_entries()`'s sweep to
   honor a refresh callback so the shared-tag path is covered too). Verified via 10/10 direct
   repro runs, full ctest (2804/2804 excluding the separately-timed shell test), and a clean
   valgrind pass. Since the underlying mechanism is confirmed byte-identical to mainline, this
   `refresh`-callback fix (and the reader shadow-index leak fix found alongside it) is a strong
   candidate to report upstream independent of this port.
7. Consider whether any of the 4 "Pre-existing develop bugs", 5 "Phase 3 bugs", 3 bugs from the
   MPMDE session, the 8 bugs from "Session N+1" (particularly #1–#5, which are general
   page-buffer/library correctness issues, not VFD-SWMR-specific), the 2 bugs from "Session N+2"'s
   first pass (the missing VFD-SWMR-raw-data-bypass rule and the mpmde `tl_size` accounting bug),
   or the 4 bugs from the `few_big`/`many_small` pass (the `H5open()` gap, the missing
   `WAIT_MESSAGE` sync, the EOT-queue-insert nrefs gate, and the VDS shadow-file-unlink race — the
   nrefs gate especially, since it's a generic library use-after-free/leak, not
   VFD-SWMR-test-specific) are worth splitting into standalone `develop` PRs, independent of this
   port.
8. **TODO: open a standalone PR for the v2 B-tree header refresh fix (+ the reader shadow-index
   leak fix found alongside it).** This is the highest-value PR candidate — a real, verified
   core-library fix whose buggy mechanism is byte-identical to mainline, so it stands on its own
   independent of the rest of this port. Commits on `feature/vfd-swmr-port`:
   `b606844abae` (fix) and `bdb592efbc5` (docs); the fix touches `src/H5B2cache.c`,
   `src/H5B2hdr.c`, `src/H5B2pkg.h`, `src/H5B2int.c`, `src/H5B2.c`, `src/H5C.c`, `src/H5Cpkg.h`,
   `src/H5Cprivate.h`, `src/H5Ctag.c`, `src/H5AC.c`, `src/H5Fint.c`.
   **Target still to be decided:** `origin/develop` *does* already contain the VFD SWMR machinery
   the fix depends on (`H5Fvfd_swmr.c`, the `H5C` refresh-callback path), so the fix can go either
   to this repo's `develop` or to the upstream branch it derives from — confirm which base is
   wanted before opening. Cherry-pick the two commits onto a fresh branch off the chosen base,
   confirm it builds + the `few_big -d 2` repro passes there, then open the PR.

---

## Key source locations

| What | File | Notes |
|------|------|-------|
| FAPL config ingestion, `H5F__new`/`H5F_open` reordering | `src/H5Fint.c` | Phase 0-pre/0a/0b |
| `H5F_VFD_SWMR_CONFIG` macros | `src/H5Fprivate.h` | Phase 0-pre |
| `VFD_SWMR_ENTER`/`LEAVE`, `eot_queue_t` relocation | `src/H5private.h` | Phase 0c |
| `H5FD_vfd_swmr_init()` registration fix | `src/H5FDvfd_swmr.c` | Bug #2 fix |
| `H5PB_t`/`H5PB_entry_t` VFD SWMR fields | `src/H5PBprivate.h`, `src/H5PBpkg.h` (struct) | Phase 3 |
| TL/DWL wrapper macros | `src/H5PBpkg.h`, right before the fenced `#if 0` block | Phase 3 |
| `H5PB__vfd_swmr_track_write`, the four `H5PB_vfd_swmr__*` functions | `src/H5PB.c` | Phase 3 |
| `page_entry->size` init (3 sites), `image_ptr`→`page_buf_ptr` fix, `H5PB__make_space` NULL guard, `H5PB__dest_cb` DWL-unlink fix, tick-list LRU protection | `src/H5PB.c` | zoo debugging pass bugs #1–#5 |
| Stale `end_of_tick` fix | `src/H5Fvfd_swmr.c`, `H5F_vfd_swmr_reader_end_of_tick()` | This session, bug #1 |
| EOA-check leak fix (2 sites) | `src/H5PB.c`, `H5PB_read()`/`H5PB_write()` | This session, bug #2 |
| Untracked-write bypass fix | `src/H5PB.c`, `H5PB_write()`'s "not found, make space" branch | This session, bug #3 |
| `mpmde_count` field | `src/H5PBprivate.h`, `H5PB_t` | This session, MPMDE support |
| `H5PB__write_mpmde()`, intercept in `H5PB_write()`, `is_mpmde` guards in track_write/release_tick_list/release_delayed_writes/dest_cb/remove_entry, `H5PB__write_entry()` size fix | `src/H5PB.c` | This session, MPMDE support |
| `H5C__INSERT_IN_INDEX`/`DELETE_FROM_INDEX` (page_index) | `src/H5Cpkg.h` | Phase 1 (pre-existing) |
| `H5F_vfd_swmr_reader_end_of_tick` | `src/H5Fvfd_swmr.c` | Phase 2 (pre-existing) |
| VFD SWMR test executables + shell driver wiring | `test/CMakeLists.txt`, `test/ShellTests.cmake`, `utils/test/CMakeLists.txt` | New this session |
| Design analysis: skip-list vs hash-table decision | `docs/H5PB_index_design_analysis.md` | Full research record |
| Early `vfd_swmr_reader` flag set (before superblock load) | `src/H5AC.c`, `H5AC_create()` | Session N+1, zoo root cause fix |
| `H5F__cache_superblock_refresh()` + 15th `H5AC_SUPERBLOCK` field | `src/H5Fsuper_cache.c` | Session N+1, zoo root cause fix |
| `H5PB_read()` "not found" branch EOA exemption | `src/H5PB.c` | Session N+1, defensive fix (bug list item after root cause) |
| `sigtimedwait` CMake detection | `config/ConfigureChecks.cmake`, `src/H5pubconf.h.in` | Session N+1, bug #1 |
| LRU/DWL shared-field guard (6 sites: `track_write`, `update_entry`, `H5PB_read()` ×3, `write_mpmde`, `H5PB_write()`) | `src/H5PB.c` | Session N+1, bug #2 |
| `TAILQ_INIT(&f->shared->shadow_defrees)` | `src/H5Fint.c` | Session N+1, bug #3 |
| `H5F_open()` status_flags VFD-SWMR exemption (both sides) | `src/H5Fint.c` | Session N+1, bugs #4–5 |
| Explicit `H5open()` in `state_init()` | `test/vfd_swmr_group_writer.c` | Session N+1, bug #6 |
| `H5PB_remove_entry()` tick-list/DWL unlink before free | `src/H5PB.c` | Session N+1, bug #7 |
| `astr_val = NULL` initialization (both variants) | `test/vfd_swmr_group_writer.c`, `verify_group_vlstr_attr()` | Session N+1, bug #8 (crash fix; underlying bug root-caused and fixed in Session N+2) |
| Conditional `H5FD_MEM_GHEAP`→`H5FD_MEM_DRAW` remap (4 sites: `H5F_shared_block_read/write`, `H5F_block_read/write`) | `src/H5Fio.c` | Session N+2, root cause fix for bug #8 |
| Removed stale `assert(type != H5FD_MEM_GHEAP)` | `src/H5PB.c`, `H5PB_read()` | Session N+2, follow-on to the `H5Fio.c` fix |
| VFD-SWMR raw-data page-buffer bypass (`page_buf->vfd_swmr && H5FD_MEM_DRAW == type`) | `src/H5PB.c`, `H5PB_read()`/`H5PB_write()` | Session N+2, independent reference-parity fix |
| mpmde tick-list `tl_size` growth-delta accounting | `src/H5PB.c`, `H5PB__write_mpmde()`'s "existing entry must grow" branch | Session N+2, new bug exposed by the `H5Fio.c` fix |
| Raw-data shadow-index bypass on read (`H5FD_MEM_DRAW` skips the index lookup) | `src/H5FDvfd_swmr.c`, `H5FD__vfd_swmr_read()` | Session N+2, `expand_shrink` "Incorrect record value" fix |
| Explicit `H5open()` in `state_init()` | `test/vfd_swmr_bigset_writer.c` | Session N+2, `few_big`/`many_small` bug #1 |
| `WRITER_MESSAGE` signal + `WAIT_MESSAGE` sync (writer + script, 2 sections) | `test/vfd_swmr_bigset_writer.c` (`main()`), `test/test_vfd_swmr.sh.in` (`many_small`/`few_big` sections) | Session N+2, `few_big`/`many_small` bug #2 |
| `H5F_vfd_swmr_insert_entry_eot()` gated on `nrefs == 1` | `src/H5Fint.c`, `H5F_open()` | Session N+2, `few_big`/`many_small` bug #3 (generic library UAF/leak) |
| Shadow-index array free at writer close (`mdf_idx`/`old_mdf_idx`) | `src/H5Fvfd_swmr.c`, `H5F_vfd_swmr_close_or_flush()` | Session N+2, `few_big`/`many_small` bug #3 (companion 2.3MB leak) |
