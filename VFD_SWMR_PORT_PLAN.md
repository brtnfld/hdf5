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
| 3 | Writer machinery: implement the four stub `H5PB_vfd_swmr__*` functions + write-path wiring on the **skip-list** page buffer | **Implementation done; end-to-end validation pending** — see "Known limitations" below |

**Validation state:** full regression suite (~2727 tests) passes clean at 2726/2727 after every
phase above. The single known failure (`H5SHELL-test_vfd_swmr`, the `zoo` scenario) is a
pre-existing test-harness synchronization gap unrelated to the phases — see "Known limitations."

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
- **Only 2 of the ~13 default acceptance-test scenarios have actually been run:**
  `generator` (single-process, writer-only) **passes cleanly**, including its VFD-SWMR-write
  variant. `zoo` (writer + reader) **still fails, but is now precisely characterized** — see
  below and the "Phase 3 bugs found in the zoo end-to-end debugging pass" section above. The
  other 11 (`expand`, `shrink`, `expand_shrink`, `sparse`, `vlstr_null`, `vlstr_oob`, `groups`,
  `groups_attrs`, `groups_ops`, `few_big`, `many_small`) are **untested**. Don't assume they pass.
- **The `zoo` scenario's original writer/reader launch race (zero synchronization) is fixed.**
  `test_vfd_swmr.sh.in`'s `zoo` block now does a `WAIT_MESSAGE`/`h5_send_message` handshake
  (mirroring the pattern already used by `expand`/`shrink`/`sparse`), placed *after* the writer's
  first manual tick (not right after `H5Fcreate`, which still races the flush). The writer and
  reader now correctly reach the socket handshake and `create_zoo`/`validate_zoo` every run.
- **The real remaining blocker, as of this session, is a reproducible metadata corruption bug in
  reading back the dense group's fractal heap indirect block** (selector 2's `H5C__load_entry`
  failing its own internal checksum "after all read attempts," confirmed over 26+ million retries
  with zero successes) — see "Follow-up 2" in the section above for the full trace. This
  supersedes the "just slow" framing from earlier in this same session: selector 0 (empty group)
  succeeds fast and reliably every time tested; the earlier-observed slow/variable overall
  convergence was this one selector never succeeding, not a broad performance gap. This also
  means: **do not assume the other 11 untested scenarios will pass** — any scenario exercising a
  dense group or similarly large fractal-heap-backed structure likely hits the same bug; smaller
  scenarios may be unaffected.
- **The `H5SHELL-test_vfd_swmr` ctest entry currently runs the entire default scenario set with no
  per-scenario opt-out.** As long as any one scenario fails, the whole entry reports failed. If
  you want to merge this branch (or just this test wiring) before all scenarios are proven out,
  either mark the ctest entry `DISABLED` (precedent exists elsewhere in this test suite) or pass a
  restricted scenario argument (e.g. just `generator`) until more are verified.
- **Multi-page metadata entries (`is_mpmde`) are now supported** (see "Bugs found and MPMDE
  support added in this session" above) — implemented and confirmed via tracing to correctly
  publish the large writes it targets. This alone was not sufficient to make `zoo` converge; see
  the root-group blocker above.

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

# Full regression suite (expect 2726/2727; the one failure is the known zoo timing issue)
ctest --test-dir ../hdf5_swmr_build -j 16 --output-on-failure --timeout 120

# Just the VFD SWMR generator scenario (known-passing, fast, no synchronization needed)
cd ../hdf5_swmr_build/test/H5TEST
bash test_vfd_swmr.sh generator

# The zoo scenario (known to fail on a timing budget mismatch, not a crash or race --
# see "The remaining zoo failure is a timing-budget mismatch, not a bug" above)
timeout 90 bash test_vfd_swmr.sh zoo
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

1. **Root-cause the fractal heap indirect block checksum failure** (see "Follow-up 2"/"Follow-up
   3" above — `H5C__load_entry(): incorrect metadata checksum after all read attempts`,
   reproducible on every attempt to validate selector 2, the dense group). **Already ruled out:**
   the failing entry (`addr=7712, size=149`) is confirmed *not* MPMDE-sized — this is not a gap in
   the MPMDE work. Suggested starting points, in order:
   - **Multiple structures sharing one page-buffer page**: `addr=7712` falls in HDF5 page 1
     (4096–8191), which earlier tracing also showed receiving several distinct v2 B-tree node
     writes (`4096, 4608, 5120, 5632, 6144, 6656, 7168`) immediately before it. Check whether
     writes to different byte ranges within the same shared page-buffer entry correctly preserve
     each other's content — particularly across tick boundaries, or if the entry is ever evicted
     and reloaded between writes to different structures within it.
   - **Compare exact bytes published vs. read back**: dump both sides' images and diff (or add a
     targeted trace at `H5F_update_vfd_swmr_metadata_file()`'s checksum computation and
     `H5FD__vfd_swmr_read()`'s verification) to find precisely where the content diverges — since
     `H5C__load_entry`'s own retry loop already rules out a merely-transient torn read; this is a
     genuine, reproducible mismatch happening the same way every time.
   - `H5FD_MEM_FHEAP_IBLOCK` is a `#define` alias for `H5FD_MEM_OHDR` (`src/H5FDdevelop.h`) — worth
     keeping in mind that any trace filtering on "type == OHDR" catches both ordinary object
     headers and fractal heap indirect blocks; disambiguate by address/size if needed.
2. Once fixed, **re-verify `zoo` convergence against the reference's ~6-second baseline over
   several repeated runs** — a single successful run is not enough, given this session's evidence
   that even selector 0 alone succeeds reliably while selector 2 can fail arbitrarily many times.
3. **Run the other 11 default scenarios** and fix whatever surfaces, once `zoo` itself converges
   reliably — expect scenarios with large/dense structures to be most likely to hit a similar bug.
4. **Decide on `H5SHELL-test_vfd_swmr`'s CI posture** if this needs to merge before all scenarios
   pass (see "Known limitations" above for the options).
5. Consider whether any of the 4 "Pre-existing develop bugs", 5 "Phase 3 bugs", or 3 bugs fixed
   this session are worth splitting into standalone `develop` PRs, independent of this port.

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
