# Plan — Complete the VFD SWMR Page-Buffer / Cache Port (Review item #1)

**Strategy chosen:** **B — Re-implement VFD SWMR semantics on develop's modern page buffer.**
Keep develop's skip-list/LRU `H5PB_t` and split `H5C*`; add the SWMR concepts (per-tick
dirtied-page tracking, delayed writes, shadow-index construction) as *additive* layers, and
re-apply the metadata-cache `page_index` producer hooks to the now-split `H5Centry.c`.

## Background / why this is real engineering, not a copy-paste

`feature/vfd_swmr` forked from upstream in **2020** (base `33c0016`). The page buffer has used
a **skip list** in mainline for a long time (e.g. hdf5-1_10_5 `H5PB.c` ~L330). The
**vfd_swmr branch rewrote the page buffer from skip-list to a hash table** (`ht[]`) to better
fit VFD SWMR's per-page tick/index bookkeeping. develop did *not* change this — it kept the
long-standing skip-list design and has since evolved it (and split the once-monolithic H5C.c
into ~16 files). So the divergence is: **the feature changed the page-buffer data structure;
the merge correctly kept mainline's skip-list buffer and discarded the feature's hash-table
rewrite — but left the feature's dead hash-table scaffolding behind.**

| | feature/vfd_swmr (2020, ref `05b54b7046`) | upstream/develop |
| --- | --- | --- |
| H5PB.c | 4,865 lines — **hash table** (`ht[]`) + index list + DWL + tick list (a *feature-branch rewrite*) | skip-list (`slist_ptr`) + LRU — the long-standing mainline design |
| H5PB_t | `ht[]`, `il_*`, `dwl_*`, `tl_*`, `cur_tick`, `index_len`… | none of those |
| H5PB_entry_t | full struct in pkg.h | **opaque** fwd-decl; real struct private in H5PB.c |
| H5C.c | 9,822-line monolith; page-index producer inline (~9 sites) | split into ~16 files; entry mgmt in `H5Centry.c` |

The 1,688 dead macro lines in `H5PBpkg.h` and the 4 no-op stubs are the feature's *hash-table*
design grafted onto mainline's *skip-list* page buffer; they reference an `H5PB_t` that does not
exist here. This reinforces Strategy B: rather than resurrect the feature's hash-table fork, add
the VFD SWMR semantics to the canonical skip-list buffer.

**Reference implementation to port FROM:** commit `05b54b7046` (pre-merge `feature/vfd_swmr`
tip). Extracted copy for study: `scratchpad/feature_H5PB.c`, `scratchpad/feature_H5PBpkg.h`.

## Acceptance criterion (the one test that must pass)

A SWMR **writer** dirties a metadata page; after `max_lag` ticks a separate **reader** opens
the shadow index, refreshes its metadata cache, and **observes the new value**. This end-to-end
assertion currently cannot pass because both the writer-side index build and the reader-side
cache refresh are no-ops. Build this test FIRST (Phase 0) — it is red until Phase 4, green when
the port is complete.

---

## Phase 0 — De-risk & safety net (small)

1. **Fail-loud guard while under construction.** Change the 4 stubs from `return SUCCEED` to
   `HGOTO_ERROR(H5E_PAGEBUF, H5E_UNSUPPORTED, FAIL, "VFD SWMR page buffer not yet implemented")`,
   and have `H5Pset_vfd_swmr_config` reject writer configs. Prevents silent-wrong behavior in any
   interim build/merge. (This is review item #9's "fail-loud" half and makes the tree honest now.)
2. **Author the acceptance test** (writer→reader-across-a-tick) under `test/vfd_swmr*`, marked
   expected-fail/skipped until Phase 4. Encodes "done."
3. **Pin the non-SWMR baseline.** Capture current `page_buffer`/`cache`/`cache_image` test
   results so we can prove the non-SWMR path is untouched at every phase.

## Phase 1 — Reader-side: H5C `page_index` producer hooks (independent, do first)

The consumer `H5C_evict_or_refresh_all_entries_in_page` (`src/H5C.c:643`, live at
`H5Fvfd_swmr.c:1378`) reads `cache_ptr->page_index[]`, but nothing populates it.

1. **Confirm entry-struct fields exist** on develop's `H5C_cache_entry_t`: `page`, `pi_next`,
   `pi_prev`, `refreshed_in_tick`. (Consumer already references them, so most are present —
   verify and add any missing.)
2. **Add `page_index` insert/remove macros** (`H5C__INSERT_IN_PAGE_INDEX` /
   `H5C__DELETE_FROM_PAGE_INDEX`) in `H5Cpkg.h`, modeled on the existing hash-table index macros.
3. **Wire hooks into `H5Centry.c`** at the lifecycle points (develop moved these out of the old
   monolith):
   - entry insert / `H5C__load_entry`: set `entry->page = addr / page_size`, link into index
     (feature ref: old H5C.c:1803-1808).
   - move/relocate: recompute `entry->page = new_addr / page_size`, re-link (ref: 2317).
   - evict / `H5C__flush_single_entry` with destroy / remove: unlink from index.
   - Gate every hook on `cache_ptr->vfd_swmr_reader` so the non-SWMR path is a no-op.
4. **Fix the coupled `H5AC_set_vfd_swmr_reader` bug** (`H5AC.c:2607`): set the
   `vfd_swmr_reader` flag unconditionally, not only when `page_size` changes (review Tenet-3/H5CL
   reviewer C2 — otherwise the consumer's `assert(vfd_swmr_reader)` trips).
5. **Test:** reader picks up writer changes given a *manually* populated shadow index (decouples
   from Phase 2-4). Reader coherence is now provable in isolation.

## Phase 2 — Writer-side state: extend develop's page buffer (additive)

1. **`H5PB_t` (`src/H5PBprivate.h`)** — add SWMR fields (some exist): tick list
   `tl_head_ptr`/`tl_tail_ptr`/`tl_len`, delayed-write list `dwl_head_ptr`/`dwl_tail_ptr`
   (`dwl_len` already present), `cur_tick`, `max_delay`, `vfd_swmr_writer`.
2. **`H5PB_entry_t` (private struct in `src/H5PB.c`)** — add: `uint64_t page`,
   `tl_next`/`tl_prev`, `dwl_next`/`dwl_prev`, `uint64_t delay_write_until`,
   `bool modified_this_tick`. Keep it private; expose only via H5PB API.
3. **Initialize** the new fields in `H5PB_create`/entry-alloc; tear down in `H5PB_dest`.
   All additive — non-SWMR files leave them zero/unused.

## Phase 3 — Writer-side capture: populate the tick list

1. At each existing dirty-marking site in `H5PB.c` (lines **1085, 1105, 1158, 1278**) and in
   `H5PB__insert_entry`, when `page_buf->vfd_swmr_writer`: set `entry->page`, add/move the entry
   onto the **tick list**, and set `delay_write_until = cur_tick + max_delay` for metadata pages.
   Factor into one helper `H5PB__vfd_swmr__mark_for_tick(page_buf, entry)`.
2. Non-SWMR path: helper is a no-op (guarded by `vfd_swmr_writer`).

## Phase 4 — Implement the 4 functions on the new state (replaces stubs)

Port from `scratchpad/feature_H5PB.c`, adapting entry access from the old hash table to
develop's skip-list/LRU + the new tick/DWL lists:

- **`__set_tick`** (ref 1932): `page_buf->cur_tick = shared->tick_num`.
- **`__update_index`** (ref 2052-2146+): scan `tl_head_ptr`; for each dirtied page,
  insert-or-update `shared->mdf_idx` (alloc via `H5F_vfd_swmr_enlarge_shadow_index`, handle
  size-change via `H5F_shadow_image_defer_free`); return added/modified counts. This is the
  function whose no-op currently freezes `idx_entries_added` at 0.
- **`__release_tick_list`** (ref 1852): unlink all tick-list entries, reset `tl_len`.
- **`__release_delayed_writes`** (ref 1780): flush/release entries whose `delay_write_until <=
  cur_tick`.
- Remove `H5_ATTR_UNUSED` from params; drop the "minimal stubs" comment.

## Phase 5 — Delayed-write enforcement (the SWMR correctness guarantee)

In the page-buffer flush/evict path (`H5PB__flush_entry` / eviction), when
`vfd_swmr_writer && entry->delay_write_until > cur_tick`, **skip** writing the entry to the
lower file. This is the "no messages from the future" guarantee — readers must never see a
metadata page before its tick is durable in the shadow file.

## Phase 6 — Cleanup & re-enable (closes items #1, #7-#9)

1. **Delete the 1,688 dead macros** from `H5PBpkg.h` that don't map to the new design; keep only
   any genuinely reused as real code. (Review item #9.)
2. Remove the Phase-0 fail-loud guards; flip `H5Pset_vfd_swmr_config` back on.
3. Un-skip the acceptance test and the `test/vfd_swmr*` suite.

## Phase 7 — Validation

- **Acceptance test green** (writer→reader across a tick).
- Full `vfd_swmr` suite + `cache_image` (the `genall5.c` rewrite dependency) + `page_buffer` +
  `cache` regression — all pass; **non-SWMR baseline from Phase 0 unchanged.**
- ASAN + valgrind across a writer/reader cycle (the review flagged several leak/UAF risks in the
  surrounding `H5Fvfd_swmr.c` paths — fix those in passing).
- Re-run the original 7-tenet review findings that touch these paths.

---

## Sequencing & parallelism

- **Phase 1 (reader/H5C) is independent of Phases 2-5 (writer/H5PB)** — they can proceed in
  parallel and be validated separately (Phase 1 with a hand-built index; Phases 2-5 by inspecting
  the produced shadow file). They meet at the Phase-7 end-to-end test.
- Phases 2 → 3 → 4 → 5 are strictly ordered (state before capture before consumption before
  enforcement).
- **Estimated effort:** multi-week. Phase 1 ~ small/medium; Phases 2-5 are the bulk; Phase 7
  validation is non-trivial given the concurrency.

## Key risks

1. **Entry struct is opaque now** — adding SWMR fields to the private `H5PB_entry_t` must not
   leak into the public skip-list/LRU invariants. Keep all SWMR linkage internal.
2. **Develop's eviction policy differs** from 2020 — the delayed-write skip (Phase 5) must
   integrate with LRU eviction without deadlocking the cache when many entries are delay-pinned.
3. **Concurrency correctness** is the hard part and not provable by unit tests alone — rely on
   the multi-process `test/vfd_swmr*` harnesses and stress runs.
4. **`genall5.c`** was rewritten in this merge and feeds `cache_image`; keep it in the
   regression set throughout.
