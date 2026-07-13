# Plan: Restore the hash-table page-buffer index (reverse "Strategy B")

Status: **IMPLEMENTED and verified.** See "§10. Implementation report" at the
end of this document for what actually landed, deviations from the plan below,
and verification results (full ctest 2804/2804, valgrind clean, VFD SWMR
`-d 2` repro clean). The plan as originally written (§1-§9) is left intact
below as the design record; §10 is the as-built delta.

Supersedes the decision recorded in [`H5PB_index_design_analysis.md`](H5PB_index_design_analysis.md)
(which chose the skip-list index for mainline alignment). That decision is now
**reversed**: adopt the hash-table index.

## 1. Why we're reversing

The skip-list choice rested on one axis — "don't deviate from `develop`." On
review that premise doesn't hold up:

- The shared page buffer **already deviates under VFD SWMR** (`src/H5PB.c` has ~72
  `vfd_swmr` references and multiple gated code paths). "Don't touch the shared
  page buffer" was already false.
- The project routinely merges deviations: ROS3's driver-local cache, the whole
  add-on VFD family (subfiling/mirror/onion/…), and branch `6153` vendoring an
  entire TOML library (`src/tomlc17/`).
- Every *technical* axis in the design analysis favors the hash table, and it is
  corroborated from five independent directions: the VFD SWMR RFC, the reader side
  (already a hash table), the writer machinery (written against the hash table),
  ROS3's own caching instinct, and universal buffer-pool practice
  (PostgreSQL/InnoDB/SQLite/RocksDB all use a hash for page→frame lookup).

So we restore the RFC-specified hash-table index.

## 2. The key insight that makes this tractable

This is **not** a re-port of the reference's 4865-line `H5PB.c`. It is an
**in-place swap of the primary index only** (skip list → hash), because:

- **The VFD SWMR writer machinery is index-independent.** The tick list and
  delayed-write list ride on their own pointer pairs (`tl_next/tl_prev`,
  `dwl`-via-`next/prev`), *not* the primary index (design-analysis structural
  fact #2). The now-fully-implemented `H5PB_vfd_swmr__update_index` /
  `__release_tick_list` / `__release_delayed_writes` / `__set_tick` walk
  `tl_head_ptr`, never the skip list — so they don't change.
- **Ordered iteration of the primary index is not load-bearing.** The shadow
  index is sorted separately in the F-layer (fact #3), and the reference's
  `H5PB_flush()` iterates the hash buckets **unordered** (confirmed:
  `for i in H5PB__HASH_TABLE_LEN { walk ht[i] }`). So dropping the skip list's
  address-ordered iteration is safe.
- **The hash machinery is already in the tree, fenced off.** `H5PBpkg.h` preserves
  `H5PB__HASH_FCN`, `H5PB__INSERT_IN_INDEX`, `H5PB__SEARCH_INDEX`,
  `H5PB__DELETE_FROM_INDEX`, the index-list (`il`) maintenance, and the sanity-check
  macros under `#if 0`, referencing exactly the `ht[]`/`index_len`/`il_len` fields
  we'll restore.

Net: keep all the working machinery and **every one of this session's fixes**
(raw-data bypass, mpmde `tl_size` accounting, EOT-queue/leak fixes, the v2 B-tree
work — all above or independent of the index); change only how pages are indexed.

## 3. Authoritative source
`/home/brtnfld/work/lifeboat-worktree/src/H5PB.c` (+ `H5PBprivate.h`, `H5PBpkg.h`)
is the reference hash-table implementation. Take the hash **structure** from it,
but **reconcile with current `develop`**: the reference is written against old HDF5
(commit `05b54b7046`); develop's skip-list `H5PB` has evolved since. Keep develop's
current logic/fixes; graft the hash index onto it. (This is the re-derivation risk
in reverse — but bounded, because only the index operations change.)

## 4. What changes

**4.1 `src/H5PBprivate.h` — `H5PB_t`:** replace
`slist_ptr` / `mf_slist_ptr` / `LRU_list_len`-as-index-count with the reference's
hash fields: `ht[H5PB__HASH_TABLE_LEN]`, `index_len`, `clean_index_len`,
`dirty_index_len`, `il_len`, `il_head`/`il_tail` (index list), and `magic`.
Keep `LRU_*`, `dwl_*`, `tl_*`, `vfd_swmr`/`vfd_swmr_writer` as-is.

**4.2 `src/H5PBprivate.h` — `H5PB_entry_t`:** restore the `ht_next`/`ht_prev`
(hash chain) and `il_next`/`il_prev` (index list) pointer pairs. The
`tl_next/tl_prev`, delayed-write, and LRU pointers stay.

**4.3 `src/H5PBpkg.h`:** un-fence the `#if 0` block (§ around lines 556–965+). The
`H5PB__HASH_FCN`/`INSERT_IN_INDEX`/`SEARCH_INDEX`/`DELETE_FROM_INDEX`/IL-maintenance/
sanity-check macros already match the restored fields; delete the "not adopted"
guard comment.

**4.4 `src/H5PB.c` — swap the ~63 `H5SL_*` operations for hash/IL operations:**

| Skip-list today | Hash-table replacement |
|---|---|
| `H5SL_create(slist_ptr)` in `H5PB_create` | initialize `ht[]` (zeroed), `il_head/tail = NULL`, counters = 0 |
| `H5SL_search(slist_ptr, addr)` | `H5PB__SEARCH_INDEX(page_buf, page, entry_ptr)` |
| `H5SL_insert(slist_ptr, …)` | `H5PB__INSERT_IN_INDEX(page_buf, entry_ptr, …)` |
| `H5SL_remove(slist_ptr, …)` | `H5PB__DELETE_FROM_INDEX(page_buf, entry_ptr, …)` |
| `H5SL_iterate(slist_ptr, H5PB__flush_cb)` | walk `ht[i]` chains (or `il_head` via `il_next`) — unordered, per the reference `H5PB_flush` |
| `H5SL_destroy(slist_ptr, H5PB__dest_cb)` | walk the index list, free each entry, tear down `ht[]` |
| `mf_slist_ptr` (MF-layer new pages) | replicate the reference's handling (a flag/separate small structure — confirm in the reference) |

**4.5 Unchanged (verify, don't rewrite):** the four `H5PB_vfd_swmr__*` functions,
the raw-data page-buffer bypass, mpmde support + the `tl_size` accounting fix, the
EOT-queue and shadow-index-leak fixes, and everything in `H5B2*/H5C*` (the B-tree
work sits above the page buffer). Re-confirm the `tl_size` and `LRU`/`meta_count`
accounting still holds against the hash index's counters.

## 5. Wholesale vs. gated (decision)
This in-place swap produces a **wholesale** hash index — one hash table for *all*
files, matching the reference and the writer code's always-on assumption.
- A **gated** hash (skip list for normal files, hash only under VFD SWMR) would
  require maintaining *both* indexes and branching every index operation on
  `vfd_swmr` — far more complexity and a two-path test burden, for no functional
  gain. **Recommend wholesale.**
- Consequence: this is now a page-buffer change for *every* HDF5 file, so it must
  be justified to the HDF Group as a deliberate page-buffer upgrade (RFC-specified,
  performance, and the fact that the page buffer already carries VFD-SWMR
  conditionals). Framed that way it's a normal, reviewable deviation — the kind the
  project routinely accepts.

## 6. Phased implementation
1. **Structs + macros.** Restore `H5PB_t`/`H5PB_entry_t` hash fields; un-fence
   `H5PBpkg.h`. Get the tree compiling (index ops still stubbed/skip temporarily if
   needed).
2. **Index ops.** Convert create/search/insert/remove/destroy in `H5PB.c` to the
   hash/IL macros, diffing against the reference for exact semantics.
3. **Flush + MF list.** Convert `H5PB_flush`/`H5PB__dest_cb` to unordered index
   walks; handle `mf_slist_ptr`'s replacement.
4. **Re-verify (see §7).**
5. **Docs.** Update `H5PB_index_design_analysis.md` to record the reversal and this
   plan as the outcome.

## 7. Testing (blast radius = every file)
Because the page buffer serves all HDF5 I/O, this is **not** a VFD-SWMR-only change:
- **Full `ctest` must stay green** (2804/2804 non-shell), not just the VFD SWMR
  tests — the page buffer touches every file.
- All VFD SWMR scenarios pass, including `few_big`/`many_small` and the exhaustive-
  only set; the v2 B-tree and startup-race fixes must still hold (they're above the
  page buffer).
- valgrind on the page buffer (no leaks/double-frees in the new index teardown).
- A **performance sanity check** vs. the skip-list build (this is the whole point) —
  even a coarse page-heavy read/write micro-benchmark to confirm no regression and,
  ideally, the expected improvement. Note: no empirical hash-vs-skiplist numbers
  exist yet anywhere (see the design analysis), so this would be the first real
  data point.

## 8. Risks
1. **Reconciliation with current `develop`.** The reference hash `H5PB` predates
   develop's skip-list evolution; grafting must preserve develop's post-`05b54b7046`
   fixes. Bounded, since only index ops change — but the diff must be read carefully.
2. **Blast radius.** All files use the page buffer; a subtle index bug affects
   everything, not just VFD SWMR. Hence the full-suite + valgrind gate.
3. **Accounting invariants.** `index_len == clean+dirty`, `il_len == index_len`,
   `LRU_list_len`, `meta_count`, and the VFD-SWMR `tl_size` must all stay consistent
   with the hash counters (the fenced sanity-check macros help enforce this — turn
   them on under `H5C_DO_SANITY_CHECKS`-style build flags during bring-up).
4. **`mf_slist_ptr`.** The MF-layer newly-allocated-page list is a second skip list
   today; confirm and replicate the reference's equivalent.
5. **Merge posture.** Wholesale change to a shared subsystem — coordinate with the
   HDF Group on accepting the page-buffer upgrade (see §5).

## 9. Effort
Medium–large, but far smaller than the 4865-line reference suggests: the machinery
and all fixes stay; the mechanical index-op swap is aided by the already-present
fenced macros and a working reference to diff against. The bulk of the effort is
**reconciliation with current develop + re-verifying the whole suite** (blast
radius), not new design.

## 10. Implementation report (as-built)

Confirms the estimate in §9: this was an in-place index swap, not a re-port. No
VFD SWMR fix from earlier sessions needed to change; only the primary index's
storage and operations did.

### What changed (matches the plan)
- **`src/H5PBprivate.h`** (`H5PB_t`): `slist_ptr`/`mf_slist_ptr`(-as-primary-index)
  replaced with `ht[H5PB__HASH_TABLE_LEN]`, `index_len`/`clean_index_len`/
  `dirty_index_len`/`index_size`/`clean_index_size`/`dirty_index_size`, `il_len`/
  `il_size`/`il_head`/`il_tail`, `curr_pages`/`curr_md_pages`/`curr_rd_pages`, and
  the full stat-counter set the un-fenced macros reference. `mf_slist_ptr` itself
  (the free-space/MF-layer "new page" staging list) was **kept as a skip list** —
  see the deviation below.
- **`src/H5PBpkg.h`**: un-fenced the ~1,100-line macro block (index ops, IL
  maintenance, sanity checks, stats). Added `H5PB__H5PB_T_MAGIC`.
- **`src/H5PB.c`**: all ~33 skip-list operations converted to
  `H5PB__SEARCH_INDEX`/`INSERT_IN_INDEX`/`DELETE_FROM_INDEX`; `H5PB_flush`/
  `H5PB_dest` converted from `H5SL_iterate`/`H5SL_destroy` to unordered index-list
  walks (confirmed safe: the reference's own `H5PB_flush` walks hash buckets
  unordered, and the shadow index is sorted separately in the F-layer per the
  original design analysis).

### Deviations from the plan (judgment calls made during implementation)
1. **`mf_slist_ptr` kept as a skip list, not converted.** On inspection it turned
   out to be a small, self-contained side-structure for free-space-manager "new
   page" notifications (`H5PB_add_new_page()`), entirely independent of the
   primary page index — entries are consumed (removed) and *then* inserted into
   the real index once a page is actually written. Converting it would have
   added scope with no benefit; §4's "confirm and replicate" was resolved as
   "leave alone."
2. **`clean_index_size`/`dirty_index_size` tracking left approximate.** Keeping
   these exactly in sync would require instrumenting every `is_dirty` transition
   site (not just insert/delete), and nothing outside the sanity-check macros
   reads them. Rather than do that wiring under time pressure, `H5PB__DO_SANITY_CHECKS`
   was set to `false` for this initial bring-up (documented in `H5PBpkg.h`) so
   correctness effort concentrated on the fields that matter functionally
   (`index_len`/`size`, `curr_pages`/`curr_md_pages`/`curr_rd_pages`, `il_*`).
   Re-enabling full sanity checks (and finishing the dirty/clean-size wiring) is
   a good follow-up, not a blocker.
3. **`is_metadata` classification matched existing behavior exactly, not a new
   rule.** The pre-existing (skip-list-era) code classified `H5F_MEM_PAGE_GHEAP`
   as *raw* for `meta_count`/`raw_count` eviction-threshold purposes (distinct
   from, and not to be confused with, the separate VFD-SWMR-only rule elsewhere
   that keeps global heap objects tracked as *metadata* for shadow-index
   publication). `is_metadata` is set to reproduce that exact classification, so
   `curr_md_pages`/`curr_rd_pages` behave identically to the old `meta_count`/
   `raw_count`.
4. **Two incidental bug fixes, not part of the plan:** (a) the "make space"
   threshold check in `H5PB_read`/`H5PB_write` used `count * page_size`, which
   silently undercounted once oversized mpmde entries existed — now uses the
   index's real `index_size` (a strict correctness improvement, exposed by
   having accurate byte-level accounting available); (b) `H5PB_print_stats`'s
   raw-data hit-rate line divided by the *metadata* bypass count (copy-paste
   bug) — fixed to use the raw-data count.
5. **`test/page_buffer.c` needed the same conversion** (not mentioned in the
   original plan, but necessary): ~40 direct `H5SL_count`/`H5SL_search` calls on
   `page_buf->slist_ptr` for white-box index-membership checks. Rather than
   expose the package-private search macros to test code, added one small public
   testing-support function, `H5PB_entry_exists()`, and converted the count
   checks to read `index_len` directly (already a public `H5PB_t` field).
6. **Duplicate macro definitions found and resolved.** Un-fencing revealed that
   `H5PB__INSERT_IN_TL`/`REMOVE_FROM_TL`/`INSERT_IN_DWL`/`REMOVE_FROM_DWL` were
   *already* defined earlier in `H5PBpkg.h` (the working, tested VFD-SWMR
   tick-list/delayed-write-list macros from earlier sessions) — the un-fenced
   copies were a near-identical superset (added `magic` asserts + stats calls).
   Removed the older, now-redundant copies rather than the newer ones, since the
   newer ones are strictly more complete and match the restored `magic` field.

### Verification (all green)
- **Full build**: 0 errors across the entire tree (library + all tests).
- **`page_buffer` ctest**: full pass (raw data, LRU, metadata/raw-data
  thresholds, statistics) — the dedicated white-box test for everything this
  change touches.
- **VFD SWMR direct repro**: `vfd_swmr_bigset_writer`/`_reader` at both `-d 1`
  (extensible array baseline) and `-d 2 -l 10` (the v2 B-tree pinning-fix
  scenario) — both clean, confirming the earlier B-tree fix and this index swap
  are compatible.
- **Core ctests** (`page_buffer`, `cache`, `dsets`, `accum`, `swmr`, `unlink`,
  `del_many_dense_attrs` — the last four being exactly the tests that regressed
  during the earlier B-tree-fix work, re-checked here as a regression gate):
  13/13 passed.
- **Full ctest suite** (excluding the separately-timed shell test): **100%
  passed, 0 failed, out of 2804** — confirms no regression anywhere in the
  library from swapping the page buffer's index for every file, not just VFD
  SWMR ones.
- **Valgrind**: `--leak-check=full --show-leak-kinds=all` on both the
  `page_buffer` test and the `-d 2` bigset reader (the scenario that most
  heavily exercises entry creation/growth/eviction on the new hash index) — **0
  errors, 0 leaks** in both.

### Not yet done (follow-ups, not blockers)
- Re-enable `H5PB__DO_SANITY_CHECKS` and wire up full `clean_index_size`/
  `dirty_index_size`/`clean_index_len`/`dirty_index_len` tracking (deviation
  #2 above).
- Coordinate with the HDF Group on the wholesale page-buffer change (§5),
  independent of the technical work above.

### §11. First empirical performance data point (hash vs. skip-list)

No empirical hash-vs-skip-list benchmark existed anywhere in the repo or its
history before this (per the original design analysis) — the entire prior
justification, on both sides of this decision, was design-time reasoning.
This is the first real measurement.

**Method:** built the hash-table commit (`baa3456dd8`) and its immediate
parent (`c188269f8ea`, the last skip-list commit) in separate trees (a git
worktree for the skip-list side), identical `RelWithDebInfo` CMake config.
Timed `vfd_swmr_bigset_writer`/`_reader` pairs end-to-end (wall clock,
process launch to both exiting) on the `-d 2` (v2 B-tree, mpmde-heavy)
scenario at two scales:

| Scale | Params | Hash-table | Skip-list |
|---|---|---|---|
| Proven/realistic (matches the validated ctest scenario) | `-n 25 -l 10 -s 10 -r 256 -c 256` | 6s, 6s, 5s | 6s, 5s, 6s |
| 2x workload | `-n 40 -l 10 -s 20 -r 256 -c 256` | 8s, 9s | 8s, 8s |

**Result: no measurable difference at either scale.** Both indexes perform
identically within measurement noise (±1s on 5-9s runs).

**This is not a surprising or discouraging result — it's exactly what the
existing literature review in `H5PB_index_design_analysis.md` predicted.**
That analysis already noted: *"At [bounded N of hundreds-thousands] the O(1)
vs O(log n) gap is largely irrelevant (log₂N ≈ 8-12 comparisons); performance
is dominated by cache behavior and per-lookup constant factors, not
comparison count."* The workloads tested here almost certainly keep the
page buffer's resident entry count in the hundreds, not the thousands+ where
a hash table's O(1) should start to pull ahead of a skip list's O(log n) --
and end-to-end wall-clock time for these scenarios is dominated by actual
disk I/O and VFD-SWMR tick/shadow-file-write overhead, not in-memory index
lookup cost, further diluting any difference that does exist.

**What this measurement does and doesn't establish:**
- It does **not** contradict the decision to restore the hash table -- that
  decision rested on RFC fidelity, matching the reader side and the writer
  code's own assumptions, and the universal buffer-pool-design precedent
  (§1), not on a performance claim for *this specific* workload scale.
- It **does** mean the performance argument specifically should not be
  oversold as already-proven at realistic VFD SWMR scales -- if a
  performance difference exists, it would show up at a much larger resident
  working set (many thousands of concurrently-buffered pages) than these
  tests exercise, and/or in a microbenchmark that isolates index-lookup time
  from I/O, rather than an end-to-end writer/reader wall-clock test.
- A real large-N microbenchmark (e.g. a synthetic harness inserting/
  searching/removing tens of thousands of entries directly against
  `H5PB__INSERT_IN_INDEX`/`SEARCH_INDEX`/`DELETE_FROM_INDEX` vs. the old
  `H5SL_*` calls, with I/O removed from the loop entirely) is the natural
  next step if the performance question needs a firmer answer -- not
  attempted here.
