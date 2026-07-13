# Plan: Restore the hash-table page-buffer index (reverse "Strategy B")

Status: **plan only — not yet implemented.**
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
