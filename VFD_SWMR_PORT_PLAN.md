# VFD SWMR Incomplete Port — Implementation Plan

## Background

The VFD SWMR feature was developed on a separate feature branch that diverged significantly
from mainline HDF5.  The port strategy (Strategy B) re-implements VFD SWMR semantics on top
of develop's existing skip-list page buffer rather than restoring the feature branch's
hash-table page-buffer rewrite.

The original feature branch reference commit is `05b54b7046`.

---

## Current Status

**Branch:** `feature/vfd-swmr-port`

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Wire `page_index` producer hooks in `H5C` metadata cache | **Done** — commit `0f4a936` |
| 2 | Reader tick-refresh: call consumer at end-of-tick | Pending |
| 3–6 | Extend `H5PB_t` and re-enable dead page-buffer macros | Pending |

---

## Phase 1 — Done (`0f4a936`)

### What it does
Makes `H5C_t.page_index[]` (4096-bucket hash table) a live per-page linked list of all
cached metadata entries, maintained on every insert/evict.  The consumer
`H5C_evict_or_refresh_all_entries_in_page` already exists and walks `pi_next` chains; Phase 1
closes the gap so it always finds entries rather than scanning an empty index.

### Files changed

**`src/H5Cpkg.h`**
- `H5C__INSERT_IN_INDEX` (~line 851): added page_index prepend block gated on
  `(cache_ptr)->vfd_swmr_reader`; reuses existing `int k` local; runs before the addr-hash block
- `H5C__DELETE_FROM_INDEX` (~line 891): added page_index unlink block gated similarly

**`src/H5Centry.c`**
- `H5C__load_entry` (~line 1334): init `entry->page`, `refreshed_in_tick`, `pi_next`, `pi_prev`
  after `entry->tag_info = NULL`; uses `f->shared->cache->page_size`
- `H5C__deserialize_prefetched_entry` (~line 1994): same four fields for `ds_entry_ptr`;
  uses `cache_ptr->page_size`
- `H5C_insert_entry` (~line 2279): same four fields for `entry_ptr`; uses `cache_ptr->page_size`
- `H5C_move_entry` (~line 2748): updates `entry_ptr->page = new_addr / cache_ptr->page_size`
  when `vfd_swmr_reader`

### Already present in develop (no changes needed)
- `H5C_t.vfd_swmr_reader`, `.page_index[]`, `.page_size` — `H5Cpkg.h:2926-2928`
- `H5C_cache_entry_t.page`, `.refreshed_in_tick`, `.pi_next`, `.pi_prev` — `H5Cprivate.h:1615-1619`
- `H5C__PAGE_HASH_TABLE_LEN`, `H5C__PI_HASH_MASK`, `H5C__PI_HASH_FCN` — `H5Cpkg.h:47-49`
- `H5C_evict_or_refresh_all_entries_in_page` consumer — already exists

---

## Phase 2 — Reader tick-refresh (next)

### Goal
At reader end-of-tick, for every page the shadow index reports as updated, call
`H5C_evict_or_refresh_all_entries_in_page` so stale cached metadata is evicted or refreshed.

### Where to look
- `src/H5Fvfd_swmr.c` — reader end-of-tick logic; locate where the shadow index is consumed
- Feature branch reference: `H5Fvfd_swmr.c` function that iterates `mdf_idx` entries and calls
  the evict/refresh consumer per updated page

### Key types
- `H5FD_vfd_swmr_md_index_entry_t` — one entry in the shadow index (haddr, length, tick)
- `shared->mdf_idx` / `shared->mdf_idx_entries_used` — current shadow index in `H5F_shared_t`

---

## Phase 3–6 — Page buffer extension (deferred)

`src/H5PBpkg.h` lines 419–1544 contain a dead macro block (fenced with `#if 0`) that
references `H5PB_t` fields from the feature branch's hash-table page-buffer design.
These phases extend `H5PB_t` to match and re-enable those macros.

**Do not touch until Phase 2 is working and tested.**

---

## Key source locations

| What | File | Approx. lines |
|------|------|---------------|
| `H5C__INSERT_IN_INDEX` macro | `src/H5Cpkg.h` | ~851 |
| `H5C__DELETE_FROM_INDEX` macro | `src/H5Cpkg.h` | ~891 |
| `H5C__PI_HASH_FCN` and friends | `src/H5Cpkg.h` | 47–49 |
| `H5C_cache_entry_t` struct (pi fields) | `src/H5Cprivate.h` | 1615–1619 |
| `H5C_t` struct (page_index, vfd_swmr_reader) | `src/H5Cpkg.h` | 2926–2928 |
| `H5C__load_entry` init block | `src/H5Centry.c` | ~1334 |
| `H5C__deserialize_prefetched_entry` init | `src/H5Centry.c` | ~1994 |
| `H5C_insert_entry` init block | `src/H5Centry.c` | ~2279 |
| `H5C_move_entry` page update | `src/H5Centry.c` | ~2748 |
| `H5C_evict_or_refresh_all_entries_in_page` | `src/H5Cvfd_swmr.c` | (search) |
| Reader end-of-tick / shadow index consumer | `src/H5Fvfd_swmr.c` | (search) |
| Dead PB macros (fenced, for Phase 3–6) | `src/H5PBpkg.h` | 419–1544 |

---

## First steps on a new machine

```bash
git clone <repo>
git checkout feature/vfd-swmr-port

# Configure a build (adjust preset/path as needed)
cmake --preset ci-StdShar-macos-Clang -B ../hdf5_swmr_build
cmake --build ../hdf5_swmr_build --parallel

# Run VFD SWMR tests to confirm Phase 1 is clean
ctest --test-dir ../hdf5_swmr_build -R vfd_swmr -V
```
