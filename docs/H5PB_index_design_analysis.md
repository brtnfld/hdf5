# H5PB Page-Buffer Index: Hash Table vs. Skip List — Design Analysis

**Status:** **REVERSED and IMPLEMENTED — the hash table is now in place.** This document
originally recorded a decision to keep develop's skip-list index for mainline alignment (text
below, unchanged, for the record). That decision was reversed: the "don't deviate from `develop`"
premise did not hold up on review (the page buffer already carries ~72 VFD-SWMR conditionals; the
project routinely merges deviations — ROS3's cache, the add-on VFD family, `6153`'s vendored TOML
library), while every technical axis plus five independent corroborations (RFC, reader side,
writer code, ROS3, buffer-pool literature) favor the hash table. The restoration has since been
implemented and fully verified (full ctest 2804/2804, valgrind clean, VFD SWMR `-d 2` repro
clean) — see [`H5PB_restore_hashtable_plan.md`](H5PB_restore_hashtable_plan.md), specifically
its §10 "Implementation report" for what actually landed. The original analysis and scorecard
below remain accurate and are the evidence base for the reversal.

---

**Original status (superseded):** **DECIDED (skip-list).** This port is destined for HDF5 `develop`, so mainline
alignment is the priority: keep develop's skip-list index and implement the VFD SWMR writer
machinery (tick list + delayed-write list + the four stub functions) on top of it. Do **not**
restore the hash-table page buffer. The hash table remains a documented, measure-first future
optimization only. This document records the research behind that decision.

**Context:** The VFD SWMR feature branch rewrote the page buffer (`src/H5PB.c`) from
mainline's **skip-list** index into a **4096-bucket power-of-two hash table with chaining**.
develop's page buffer is the skip-list (mainline) design. The port must choose whether to
(a) restore the hash-table page buffer, (b) implement the VFD SWMR writer machinery on top of
develop's skip-list, or (c) a staged hybrid. The reader-side H5C `page_index[]` is *already*
a hash table (wired in Phase 1, commit `0f4a936`) and is not in question here.

Reference feature-branch commit: `05b54b7046`. Page-buffer rewrite commit: `e62f4bd4fab`.

---

## Key structural facts (from this repo)

1. **The writer functions are stubs.** `H5PB_vfd_swmr__update_index`,
   `__release_tick_list`, `__release_delayed_writes`, `__set_tick` in
   `src/H5PB.c:1566-1594` are no-op stubs. They are called every writer end-of-tick
   (`src/H5Fvfd_swmr.c:975`). Because they do nothing, **the writer never publishes changed
   pages to the shadow file** — this is the missing writer half of VFD SWMR.

2. **The tick list and delayed-write list are independent of the index structure.** The
   `H5PB_entry_t` struct uses distinct pointer pairs per list: `ht_next/ht_prev` (hash index),
   `il_next/il_prev` (index list), `next/prev` (LRU **or** delayed-write list), `tl_next/tl_prev`
   (tick list). The original `update_index` walks `page_buf->tl_head_ptr` via `tl_next` — **not**
   the hash table. So the VFD SWMR machinery can ride on top of either a hash-table or a
   skip-list primary index.

3. **The shadow index is sorted separately, in the F-layer.** `H5F_update_vfd_swmr_metadata_file`
   restores `mdf_idx` to sorted order. So the primary index being ordered was never load-bearing
   for building the reader's sorted index.

4. **Entry count is unbounded under the SWMR writer.** `H5PBprivate.h:76-80`: the nominal
   `max_pages` limit "under certain circumstances (mostly related to VFD SWMR) this limit can be
   exceeded by large amounts," because the writer must retain all metadata written in the last
   `max_lag` ticks (delayed writes).

---

## Why the feature branch chose a hash table (documented rationale)

From the official **VFD SWMR RFC** (`docs/VFD_SWMR_RFC_2020-02-03.docx`; published version:
[VFD_SWMR_RFC_220519.pdf](https://support.hdfgroup.org/releases/hdf5/documentation/rfc/VFD_SWMR_RFC_220519.pdf))
and mirrored in `src/H5PBprivate.h:119-139` header comments at `05b54b7046`:

- Power-of-two hash table with chaining; hash = bitwise-AND of the page number → very cheap
  lookup. *"This unusual design decision is based on the observation that if the principle of
  locality holds, collisions between hot pages are unlikely if the hash function maps adjacent
  pages to adjacent locations in the hash table."*
- Deliberately mirrors the H5C metadata cache so the same battle-tested hash-table maintenance
  macros could be reused ("lightweight and easy to implement").
- The skip list was **explicitly weighed and rejected** (RFC §3.6.1, re the reader-side
  page→entry map): *"maintaining and searching this list will impose significant overhead, as the
  skip list is not exactly a lightweight data structure."* The hash table "will be retained in the
  first production implementation. If for whatever reason it proves impractical, a skip list …
  will be the fallback."
- **No empirical benchmark exists** anywhere in the repo or git history. The rationale is
  design-time, not measured. The RFC notes stats were added "allowing us to test" the locality
  assumption; no published results were found.

Talks: John Mainzer, "New VFDs and SWMR Re-Design" (HDF Group, Oct 2020); HUG 2025
"Revisiting HDF5 SWMR" — both defer page-buffer index details to the RFC and report no perf
numbers (HUG 2025 lists "Performance" as an open item).

---

## Literature findings

### Lookup performance at bounded N (hundreds–thousands)
At this scale the O(1) vs O(log n) gap is **largely irrelevant** (log₂N ≈ 8–12 comparisons);
performance is dominated by cache behavior and per-lookup constant factors, not comparison count.
A well-implemented hash table still edges out ordered pointer structures by a small constant.
- Khuong & Morin, *Array Layouts for Comparison-Based Searching*, ACM JEA 2017 —
  [arXiv:1509.05053](https://arxiv.org/abs/1509.05053): "for small values of n, sorted order
  combined with a good implementation of binary search is best"; large-n delay "dominated by RAM
  latency." Performance governed by branch prediction + memory latency.
- No rigorous peer-reviewed head-to-head "hash vs skip list at N=hundreds" lookup benchmark was
  located; treat exact ratios as approximate.

### Memory overhead & cache locality
- Skip list: ~2 forward pointers/node, ~2n total nodes; O(log n) pointer-chasing cache misses.
- Hash table: ~1–2 misses/lookup regardless of n, plus load-factor slack; point queries only.
- Luo et al., *Locality-Optimized In-Memory B-Skiplist*, ICPP '25
  [arXiv:2507.21492](https://arxiv.org/abs/2507.21492): a SOTA skip list (Folly) incurs
  **2.4–4.8× more cache misses** than a cache-packed B-tree; root cause is one element per node.
- Skip lists survive in real systems (RocksDB/LevelDB memtables) for *no-rebalancing + easy
  concurrency*, **not** cache behavior.

### Ordered iteration
- The frequency math favors hash + sort-on-demand when mutations vastly outnumber ordered emits:
  `W·O(1) + R·O(n log n)` vs always-sorted `W·O(log n) + R·O(n)`. A buffer pool is looked up on
  every I/O (large W) but emits a sorted index rarely (small R) → hash wins.
- SQL Server indirect checkpoint keeps an unordered structure and **sorts dirty pages at flush
  time** (ascending page id, WriteFileGather 32→128 pages), double-buffered via list-swap
  ([MS docs](https://learn.microsoft.com/en-us/archive/blogs/bobsql/sql-2016-it-just-runs-faster-indirect-checkpoint-default)).
- Pugh, *Skip Lists*, CACM 33(6) 1990 — always-sorted gives O(1) successor, but that isn't needed
  here (see structural fact #3).

### Concurrency under SWMR
- **Largely moot for HDF5:** readers and writer are *separate processes*, each with its own
  single-threaded page buffer/metadata cache. Consistency comes from the on-disk shadow-file
  snapshot protocol (checksums + `max_lag` delayed writes), not in-memory lock-free structures.
- For completeness: the lock-free/ordered-structure literature favors skip lists
  (Herlihy/Lev/Luchangco/Shavit OPODIS 2006; Fraser's EBR, Cambridge TR UCAM-CL-TR-579; Java
  `ConcurrentSkipListMap`), but this does not bear on the single-threaded H5PB index.

### Real buffer-pool implementations (the strongest corroboration)
Every classic disk-page buffer pool uses a **hash table** for page→frame lookup, and provides
ordered flushing via a *separate* structure — never by overloading the index:
- **PostgreSQL** — shared chained hash table (`buf_table.c`/`dynahash.c`), `BufferTag`→buffer id;
  clock-sweep replacement; lock partitioned into `NUM_BUFFER_PARTITIONS`.
- **InnoDB** — `page_hash` (lookup) + LRU list (eviction) + **flush list ordered by
  `oldest_modification` LSN** (checkpoint). Canonical "one structure per access pattern."
- **SQLite** — `pcache1.c` chained hash (`apHash`), unordered.
- **RocksDB** — sharded hash for the block cache (point lookup); **skip list for the memtable**
  *because it needs ordered scans + concurrent writes*.
- **LMDB** — SWMR-native outlier: no buffer pool; mmap + copy-on-write B+tree + reader table.
  Lock-free consistent reads, single-writer mutex. A larger architectural departure, not a
  porting option here.

**The feature-branch H5PB design (`ht[]` + independent tick list + delayed-write list) is exactly
this textbook idiom.**

---

## The decision

Across all evidence, every **technical / fidelity / correctness-risk** axis favors the hash table
or is neutral; the sole strong pull toward the skip list is **staying aligned with mainline develop**.

| Axis | Favors |
|---|---|
| Lookup + cache perf (esp. unbounded writer `n`) | Hash (mild→moderate) |
| Ordered iteration | Neutral — not a skip-list win here (facts #2, #3; frequency math) |
| Concurrency | Moot (separate processes, single-threaded buffers) |
| RFC fidelity / reader side already hash | Hash |
| Correctness risk (writer code + `DO_SANITY_CHECKS` written against hash) | Hash |
| **Mainline divergence / long-term maintenance** | **Skip list** |

**Deciding question — merge target:**
- **Headed back to HDF5 mainline** → maintainability dominates → skip-list, or staged hybrid
  (skip-list index + independent tick/DWL lists), accepting the porting/re-derivation risk.
- **Standalone / long-lived branch** → restore the hash-table page buffer: RFC-specified,
  matches the already-hash reader side, matches what the writer code was written against, and
  faster on the hot path.

Regardless of index choice, the real milestone is the same: **implement the four stub writer
functions and get writer→reader working end-to-end and tested.** The tick list and delayed-write
list are needed either way and are independent of the index.
