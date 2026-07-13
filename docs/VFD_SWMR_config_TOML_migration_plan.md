# Plan: Replace the custom VFD SWMR config-language parser (`H5CL`) with `tomlc17` (TOML)

Status: **plan only — not yet implemented.**
Scope: the VFD SWMR configuration file parser on `feature/vfd-swmr-port`.
Aligns with: RFC-HDFG-2026-001 (the library-wide pivot to TOML for configuration
parsing) and branch `6153` (which vendors `tomlc17` and uses it for *filter*
configuration). This plan does for the VFD SWMR config what `6153` did for
filter config.

---

## 1. Why

The VFD SWMR feature ships its own **"configuration language"** — the `H5CL`
module — a hand-written lexer/parser (~2,200 lines) for a nested
**S-expression** format:

```
( vfd_swmr_config_data
  ( ( H5F_vfd_swmr_config    ( ( version 1 ) ( tick_len 4 ) ( md_file_name "attrdset-shadow" ) ... ) )
    ( page_buffer_config      ( ( page_buf_size 4096 ) ( metadata_pages_only 1 ) ) )
    ( file_space_strategy_config ( ( persist 0 ) ) )
    ( file_space_page_size    ( ( page_size 4096 ) ) ) ) )
```

Everything RFC-HDFG-2026-001 says about the *filter* custom parser applies here,
and more so:

- HDF5 owns a **bespoke grammar** plus every quoting/escaping/edge-case decision,
  forever.
- Values are effectively **untyped**: the parser emits `H5CL_nv_pair_t`
  (`val_type` ∈ {`H5CL_VAL_INT`, `H5CL_VAL_QSTR`, `H5CL_VAL_NONE`}), and the
  consumer (`H5Fvfd_swmr.c`) re-checks the type of every field by hand.
- Booleans are encoded as `0`/`1` integers — non-standard and easy to get wrong.
- It is **~2,200 lines of parser we maintain and must keep bug-free**.

`tomlc17` (MIT, C17, vendored by `6153` at `src/tomlc17/`) gives a
**published, versioned spec**, **native typed values** (int / float / bool /
string), **nested tables**, and defined escaping — none of which we then have to
own. Replacing `H5CL` deletes ~2,300 lines and moves the VFD SWMR config onto the
same footing as the rest of the library's TOML direction.

---

## 2. Current state (what gets removed / rewired)

### 2.1 The parser module — `H5CL` (to be deleted)
- `src/H5CL.c` (~2,191 lines) + `src/H5CL{public,private,pkg,develop,module}.h`.
- Public/private API (`src/H5CLprivate.h`):
  - `H5CL_load_config_string_from_file(file_name, &cfg_str)` — slurp file → string.
  - `H5CL_parse_config(input, expected_name, nv_pairs[], num_pairs)` — parse one group.
  - `H5CL_parse_config_group(input, group_name, num_configs, configs[])` — parse a group of sub-configs.
  - `H5CL_load_vfd_config_str_into_fapl(fapl_id, str)` — top-level string → FAPL.
  - `H5CL_init_nv_pair` / `H5CL_take_down_nv_pair` — nv-pair lifecycle.
- Emits `H5CL_nv_pair_t { name_ptr, val_type, int_val, str_val, ... }`.

### 2.2 The consumer — `src/H5Fvfd_swmr.c` (to be rewritten, not removed)
Entry points (public API — **keep the signatures**, change only the internals):
- `H5Fswmr_config_env(fapl, fcpl, writer, create_file, env_var_name)`
  → reads the env var (`HDF5_VFD_SWMR_CONFIG` by default) to get the file path.
- `H5Fswmr_config_file(path, fapl, fcpl, writer, create_file)`
  → currently `H5CL_load_config_string_from_file()` → `H5F_load_swmr_config_from_string()`.
- `H5Fswmr_config_string(str, fapl, fcpl, writer, create_file)`.
- `H5F_load_swmr_config_from_string(str, fapl, fcpl, writer, create_file)` — the
  core: parses (via `H5CL`) and populates the FAPL/FCPL.
- Four field-mapping helpers (currently take `H5CL_nv_pair_t *`, to be reworked
  to walk a `toml_datum_t` table):
  - `H5F__load_vfd_swmr_config` → `H5F_vfd_swmr_config_t` (+ FAPL via `H5Pset_vfd_swmr_config`).
  - `H5F__load_vfd_swmr_page_buffer_config` → `page_buf_size`, `metadata_pages_only` (FAPL).
  - `H5F__load_vfd_swmr_fs_strategy_config` → `persist` (FCPL, create only).
  - `H5F__load_vfd_swmr_fs_page_size_config` → `fs_page_size` (FCPL, create only).

Notes established from the code:
- `writer` is **set programmatically** (`config_ptr->writer = writer`), never from the file.
- FCPL is only touched when `create_file` is true (`use_fcpl = create_file`).
- `H5F_vfd_swmr_config_t` (`src/H5Fpublic.h`) is the target struct — **unchanged**.

### 2.3 Config files (to be converted to TOML)
- `test/vfd_swmr_attrdset_config.txt`, `test/vfd_swmr_dsetchks_config.txt`.
- `HDF5Examples/C/TUTR/{credel,gaussians}_swmr_config.txt` and the
  `*_gen_updater_config.txt` variants.

---

## 3. Target library — `tomlc17`

Reuse `6153`'s vendored copy: `src/tomlc17/{tomlc17.c,tomlc17.h,LICENSE,README.md}`.

API we need:
- `toml_result_t toml_parse(const char *src, int len);`
  `toml_result_t toml_parse_file_ex(const char *fname);` — returns
  `{ bool ok; toml_datum_t toptab; char errmsg[...]; }`.
- `void toml_free(toml_result_t result);`
- `toml_datum_t toml_get(toml_datum_t table, const char *key);`
  `toml_datum_t toml_seek(toml_datum_t table, const char *dotted_path);`
- `toml_datum_t { toml_type_t type; union { const char* s; int64_t int64;
  double fp64; bool boolean; ... } u; }` with
  `type ∈ {TOML_STRING, TOML_INT64, TOML_FP64, TOML_BOOLEAN, TOML_ARRAY, TOML_TABLE, TOML_UNKNOWN, ...}`.

Build integration (mirror `6153`, `src/CMakeLists.txt`):
- `list(APPEND <lib sources> ${HDF5_SRC_DIR}/tomlc17/tomlc17.c)`.
- `set_source_files_properties(${HDF5_SRC_DIR}/tomlc17/tomlc17.c ...)` to suppress
  warnings on the vendored third-party file.
- No autotools change (develop dropped autotools).
- If `6153` merges first, the object is already built and this is a no-op besides
  `#include "tomlc17.h"` from `H5Fvfd_swmr.c`.

---

## 4. Refined TOML schema (complete)

One TOML document per config file. Four tables, matching the current four
S-expression sections one-to-one. Types are now native (booleans replace the
`0`/`1` integers).

```toml
# VFD SWMR configuration (TOML).
# Referenced by the HDF5_VFD_SWMR_CONFIG environment variable (or a caller-chosen
# variable name passed to H5Fswmr_config_env()).
# 'writer' is NOT specified here -- it is supplied by the API caller.

[vfd_swmr_config]                        # was ( H5F_vfd_swmr_config ... )
version                 = 1              # int32  ; config schema version (currently 1)
tick_len                = 4              # uint32 ; tenths of a second per tick
max_lag                 = 7              # uint32 ; max reader lag, in ticks
presume_posix_semantics = false          # bool   ; (was 0/1)
maintain_metadata_file  = true           # bool
generate_updater_files  = false          # bool
flush_raw_data          = true           # bool
md_pages_reserved       = 128            # uint32
pb_expansion_threshold  = 0              # uint32
md_file_path            = "./"           # string (<= H5F__MAX_VFD_SWMR_FILE_NAME_LEN)
md_file_name            = "attrdset-shadow"   # string
# updater_file_path     = "./credel_updater_file"  # string; required iff generate_updater_files = true
# log_file_path         = "./vfd_swmr.log"          # string; optional

[page_buffer_config]                     # was ( page_buffer_config ... )  -> FAPL
page_buf_size       = 4096               # size_t
metadata_pages_only = true               # bool   ; (was 0/1)

[file_space_strategy_config]             # was ( file_space_strategy_config ... ) -> FCPL (create only)
persist = false                           # bool   ; (was 0/1)

[file_space_page_size]                   # was ( file_space_page_size ... ) -> FCPL (create only)
page_size = 4096                          # hsize_t
```

### 4.1 Field reference

| TOML table / key | Type | Target | Notes |
|---|---|---|---|
| `vfd_swmr_config.version` | int | `config.version` | currently only `1` accepted |
| `vfd_swmr_config.tick_len` | int | `config.tick_len` | |
| `vfd_swmr_config.max_lag` | int | `config.max_lag` | |
| `vfd_swmr_config.presume_posix_semantics` | bool | `config.presume_posix_semantics` | was `0/1` |
| `vfd_swmr_config.maintain_metadata_file` | bool | `config.maintain_metadata_file` | was `0/1` |
| `vfd_swmr_config.generate_updater_files` | bool | `config.generate_updater_files` | was `0/1` |
| `vfd_swmr_config.flush_raw_data` | bool | `config.flush_raw_data` | was `0/1` |
| `vfd_swmr_config.md_pages_reserved` | int | `config.md_pages_reserved` | |
| `vfd_swmr_config.pb_expansion_threshold` | int | `config.pb_expansion_threshold` | |
| `vfd_swmr_config.md_file_path` | string | `config.md_file_path[]` | length-checked |
| `vfd_swmr_config.md_file_name` | string | `config.md_file_name[]` | length-checked |
| `vfd_swmr_config.updater_file_path` | string (opt) | `config.updater_file_path[]` | required iff `generate_updater_files` |
| `vfd_swmr_config.log_file_path` | string (opt) | `config.log_file_path[]` | optional |
| `page_buffer_config.page_buf_size` | int | FAPL page buffer size | |
| `page_buffer_config.metadata_pages_only` | bool | FAPL | was `0/1` |
| `file_space_strategy_config.persist` | bool | FCPL file-space strategy | create only; was `0/1` |
| `file_space_page_size.page_size` | int | FCPL file-space page size | create only |

### 4.2 HDF5-specific constraints (from the RFC, carried over)
- Reject TOML types outside the supported subset (arrays, datetimes, floats where
  an int is expected) with a clear error rather than silent coercion.
- Enforce a max document size and a top-level-table/key cap (defensive limits,
  matching the RFC's 4096-byte / 64-key posture; exact numbers TBD).
- Keep all existing per-field range and string-length validation.
- Reject unknown keys/tables (or warn) — decision to confirm (see §7).

---

## 5. Implementation plan (phased; do **not** start yet)

**Phase 1 — vendor + build.** Add `src/tomlc17/` (from `6153`); wire it into
`src/CMakeLists.txt` with warning suppression. Confirm the tree still builds with
`tomlc17.o` linked and unused. (No-op if `6153` lands first.)

**Phase 2 — new loader, behind the existing entry points.** In `H5Fvfd_swmr.c`:
- Replace `H5Fswmr_config_file()`'s body: read the file (or `toml_parse_file_ex`)
  → `toml_result_t`; on `!ok` `HGOTO_ERROR(... result.errmsg)`; `toml_free()` in `done:`.
- Rewrite `H5F_load_swmr_config_from_string()` to `toml_parse()` the string and
  hand the resulting `toptab` to the four mappers.
- Rework the four `H5F__load_vfd_swmr_*` mappers to walk `toml_datum_t` tables:
  `toml_get(toptab,"vfd_swmr_config")`, then per key `toml_get`/`toml_seek`,
  `switch`/check `.type`, read `.u.int64` / `.u.boolean` / `.u.s`, apply the
  existing validation, and populate `H5F_vfd_swmr_config_t` / FAPL / FCPL exactly
  as today.
- Keep `writer` programmatic; keep the `create_file`→FCPL gating.

**Phase 3 — convert config files.** Rewrite the `*_config.txt` fixtures/examples
as TOML (keep filenames where an env var/test points at them, or update the
references). Provide a one-paragraph migration note for downstream users; a small
S-expr→TOML converter script is a nice-to-have.

**Phase 4 — delete `H5CL`.** Remove `src/H5CL.c` + `H5CL*.h`, their
`src/CMakeLists.txt` entries, and the module/package registration. Grep for any
stray `H5CL_` references.

**Phase 5 — tests + docs.** See §6; update any VFD SWMR user documentation that
shows the S-expression format.

---

## 6. Testing
- Convert the existing config fixtures; the VFD SWMR tests that set
  `HDF5_VFD_SWMR_CONFIG` (run via `test_vfd_swmr.sh`) then exercise the new loader
  end-to-end with no code change to the tests.
- Add focused unit tests (new small test, or extend `test/vfd_swmr.c`):
  - valid config → correct `H5F_vfd_swmr_config_t` + FAPL/FCPL props;
  - malformed TOML → clean `HGOTO_ERROR` (with `errmsg`), no crash/leak;
  - wrong-type / out-of-range values rejected;
  - missing optional sections/keys default correctly;
  - `generate_updater_files = true` without `updater_file_path` → error.
- Run the loader under valgrind (must always `toml_free`).
- Full `ctest` stays green; VFD SWMR scenarios pass.

---

## 7. Risks / open decisions
1. **Vendored-file location.** `6153` uses `src/tomlc17/tomlc17.{c,h}`; the RFC
   text mentions `src/H5Ztoml.{c,h}`. Pick one and be consistent — recommend
   reusing `6153`'s `src/tomlc17/` (already wired), and coordinate so the two
   efforts share one vendored copy rather than duplicating it.
2. **Format break (not backward compatible).** TOML files cannot be read by the
   old S-expr parser and vice-versa. Every existing config file is a one-time
   conversion. Ship a migration note; consider a converter script.
3. **Boolean convention change.** `0/1` → native `true/false`. Cleaner and
   spec-conformant, but a deliberate, documented schema change.
4. **Unknown-key policy.** `H5CL` iterates a fixed nv-pair array; with TOML we
   choose whether unknown keys/tables are errors or ignored-with-warning.
   Recommend: error on unknown keys in the known tables (catch typos), to confirm.
5. **Coordination with `6153`.** Landing order matters for the shared `tomlc17`
   build wiring; ideally sequence after (or alongside) `6153` so there is a single
   vendored copy and one set of `src/CMakeLists.txt` edits.
6. **Scope.** Independent of the B-tree / hang work on this branch; a clean,
   self-contained change and a natural companion to the filter-TOML effort.

---

## 8. Rough size
- **Removed:** `src/H5CL.c` (~2,191) + `H5CL*.h` (~150) ≈ **−2,300 lines**.
- **Added:** thin tomlc17 glue in `H5Fvfd_swmr.c` (a few hundred lines, mostly the
  four mappers rewritten) + the vendored `tomlc17.c` (shared with `6153`, not
  charged to this change if it lands first).
- **Net:** a large reduction in HDF5-owned parser code, plus typed values and
  nested tables for free.
