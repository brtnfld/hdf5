# TOML Parser Replacement Rationale

## Context

The `hdf5_swmr` branch currently uses the **H5CL module** — a custom VFD
Configuration Language (S-expression parser) — to read SWMR configuration
files.  The upstream `develop` branch introduces **tomlc17**, a full TOML
1.0.0 library (by CK Tan, MIT-licensed), along with a CMake integration
module.  The plan for this branch is to replace H5CL entirely with tomlc17,
adopting TOML as the SWMR configuration file format.

The discussion below was informed by **RFC-HDFG-2026-001** (String-Based
Filter Configuration API), which independently evaluated the same
TOML-vs-custom-parser trade-off for the filter parameter string API.  The
relevant RFC documents are in `~/work/RFC-HDFG-2026-001/`.

---

## Current Format (S-expression / H5CL)

```
( vfd_swmr_config_data
  (
    ( H5F_vfd_swmr_config
      (
        ( version 1 )
        ( tick_len 4 )
        ( max_lag 7 )
        ( presume_posix_semantics 0 )
        ( maintain_metadata_file 1 )
        ( generate_updater_files 0 )
        ( flush_raw_data 1 )
        ( md_pages_reserved 128 )
        ( md_file_path "./" )
        ( md_file_name "attrdset-shadow" )
        ( pb_expansion_threshold 0 )
      )
    )
    ( page_buffer_config
      (
        ( page_buf_size 4096 )
        ( metadata_pages_only 1 )
      )
    )
    ( file_space_strategy_config
      (
        ( persist 0 )
      )
    )
    ( file_space_page_size
      (
        ( page_size 4096 )
      )
    )
  )
)
```

## New Format (TOML)

```toml
[H5F_vfd_swmr_config]
version = 1
tick_len = 4
max_lag = 7
presume_posix_semantics = false
maintain_metadata_file = true
generate_updater_files = false
flush_raw_data = true
md_pages_reserved = 128
md_file_path = "./"
md_file_name = "attrdset-shadow"
pb_expansion_threshold = 0

[page_buffer_config]
page_buf_size = 4096
metadata_pages_only = true

[file_space_strategy_config]
persist = false

[file_space_page_size]
page_size = 4096
```

Boolean fields (`presume_posix_semantics`, `maintain_metadata_file`,
`generate_updater_files`, `flush_raw_data`, `metadata_pages_only`, `persist`)
switch from 0/1 integers to native TOML `true`/`false`.

---

## Advantages of TOML over H5CL

### For users writing config files

- Familiar, human-readable syntax — TOML is widely known; the S-expression
  format is unique to this codebase.
- Native boolean values (`true`/`false`) instead of cryptic `0`/`1` integers.
- Comments (`# comment`) supported natively.
- Standard tooling: editors, linters, and validators exist for TOML.

### For developers

- **Typed values**: H5CL returns everything as a token; the caller
  re-parsed `"4"` back to an integer.  TOML delivers `int64_t`, `double`,
  `bool`, and `char *` natively — exactly what the four
  `H5F__load_vfd_swmr_config*` helpers were doing manually after the H5CL
  parse.
- **Published spec, no grammar ownership**: H5CL requires the HDF Group to
  answer every edge-case question (what escapes are valid? can keys have
  hyphens?) forever.  TOML 1.0.0 is stable and externally maintained — you
  reference it rather than write it.
- Eliminates ~78 KB of custom parser maintenance (`H5CL.c` alone).
- `toml_parse_file_ex()` replaces the two-step load-file-then-parse flow.
- Memory model is simpler: one `toml_free()` at the end vs. per-field
  `H5CL_init_nv_pair` / `H5CL_take_down_nv_pair` loops.
- Removes ~7 000 lines of lexer/parser internal tests that test only H5CL
  internals.
- Six H5CL header/source files are deleted entirely.

### From RFC-HDFG-2026-001 (`presentation-toml-pivot.md`)

> *"The switch buys typed values and a stable external spec.
> It costs one vendored dependency."*

---

## Disadvantages of TOML over H5CL

### Third-party dependency

- tomlc17 is a vendored dependency the project does not fully own.  Upstream
  changes, licensing changes, or abandonment require a response.
- The H5CL parser was under HDF Group / Lifeboat, LLC copyright and fully
  controlled.
- **RFC position**: a published, maintained spec is the better long-term
  position.  tomlc17 is MIT-licensed and updated as part of the normal HDF5
  dependency refresh.

### Breaking change for existing users

- All existing `.txt` S-expression config files must be converted — no
  backward compatibility unless a format-detection shim is added.
- Any tools, scripts, or documentation that generate or document the old
  format need updating.

### Binary blob gap

H5CL supports a `binary_blob` (`--`-prefixed hex terminal) that the *parser
itself* validates for hex-only content.  TOML has no native binary type.

**Workaround** (from RFC-HDFG-2026-001 §UC-3): encode binary data as a
quoted hex string.  The TOML parser treats it as an opaque string; the
caller validates character set, byte length, and encoding and returns
`H5E_BADVALUE` if malformed.

```toml
# H5CL (old) — parser enforces hex content:
( key --0123456789ABCDEF0123456789ABCDEF )

# TOML (new) — caller validates hex content:
key = "0123456789ABCDEF0123456789ABCDEF"
```

The RFC's conclusion: *"This is a shift in degree, not in kind."*  TOML
already delegates semantic validation to the caller for all other types;
binary content validation is one more example of that same pattern.

No current SWMR config fields use binary blobs, so this is only relevant if
a future field (e.g., an encryption key for the metadata file) requires
binary data.

### VFD-stack composability

H5CL's breadth-first parsing was designed for composing VFD stacks — parsing
the top VFD's config before recursing into sub-VFDs' config strings via the
`H5CL_VAL_LIST` type.  TOML inline tables and dotted keys cover the same
nesting idiom more cleanly (RFC §UC-5), but the calling convention is
different and existing H5CL-based VFD stack code would need updating.  Not
applicable to the current SWMR config fields.

### TOML subset restrictions

Arrays, datetimes, and multi-line strings are not in the supported subset.
Not a current SWMR concern, but worth noting if config fields expand.

---

## Summary

| | H5CL (S-expression) | TOML (tomlc17) |
|---|---|---|
| Spec ownership | HDF Group / Lifeboat | TOML v1.0.0 (external, stable) |
| Typed values | No (token + manual cast) | Yes (int, float, bool, string) |
| Booleans | 0 / 1 integers | `true` / `false` |
| Native binary | `--hex` terminal (parser-validated) | Hex string (caller-validated) |
| Nested config | `H5CL_VAL_LIST` breadth-first | Inline table + dotted keys |
| Parser size | ~78 KB owned C | ~86 KB vendored C (MIT) |
| Comments in config | Not supported | `# comment` |
| Standard tooling | None | Editors, linters, validators |
| Breaking change | — | Yes — `.txt` files must be converted |

The trade-off strongly favors TOML.  The only real costs are the vendored
dependency and the one-time config file migration.
