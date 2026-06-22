# bfloat16 Hardware Fast-Path Conversions

## What was done

HDF5 already supports bfloat16 as a storage type (`H5T_FLOAT_BFLOAT16LE`/`BE`), but
conversions to/from `double`/`float` went through the general bit-manipulation
loop (`H5T__conv_f_f_loop`), which handles arbitrary formats, byte order, and
exception callbacks element-by-element and cannot be auto-vectorized by the
compiler.

bfloat16 is just the top 16 bits of an IEEE-754 float32 (same sign bit, same
8-bit exponent, top 7 mantissa bits), so the conversion reduces to a cast plus
a 16-bit right shift — no native C type is involved (unlike `_Float16`), so it
can't reuse the existing `H5T_CONV_Ff`/`H5T_CONV_fF` macros, which require one.

Added four dedicated converter functions in `src/H5Tconv_float.c`:

- `H5T__conv_double_bfloat16` — double → bfloat16 (narrowing, forward in-place)
- `H5T__conv_float_bfloat16` — float → bfloat16 (narrowing, forward in-place)
- `H5T__conv_bfloat16_double` — bfloat16 → double (widening, backward in-place)
- `H5T__conv_bfloat16_float` — bfloat16 → float (widening, backward in-place)

Each function takes a contiguous-buffer, no-exception-callback fast path
(simple C loop the compiler can vectorize) and falls back to
`H5T__conv_f_f_loop` for strided buffers or registered exception handlers.
Declarations added to `src/H5Tconv_float.h`; all eight LE/BE × float/double
pairs registered as `H5T_PERS_HARD` in `src/H5T.c`.

Updated `test/dtypes.c`'s `test_bfloat16()`, which had asserted these specific
conversions were software-only — that assumption is now obsolete by design, so
the assertions were flipped to expect hard conversions (guarded by
`H5Tequal()` checks against the platform's native float format, matching the
existing pattern used for `_Float16`).

Work lives on `feature/bfloat16-fast-path-conversion`, branched off `develop`.

## Production-readiness review

A correctness review turned up four real issues in the first version, all
fixed:

1. **Rounding mismatch.** The first version truncated the mantissa
   (`u >> 16`) instead of rounding it, so the hard path produced different
   numeric results from `H5T__conv_f_f_loop()` for the same conversion.
   Fixed by adding the standard "add half-ULP, then truncate" round-half-up
   step before shifting — deliberately matching `H5T__conv_f_f_loop()`'s
   rounding convention (round-half-up, not round-to-nearest-even) so the
   hard and soft paths agree bit-for-bit, since every other reduced-float
   narrowing in this library already follows that same convention.

   *Why round-half-up over round-to-nearest-even (e.g. what hardware
   `VCVTNEPS2BF16` does):* the two modes only disagree at an exact tie —
   the discarded 16 bits equal exactly `0x8000`, a ~1-in-65,536 chance per
   value for arbitrary continuous data. Real scientific/sensor/simulation
   data essentially never lands exactly on a power-of-two-aligned tie
   boundary, so in practice the two modes produce the same bits for the
   overwhelming majority of real data — unlike the truncation bug above,
   which biased roughly half of all values. Given the disagreement is that
   rare and that small, the deciding factor is consistency, not numerical
   purity: HDF5's general loop already uses round-half-up for every other
   reduced-precision type it handles (FP8, FP6, FP4, float16). Using
   round-to-nearest-even for bfloat16 alone would make it the one type
   whose hard and soft paths can disagree, and the one type whose rounding
   convention doesn't match its siblings — a real, concrete cost (the
   bytes written to disk could depend on incidental factors like buffer
   alignment or whether an exception callback happens to be registered).
   Matching the rest of this codebase's existing convention was judged
   more valuable than matching an external hardware default.
2. **NaN → Infinity miscategorization.** A float32 NaN whose payload bits
   live entirely in the low 16 bits would truncate to a zero mantissa with
   the exponent still all-1s — Infinity, not NaN. Fixed with a branchless
   mask that detects NaN inputs and forces the result mantissa non-zero.
3. **Undefined behavior on `nelmts == 0`.** The widening converters
   (`bfloat16→double`, `bfloat16→float`) computed `buf + (nelmts - 1)`
   before checking `nelmts`, underflowing to a wild pointer when
   `nelmts == 0`. `H5Tconvert()`'s public API doesn't guard against this.
   Fixed by checking `nelmts > 0` before computing the backward pointer.
   (HDF5's own general loop has the identical pattern for widening
   conversions — this is a pre-existing class of issue in the library, not
   something newly introduced here.)
4. **No alignment check.** Unlike the generic conversion-macro framework
   (which checks `H5T_NATIVE_*_ALIGN_g` and copies to an aligned scratch
   buffer when needed), the fast path read/wrote through raw pointer casts
   unconditionally. Fixed by falling back to the general loop whenever the
   buffer isn't naturally aligned for the type involved.

Verified with a dedicated fuzz/edge-case harness (1,000,000 random values
plus deliberate round-half-up tie cases, low-payload NaNs, canonical NaNs,
and `nelmts == 0` calls) comparing the hard path against the soft path
bit-for-bit: `float → bfloat16` (single rounding step) matches exactly,
0 mismatches. `double → bfloat16` (double→float32→bfloat16, two
correctly-rounded narrowing steps) showed 1 mismatch in 1,000,000 — the
well-known "double rounding" effect, where rounding twice is occasionally
not equivalent to rounding once, even though each step rounds correctly.

A follow-up experiment (50M samples, full 52-bit-random double mantissas)
found this is **not** symmetric, self-canceling noise — every single
mismatch had the hard path rounding strictly higher than the soft path
(0 counterexamples across two independent 50M-sample runs). It's a real,
consistent directional bias, same in *character* as the round-half-up tie
bias above, just rarer (~1-in-129,000 vs. ~1-in-65,536). See "Quantifying
the rounding-mode trade-off" below for why this still doesn't change the
decision to accept it: the math that shows the tie bias is undetectable in
aggregate applies with even more room to spare here, since this effect is
about half as frequent and the same ~1-ULP magnitude. Eliminating it would
mean abandoning the hardware double→float32 cast and reimplementing the
general loop's bit-level mantissa extraction by hand, which would also
eliminate the auto-vectorization the fast path exists for.

One additional narrow, accepted divergence: at the extreme top of the
finite range (exponent one below max, mantissa all 1s), the general loop
suppresses a rounding-induced carry to avoid manufacturing Infinity from
rounding; the fast path rounds up normally and does produce Infinity
there. This is unreachable for any value of sane magnitude.

Rebuilding and re-benchmarking after all four fixes showed no measurable
performance regression (still fully auto-vectorized, confirmed via
`objdump`) — the conversion remains memory-bandwidth bound, so the small
amount of extra rounding/NaN-detection arithmetic is free.

## Quantifying the rounding-mode trade-off

The "round-half-up vs. round-to-nearest-even" and "double-rounding"
decisions above are backed by measurement, not just argument.

**Does round-half-up (RUP) really introduce a bias that round-to-nearest-even
(RNE, what hardware `VCVTNEPS2BF16`/`AVX512_BF16` does) avoids?** Yes,
provably so — confirmed with an exhaustive (not sampled) sweep of every
possible exact-tie case for float32→bfloat16 (all 254 exponents × all 128
kept-mantissa values = 32,384 cases):

```
round-half-up:         mean relative error = +0.270760%   (rounds up 100% of ties)
round-to-nearest-even: mean relative error = -0.000763%   (rounds up ~50% of ties)
```

RUP's bias at the tie boundary is real and ~350x larger than RNE's residual
(which is floating-point summation noise, not a real effect). This is the
direct, measured confirmation of the textbook claim that RNE doesn't
introduce a systematic bias and RUP does.

**Does that bias actually matter for real data?** Run against 20 million
random (non-adversarial) float32 values instead of an exhaustive tie sweep:

```
exact ties hit: 299 of 20,000,000 (0.0015%, matches the expected ~1/65536 rate)
round-half-up:         mean relative error = -0.00011005%
round-to-nearest-even: mean relative error = -0.00011367%
```

The two are statistically indistinguishable. RUP's tie-specific bias
(+0.27%) only acts on the ~0.0015% of values that hit an exact tie, so its
contribution to the dataset-wide mean is ~0.27% × 0.000015 ≈ 4×10⁻⁶% —
about 30x smaller than the ~10⁻⁴% noise floor that 20 million values
produce just from ordinary bfloat16 quantization. The bias is real
(provably, per the exhaustive sweep above) but invisible against real
data's noise floor at any practical sample size.

**Is the double-rounding effect the same kind of thing?** Measured
directly: generate doubles with a genuinely random 52-bit mantissa
(`rand()/RAND_MAX` alone only has ~31 bits of entropy — far short of a
double's 52-bit mantissa, and using it directly silently biases exactly
this kind of boundary-crossing measurement), convert via both the hard
path and the soft path, and classify every mismatch by direction:

```
N=50,000,000  mismatches=388 (0.000776%)
  hard > soft (double-rounding pushed UP):   388 (100.0% of mismatches)
  hard < soft (double-rounding pushed DOWN): 0 (0.0% of mismatches)
```

Reproduced on a second independent 50M-sample run (366 mismatches, again
100% one-directional). So double-rounding is **not** symmetric,
self-canceling noise — it's a consistent directional bias, same in
character as the round-half-up tie bias. But running the same
aggregate-contribution math: at ~1-in-129,000 frequency and ~1-ULP
(~0.78% relative) magnitude per event, its contribution to a dataset mean
is ~0.78% × (1/129,000) ≈ 6×10⁻⁶% — the same order of magnitude as RUP's
tie contribution above, and ~30x below the measured noise floor. The
*character* of the claim changes (it's a real bias, not random noise) but
the *magnitude* conclusion doesn't (still practically undetectable, still
not worth sacrificing vectorization to eliminate).

## Results

Benchmarked on this machine (AMD Ryzen 9 9950X3D, Zen 5, AVX-512 + AVX512_BF16
capable) with 50M elements, comparing the new hard path against the old
general loop (forced via runtime `H5Tunregister` on identical input data, in
both a Debug build and a Release `-O3` build):

| Conversion | Hard path | Soft path | Speedup |
|---|---|---|---|
| double → bfloat16 | 0.012–0.014 s | 2.5–3.2 s | ~175–213x |
| float → bfloat16 | 0.010–0.011 s | 2.4–3.1 s | ~235–279x |
| bfloat16 → double | 0.049–0.054 s | 1.8–2.4 s | ~33–48x |
| bfloat16 → float | 0.024 s | 1.7–2.3 s | ~69–97x |

`objdump` on `H5T__conv_double_bfloat16` confirms the compiler auto-vectorized
the fast path with AVX-512 instructions (`vcvtpd2ps`, `vpsrld`, `vmovupd`) with
no intrinsics written by hand.

Rebuilding with `-march=native` (exposing AVX-512 explicitly, including the
dedicated `AVX512_BF16` ISA) made negligible difference to the hard-path
timings — the conversion is memory-bandwidth bound, not compute bound. The
per-element work (a shift and a `memcpy`) is too cheap for ALU throughput to
be the bottleneck at any reasonable clock speed.

### Compiler comparison: GCC 15 vs Clang 21

The fast-path functions were compiled and benchmarked under both compilers
available on this machine (GCC 15.2.1 and Clang 21.1.8). Both pass the full
correctness harness (rounding ties, NaN preservation, `nelmts == 0`, and 1M
random-value comparison against the soft path) with zero unexpected failures.

Speedup vs the soft path at 50M elements:

| Conversion | GCC 15 (no `-march`) | Clang 21 (no `-march`) | Clang 21 (`-march=native`) |
|---|---|---|---|
| double → bfloat16 | 170x | 150x | **180x** |
| float → bfloat16 | **134x** | 65x | 89x |
| bfloat16 → double | 39x | 36x | 37x |
| bfloat16 → float | 74x | 77x | 76x |

`objdump` on the compiled libraries explains the performance differences:

- **Without `-march=native`:** GCC 15 auto-selects AVX-512 (ZMM, 512-bit)
  even at the default x86-64 target; Clang 21 conservatively emits only
  SSE2 (XMM, 128-bit). That 4× reduction in lane width roughly accounts
  for Clang's ~2× gap on `float → bfloat16`.
- **With `-march=native`:** Both compilers generate ZMM code, and both
  become competitive or better than GCC's default for most paths. Clang
  still trails on `float → bfloat16` because it generates proportionally
  more XMM epilog/prologue code around the vectorized kernel — a different
  loop-unrolling strategy, not a missed instruction.
- **Widening paths** (`bfloat16 → double/float`) are nearly identical
  across all three configurations: the conversion is memory-bandwidth
  limited on the widening side, so vectorization width barely matters once
  you clear the soft-path overhead.

The takeaway is that massive speedups materialize under both compilers in
all cases. The float→bfloat16 gap between compilers at default flags
(134x vs 65x) is a compiler auto-vectorization artifact — GCC happens to
make a better unrolling decision for that specific 4→2 byte narrowing loop.
It is not a code deficiency: the same source produces AVX-512 output from
both compilers when given `-march=native`.

## Conclusion

The fast path delivers a 30–280x speedup over the general conversion loop,
and the win comes almost entirely from doing dramatically less work per
element (no exception-callback checks, no generic bit-field math, no
function-pointer indirection per element) rather than from exotic
instructions — a plain auto-vectorized C loop saturates memory bandwidth
regardless of whether AVX2 or AVX-512 is available. This means the same
approach should pay off similarly on older hardware (SSE2-only x86, or other
architectures GCC/Clang can auto-vectorize for), not just on AVX-512 capable
chips like this one.

Both GCC 15 and Clang 21 confirm correctness and deliver large speedups; the
remaining compiler-to-compiler performance differences are auto-vectorization
artifacts, not gaps in the implementation.
