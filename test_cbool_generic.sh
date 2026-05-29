#!/bin/sh
# Test whether the system gfortran can compile a generic interface that
# distinguishes LOGICAL(C_BOOL) from default LOGICAL.
#
# This reproduces the pattern used in the generated tf_gen.F90 and the
# PROG_FC_C_BOOL_EQ_LOGICAL configure check, and tries both variants:
#
#   Case A - WITH call sites (what the configure check currently does)
#   Case B - WITHOUT call sites (what tf_gen.F90 compilation looks like)
#
# Each case is compiled with and without -m32 if the compiler supports it.
#
# Usage:
#   ./test_cbool_generic.sh [gfortran]

FC=${1:-gfortran}
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

echo "Compiler: $($FC --version | head -1)"
echo ""

# --------------------------------------------------------------------------
# Write the two test sources
# --------------------------------------------------------------------------

# Case A: generic interface WITH call sites (current configure check pattern)
cat > "$TMPDIR/test_with_calls.f90" << 'FORTRAN'
MODULE l_type_mod
  USE ISO_C_BINDING
  INTERFACE h5t
     MODULE PROCEDURE h5t_c_bool
     MODULE PROCEDURE h5t_logical
  END INTERFACE
CONTAINS
  SUBROUTINE h5t_c_bool(lcb)
    LOGICAL(KIND=C_BOOL) :: lcb
  END SUBROUTINE h5t_c_bool
  SUBROUTINE h5t_logical(l)
    LOGICAL :: l
  END SUBROUTINE h5t_logical
END MODULE l_type_mod
PROGRAM test_with_calls
  USE ISO_C_BINDING
  USE l_type_mod
  LOGICAL(KIND=C_BOOL) :: lcb
  LOGICAL              :: l
  lcb = .TRUE._C_BOOL
  l   = .TRUE.
  CALL h5t(lcb)
  CALL h5t(l)
END PROGRAM test_with_calls
FORTRAN

# Case B: generic interface WITHOUT call sites (mirrors tf_gen.F90 compilation)
cat > "$TMPDIR/test_no_calls.f90" << 'FORTRAN'
MODULE l_type_mod
  USE ISO_C_BINDING
  INTERFACE h5t
     MODULE PROCEDURE h5t_c_bool
     MODULE PROCEDURE h5t_logical
  END INTERFACE
CONTAINS
  SUBROUTINE h5t_c_bool(lcb)
    LOGICAL(KIND=C_BOOL) :: lcb
  END SUBROUTINE h5t_c_bool
  SUBROUTINE h5t_logical(l)
    LOGICAL :: l
  END SUBROUTINE h5t_logical
END MODULE l_type_mod
PROGRAM test_no_calls
  USE l_type_mod
END PROGRAM test_no_calls
FORTRAN

# --------------------------------------------------------------------------
# Also report the actual kinds so we know if C_BOOL == default LOGICAL
# --------------------------------------------------------------------------
cat > "$TMPDIR/report_kinds.f90" << 'FORTRAN'
PROGRAM report_kinds
  USE ISO_C_BINDING
  IMPLICIT NONE
  INTEGER :: cbool_kind, logical_kind
  cbool_kind  = C_BOOL
  logical_kind = KIND(.TRUE.)
  WRITE(*,'(A,I0)') "  KIND(C_BOOL)    = ", cbool_kind
  WRITE(*,'(A,I0)') "  KIND(.TRUE.)    = ", logical_kind
  IF (cbool_kind == logical_kind) THEN
    WRITE(*,'(A)') "  => SAME kind: C_BOOL and default LOGICAL are identical"
  ELSE
    WRITE(*,'(A)') "  => DIFFERENT kinds: C_BOOL and default LOGICAL are distinct"
  END IF
END PROGRAM report_kinds
FORTRAN

# --------------------------------------------------------------------------
# Helper: compile and report result
# --------------------------------------------------------------------------
try_compile() {
    label="$1"
    src="$2"
    shift 2
    flags="$*"
    out=$(cd "$TMPDIR" && $FC $flags -c "$src" 2>&1)
    if [ $? -eq 0 ]; then
        echo "  PASS  $label"
    else
        echo "  FAIL  $label"
        echo "$out" | sed 's/^/        /'
    fi
}

try_run() {
    label="$1"
    src="$2"
    shift 2
    flags="$*"
    out=$(cd "$TMPDIR" && $FC $flags "$src" -o kinds_prog 2>&1 && ./kinds_prog 2>&1)
    echo "$out" | sed "s/^/  $label: /"
}

# --------------------------------------------------------------------------
# Detect -m32 support using a trivial program, not the ambiguous-interface
# test files (which fail to compile for other reasons and would give a
# false "not supported" result).
# --------------------------------------------------------------------------
cat > "$TMPDIR/probe_m32.f90" << 'FORTRAN'
PROGRAM probe
END PROGRAM probe
FORTRAN

M32_SUPPORTED=0
if $FC -m32 "$TMPDIR/probe_m32.f90" -o "$TMPDIR/probe_m32" 2>/dev/null; then
    M32_SUPPORTED=1
fi

# --------------------------------------------------------------------------
# Run tests
# --------------------------------------------------------------------------
echo "=== Kind information (default flags) ==="
try_run "" report_kinds.f90
if [ "$M32_SUPPORTED" = "1" ]; then
    echo "=== Kind information (-m32) ==="
    try_run "-m32" report_kinds.f90 -m32
fi
echo ""

echo "=== Case A: generic interface WITH call sites ==="
try_compile "default flags          " test_with_calls.f90
try_compile "-std=f2008             " test_with_calls.f90 -std=f2008
try_compile "-std=f2008 -pedantic   " test_with_calls.f90 -std=f2008 -pedantic

if [ "$M32_SUPPORTED" = "1" ]; then
    try_compile "-m32                   " test_with_calls.f90 -m32
    try_compile "-m32 -std=f2008        " test_with_calls.f90 -m32 -std=f2008
    try_compile "-m32 -std=f2008 -pedantic" test_with_calls.f90 -m32 -std=f2008 -pedantic
else
    echo "  SKIP  -m32 (not supported by this compiler/platform)"
fi
echo ""

echo "=== Case B: generic interface WITHOUT call sites (mirrors tf_gen.F90) ==="
try_compile "default flags          " test_no_calls.f90
try_compile "-std=f2008             " test_no_calls.f90 -std=f2008
try_compile "-std=f2008 -pedantic   " test_no_calls.f90 -std=f2008 -pedantic

if [ "$M32_SUPPORTED" = "1" ]; then
    try_compile "-m32                   " test_no_calls.f90 -m32
    try_compile "-m32 -std=f2008        " test_no_calls.f90 -m32 -std=f2008
    try_compile "-m32 -std=f2008 -pedantic" test_no_calls.f90 -m32 -std=f2008 -pedantic
else
    echo "  SKIP  -m32 (not supported by this compiler/platform)"
fi
echo ""

echo "=== Summary ==="
echo "If Case A passes but Case B fails: the compiler suppresses the ambiguity"
echo "  error when call sites are present -- and the configure check is masking"
echo "  a real compiler bug."
echo "If both A and B fail: C_BOOL and LOGICAL are the same kind on this platform"
echo "  (correct behavior -- H5_FORTRAN_C_BOOL_IS_UNIQUE should be 0)."
echo "If A/B fail without -m32 but pass with -m32 (or vice versa): the flag"
echo "  changes the C_BOOL kind mapping, pointing to a flag-mismatch between"
echo "  the CMake configure check and the actual build."
echo "If both pass: no issue on this platform/flags."
