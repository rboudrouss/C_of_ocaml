#!/bin/bash
# Run the tests of compiler/tests-ocaml with c_of_ocaml and emscripten.
#
#   tools/c_of_ocaml_tests.sh [TEST.ml...]
#
# Without arguments, runs all the tests. Each test is compiled with ocamlc,
# translated to C by c_of_ocaml, compiled by emcc against the wasm32 OCaml
# runtime built by ocaml-emcc, run with node, and its output compared with
# its .expected file. Prints one line per test: PASS, FAIL or SKIP (the
# test could not be compiled with ocamlc, usually because of its test
# infrastructure).
#
# Environment:
#   WORD_SIZE   32 (wasm32, the default) or 64 (wasm64)
#   SYSROOT     sysroot of ocaml-emcc for the target
#               (default: $OPAM_SWITCH_PREFIX/emcc-sysroot/wasm32 or wasm64)
#   C_OF_OCAML  the compiler (default: the one built by dune in this tree)
#   JOBS        number of tests run in parallel (default: nproc)
#   WORKDIR     where the tests are built (default: a temporary directory,
#               removed at the end)
#   SPLIT       if set, the C code is split (c_of_ocaml --split), in parts of
#               SPLIT kilobytes
#
# emcc, node and ocamlfind must be in PATH (node >= 24 for wasm64).

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
WORD_SIZE=${WORD_SIZE:-32}
SYSROOT=${SYSROOT:-$OPAM_SWITCH_PREFIX/emcc-sysroot/wasm$WORD_SIZE}
C_OF_OCAML=${C_OF_OCAML:-$ROOT/_build/default/compiler/bin-c_of_ocaml/c_of_ocaml.exe}
JOBS=${JOBS:-$(nproc)}
RT=$ROOT/runtime/c
CFLAGS="-O1 -mtail-call -fno-strict-aliasing -fwrapv -fno-strict-overflow \
  -sSUPPORT_LONGJMP=wasm -fwasm-exceptions -I$RT -I$SYSROOT/lib/ocaml"
COOC_FLAGS=
LDFLAGS=
if [ "$WORD_SIZE" = 64 ]; then
  CFLAGS="$CFLAGS -m64"
  COOC_FLAGS="--word-size 64"
  LDFLAGS="-sMAXIMUM_MEMORY=16GB"
fi
# The C part of the unix library (libunix.a before OCaml 5)
UNIX_LIB=$SYSROOT/lib/libunixbyt.a
[ -f "$UNIX_LIB" ] || UNIX_LIB=$SYSROOT/lib/libunix.a
export SYSROOT C_OF_OCAML RT CFLAGS UNIX_LIB COOC_FLAGS LDFLAGS

run_test() {
  local ml=$1
  local dir name exp w deps files rc
  dir=$(cd "$(dirname "$ml")" && pwd)
  name=$(basename "$ml" .ml)
  exp=$dir/$name.expected
  w=$WORKDIR/$(basename "$dir")_$name
  rm -rf "$w"
  mkdir -p "$w"
  cp "$ml" "$w/"
  cd "$w" || return
  res() {
    echo "$1 $(basename "$dir")/$name $2"
  }
  # The modules of the directory and the testing library used by the test
  deps=""
  for m in $(ocamlfind ocamldep -modules "$name.ml" | cut -d: -f2); do
    local f
    f=$(echo "${m:0:1}" | tr A-Z a-z)${m:1}
    if [ -f "$dir/$f.ml" ] && [ "$f" != "$name" ]; then
      cp "$dir/$f.ml" .
      [ -f "$dir/$f.mli" ] && cp "$dir/$f.mli" .
      deps="$deps $f.ml"
    fi
    if [ "$m" = Testing ]; then
      cp "$dir/../testing.ml" .
      deps="$deps testing.ml"
    fi
  done
  files=$(ocamlfind ocamldep -sort $deps "$name.ml")
  ocamlfind ocamlc -package unix -linkpkg -w -a $(ls ./*.mli 2>/dev/null) $files \
    -o "$name.bc" > build.log 2>&1 || { res SKIP ocamlc; return; }
  local objs=$name.o
  if [ -n "${SPLIT:-}" ]; then
    "$C_OF_OCAML" $COOC_FLAGS "$name.bc" --split parts --part-size "$SPLIT" > cooc.log 2>&1 \
      || { res FAIL "c_of_ocaml: $(head -c 150 cooc.log | tr '\n' ' ')"; return; }
    objs=
    for c in parts/*.c; do
      emcc $CFLAGS -Wno-everything -c "$c" -o "${c%.c}.o" >> cc.log 2>&1 \
        || { res FAIL "cc: $(grep -m1 error cc.log | head -c 150)"; return; }
      objs="$objs ${c%.c}.o"
    done
  else
    "$C_OF_OCAML" $COOC_FLAGS "$name.bc" -o "$name.c" > cooc.log 2>&1 \
      || { res FAIL "c_of_ocaml: $(head -c 150 cooc.log | tr '\n' ' ')"; return; }
    emcc $CFLAGS -Wno-everything -c "$name.c" -o "$name.o" > cc.log 2>&1 \
      || { res FAIL "cc: $(grep -m1 error cc.log | head -c 150)"; return; }
  fi
  emcc $CFLAGS $objs "$WORKDIR/camlc_runtime.o" "$UNIX_LIB" \
    "$SYSROOT/lib/libcamlrun.a" -lm \
    -sALLOW_MEMORY_GROWTH=1 -sEXIT_RUNTIME=1 $LDFLAGS -o "$name.js" > ld.log 2>&1 \
    || { res FAIL "ld: $(grep -m1 error ld.log | head -c 150)"; return; }
  timeout 60 node "$name.js" > out.txt 2>&1
  rc=$?
  if [ -f "$exp" ]; then
    # Ignore a missing newline at the end of the expected output
    if diff -q <(sed -e '$a\' out.txt) <(sed -e '$a\' "$exp") > /dev/null; then
      res PASS ""
    else
      res FAIL "rc=$rc $(diff out.txt "$exp" | head -3 | tr '\n' ' ' | head -c 150)"
    fi
  elif [ $rc = 0 ]; then
    res PASS "(no expected output)"
  else
    res FAIL "rc=$rc $(tail -2 out.txt | tr '\n' ' ' | head -c 150)"
  fi
}
export -f run_test

if [ -z "${WORKDIR:-}" ]; then
  WORKDIR=$(mktemp -d)
  trap 'rm -rf "$WORKDIR"' EXIT
  trap 'exit 1' INT TERM
fi
mkdir -p "$WORKDIR"
export WORKDIR
emcc $CFLAGS -c "$RT/camlc_runtime.c" -o "$WORKDIR/camlc_runtime.o" || exit 1

if [ $# -gt 0 ]; then
  tests=("$@")
else
  mapfile -t tests < <(ls "$ROOT"/compiler/tests-ocaml/*/*.ml)
fi
printf '%s\n' "${tests[@]}" | xargs -P "$JOBS" -I{} bash -c 'run_test "$1"' _ {}
