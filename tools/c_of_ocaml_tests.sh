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
# infrastructure). The tests listed in compiler/tests-c/known_failures for
# this version of OCaml are expected to fail. The script exits with 1 when
# another test fails.
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
#   COOC_FLAGS  more flags for c_of_ocaml (--cps-stack for instance)
#   OCAMLC_FLAGS  more flags for ocamlc (-g: debug information, the
#               backtrace slots recorded by the compiled code)
#   TAIL_CALLS  no: compiled without the tail calls of WebAssembly (no
#               -mtail-call), as ocaml-emcc --no-tail-calls
#   REF         bytecode: the expected output of a test is that of its
#               bytecode run by the ocamlrun of the switch, instead of its
#               .expected file (those of the latest OCaml)
#   NATIVE      if set, natively: the C code compiled by $CC (default cc)
#               and linked with the bytecode runtime of the switch
#               (libcamlrun.a), the program run directly. Neither emcc nor
#               node nor a sysroot is needed (WORD_SIZE and SYSROOT unused)
#
# emcc, node and ocamlfind must be in PATH (node >= 24 for wasm64).

set -u

ROOT=$(cd "$(dirname "$0")/.." && pwd)
C_OF_OCAML=${C_OF_OCAML:-$ROOT/_build/default/compiler/bin-c_of_ocaml/c_of_ocaml.exe}
JOBS=${JOBS:-$(nproc)}
RT=$ROOT/runtime/c
COOC_FLAGS=${COOC_FLAGS:-}
if [ -n "${NATIVE:-}" ]; then
  # (the C compiler, the runtime and the libraries of the switch)
  CCX=${CC:-cc}
  LIBDIR=$(ocamlfind ocamlc -where)
  WORD_SIZE=$(ocamlfind ocamlc -config | sed -n 's/^word_size: //p')
  CFLAGS="-O1 -fno-strict-aliasing -fwrapv -fno-strict-overflow -I$RT -I$LIBDIR"
  COOC_FLAGS="$COOC_FLAGS --word-size $WORD_SIZE"
  LDFLAGS="-lm -lpthread -ldl"
  CCFLAGS=$CFLAGS
  RUN=
else
  CCX=emcc
  WORD_SIZE=${WORD_SIZE:-32}
  SYSROOT=${SYSROOT:-$OPAM_SWITCH_PREFIX/emcc-sysroot/wasm$WORD_SIZE}
  LIBDIR=$SYSROOT/lib
  CFLAGS="-O1 -fno-strict-aliasing -fwrapv -fno-strict-overflow \
    -sSUPPORT_LONGJMP=wasm -fwasm-exceptions -I$RT -I$SYSROOT/lib/ocaml"
  [ "${TAIL_CALLS:-yes}" = no ] || CFLAGS="$CFLAGS -mtail-call"
  LDFLAGS="-lm -sALLOW_MEMORY_GROWTH=1 -sEXIT_RUNTIME=1"
  if [ "$WORD_SIZE" = 64 ]; then
    CFLAGS="$CFLAGS -m64"
    COOC_FLAGS="$COOC_FLAGS --word-size 64"
    LDFLAGS="$LDFLAGS -sMAXIMUM_MEMORY=16GB"
  fi
  # The C code is compiled as the runtime of the sysroot, with -pthread if
  # it is (its thread-local variables), then linked without, as ocaml-emcc
  # does
  CCFLAGS=$CFLAGS
  if grep -qs '^cflags=.*-pthread' "$SYSROOT/manifest"; then CCFLAGS="$CCFLAGS -pthread"; fi
  RUN=node
fi
# The C part of the unix library (libunix.a before OCaml 5)
UNIX_LIB=$LIBDIR/libunixbyt.a
[ -f "$UNIX_LIB" ] || UNIX_LIB=$LIBDIR/libunix.a
CAMLRUN_LIB=$LIBDIR/libcamlrun.a
export C_OF_OCAML RT CCX CFLAGS CCFLAGS UNIX_LIB CAMLRUN_LIB COOC_FLAGS LDFLAGS RUN

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
  ocamlfind ocamlc -package unix -linkpkg -w -a ${OCAMLC_FLAGS:-} $(ls ./*.mli 2>/dev/null) $files \
    -o "$name.bc" > build.log 2>&1 || { res SKIP ocamlc; return; }
  if [ "${REF:-}" = bytecode ]; then
    exp=$w/ocamlrun.txt
    timeout 60 ocamlrun "./$name.bc" > "$exp" 2>&1
  fi
  local objs=$name.o
  if [ -n "${SPLIT:-}" ]; then
    "$C_OF_OCAML" $COOC_FLAGS "$name.bc" --split parts --part-size "$SPLIT" > cooc.log 2>&1 \
      || { res FAIL "c_of_ocaml: $(head -c 150 cooc.log | tr '\n' ' ')"; return; }
    objs=
    for c in parts/*.c; do
      $CCX $CCFLAGS -w -c "$c" -o "${c%.c}.o" >> cc.log 2>&1 \
        || { res FAIL "cc: $(grep -m1 error cc.log | head -c 150)"; return; }
      objs="$objs ${c%.c}.o"
    done
  else
    "$C_OF_OCAML" $COOC_FLAGS "$name.bc" -o "$name.c" > cooc.log 2>&1 \
      || { res FAIL "c_of_ocaml: $(head -c 150 cooc.log | tr '\n' ' ')"; return; }
    $CCX $CCFLAGS -w -c "$name.c" -o "$name.o" > cc.log 2>&1 \
      || { res FAIL "cc: $(grep -m1 error cc.log | head -c 150)"; return; }
  fi
  $CCX $CFLAGS $objs "$WORKDIR/camlc_runtime.o" "$UNIX_LIB" "$CAMLRUN_LIB" \
    $LDFLAGS -o "$name.js" > ld.log 2>&1 \
    || { res FAIL "ld: $(grep -m1 error ld.log | head -c 150)"; return; }
  # (natively, $name.js is the executable)
  if [ -n "$RUN" ]; then
    timeout 60 "$RUN" "$name.js" > out.txt 2>&1
  else
    timeout 60 "./$name.js" > out.txt 2>&1
  fi
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
$CCX $CCFLAGS -c "$RT/camlc_runtime.c" -o "$WORKDIR/camlc_runtime.o" || exit 1

if [ $# -gt 0 ]; then
  tests=("$@")
else
  mapfile -t tests < <(ls "$ROOT"/compiler/tests-ocaml/*/*.ml)
fi
printf '%s\n' "${tests[@]}" | xargs -P "$JOBS" -I{} bash -c 'run_test "$1"' _ {} \
  | tee "$WORKDIR/results.txt"

version=$(ocamlfind ocamlc -vnum)
unexpected=
for t in $(grep '^FAIL' "$WORKDIR/results.txt" | cut -d' ' -f2); do
  known=
  while read -r v k; do
    case $v in '' | '#'*) continue ;; esac
    # shellcheck disable=SC2053
    if [[ $version == $v && $t == "$k" ]]; then known=1; fi
  done < "$ROOT/compiler/tests-c/known_failures"
  [ -n "$known" ] || unexpected="$unexpected $t"
done
if [ -n "$unexpected" ]; then
  echo "Unexpected failures:$unexpected"
  exit 1
fi
