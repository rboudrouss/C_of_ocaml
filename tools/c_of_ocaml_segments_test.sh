#!/bin/bash
# Test the segmented stack (compiler/tests-c/segments): deep recursions,
# compiled by c_of_ocaml and emscripten with JSPI (-sJSPI, the new stacks
# given by jspi_host.c, as a host of the generated code does), or with
# c_of_ocaml --cps-stack (MODE=cps), run by node on its main thread, whose
# stack (984 KB) holds tens of thousands of nested calls only.
#
#   tools/c_of_ocaml_segments_test.sh
#
# The output and the exit code are compared with segments.expected and 3,
# those of callbacks.ml (with callbacks_stubs.c) with callbacks.expected and
# 0, and with OCaml 5.4 or later, those of ramp_up.ml with ramp_up.expected
# and 0. Prints PASS or FAIL for each, or SKIP when node does not support
# JSPI.
#
# Environment:
#   MODE        jspi (the default) or cps
#   WORD_SIZE   32 (wasm32, the default) or 64 (wasm64)
#   SYSROOT     sysroot of ocaml-emcc for the target
#               (default: $OPAM_SWITCH_PREFIX/emcc-sysroot/wasm32 or wasm64)
#   C_OF_OCAML  the compiler (default: the one built by dune in this tree)
#   NODE        node (default: node; >= 24, and >= 24 for wasm64)
#   STACK_KB    the stack of node, in KB (default: 984, as in browsers)
#   WORKDIR     where the test is built (default: a temporary directory,
#               removed at the end)
#
# emcc and ocamlfind must be in PATH.

set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
TEST=$ROOT/compiler/tests-c/segments
WORD_SIZE=${WORD_SIZE:-32}
SYSROOT=${SYSROOT:-$OPAM_SWITCH_PREFIX/emcc-sysroot/wasm$WORD_SIZE}
C_OF_OCAML=${C_OF_OCAML:-$ROOT/_build/default/compiler/bin-c_of_ocaml/c_of_ocaml.exe}
NODE=${NODE:-node}
STACK_KB=${STACK_KB:-984}
RT=$ROOT/runtime/c
MODE=${MODE:-jspi}

if [ "$MODE" = jspi ] && ! "$NODE" --experimental-wasm-jspi -e \
  'process.exit(typeof WebAssembly.Suspending == "function" ? 0 : 1)' 2>/dev/null
then
  echo SKIP
  exit 0
fi

CFLAGS="-O2 -mtail-call -fno-strict-aliasing -fwrapv -fno-strict-overflow \
  -sSUPPORT_LONGJMP=wasm -fwasm-exceptions -I$RT -I$SYSROOT/lib/ocaml"
COOC_FLAGS=
LDFLAGS=
NODE_FLAGS=
HOST=
case $MODE in
  jspi) LDFLAGS=-sJSPI; NODE_FLAGS=--experimental-wasm-jspi; HOST=jspi_host.o ;;
  cps) COOC_FLAGS=--cps-stack ;;
  *) echo "MODE: jspi or cps"; exit 2 ;;
esac
if [ "$WORD_SIZE" = 64 ]; then
  CFLAGS="$CFLAGS -m64"
  COOC_FLAGS="$COOC_FLAGS --word-size 64"
  LDFLAGS="$LDFLAGS -sMAXIMUM_MEMORY=16GB"
fi
CCFLAGS=$CFLAGS
if grep -qs '^cflags=.*-pthread' "$SYSROOT/manifest"; then CCFLAGS="$CCFLAGS -pthread"; fi

if [ -z "${WORKDIR:-}" ]; then
  WORKDIR=$(mktemp -d)
  trap 'rm -rf "$WORKDIR"' EXIT
  trap 'exit 1' INT TERM
fi
mkdir -p "$WORKDIR"
cd "$WORKDIR"
# build NAME: NAME.js from $TEST/NAME.ml
build() {
  cp "$TEST/$1.ml" .
  local stubs=
  if [ -f "$TEST/${1}_stubs.c" ]; then
    emcc $CCFLAGS -c "$TEST/${1}_stubs.c" -o "${1}_stubs.o"
    stubs=${1}_stubs.o
  fi
  if [ -n "$stubs" ]; then
    # The primitives of the stubs, unknown to ocamlrun
    sed -n 's/^value \([a-z_]*\)(.*/\1/p' "$TEST/${1}_stubs.c" > prims
    ocamlrun -p >> prims
    ocamlfind ocamlc -use-prims prims "$1.ml" -o "$1.bc"
  else
    ocamlfind ocamlc "$1.ml" -o "$1.bc"
  fi
  "$C_OF_OCAML" $COOC_FLAGS "$1.bc" -o "$1.c"
  emcc $CCFLAGS -Wno-everything -c "$1.c" -o "$1.o"
  # The C stack of emscripten (in the linear memory): its default size,
  # 64 KB, checked, as the deep recursions must not grow it
  emcc $CFLAGS "$1.o" $stubs camlc_runtime.o $HOST "$SYSROOT/lib/libcamlrun.a" -lm \
    -sALLOW_MEMORY_GROWTH=1 -sEXIT_RUNTIME=1 -sSTACK_SIZE=64KB \
    -sSTACK_OVERFLOW_CHECK=2 $LDFLAGS -o "$1.js"
}
# check NAME CODE: its output and exit code are $TEST/NAME.expected and CODE
check() {
  local rc=0
  timeout 300 "$NODE" $NODE_FLAGS --stack-size="$STACK_KB" "$1.js" \
    > "$1.txt" 2>&1 || rc=$?
  if diff "$1.txt" "$TEST/$1.expected" && [ $rc = "$2" ]; then
    echo "PASS $1"
  else
    echo "FAIL $1 (exit code $rc)"
  fi
}

emcc $CCFLAGS -c "$RT/camlc_runtime.c" -o camlc_runtime.o
if [ -n "$HOST" ]; then emcc $CCFLAGS -c "$TEST/jspi_host.c" -o "$HOST"; fi
build segments
check segments 3
build callbacks
check callbacks 0
# Gc.ramp_up, OCaml 5.4 or later
if [ "$(printf '%s\n' 5.4 "$(ocamlfind ocamlc -vnum)" | sort -V | head -1)" = 5.4 ]; then
  build ramp_up
  check ramp_up 0
fi
