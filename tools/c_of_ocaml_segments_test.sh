#!/bin/bash
# Test the segmented stack (compiler/tests-c/segments): deep recursions,
# compiled by c_of_ocaml and emscripten with JSPI (-sJSPI, the new stacks
# given by jspi_host.c, as a host of the generated code does), or with
# c_of_ocaml --cps-stack (MODE=cps), run by node on its main thread, whose
# stack (984 KB) holds tens of thousands of nested calls only.
#
#   tools/c_of_ocaml_segments_test.sh
#
# The output and the exit code are compared with segments.expected and 3.
# Prints PASS, FAIL, or SKIP when node does not support JSPI.
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
cp "$TEST/segments.ml" .

ocamlfind ocamlc segments.ml -o segments.bc
"$C_OF_OCAML" $COOC_FLAGS segments.bc -o segments.c
emcc $CCFLAGS -Wno-everything -c segments.c -o segments.o
emcc $CCFLAGS -c "$RT/camlc_runtime.c" -o camlc_runtime.o
if [ -n "$HOST" ]; then emcc $CCFLAGS -c "$TEST/jspi_host.c" -o "$HOST"; fi
emcc $CFLAGS segments.o camlc_runtime.o $HOST "$SYSROOT/lib/libcamlrun.a" -lm \
  -sALLOW_MEMORY_GROWTH=1 -sEXIT_RUNTIME=1 $LDFLAGS -o segments.js
rc=0
timeout 300 "$NODE" $NODE_FLAGS --stack-size="$STACK_KB" segments.js \
  > segments.txt 2>&1 || rc=$?
if diff segments.txt "$TEST/segments.expected" && [ $rc = 3 ]; then
  echo PASS
else
  echo "FAIL (exit code $rc)"
fi
