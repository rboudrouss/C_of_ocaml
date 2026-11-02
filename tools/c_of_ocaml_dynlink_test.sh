#!/bin/bash
# Test the dynamic loading of plugins (compiler/tests-c/dynlink) with
# c_of_ocaml and emscripten.
#
#   tools/c_of_ocaml_dynlink_test.sh
#
# The program is compiled with c_of_ocaml --dynlink and linked with
# -sMAIN_MODULE=2, exporting the symbols the plugins need; the plugins
# (.cmo and .cma files) are compiled by c_of_ocaml to side modules
# (-sSIDE_MODULE=1), which it loads with dlopen. Its output is compared with
# dynlink.expected. Prints PASS or FAIL.
#
# Environment:
#   WORD_SIZE   32 (wasm32, the default) or 64 (wasm64)
#   SYSROOT     sysroot of ocaml-emcc for the target
#               (default: $OPAM_SWITCH_PREFIX/emcc-sysroot/wasm32 or wasm64)
#   C_OF_OCAML  the compiler (default: the one built by dune in this tree)
#   EFFECTS     if set, the effects option of c_of_ocaml (double-translation
#               also tests a plugin performing effects)
#   SPLIT       if set, the C code is split (c_of_ocaml --split), in parts of
#               SPLIT kilobytes
#   WORKDIR     where the test is built (default: a temporary directory,
#               removed at the end)
#   WASM_DIS    wasm-dis of Binaryen (default: the one of emsdk)
#
# emcc, node and ocamlc must be in PATH (node >= 24 for wasm64).

set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
TEST=$ROOT/compiler/tests-c/dynlink
WORD_SIZE=${WORD_SIZE:-32}
SYSROOT=${SYSROOT:-$OPAM_SWITCH_PREFIX/emcc-sysroot/wasm$WORD_SIZE}
C_OF_OCAML=${C_OF_OCAML:-$ROOT/_build/default/compiler/bin-c_of_ocaml/c_of_ocaml.exe}
# Binaryen's disassembler, in emsdk
WASM_DIS=${WASM_DIS:-$(dirname "$(dirname "$(command -v emcc)")")/bin/wasm-dis}
[ -x "$WASM_DIS" ] || { echo "wasm-dis not found (set WASM_DIS)"; exit 2; }
CFLAGS="-O1 -mtail-call -fno-strict-aliasing -fwrapv -fno-strict-overflow \
  -sSUPPORT_LONGJMP=wasm -fwasm-exceptions -I$ROOT/runtime/c -I$SYSROOT/lib/ocaml"
COOC_FLAGS=
LDFLAGS=
if [ "$WORD_SIZE" = 64 ]; then
  CFLAGS="$CFLAGS -m64"
  COOC_FLAGS="--word-size 64"
  LDFLAGS="-sMAXIMUM_MEMORY=16GB"
fi
if [ -n "${EFFECTS:-}" ]; then COOC_FLAGS="$COOC_FLAGS --effects $EFFECTS"; fi

if [ -z "${WORKDIR:-}" ]; then
  WORKDIR=$(mktemp -d)
  trap 'rm -rf "$WORKDIR"' EXIT
  trap 'exit 1' INT TERM
fi
mkdir -p "$WORKDIR"
cd "$WORKDIR"
cp "$TEST"/*.ml "$TEST"/stub.c .

# compile INPUT NAME [EMCC FLAGS...]: translates INPUT to C and compiles it,
# printing the object files
compile() {
  local input=$1 name=$2
  shift 2
  if [ -n "${SPLIT:-}" ]; then
    "$C_OF_OCAML" $COOC_FLAGS ${DYNLINK:-} "$input" --split "$name.parts" \
      --part-size "$SPLIT" >&2
    for c in "$name".parts/*.c; do
      emcc $CFLAGS "$@" -c "$c" -o "${c%.c}.o" >&2
      echo "${c%.c}.o"
    done
  else
    "$C_OF_OCAML" $COOC_FLAGS ${DYNLINK:-} "$input" -o "$name.c" >&2
    emcc $CFLAGS "$@" -c "$name.c" -o "$name.o" >&2
    echo "$name.o"
  fi
}

ocamlc -c shared.ml
ocamlc -linkall -use-prims <(echo test_loadfile; ocamlrun -p) shared.cmo main.ml \
  -o main.bc
for m in plugin p2a p2b p3 p4uses; do ocamlc -c $m.ml; done
ocamlc -a p2a.cmo p2b.cmo -o p2.cma
plugins="plugin.cmo p2.cma p3.cmo p4uses.cmo shared.cmo"
if [ "${EFFECTS:-}" = double-translation ]; then
  ocamlc -c peffect.ml
  plugins="$plugins peffect.cmo"
fi
main_objs=$(DYNLINK=--dynlink compile main.bc main)
for p in $plugins; do
  objs=$(compile "$p" "${p%.*}" -fPIC)
  emcc $CFLAGS -sSIDE_MODULE=1 $objs -o "${p%.*}.so"
done
emcc $CFLAGS -c "$ROOT/runtime/c/camlc_runtime.c" -o camlc_runtime.o
emcc $CFLAGS -c stub.c -o stub.o
# The program exports the symbols imported by the plugins
exports=_main
for s in $(for p in $plugins; do "$WASM_DIS" "${p%.*}.so"; done \
             | grep -o '(import "\(env\|GOT.mem\|GOT.func\)" "[^"]*"' \
             | awk '{print $3}' | tr -d '"' | grep -v '^__\|^memory$' | sort -u); do
  exports=$exports,_$s
done
emcc $CFLAGS $main_objs camlc_runtime.o stub.o "$SYSROOT/lib/libcamlrun.a" -lm \
  -sMAIN_MODULE=2 -sEXPORTED_FUNCTIONS="$exports" -sNODERAWFS=1 \
  -sALLOW_MEMORY_GROWTH=1 -sEXIT_RUNTIME=1 $LDFLAGS -o main.js

result=PASS
node main.js plugin.so p4uses.so p2.so p4uses.so p3.so shared.so p2.so > out.txt 2>&1 \
  || true
if ! diff out.txt "$TEST/dynlink.expected"; then result=FAIL; fi
if [ "${EFFECTS:-}" = double-translation ]; then
  node main.js peffect.so > effect.txt 2>&1 || true
  grep -q '^peffect: 42$' effect.txt || { cat effect.txt; result=FAIL; }
fi
echo $result
