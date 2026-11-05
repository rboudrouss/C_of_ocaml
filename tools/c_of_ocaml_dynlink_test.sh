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
#   CPS_STACK   if set, the program and the plugins are compiled with
#               --cps-stack (and so with double translation), and plugins
#               with deep recursions are tested, one compiled without it
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
# The flags of the plugin compiled without --cps-stack
PLAIN_FLAGS="$COOC_FLAGS --effects double-translation"
PEFFECT=
if [ "${EFFECTS:-}" = double-translation ]; then PEFFECT=1; fi
if [ -n "${CPS_STACK:-}" ]; then
  COOC_FLAGS="$COOC_FLAGS --cps-stack"
  # The effects of peffect.ml: OCaml 5
  case $(ocamlc -version) in 4.*) ;; *) PEFFECT=1 ;; esac
fi
# A runtime built with -pthread (its thread-local variables): so is the C
# code, and the program and the plugins are linked with threads, emscripten
# then relocating the thread-local variables of the plugins
if grep -qs '^cflags=.*-pthread' "$SYSROOT/manifest"; then CFLAGS="$CFLAGS -pthread"; fi

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
# A plugin compiled against another version of the interface of Shared
mkdir -p v2
cp shared.ml pcrc.ml v2/
echo "let extra = 0" >> v2/shared.ml
(cd v2 && ocamlc -c shared.ml && ocamlc -c pcrc.ml)
cp v2/pcrc.cmo .
plugins="plugin.cmo p2.cma p3.cmo p4uses.cmo shared.cmo pcrc.cmo"
if [ -n "$PEFFECT" ]; then
  ocamlc -c peffect.ml
  plugins="$plugins peffect.cmo"
fi
if [ -n "${CPS_STACK:-}" ]; then
  ocamlc -c pdeep.ml
  ocamlc -c pplain.ml
  plugins="$plugins pdeep.cmo pplain.cmo"
fi
main_objs=$(DYNLINK=--dynlink compile main.bc main)
for p in $plugins; do
  if [ "$p" = pplain.cmo ]; then
    objs=$(COOC_FLAGS=$PLAIN_FLAGS compile "$p" "${p%.*}" -fPIC)
  else
    objs=$(compile "$p" "${p%.*}" -fPIC)
  fi
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
# A C stack of emscripten (in the linear memory, not that of the engine) of
# 1 MB, as a host gives (ocaml-emcc: 8 MB), rather than 64 KB: the functions
# with a frame there (the arguments of an application whose arity differs)
# use it up to the segment limit, and with --cps-stack, two of them alternate
# up to it. An overflow aborts rather than corrupting the memory.
emcc $CFLAGS $main_objs camlc_runtime.o stub.o "$SYSROOT/lib/libcamlrun.a" -lm \
  -sMAIN_MODULE=2 -sEXPORTED_FUNCTIONS="$exports" -sNODERAWFS=1 \
  -sSTACK_SIZE=1MB -sSTACK_OVERFLOW_CHECK=2 \
  -sALLOW_MEMORY_GROWTH=1 -sEXIT_RUNTIME=1 $LDFLAGS -o main.js

result=PASS
node main.js plugin.so pcrc.so p4uses.so p2.so p4uses.so p3.so shared.so p2.so \
  > out.txt 2>&1 \
  || true
if ! diff out.txt "$TEST/dynlink.expected"; then result=FAIL; fi
if [ -n "$PEFFECT" ]; then
  node main.js peffect.so > effect.txt 2>&1 || true
  grep -q '^peffect: 42$' effect.txt || { cat effect.txt; result=FAIL; }
fi
if [ -n "${CPS_STACK:-}" ]; then
  node main.js pdeep.so pplain.so > deep.txt 2>&1 || true
  printf 'main\npdeep: 1000000 1000000 1000000\nloaded pdeep.so\n%s\n%s\n%s\n' \
    'pplain: 1000 1000000' 'loaded pplain.so' 'counter = 0, hooks: ' > deep.expected
  diff deep.txt deep.expected || result=FAIL
fi
echo $result
