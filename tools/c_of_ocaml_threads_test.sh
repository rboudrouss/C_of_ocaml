#!/bin/bash
# Test the systhreads and the domains (compiler/tests-c/threads) with
# c_of_ocaml, natively: the C code is compiled with the C compiler and
# linked with the bytecode runtime of the OCaml switch.
#
#   tools/c_of_ocaml_threads_test.sh
#
# The outputs are compared with threads.expected and, with OCaml 5,
# domains.expected. Prints PASS or FAIL.
#
# Environment:
#   C_OF_OCAML  the compiler (default: the one built by dune in this tree)
#   CC          the C compiler (default: clang)
#   CFLAGS      more flags of the C compiler (-fsanitize=thread...)
#   EFFECTS     if set, the effects option of c_of_ocaml
#   WORKDIR     where the test is built (default: a temporary directory,
#               removed at the end)
#
# ocamlfind and the threads library must be installed in the switch.

set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
TEST=$ROOT/compiler/tests-c/threads
C_OF_OCAML=${C_OF_OCAML:-$ROOT/_build/default/compiler/bin-c_of_ocaml/c_of_ocaml.exe}
CC=${CC:-clang}
STDLIB=$(ocamlc -where)
WORD_SIZE=$(ocaml -version >/dev/null; ocamlfind ocamlc -config | sed -n 's/^word_size: //p')
COOC_FLAGS="--word-size $WORD_SIZE"
if [ -n "${EFFECTS:-}" ]; then COOC_FLAGS="$COOC_FLAGS --effects $EFFECTS"; fi
LIBUNIX=$STDLIB/libunixbyt.a
[ -f "$LIBUNIX" ] || LIBUNIX=$STDLIB/libunix.a

if [ -z "${WORKDIR:-}" ]; then
  WORKDIR=$(mktemp -d)
  trap 'rm -rf "$WORKDIR"' EXIT
  trap 'exit 1' INT TERM
fi
mkdir -p "$WORKDIR"
cd "$WORKDIR"
cp "$TEST"/*.ml .

tests=threads
case $(ocamlc -version) in 4.*) ;; *) tests="$tests domains" ;; esac
"$CC" -O1 -g ${CFLAGS:-} -I"$ROOT/runtime/c" -I"$STDLIB" \
  -c "$ROOT/runtime/c/camlc_runtime.c" -o camlc_runtime.o
result=PASS
for t in $tests; do
  ocamlfind ocamlc -thread -package threads.posix -linkpkg $t.ml -o $t.bc \
    2>/dev/null
  "$C_OF_OCAML" $COOC_FLAGS $t.bc -o $t.c
  "$CC" -O1 -g ${CFLAGS:-} -fno-strict-aliasing -fwrapv -w -I"$ROOT/runtime/c" \
    -I"$STDLIB" $t.c camlc_runtime.o "$STDLIB/libthreads.a" "$LIBUNIX" \
    "$STDLIB/libcamlrun.a" -lm -lpthread -o $t
  timeout 120 ./$t > $t.txt 2>&1 || true
  if ! diff $t.txt "$TEST/$t.expected"; then result=FAIL; fi
done
echo $result
