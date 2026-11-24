#!/bin/bash
# Install c_of_ocaml in the current opam switch as opam-repository will:
# from an archive of HEAD (git archive, as the release tarball of GitHub),
# with c_of_ocaml.opam and a url in a local opam repository, so that opam
# builds the package as a release (no dune subst, no .git). Then the
# installed command compiles a program to C, compiled natively with the
# bytecode runtime of the switch, and run (on macOS, compiled only).
#
#   tools/c_of_ocaml_opam_test.sh
#
# Environment:
#   OPAM_ARGS   more arguments of opam install (--with-test...)
#   LOWER_BOUNDS  if set, the oldest dependencies opam can choose, as the
#               lower-bounds job of opam-repository's CI does
#   CC          the C compiler (default: cc)
#   WORKDIR     where the repository and the test are (default: a temporary
#               directory, removed at the end)
#
# Removes the c_of_ocaml package and the local repository at the end,
# without WORKDIR.

set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
CC=${CC:-cc}
VERSION=$(sed -n 's/^let version = "\(.*\)"$/\1/p' "$ROOT/compiler/bin-c_of_ocaml/c_of_ocaml.ml")
[ -n "$VERSION" ] || { echo "no version in c_of_ocaml.ml" >&2; exit 1; }

if [ -z "${WORKDIR:-}" ]; then
  WORKDIR=$(mktemp -d)
  trap 'opam remove -y c_of_ocaml >/dev/null 2>&1 || true
        opam repo remove --all cooc-test >/dev/null 2>&1 || true
        rm -rf "$WORKDIR"' EXIT
  trap 'exit 1' INT TERM
fi
mkdir -p "$WORKDIR"
WORKDIR=$(cd "$WORKDIR" && pwd)

archive=$WORKDIR/c_of_ocaml-$VERSION.tar.gz
git -C "$ROOT" archive --prefix="c_of_ocaml-$VERSION/" -o "$archive" HEAD
pkg=$WORKDIR/repo/packages/c_of_ocaml/c_of_ocaml.$VERSION
mkdir -p "$pkg"
echo 'opam-version: "2.0"' > "$WORKDIR/repo/repo"
{
  grep -v '^# This file is generated' "$ROOT/c_of_ocaml.opam"
  printf 'url {\n  src: "file://%s"\n  checksum: "sha256=%s"\n}\n' \
    "$archive" "$( (sha256sum "$archive" 2>/dev/null || shasum -a 256 "$archive") |
                   cut -d' ' -f1)"
} > "$pkg/opam"
opam lint "$pkg/opam"

# (--all: a repository removed from the switch only stays configured)
opam repo remove --all cooc-test >/dev/null 2>&1 || true
opam repo add -y --this-switch cooc-test "file://$WORKDIR/repo"
if [ -n "${LOWER_BOUNDS:-}" ]; then
  # (opam-repo-ci's solver and criteria)
  export OPAMEXTERNALSOLVER=builtin-0install
  export OPAMCRITERIA='+removed,+count[version-lag,solution]'
  export OPAMFIXUPCRITERIA=$OPAMCRITERIA OPAMUPGRADECRITERIA=$OPAMCRITERIA
fi
opam install -y ${OPAM_ARGS:-} "c_of_ocaml.$VERSION"
unset OPAMEXTERNALSOLVER OPAMCRITERIA OPAMFIXUPCRITERIA OPAMUPGRADECRITERIA
opam list --installed --columns=name,version

eval "$(opam env)"
c_of_ocaml --version
c_of_ocaml --version | grep -q "^$VERSION "
# only the command and the files of the package
opam show --list-files c_of_ocaml
if opam show --list-files c_of_ocaml | grep -v '/c_of_ocaml\(\.exe\)\?$' |
     grep -v '/\(lib\|doc\)/c_of_ocaml/' | grep -q .; then
  echo "c_of_ocaml installs files out of its directories" >&2
  exit 1
fi

cd "$WORKDIR"
STDLIB=$(ocamlc -where)
WORD_SIZE=$(ocamlc -config | sed -n 's/^word_size: //p')
cat > hello.ml <<'EOF'
let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
let () = Printf.printf "hello %d %s\n" (fib 20) (String.concat "," [ "a"; "b" ])
EOF
ocamlc -g hello.ml -o hello.bc
mkdir rt
c_of_ocaml --runtime-files rt
c_of_ocaml --word-size "$WORD_SIZE" hello.bc -o hello.c
CFLAGS="-O1 -fno-strict-aliasing -fwrapv -w -Irt -I$STDLIB"
if [ "$(uname -s)" = Darwin ]; then
  # macOS's linker takes other members of libcamlrun.a than the linkers of
  # GNU and wasm (emcc) do, whose definitions the runtime support replaces:
  # the C code is compiled only
  "$CC" $CFLAGS -c hello.c rt/camlc_runtime.c
  echo PASS
  exit 0
fi
"$CC" $CFLAGS hello.c rt/camlc_runtime.c "$STDLIB/libcamlrun.a" -lm -lpthread \
  -o hello
out=$(./hello)
echo "$out"
[ "$out" = "hello 6765 a,b" ]
echo PASS
