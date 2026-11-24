# Releasing c_of_ocaml

## Merging js_of_ocaml

c_of_ocaml modifies js_of_ocaml's compiler library and builds it from this
tree: the `c_of_ocaml` package does not depend on `js_of_ocaml-compiler`,
the js_of_ocaml it is based on is the last commit of js_of_ocaml merged.

1. `git fetch upstream && git merge upstream/master` (or a tag of a
   release of js_of_ocaml, preferably).
2. Set `js_of_ocaml_base` in `compiler/bin-c_of_ocaml/c_of_ocaml.ml` to
   `git merge-base HEAD upstream/master` (the `opam` workflow checks it),
   and update the line "Based on js_of_ocaml" of the unreleased version
   in `CHANGES_c_of_ocaml.md`.
3. If js_of_ocaml's dependencies changed (`js_of_ocaml-compiler`'s in
   `dune-project`), change those of `c_of_ocaml` the same way, then
   `dune build c_of_ocaml.opam`.

## A release

1. Choose the version (see [Versions](README.md#versions)): the minor
   version when the options, the C code or the runtime support change,
   since ocaml-emcc depends on them.
2. Set `version` in `compiler/bin-c_of_ocaml/c_of_ocaml.ml`, replace
   "unreleased" by the date in `CHANGES_c_of_ocaml.md`, commit.
3. Check that the `c_of_ocaml` and `opam` workflows pass on this commit.
   `tools/c_of_ocaml_opam_test.sh` installs the package as
   opam-repository will, in the current switch.
4. Tag it with the version only (`0.1.0`, the `opam` workflow checks it
   against `version`) and push the tag.
5. `opam publish` (opam-publish) opens the pull request on
   opam-repository, with the archive of the tag:
   `https://github.com/rboudrouss/C_of_ocaml/archive/refs/tags/VERSION.tar.gz`.
6. Then a new unreleased section in `CHANGES_c_of_ocaml.md`, and the
   bounds of `c_of_ocaml` in ocaml-emcc's opam file if needed.
