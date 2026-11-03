(lang dune 3.20)
(name ppx_devtools)
(generate_opam_files true)

(authors "Ocsigen team <dev@ocsigen.org>")
(maintainers "Ocsigen team <dev@ocsigen.org>")
(source (github ocsigen/ppx_devtools))
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(package
 (name ppx_devtools-optcomp)
 (synopsis "Lightweight ppx for conditional compilation")
 (description
  "ppx_devtools-optcomp gates code at compile time with [@if] attributes, depending on the OCaml version, the operating system or the compiler flavour.")
 (depends
  (ocaml (>= 4.13))
  (ppxlib (>= 0.33))))

(package
 (name ppx_devtools-expect)
 (synopsis "Lightweight inline expect tests, with conditional snapshots")
 (description
  "ppx_devtools-expect is an inline expect-test ppx in the style of ppx_expect, with snapshots conditional on runtime axes such as the backend or the OCaml version. Tests run natively, with js_of_ocaml and with wasm_of_ocaml.")
 (depends
  (ocaml (>= 4.13))
  (ppxlib (>= 0.33))
  (ppx_devtools-optcomp (= :version))
  out-channel-redirect))
