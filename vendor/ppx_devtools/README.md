# ppx_devtools

Two lightweight ppx rewriters, extracted from
[js_of_ocaml](https://github.com/ocsigen/js_of_ocaml):

- `ppx_devtools-optcomp`: compile-time gating of code with `[@if]`
  attributes.
- `ppx_devtools-expect`: inline expect tests in the style of `ppx_expect`,
  with conditional snapshots. Tests run natively, and under js_of_ocaml and
  wasm_of_ocaml.

They are released as two opam packages, `ppx_devtools-optcomp` and
`ppx_devtools-expect`. `ppx_devtools-optcomp` only depends on `ppxlib`;
`ppx_devtools-expect` also uses `out-channel-redirect` to capture the output of
tests. Both support OCaml 4.13 and later, including OxCaml.

## `ppx_devtools-optcomp`

```dune
(preprocess (pps ppx_devtools-optcomp))
```

An item carrying `[@if COND]` (or `[@@if COND]`) is removed when `COND` is
false:

```ocaml
let input_all = In_channel.input_all [@@if ocaml_version >= (5, 1, 0)]

let input_all ic = really_input_string ic (in_channel_length ic)
[@@if ocaml_version < (5, 1, 0)]
```

`[@if]` can be placed on toplevel bindings, externals, modules (also in
signatures), toplevel extension nodes such as `let%expect_test`, and the
patterns of match cases. A floating `[@@@if COND]` removes all the items that
follow it in the file when `COND` is false.

The available atoms are:

- `ocaml_version`, compared with a version tuple: `ocaml_version >= (5, 1, 0)`;
- `ast_version`, the ppxlib AST version;
- `os_type`: `"Unix"`, `"Win32"` or `"Cygwin"`;
- `arch_sixtyfour`;
- `oxcaml`: whether the compiler is OxCaml;
- `introspect`: whether the compiler was built with introspection support.

They can be combined with `not`, `&&`, `||` and the comparison operators.

`ppx_devtools-optcomp` is **not** a replacement for `ppx_optcomp`: there is no
`[%%if]`, `[%%define]` or `.mlh` import. Both use the `[@if]` attribute, so
they should not be used together.

## `ppx_devtools-expect`

```dune
(library
 (name my_tests)
 (inline_tests
  (modes best js wasm))
 (preprocess
  (pps ppx_devtools-expect)))
```

```ocaml
let%expect_test "addition" =
  Printf.printf "%d" (1 + 2);
  [%expect {| 3 |}]
```

`[%expect {| ... |}]` checks the output printed since the previous assertion
point. `[%expect.output]` returns it as a string instead. When the output does
not match, `dune promote` updates the test.

### Conditional snapshots and tests

A `[@when COND]` attribute on an expectation makes it apply only when `COND`
holds at runtime. The plain `[%expect]` of the group is the default:

```ocaml
let%expect_test "float32 marshalling" =
  print_string (Marshal.to_string 1.0s []);
  [%expect ({| <js bytes> |} [@when js])];
  [%expect {| <native/wasm bytes> |}]
```

On the test name, `[@when COND]` skips the whole test when `COND` is false, and
`[@tags "a", "b"]` attaches inline-test tags: `disabled`, `js-only` /
`no-js`, `wasm-only` / `no-wasm`, `native-only`, `64-bits-only` /
`32-bits-only`.

The identifiers of runtime predicates are *axes*. The predefined ones are
`backend` (`"native"`, `"js"` or `"wasm"`, with shorthands `native`, `js`,
`wasm`), `os_type` (with shorthands `unix`, `win32`, `cygwin`),
`ocaml_version` and `oxcaml`.

### Adding axes

A ppx library can add axes with `Ppx_devtools_expect.register_axes`, mapping an
identifier to the fully qualified name of its runtime value. The values come
from a library listed in its `ppx_runtime_libraries`:

```dune
(library
 (name my_axes_ppx)
 (kind ppx_rewriter)
 (libraries ppx_devtools-expect)
 (ppx_runtime_libraries my_axes))
```

```ocaml
let () =
  Ppx_devtools_expect.register_axes (function
    | "fancy" -> Some "My_axes.fancy"
    | _ -> None)
```

Users then just add the ppx: `(pps ppx_devtools-expect my_axes_ppx)`. An axis is
a `bool` when used on its own, and otherwise has the type of the literal it is
compared with. Axes registered later take precedence over earlier ones and over
the predefined axes.

## License

LGPL-2.1-or-later with the OCaml linking exception. See [LICENSE](LICENSE).
