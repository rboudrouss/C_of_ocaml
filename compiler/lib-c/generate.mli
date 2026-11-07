(* C_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type mode =
  [ `Program  (** A program *)
  | `Dynlink
    (** A program which can load plugins: it registers its compilation
        units, and handles the partial applications of functions of arity up
        to 64 *)
  | `Plugin
    (** A plugin, from compilation units which refer to the units of the
        program or of other plugins by name *)
  ]

type result =
  { files : string list  (** The C files *)
  ; primitives : (string * int) list
        (** The C primitives the code calls, with the number of arguments of
            their calls, or -1 for those called with an array of arguments
            and its length (more than 5 arguments), sorted by name *)
  }

val f :
     Code.program
  -> ?mode:mode
  -> ?interfaces:(string * string) list
  -> global_flow_data:Global_flow.state * Global_flow.info
  -> deadcode_sentinel:Code.Var.t
  -> debug_info:bool
  -> in_cps:Effects.in_cps
  -> trampolined_calls:Effects.trampolined_calls
  -> ?toplevel_chunk_size:int
  -> ?part_size:int
  -> Closure_conversion.closure Code.Var.Map.t
  -> result
(** [f p ~global_flow_data ~deadcode_sentinel ~debug_info ~in_cps
     ~trampolined_calls closures] is the C translation of the whole program [p],
    whose closures are [closures] (see {!Closure_conversion.f}): a single C
    file, or with [part_size], several, which can be compiled separately: the
    static data and the tables of the runtime, then the functions, in parts
    of about [part_size] bytes. A plugin defines the entry point
    [camlc_plugin_init] (see [runtime/c/camlc.h]), its other symbols are
    hidden. [interfaces] are the names and CRCs (in hexadecimal) of the
    interfaces a program compiled with [`Dynlink] or a plugin is compiled
    against, which the runtime checks when it loads a plugin. With
    [debug_info], it records backtraces at the locations of the debug events of
    [p]. With effect handlers (double translation), [in_cps] are the CPS calls
    and [trampolined_calls] those which may bounce (see {!Effects.f}). The
    toplevel code is split into functions of about [toplevel_chunk_size]
    instructions (default 2000, 0 for a single function). *)
