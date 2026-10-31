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

val f :
     Code.program
  -> global_flow_data:Global_flow.state * Global_flow.info
  -> deadcode_sentinel:Code.Var.t
  -> debug_info:bool
  -> in_cps:Effects.in_cps
  -> trampolined_calls:Effects.trampolined_calls
  -> ?part_size:int
  -> Closure_conversion.closure Code.Var.Map.t
  -> string list
(** [f p ~global_flow_data ~deadcode_sentinel ~debug_info ~in_cps
     ~trampolined_calls closures] is the C translation of the whole program [p],
    whose closures are [closures] (see {!Closure_conversion.f}): a single C
    file, or with [part_size], several, which can be compiled separately: the
    static data and the tables of the runtime, then the functions, in parts
    of about [part_size] bytes. With
    [debug_info], it records backtraces at the locations of the debug events of
    [p]. With effect handlers (double translation), [in_cps] are the CPS calls
    and [trampolined_calls] those which may bounce (see {!Effects.f}). *)
