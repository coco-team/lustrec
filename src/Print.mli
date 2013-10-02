(* $Id$ *)

(*
 * Copyright (c) 2009 CNRS & Université Bordeaux 1.
 *
 * Author(s): Grégoire Sutre <gregoire.sutre@labri.fr>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)


(**
  Signatures and helper functions for pretty-printing.
 
  This module follows the standard OCaml pretty-printing facility provided in
  the module [Format] of the standard library.  In particular, pretty-printing
  commands assume that there is an opened pretty-printing box.  This permits
  more flexibility, since the choice of the enclosing pretty-printing box may
  depend on the context.
 
  @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html> Format
 *)


(** Signature for a type equipped with a pretty-printing function. *)
module type PRINTABLE_TYPE =
sig
  (** The type. *)
  type t

  (** A pretty-printer for this type.  This function prints values of
   type [t] in the current pretty-printing box. *) val print :
   Format.formatter -> t -> unit end


(** Transform a pretty-printer into a string converter. *)
val string_converter_from_printer :
  (Format.formatter -> 'a -> unit) -> 'a -> string

(** [hashtbl_printer_from_printer beg sep endf printer] returns a
  pretty-printer for hashtbls of type [('a,'b) Hashtbl.t], given a three formatters
  [beg], [sep] and [endf], and a pretty-printer [printer] for values of
  type [('a,'b)].  The resulting pretty-printer first prints [beg], then each
  element of its argument list with [sep] between each element, and
  finally prints [endf].  Each element in the hashtbl is printed in a new
  enclosing pretty-printing box.  In other words,
  [hashtbl_printer_from_printer sep printer fmt [{k1|->v1; ...; kn|->vn}]] is
  equivalent to [
  begin
    Format.fprintf fmt beg;
    Format.fprintf fmt "@[%a@]" printer (k1,v1) ;
    Format.fprintf fmt sep ;
    ... ;
    Format.fprintf fmt sep ;
    Format.fprintf fmt "@[%a@]" printer (kn,vn);
    Format.fprintf fmt endf;
    end
  ].  Note that the separator [sep] may contain pretty-printing commands.  For
  instance [";@ "] could be used as a separator argument to this function.
 *)
val hashtbl_printer_from_printer :
    (unit, Format.formatter, unit) format ->
  (unit, Format.formatter, unit) format ->
  (unit, Format.formatter, unit) format ->
  (Format.formatter -> ('a*'b) -> unit) ->
  Format.formatter -> ('a,'b) Hashtbl.t -> unit

(** [list_printer_from_printer beg sep endf printer] returns a
  pretty-printer for lists of type ['a list], given three formatters
  [beg], [sep] and [endf], and a pretty-printer [printer] for values of
  type ['a].  The resulting pretty-printer first prints [beg], then each
  element of its argument list with [sep] between each element, and
  finally prints [endf].  Each element in the list is printed in a new
  enclosing pretty-printing box.  In other words,
  [list_printer_from_printer beg sep endf printer fmt [a1; ...; aN]] is
  equivalent to [
  begin
    Format.fprintf fmt beg;
    Format.fprintf fmt "@[%a@]" printer a1 ;
    Format.fprintf fmt sep ;
    ... ;
    Format.fprintf fmt sep ;
    Format.fprintf fmt "@[%a@]" printer aN
    Format.fprintf fmt endf;
    end
  ].  Note that the separator [sep] may contain pretty-printing commands.  For
  instance [";@ "] could be used as a separator argument to this function.
 *)
val list_printer_from_printer :
  (unit, Format.formatter, unit) format ->
  (unit, Format.formatter, unit) format ->
  (unit, Format.formatter, unit) format ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit

(** [array_printer_from_printer sep printer] returns a pretty-printer
  for arrays of type ['a array], given three formatters [beg], [sep] and
  [endf], and a pretty-printer [printer] for values of type [int * 'a].
  The resulting pretty-printer first prints [beg], then prints each pair
  [(i, a.(i))] of its argument array [a] and prints [sep] between each
  pair and finally prints [endf]. Each pair in the array is printed in
  a new enclosing pretty-printing box. In other words,
  [array_printer_from_printer beg sep end printer fmt [|a1; ...; aN|]] is
  equivalent to [
  begin
    Format.fprintf fmt beg;
    Format.fprintf fmt "@[%a@]" printer (0, a1) ;
    Format.fprintf fmt sep ;
    ... ;
    Format.fprintf fmt sep ;
    Format.fprintf fmt "@[%a@]" printer (N-1, aN)
    Format.fprintf fmt endf;
    end
  ].  Note that the separator [sep] may contain pretty-printing commands.
  For instance [";@ "] could be used as a separator argument to this function.
*)
val
  array_printer_from_printer : (unit, Format.formatter, unit) format ->
  (unit, Format.formatter, unit) format -> (unit, Format.formatter,
  unit) format -> (Format.formatter -> (int * 'a) -> unit) ->
  Format.formatter -> 'a array -> unit
