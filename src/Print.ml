(*
 * Copyright (c) 2009 CNRS & Université Bordeaux 1.
 *
 * Author(s): Grégoire Sutre <gregoire.sutre@labri.fr>, 
 *   modified by Julien Forget <julien.forget@lifl.fr>
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


(*
 * Signatures and helper functions for pretty-printing.
 *)


module type PRINTABLE_TYPE =
sig
  type t
  val print : Format.formatter -> t -> unit
end


let string_converter_from_printer printer =
  function data ->
    Format.fprintf Format.str_formatter "@[%a@]" printer data ;
    Format.flush_str_formatter ()

let hashtbl_printer_from_printer beg_f sep_f end_f printer fmt hashtbl =
  let length = Hashtbl.length hashtbl in
  if length > 0 then
    begin
      Format.fprintf fmt beg_f;
      ignore(
      Hashtbl.fold
        (fun k v cpt ->
          if cpt < length then            
            begin
              Format.fprintf fmt "@[%a@]" printer (k,v);
              Format.fprintf fmt sep_f;
              cpt+1
            end
          else
            begin
              Format.fprintf fmt "@[%a@]" printer (k,v);
              Format.fprintf fmt end_f;
              cpt+1
            end)
        hashtbl 1)
    end

let list_printer_from_printer beg_f sep_f end_f printer fmt list =
  match list with
      []  -> ()
    | head::tail ->
        Format.fprintf fmt beg_f;
        Format.fprintf fmt "@[%a@]" printer head;
        List.iter
          (function data ->
             begin
               Format.fprintf fmt sep_f;
               Format.fprintf fmt "@[%a@]" printer data
             end)
          tail;
        Format.fprintf fmt end_f

let array_printer_from_printer beg_f sep_f end_f printer fmt array =
  if (Array.length array) > 0 then
    let n = Array.length array
    in
    Format.fprintf fmt beg_f;
      for i = 0 to n - 2 do
        Format.fprintf fmt "@[%a@]" printer (i, array.(i)) ;
        Format.fprintf fmt sep_f
      done ;
      Format.fprintf fmt "@[%a@]" printer (n-1, array.(n-1));
    Format.fprintf fmt end_f

(* Local Variables: *)
(* compile-command:"make -C .." *)
(* End: *)
