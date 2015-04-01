
(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT                    *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *)
(********************************************************************)

open Format 
open LustreSpec
open Corelang

(********************************************************************************************)
(*                      Lusic to/from Header Printing functions                             *)
(********************************************************************************************)

type lusic =
{ from_lusi : bool;
  contents  : top_decl list;
}

module HeaderMod = C_backend_header.EmptyMod
module Header = C_backend_header.Main (HeaderMod)

(* extracts a header from a program representing module owner = dirname/basename *)
let extract_header dirname basename prog =
  let owner = dirname ^ "/" ^ basename in
 List.fold_right
   (fun decl header ->
(*Format.eprintf "Lusic.extract_header: owner = %s decl_owner = %s@." owner decl.top_decl_owner;*)
     if decl.top_decl_itf || decl.top_decl_owner <> owner then header else
    match decl.top_decl_desc with
    | Node nd        -> { decl with top_decl_desc = ImportedNode (Corelang.get_node_interface nd) } :: header 
    | ImportedNode _ -> header
    | Const _
    | TypeDef _
    | Open _         -> decl :: header)
   prog []

(* encode and write a header in a file *)
let write_lusic lusi (header : top_decl list) basename extension =
  let target_name = basename ^ extension in
  let outchan = open_out_bin target_name in
  begin
    Marshal.to_channel outchan {from_lusi = lusi; contents = header} [];
    close_out outchan
  end

(* read and decode a header from a file *)
let read_lusic basename extension =
  let source_name = basename ^ extension in
  let inchan = open_in_bin source_name in
  let lusic = (Marshal.from_channel inchan : lusic) in
  begin
    close_in inchan;
    lusic
  end

let print_lusic_to_h basename extension =
  let lusic = read_lusic basename extension in
  let header_name = basename ^ ".h" in
  let h_out = open_out header_name in
  let h_fmt = formatter_of_out_channel h_out in
  begin
    Typing.uneval_prog_generics lusic.contents;
    Clock_calculus.uneval_prog_generics lusic.contents;
    Header.print_header_from_header h_fmt (Filename.basename basename) lusic.contents;
    close_out h_out
  end


