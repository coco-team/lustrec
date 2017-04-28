open Lusic

let print_lusic_to_h basename extension =
let module HeaderMod = C_backend_header.EmptyMod in
let module Header = C_backend_header.Main (HeaderMod) in
  let lusic = read_lusic basename extension in
  let header_name = basename ^ ".h" in
  let h_out = open_out header_name in
  let h_fmt = Format.formatter_of_out_channel h_out in
  begin
    assert (not lusic.obsolete);
    (*Format.eprintf "lusic to h: %i items.@." (List.length lusic.contents);*)
    Typing.uneval_prog_generics lusic.contents;
    Clock_calculus.uneval_prog_generics lusic.contents;
    Header.print_header_from_header h_fmt (Filename.basename basename) lusic.contents;
    close_out h_out
  end

