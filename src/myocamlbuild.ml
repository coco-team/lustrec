open Ocamlbuild_plugin
open Command
;;

dispatch begin function
| After_rules ->
    (* We declare external libraries *)
    ocaml_lib ~extern:true ~dir:"/Users/Teme/.opam/system/lib/ocamlgraph" "graph";
  if @CC_NOASNEEDED@ then
    flag ["ocaml"; "link"]
      (S [A"-cclib";A"-Wl,--no-as-needed"]);
| _ -> ()
end

