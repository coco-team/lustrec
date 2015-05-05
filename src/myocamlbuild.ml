open Ocamlbuild_plugin
open Command
;;

dispatch begin function
| After_rules ->
    (* We declare external libraries *)
    ocaml_lib ~extern:true ~dir:"/Users/teme/.opam/4.02.1/lib/ocamlgraph" "graph";
  if @CC_NOASNEEDED@ then
    flag ["ocaml"; "link"]
      (S [A"-cclib";A"-Wl,--no-as-needed"]);
| _ -> ()
end

