open Vhdl_ast_map
open Vhdl_to_lustre
open Vhdl_ast_deriving
open Ppxlib_traverse_builtins

let any x = x

let replace_op_expr = object (self)
  inherit Ppxlib_traverse_builtins.map
  inherit vhdl_map as super

  method unit: unit T.map = any

  method vhdl_expr_t = function
    | Op ({id=""; args=hd::[]}) -> self#vhdl_expr_t hd
    | x -> super#vhdl_expr_t x
end

let to_lustre = object (self)
  inherit Ppxlib_traverse_builtins.map
  inherit vhdl_to_lustre_map as super

  method unit: unit T.map = any
end
