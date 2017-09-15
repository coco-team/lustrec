open Basetypes 
open Datatype
open CPS_transformer
open Theta 

module Semantics = functor (T: TransformerType) (M: MODEL_T) ->
struct
  
module Prog =
struct
  let init = fst M.model
  let defs = snd M.model
  let vars = SF.states M.model
(*let _ = Format.printf "Model definitions@.%a@.####" Simulink.pp_src defs; () *)
end


module Interp = CPS_interpreter.Interpreter (T)
module KenvTheta = KenvTheta (T)
module Tables = KenvTheta.MemoThetaTables ()

let eval ((modular_entry:bool), (modular_during:bool), (modular_exit:bool)) =
  let module Modularity : KenvTheta.ModularType =
      struct
	let modular : type b. (path_t, b, bool) tag_t -> path_t -> b =
			fun tag ->
			  match tag with
			  | E -> (fun p p' f -> modular_entry)
			  | D -> (fun p      -> modular_during)
			  | X -> (fun p f    -> modular_exit)
      end
  in
  let module Thetaify = KenvTheta.ModularThetaify (Tables) (Modularity) in
  let module EvalProg = Interp.Evaluation (Thetaify) (Prog) in
  (module EvalProg: Interp.EvaluationType)
  
let compute modular  =
  let module Eval = (val (eval modular)) in
  Eval.eval_prog 
    
let code_gen fmt modular  =
  let module Eval = (val (eval modular)) in
  let principal, components =  Eval.eval_prog, Eval.eval_components in
  Format.fprintf fmt "%a@." T.pp_principal principal;

  List.iter
    (fun (c, tr) -> Format.fprintf fmt "@.%a@." (fun fmt -> T.pp_component fmt Ecall c) tr)
    (components Ecall);
  List.iter
    (fun (c, tr) -> Format.fprintf fmt "@.%a@." (fun fmt -> T.pp_component fmt Dcall c) tr)
    (components Dcall);
  List.iter
    (fun (c, tr) -> Format.fprintf fmt "@.%a@." (fun fmt -> T.pp_component fmt Xcall c) tr)
    (components Xcall);

  ()

end
