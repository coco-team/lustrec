open Basetypes
open ActiveStates
open CPS_transformer

module CodeGenerator : ComparableTransformerType =
struct
  include TransformerStub

  type t =
  | Bot
  | Act of act_t
  | Seq of t list
  | Ite of cond_t * t * t

  let null = Seq []

  let bot = Bot
 
  let ( >> ) tr1 tr2 =
    match tr1, tr2 with
    | Seq trl1, Seq trl2 -> Seq (trl1@trl2)
    | Seq trl1, _        -> Seq (trl1@[tr2])
    | _       , Seq trl2 -> Seq (tr1::trl2)
    | _                  -> Seq ([tr1;tr2])

  let rec ( == ) tr1 tr2 = tr1 = tr2

  let eval_act kenv (action : act_t) =
    (*Format.printf "----- action = %a@." Action.pp_act action;*)
    Act action

  (*if (match trans.event with None -> true | _ -> e = trans.event) && trans.condition rho*)
  let rec eval_cond condition ok ko =
    (*Format.printf "----- cond = %a@." Condition.pp_cond condition;*)
    Ite (condition, ok, ko)
    
  let rec pp_transformer fmt tr =
    match tr with
    | Bot           -> Format.fprintf fmt "bot"
    | Act a         ->
       Format.fprintf fmt "<%a>" pp_act a
    | Seq trl       ->
       Format.fprintf fmt "@[<v 0>%a@]"
	 (Utils.fprintf_list ~sep:";@ " pp_transformer)
	 trl
    | Ite (c, t, e) ->
       Format.fprintf fmt "@[<v 0>if %a@ @[<v 2>then@ %a@]@ @[<v 2>else@ %a@]@ endif@]" pp_cond c pp_transformer t pp_transformer e

  let pp_principal fmt tr =
    Format.fprintf fmt "principal =@.%a" pp_transformer tr
      
  let pp_component : type c. Format.formatter -> c call_t -> c -> t -> unit =
    fun fmt call -> match call with
    | Ecall -> (fun (p, p', f) tr ->
      Format.fprintf fmt "component %a(%a, %a, %a) =@.@[<v 2>begin@ %a@]@.end" pp_call call pp_path p pp_path p' pp_frontier f pp_transformer tr)
    | Dcall -> (fun p tr ->
      Format.fprintf fmt "component %a(%a) =@.@[<v 2>begin@ %a@]@.end" pp_call call pp_path p pp_transformer tr)
    | Xcall -> (fun (p, f) tr ->
      Format.fprintf fmt "component %a(%a, %a) =@.@[<v 2>begin@ %a@]@.end" pp_call call pp_path p pp_frontier f pp_transformer tr)

		     let mkcomponent _  = assert false
		     let mkprincipal _  = assert false
		     let mktransformer _  = assert false
		       
end

