let base_types =
  ["integer";
  "character";
  "bit";
  "real";
  "natural";
  "positive";
  "std_logic";
  "std_logic_vector"] 
let std_logic_cst = ["U"; "X"; "0"; "1"; "Z"; "W"; "L"; "H"; "-"] 
let literal_base = ["B"; "O"; "X"; "UB"; "UO"; "UX"; "SB"; "SO"; "SX"; "D"] 
type vhdl_cst_val_t =
  | CstInt of int 
  | CstStdLogic of string 
  | CstLiteral of string [@name "CST_LITERAL"]

let rec (pp_vhdl_cst_val_t :
          Format.formatter -> vhdl_cst_val_t -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | CstInt a0 ->
             (Format.fprintf fmt "%d") a0;
        | CstStdLogic a0 ->
             (Format.fprintf fmt "%S") a0;
        | CstLiteral a0 ->
             (Format.fprintf fmt "%s") a0;)
  [@ocaml.warning "-A"])

and show_vhdl_cst_val_t : vhdl_cst_val_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_cst_val_t x

let rec (vhdl_cst_val_t_to_yojson : vhdl_cst_val_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | CstInt arg0 ->
          `List
            [`String "CstInt";
            ((fun (x : Ppx_deriving_runtime.int)  -> `Int x)) arg0]
      | CstStdLogic arg0 ->
          `List
            [`String "CstStdLogic";
            ((fun (x : Ppx_deriving_runtime.string)  -> `String x)) arg0]
      | CstLiteral arg0 ->
          `List
            [`String "CST_LITERAL";
            ((fun (x : Ppx_deriving_runtime.string)  -> `String x)) arg0])
  [@ocaml.warning "-A"])

and (vhdl_cst_val_t_of_yojson :
      Yojson.Safe.json -> vhdl_cst_val_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "CstInt")::arg0::[]) ->
          ((function
            | `Int x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_cst_val_t") arg0) >>=
            ((fun arg0  -> Result.Ok (CstInt arg0)))
      | `List ((`String "CstStdLogic")::arg0::[]) ->
          ((function
            | `String x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_cst_val_t") arg0) >>=
            ((fun arg0  -> Result.Ok (CstStdLogic arg0)))
      | `List ((`String "CST_LITERAL")::arg0::[]) ->
          ((function
            | `String x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_cst_val_t") arg0) >>=
            ((fun arg0  -> Result.Ok (CstLiteral arg0)))
      | _ -> Result.Error "Vhdl_ast.vhdl_cst_val_t")
  [@ocaml.warning "-A"])

type vhdl_type_t =
  | Base of string 
  | Range of string option * int * int 
  | Bit_vector of int * int 
  | Array of
  {
  indexes: vhdl_name_t list [@default []];
  const: vhdl_constraint_t option [@default None];
  definition: vhdl_subtype_indication_t } [@name "ARRAY_TYPE_DEFINITION"]
  | Record of vhdl_element_declaration_t list
  [@name "RECORD_TYPE_DEFINITION"]
  | Enumerated of vhdl_name_t list [@name "ENUMERATION_TYPE_DEFINITION"]
  | Void 
and vhdl_element_declaration_t =
  {
  names: vhdl_name_t list ;
  definition: vhdl_subtype_indication_t }
and vhdl_subtype_indication_t =
  {
  name: vhdl_name_t [@default NoName];
  functionName: vhdl_name_t [@default NoName];
  const: vhdl_constraint_t [@default NoConstraint]}
and vhdl_discrete_range_t =
  | SubDiscreteRange of vhdl_subtype_indication_t
  [@name "SUB_DISCRETE_RANGE"]
  | NamedRange of vhdl_name_t [@name "NAMED_RANGE"]
  | DirectedRange of
  {
  direction: string ;
  from: vhdl_expr_t ;
  _to: vhdl_expr_t } [@name "RANGE_WITH_DIRECTION"]
and vhdl_constraint_t =
  | RefConstraint of {
  ref_name: vhdl_name_t } 
  | RangeConstraint of {
  range: vhdl_discrete_range_t } [@name "RANGE_CONSTRAINT"]
  | IndexConstraint of {
  ranges: vhdl_discrete_range_t list } [@name "INDEX_CONSTRAINT"]
  | ArrayConstraint of
  {
  ranges: vhdl_discrete_range_t list ;
  sub: vhdl_constraint_t } [@name "ARRAY_CONSTRAINT"]
  | RecordConstraint 
  | NoConstraint 
and vhdl_definition_t =
  | Type of {
  name: vhdl_name_t ;
  definition: vhdl_type_t } [@name "TYPE_DECLARATION"]
  | Subtype of {
  name: vhdl_name_t ;
  typ: vhdl_subtype_indication_t } [@name "SUBTYPE_DECLARATION"]
and vhdl_expr_t =
  | Call of vhdl_name_t [@name "CALL"]
  | Cst of
  {
  value: vhdl_cst_val_t ;
  unit_name: vhdl_name_t option [@default None]} [@name "CONSTANT_VALUE"]
  | Op of {
  id: string [@default ""];
  args: vhdl_expr_t list [@default []]} [@name "EXPRESSION"]
  | IsNull [@name "IsNull"]
  | Time of {
  value: int ;
  phy_unit: string [@default ""]} 
  | Sig of {
  name: vhdl_name_t ;
  att: vhdl_signal_attributes_t option } 
  | SuffixMod of {
  expr: vhdl_expr_t ;
  selection: vhdl_suffix_selection_t } 
  | Aggregate of {
  elems: vhdl_element_assoc_t list } [@name "AGGREGATE"]
  | Others [@name "OTHERS"]
and vhdl_name_t =
  | Simple of string [@name "SIMPLE_NAME"]
  | Identifier of string [@name "IDENTIFIER"]
  | Selected of vhdl_name_t list [@name "SELECTED_NAME"]
  | Index of {
  id: vhdl_name_t ;
  exprs: vhdl_expr_t list } [@name "INDEXED_NAME"]
  | Slice of {
  id: vhdl_name_t ;
  range: vhdl_discrete_range_t } [@name "SLICE_NAME"]
  | Attribute of
  {
  id: vhdl_name_t ;
  designator: vhdl_name_t ;
  expr: vhdl_expr_t [@default IsNull]} [@name "ATTRIBUTE_NAME"]
  | Function of {
  id: vhdl_name_t ;
  assoc_list: vhdl_assoc_element_t list } [@name "FUNCTION_CALL"]
  | NoName 
and vhdl_assoc_element_t =
  {
  formal_name: vhdl_name_t option [@default None];
  formal_arg: vhdl_name_t option [@default None];
  actual_name: vhdl_name_t option [@default None];
  actual_designator: vhdl_name_t option [@default None];
  actual_expr: vhdl_expr_t option [@default None]}
and vhdl_element_assoc_t = {
  choices: vhdl_expr_t list [@default []];
  expr: vhdl_expr_t }
and vhdl_array_attributes_t =
  | AAttInt of {
  id: string ;
  arg: int } 
  | AAttAscending 
and vhdl_signal_attributes_t =
  | SigAtt of string 
and vhdl_string_attributes_t =
  | StringAtt of string 
and vhdl_suffix_selection_t =
  | Idx of int 
  | SuffixRange of int * int

let rec pp_vhdl_type_t :
  Format.formatter -> vhdl_type_t -> Ppx_deriving_runtime.unit =
  let __4 () = pp_vhdl_name_t
  
  and __3 () = pp_vhdl_element_declaration_t
  
  and __2 () = pp_vhdl_subtype_indication_t
  
  and __1 () = pp_vhdl_constraint_t
  
  and __0 () = pp_vhdl_name_t
in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Base a0 ->
             (Format.fprintf fmt "%s") a0;
        | Range (a0,a1,a2) ->
             ((
               (Format.fprintf fmt "%d") a1);
               ((function
                 | None  -> Format.pp_print_string fmt ""
                 | Some x ->
                      (Format.fprintf fmt "%s") x;
                      )) a0;
              (Format.fprintf fmt "%d") a2);
        | Bit_vector (a0,a1) ->
             (Format.fprintf fmt "array (%d,%d) of bit") a0 a1;
        | Array
            { indexes = aindexes; const = aconst; definition = adefinition }
            ->
            Format.fprintf fmt "array";
            (match aindexes with
            | [] -> Format.fprintf fmt "";
            | _ ->
              ((fun x  ->
                ignore
                (List.fold_left
                  (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ",@ ";
                      ((__0 ()) fmt) x;
                      true) false x)) aindexes));
            (function
              | None  -> Format.pp_print_string fmt ""
              | Some x ->
                ((__1 ()) fmt) x) aconst;
            Format.fprintf fmt " of ";
            ((__2 ()) fmt) adefinition;
        | Record a0 ->
            Format.fprintf fmt "@[<v 2>record@;";
            (fun x  ->
              ignore
                (List.fold_left
                  (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@;";
                        ((__3 ()) fmt) x;
                        true) false x);
              Format.fprintf fmt "@]@;end record") a0;
        | Enumerated a0 ->
            (Format.fprintf fmt "(";
            ((fun x  ->
              ignore
              (List.fold_left
                (fun sep  ->
                  fun x  ->
                    if sep then Format.fprintf fmt ",@ ";
                      ((__4 ()) fmt) x;
                    true) false x))) a0;
             Format.fprintf fmt ")");
        | Void  -> Format.pp_print_string fmt "")
    [@ocaml.warning "-A"])

and show_vhdl_type_t : vhdl_type_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_type_t x

and pp_vhdl_element_declaration_t :
  Format.formatter -> vhdl_element_declaration_t -> Ppx_deriving_runtime.unit
  =
  let __1 () = pp_vhdl_subtype_indication_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
            (fun x  ->
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt ",@ ";
                          ((__0 ()) fmt) x;
                          true) false x)) x.names;
           Format.fprintf fmt ":@ ";
           ((__1 ()) fmt) x.definition)
    [@ocaml.warning "-A"])

and show_vhdl_element_declaration_t :
  vhdl_element_declaration_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_element_declaration_t x

and pp_vhdl_subtype_indication_t :
  Format.formatter -> vhdl_subtype_indication_t -> Ppx_deriving_runtime.unit
  =
  let __2 () = pp_vhdl_constraint_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          ((__0 ()) fmt) x.name;
          ((__1 ()) fmt) x.functionName;
          (match x.const with
            | NoConstraint -> Format.fprintf fmt "";
            | _ -> Format.fprintf fmt " ";
                   ((__2 ()) fmt) x.const))
    [@ocaml.warning "-A"])

and show_vhdl_subtype_indication_t :
  vhdl_subtype_indication_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_subtype_indication_t x

and pp_vhdl_discrete_range_t :
  Format.formatter -> vhdl_discrete_range_t -> Ppx_deriving_runtime.unit =
  let __3 () = pp_vhdl_expr_t
  
  and __2 () = pp_vhdl_expr_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_subtype_indication_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | SubDiscreteRange a0 ->
             ((__0 ()) fmt) a0;
        | NamedRange a0 ->
             ((__1 ()) fmt) a0;
        | DirectedRange { direction = adirection; from = afrom; _to = a_to }
            ->
               ((__2 ()) fmt) afrom;
               (Format.fprintf fmt " %s ") adirection;
               ((__3 ()) fmt) a_to;
    )
    [@ocaml.warning "-A"])

and show_vhdl_discrete_range_t :
  vhdl_discrete_range_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_discrete_range_t x

(* TODO Adapt for: ArrayConstraint, RecordConstraint *)
and pp_vhdl_constraint_t :
  Format.formatter -> vhdl_constraint_t -> Ppx_deriving_runtime.unit =
  let __4 () = pp_vhdl_constraint_t
  
  and __3 () = pp_vhdl_discrete_range_t
  
  and __2 () = pp_vhdl_discrete_range_t
  
  and __1 () = pp_vhdl_discrete_range_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | RefConstraint { ref_name = aref_name } ->
             (Format.fprintf fmt "(";
              ((__0 ()) fmt) aref_name;
              Format.fprintf fmt ")");
        | RangeConstraint { range = arange } ->
             (Format.fprintf fmt "(";
              ((__1 ()) fmt) arange;
              Format.fprintf fmt ")");
        | IndexConstraint { ranges = aranges } ->
            Format.fprintf fmt "(";
            ((fun x  ->
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt ", ";
                          ((__2 ()) fmt) x;
                          true) false x))) aranges;
            Format.fprintf fmt ")";
        | ArrayConstraint { ranges = aranges; sub = asub } ->
            (Format.fprintf fmt "@[<2>ArrayConstraint {@,";
             ((Format.fprintf fmt "@[%s =@ " "ranges";
               ((fun x  ->
                   Format.fprintf fmt "@[<2>[";
                   ignore
                     (List.fold_left
                        (fun sep  ->
                           fun x  ->
                             if sep then Format.fprintf fmt ";@ ";
                             ((__3 ()) fmt) x;
                             true) false x);
                   Format.fprintf fmt "@,]@]")) aranges;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "sub";
              ((__4 ()) fmt) asub;
              Format.fprintf fmt "@]");
             Format.fprintf fmt "@]}")
        | RecordConstraint  -> Format.pp_print_string fmt ""
        | NoConstraint  -> Format.pp_print_string fmt "")
    [@ocaml.warning "-A"])

and show_vhdl_constraint_t : vhdl_constraint_t -> Ppx_deriving_runtime.string
  = fun x  -> Format.asprintf "%a" pp_vhdl_constraint_t x

and pp_vhdl_definition_t :
  Format.formatter -> vhdl_definition_t -> Ppx_deriving_runtime.unit =
  let __3 () = pp_vhdl_subtype_indication_t
  
  and __2 () = pp_vhdl_name_t
  
  and __1 () = pp_vhdl_type_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Type { name = aname; definition = adefinition } ->
            Format.fprintf fmt "type ";
            ((__0 ()) fmt) aname;
            Format.fprintf fmt " is ";
            ((__1 ()) fmt) adefinition;
        | Subtype { name = aname; typ = atyp } ->
            Format.fprintf fmt "subtype ";
            ((__2 ()) fmt) aname;
            Format.fprintf fmt " is ";
            ((__3 ()) fmt) atyp;
   )
    [@ocaml.warning "-A"])

and show_vhdl_definition_t : vhdl_definition_t -> Ppx_deriving_runtime.string
  = fun x  -> Format.asprintf "%a" pp_vhdl_definition_t x

(* TODO adapt for Op, Time, Sig, suffixMod, Aggregate *)
and pp_vhdl_expr_t :
  Format.formatter -> vhdl_expr_t -> Ppx_deriving_runtime.unit =
  let __8 () = pp_vhdl_element_assoc_t
  
  and __7 () = pp_vhdl_suffix_selection_t
  
  and __6 () = pp_vhdl_expr_t
  
  and __5 () = pp_vhdl_signal_attributes_t
  
  and __4 () = pp_vhdl_name_t
  
  and __3 () = pp_vhdl_expr_t
  
  and __2 () = pp_vhdl_name_t
  
  and __1 () = pp_vhdl_cst_val_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Call a0 ->
             ((__0 ()) fmt) a0;
        | Cst { value = avalue; unit_name = aunit_name } ->
             ((__1 ()) fmt) avalue;
             (function
                | None  -> Format.pp_print_string fmt ""
                | Some x ->
                    Format.fprintf fmt " ";
                    ((__2 ()) fmt) x) aunit_name;
        | Op { id = aid; args = aargs } ->
            (match aargs with
            | [] -> (Format.fprintf fmt "%s") aid;
            | hd::[] ->
               (Format.fprintf fmt "%s") aid;
               ((__3 ()) fmt) hd
            | hd::(hd2::[]) -> 
               ((__3 ()) fmt) hd;
               (Format.fprintf fmt " %s ") aid;
               ((__3 ()) fmt) hd2
            | _ ->
            (Format.fprintf fmt "@[<2>Op {@,";
             ((Format.fprintf fmt "@[%s =@ " "id";
               (Format.fprintf fmt "%S") aid;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "args";
              ((fun x  ->
                  Format.fprintf fmt "@[<2>[";
                  ignore
                    (List.fold_left
                       (fun sep  ->
                          fun x  ->
                            if sep then Format.fprintf fmt ";@ ";
                            ((__3 ()) fmt) x;
                            true) false x);
                  Format.fprintf fmt "@,]@]")) aargs;
              Format.fprintf fmt "@]");
             Format.fprintf fmt "@]}"))
        | IsNull  -> Format.pp_print_string fmt ""
        | Time { value = avalue; phy_unit = aphy_unit } ->
            (Format.fprintf fmt "@[<2>Time {@,";
             ((Format.fprintf fmt "@[%s =@ " "value";
               (Format.fprintf fmt "%d") avalue;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "phy_unit";
              (Format.fprintf fmt "%S") aphy_unit;
              Format.fprintf fmt "@]");
             Format.fprintf fmt "@]}")
        | Sig { name = aname; att = aatt } ->
            (Format.fprintf fmt "--@[<2>Sig {@,";
             ((Format.fprintf fmt "@[%s =@ " "name";
               ((__4 ()) fmt) aname;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "att";
              ((function
                | None  -> Format.pp_print_string fmt "None"
                | Some x ->
                    (Format.pp_print_string fmt "(Some ";
                     ((__5 ()) fmt) x;
                     Format.pp_print_string fmt ")"))) aatt;
              Format.fprintf fmt "@]");
             Format.fprintf fmt "@]}")
        | SuffixMod { expr = aexpr; selection = aselection } ->
            (Format.fprintf fmt "--@[<2>SuffixMod {@,";
             ((Format.fprintf fmt "@[%s =@ " "expr";
               ((__6 ()) fmt) aexpr;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "selection";
              ((__7 ()) fmt) aselection;
              Format.fprintf fmt "@]");
             Format.fprintf fmt "@]}")
        | Aggregate { elems = aelems } ->
            (match aelems with
            | [] -> Format.fprintf fmt "";
            | _ ->
              (Format.fprintf fmt "(@[";
              ((fun x  ->
                  ignore
                    (List.fold_left
                       (fun sep  ->
                          fun x  ->
                            if sep then Format.fprintf fmt ", ";
                            ((__8 ()) fmt) x;
                            true) false x))) aelems;
              Format.fprintf fmt ")@]");)
        | Others  -> Format.pp_print_string fmt "others")
    [@ocaml.warning "-A"])

and show_vhdl_expr_t : vhdl_expr_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_expr_t x

and pp_vhdl_name_t :
  Format.formatter -> vhdl_name_t -> Ppx_deriving_runtime.unit =
  let __9 () = pp_vhdl_assoc_element_t
  
  and __8 () = pp_vhdl_name_t
  
  and __7 () = pp_vhdl_expr_t
  
  and __6 () = pp_vhdl_name_t
  
  and __5 () = pp_vhdl_name_t
  
  and __4 () = pp_vhdl_discrete_range_t
  
  and __3 () = pp_vhdl_name_t
  
  and __2 () = pp_vhdl_expr_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Simple a0 ->
             (Format.fprintf fmt "%s") a0;
        | Identifier a0 ->
             (Format.fprintf fmt "%s") a0;
        | Selected a0 ->
             ((fun x  ->
                 ignore
                   (List.fold_left
                      (fun sep  ->
                         fun x  ->
                           if sep then Format.fprintf fmt ".";
                           ((__0 ()) fmt) x;
                           true) false x);) a0;)
        | Index { id = aid; exprs = aexprs } ->
            ((__1 ()) fmt) aid;
            Format.fprintf fmt "(";
            (fun x  ->
                ignore
                (List.fold_left
                  (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ",@ ";
                                  ((__2 ()) fmt) x;
                                  true
                  ) false x);
            ) aexprs;
            Format.fprintf fmt ")";
        | Slice { id = aid; range = arange } ->
              ((__3 ()) fmt) aid;
              Format.fprintf fmt "(";
              ((__4 ()) fmt) arange;
              Format.fprintf fmt ")";
        | Attribute { id = aid; designator = adesignator; expr = aexpr } ->
              ((__5 ()) fmt) aid;
              Format.fprintf fmt "\'";
              ((__6 ()) fmt) adesignator;
              (match aexpr with
              | IsNull -> Format.fprintf fmt "";
              | _ ->
                Format.fprintf fmt "(";
                ((__7 ()) fmt) aexpr;
                Format.fprintf fmt ")")
        | Function { id = aid; assoc_list = aassoc_list } ->
            (((__8 ()) fmt) aid;
            Format.fprintf fmt "(";
            ((fun x  ->
              Format.fprintf fmt "@[";
              ignore
                (List.fold_left
                   (fun sep  ->
                      fun x  ->
                        if sep then Format.fprintf fmt ";@ ";
                        ((__9 ()) fmt) x;
                        true) false x);
            Format.fprintf fmt "@]")) aassoc_list;
            Format.fprintf fmt ")";)
        | NoName  -> Format.pp_print_string fmt "")
    [@ocaml.warning "-A"])

and show_vhdl_name_t : vhdl_name_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_name_t x

and pp_vhdl_assoc_element_t :
  Format.formatter -> vhdl_assoc_element_t -> Ppx_deriving_runtime.unit =
  let __4 () = pp_vhdl_expr_t
  
  and __3 () = pp_vhdl_name_t
  
  and __2 () = pp_vhdl_name_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          (match x.formal_name with
          | None -> Format.pp_print_string fmt ""
          | Some NoName -> Format.pp_print_string fmt ""
          | Some a -> 
              (((__0 ()) fmt) a;
              (match x.formal_arg with
              | None -> ()
              | Some b -> Format.fprintf fmt "(";
                          ((__1 ()) fmt) b;
                          Format.fprintf fmt ")");
              Format.fprintf fmt " => "));
          (match x.actual_name with
          | None -> Format.pp_print_string fmt ""
          | Some a -> 
              (((__2 ()) fmt) a;
              (match x.actual_designator with
              | None -> ()
              | Some NoName -> Format.pp_print_string fmt ""
              | Some b -> (Format.fprintf fmt "(";
                          ((__3 ()) fmt) b;
                          Format.fprintf fmt ")"));
              (match x.actual_expr with
              | None -> ()
              | Some IsNull -> Format.pp_print_string fmt ""
              | Some c -> (Format.fprintf fmt "(";
                          ((__4 ()) fmt) c;
                          Format.fprintf fmt ")"))));)
    [@ocaml.warning "-A"])

and show_vhdl_assoc_element_t :
  vhdl_assoc_element_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_assoc_element_t x

and pp_vhdl_element_assoc_t :
  Format.formatter -> vhdl_element_assoc_t -> Ppx_deriving_runtime.unit =
  let __1 () = pp_vhdl_expr_t
  
  and __0 () = pp_vhdl_expr_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
            (match x.choices with
            | [] -> Format.fprintf fmt "";
            | _ -> 
              (((fun x  ->
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt "|@ ";
                          ((__0 ()) fmt) x;
                          true) false x))) x.choices;
              Format.fprintf fmt " => ";));
           ((__1 ()) fmt) x.expr)
    [@ocaml.warning "-A"])

and show_vhdl_element_assoc_t :
  vhdl_element_assoc_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_element_assoc_t x

(* TODO *)
and (pp_vhdl_array_attributes_t :
      Format.formatter ->
        vhdl_array_attributes_t -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | AAttInt { id = aid; arg = aarg } ->
            (Format.fprintf fmt "@[<2>AAttInt {@,";
             ((Format.fprintf fmt "@[%s =@ " "id";
               (Format.fprintf fmt "%S") aid;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "arg";
              (Format.fprintf fmt "%d") aarg;
              Format.fprintf fmt "@]");
             Format.fprintf fmt "@]}")
        | AAttAscending  -> Format.pp_print_string fmt "AAttAscending")
  [@ocaml.warning "-A"])

and show_vhdl_array_attributes_t :
  vhdl_array_attributes_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_array_attributes_t x

(* TODO *)
and (pp_vhdl_signal_attributes_t :
      Format.formatter ->
        vhdl_signal_attributes_t -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | SigAtt a0 ->
            (Format.fprintf fmt "(@[<2>SigAtt@ ";
             (Format.fprintf fmt "%S") a0;
             Format.fprintf fmt "@])"))
  [@ocaml.warning "-A"])

and show_vhdl_signal_attributes_t :
  vhdl_signal_attributes_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_signal_attributes_t x

(* TODO *)
and (pp_vhdl_string_attributes_t :
      Format.formatter ->
        vhdl_string_attributes_t -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | StringAtt a0 ->
            (Format.fprintf fmt "(@[<2>StringAtt@ ";
             (Format.fprintf fmt "%S") a0;
             Format.fprintf fmt "@])"))
  [@ocaml.warning "-A"])

and show_vhdl_string_attributes_t :
  vhdl_string_attributes_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_string_attributes_t x

(* TODO *)
and (pp_vhdl_suffix_selection_t :
      Format.formatter ->
        vhdl_suffix_selection_t -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Idx a0 ->
            (Format.fprintf fmt "(@[<2>Idx@ ";
             (Format.fprintf fmt "%d") a0;
             Format.fprintf fmt "@])")
        | SuffixRange (a0,a1) ->
            (Format.fprintf fmt "(@[<2>SuffixRange (@,";
             ((Format.fprintf fmt "%d") a0;
              Format.fprintf fmt ",@ ";
              (Format.fprintf fmt "%d") a1);
             Format.fprintf fmt "@,))@]"))
  [@ocaml.warning "-A"])

and show_vhdl_suffix_selection_t :
  vhdl_suffix_selection_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_suffix_selection_t x

let rec (vhdl_type_t_to_yojson : vhdl_type_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | Base arg0 ->
          `List
            [`String "Base";
            ((fun (x : Ppx_deriving_runtime.string)  -> `String x)) arg0]
      | Range (arg0,arg1,arg2) ->
          `List
            [`String "Range";
            ((function
              | None  -> `Null
              | Some x ->
                  ((fun (x : Ppx_deriving_runtime.string)  -> `String x)) x))
              arg0;
            ((fun (x : Ppx_deriving_runtime.int)  -> `Int x)) arg1;
            ((fun (x : Ppx_deriving_runtime.int)  -> `Int x)) arg2]
      | Bit_vector (arg0,arg1) ->
          `List
            [`String "Bit_vector";
            ((fun (x : Ppx_deriving_runtime.int)  -> `Int x)) arg0;
            ((fun (x : Ppx_deriving_runtime.int)  -> `Int x)) arg1]
      | Array arg0 ->
          `List
            [`String "ARRAY_TYPE_DEFINITION";
            (let fields = []  in
             let fields =
               ("definition",
                 ((fun x  -> vhdl_subtype_indication_t_to_yojson x)
                    arg0.definition))
               :: fields  in
             let fields =
               if arg0.const = None
               then fields
               else
                 ("const",
                   (((function
                      | None  -> `Null
                      | Some x ->
                          ((fun x  -> vhdl_constraint_t_to_yojson x)) x))
                      arg0.const))
                 :: fields
                in
             let fields =
               if arg0.indexes = []
               then fields
               else
                 ("indexes",
                   (((fun x  ->
                        `List
                          (List.map (fun x  -> vhdl_name_t_to_yojson x) x)))
                      arg0.indexes))
                 :: fields
                in
             `Assoc fields)]
      | Record arg0 ->
          `List
            [`String "RECORD_TYPE_DEFINITION";
            ((fun x  ->
                `List
                  (List.map
                     (fun x  -> vhdl_element_declaration_t_to_yojson x) x)))
              arg0]
      | Enumerated arg0 ->
          `List
            [`String "ENUMERATION_TYPE_DEFINITION";
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))) arg0]
      | Void  -> `List [`String "Void"])
  [@ocaml.warning "-A"])

and (vhdl_type_t_of_yojson :
      Yojson.Safe.json -> vhdl_type_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "Base")::arg0::[]) ->
          ((function
            | `String x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_type_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Base arg0)))
      | `List ((`String "Range")::arg0::arg1::arg2::[]) ->
          ((function
            | `Int x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_type_t") arg2) >>=
            ((fun arg2  ->
                ((function
                  | `Int x -> Result.Ok x
                  | _ -> Result.Error "Vhdl_ast.vhdl_type_t") arg1) >>=
                  (fun arg1  ->
                     ((function
                       | `Null -> Result.Ok None
                       | x ->
                           ((function
                             | `String x -> Result.Ok x
                             | _ -> Result.Error "Vhdl_ast.vhdl_type_t") x)
                             >>= ((fun x  -> Result.Ok (Some x)))) arg0)
                       >>=
                       (fun arg0  -> Result.Ok (Range (arg0, arg1, arg2))))))
      | `List ((`String "Bit_vector")::arg0::arg1::[]) ->
          ((function
            | `Int x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_type_t") arg1) >>=
            ((fun arg1  ->
                ((function
                  | `Int x -> Result.Ok x
                  | _ -> Result.Error "Vhdl_ast.vhdl_type_t") arg0) >>=
                  (fun arg0  -> Result.Ok (Bit_vector (arg0, arg1)))))
      | `List ((`String "ARRAY_TYPE_DEFINITION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("indexes",x)::xs ->
                      loop xs
                        (((function
                           | `List xs ->
                               map_bind (fun x  -> vhdl_name_t_of_yojson x)
                                 [] xs
                           | _ -> Result.Error "Vhdl_ast.vhdl_type_t.indexes")
                            x), arg1, arg2)
                  | ("const",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `Null -> Result.Ok None
                            | x ->
                                ((fun x  -> vhdl_constraint_t_of_yojson x) x)
                                  >>= ((fun x  -> Result.Ok (Some x)))) x),
                          arg2)
                  | ("definition",x)::xs ->
                      loop xs
                        (arg0, arg1,
                          ((fun x  -> vhdl_subtype_indication_t_of_yojson x)
                             x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (Array
                                           {
                                             indexes = arg0;
                                             const = arg1;
                                             definition = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok []), (Result.Ok None),
                    (Result.Error "Vhdl_ast.vhdl_type_t.definition"))
            | _ -> Result.Error "Vhdl_ast.vhdl_type_t")) arg0
      | `List ((`String "RECORD_TYPE_DEFINITION")::arg0::[]) ->
          ((function
            | `List xs ->
                map_bind (fun x  -> vhdl_element_declaration_t_of_yojson x)
                  [] xs
            | _ -> Result.Error "Vhdl_ast.vhdl_type_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Record arg0)))
      | `List ((`String "ENUMERATION_TYPE_DEFINITION")::arg0::[]) ->
          ((function
            | `List xs -> map_bind (fun x  -> vhdl_name_t_of_yojson x) [] xs
            | _ -> Result.Error "Vhdl_ast.vhdl_type_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Enumerated arg0)))
      | `List ((`String "Void")::[]) -> Result.Ok Void
      | _ -> Result.Error "Vhdl_ast.vhdl_type_t")
  [@ocaml.warning "-A"])

and (vhdl_element_declaration_t_to_yojson :
      vhdl_element_declaration_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          ("definition",
            ((fun x  -> vhdl_subtype_indication_t_to_yojson x) x.definition))
          :: fields  in
        let fields =
          ("names",
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))
               x.names))
          :: fields  in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_element_declaration_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_element_declaration_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1) as _state) =
            match xs with
            | ("names",x)::xs ->
                loop xs
                  (((function
                     | `List xs ->
                         map_bind (fun x  -> vhdl_name_t_of_yojson x) [] xs
                     | _ ->
                         Result.Error
                           "Vhdl_ast.vhdl_element_declaration_t.names") x),
                    arg1)
            | ("definition",x)::xs ->
                loop xs
                  (arg0,
                    ((fun x  -> vhdl_subtype_indication_t_of_yojson x) x))
            | [] ->
                arg1 >>=
                  ((fun arg1  ->
                      arg0 >>=
                        (fun arg0  ->
                           Result.Ok { names = arg0; definition = arg1 })))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Error "Vhdl_ast.vhdl_element_declaration_t.names"),
              (Result.Error "Vhdl_ast.vhdl_element_declaration_t.definition"))
      | _ -> Result.Error "Vhdl_ast.vhdl_element_declaration_t")
  [@ocaml.warning "-A"])

and (vhdl_subtype_indication_t_to_yojson :
      vhdl_subtype_indication_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.const = NoConstraint
          then fields
          else
            ("const", (((fun x  -> vhdl_constraint_t_to_yojson x)) x.const))
            :: fields
           in
        let fields =
          if x.functionName = NoName
          then fields
          else
            ("functionName",
              (((fun x  -> vhdl_name_t_to_yojson x)) x.functionName))
            :: fields
           in
        let fields =
          if x.name = NoName
          then fields
          else ("name", (((fun x  -> vhdl_name_t_to_yojson x)) x.name)) ::
            fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_subtype_indication_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_subtype_indication_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2) as _state) =
            match xs with
            | ("name",x)::xs ->
                loop xs (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
            | ("functionName",x)::xs ->
                loop xs (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2)
            | ("const",x)::xs ->
                loop xs
                  (arg0, arg1, ((fun x  -> vhdl_constraint_t_of_yojson x) x))
            | [] ->
                arg2 >>=
                  ((fun arg2  ->
                      arg1 >>=
                        (fun arg1  ->
                           arg0 >>=
                             (fun arg0  ->
                                Result.Ok
                                  {
                                    name = arg0;
                                    functionName = arg1;
                                    const = arg2
                                  }))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok NoName), (Result.Ok NoName),
              (Result.Ok NoConstraint))
      | _ -> Result.Error "Vhdl_ast.vhdl_subtype_indication_t")
  [@ocaml.warning "-A"])

and (vhdl_discrete_range_t_to_yojson :
      vhdl_discrete_range_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | SubDiscreteRange arg0 ->
          `List
            [`String "SUB_DISCRETE_RANGE";
            ((fun x  -> vhdl_subtype_indication_t_to_yojson x)) arg0]
      | NamedRange arg0 ->
          `List
            [`String "NAMED_RANGE";
            ((fun x  -> vhdl_name_t_to_yojson x)) arg0]
      | DirectedRange arg0 ->
          `List
            [`String "RANGE_WITH_DIRECTION";
            (let fields = []  in
             let fields =
               ("_to", ((fun x  -> vhdl_expr_t_to_yojson x) arg0._to)) ::
               fields  in
             let fields =
               ("from", ((fun x  -> vhdl_expr_t_to_yojson x) arg0.from)) ::
               fields  in
             let fields =
               ("direction",
                 ((fun (x : Ppx_deriving_runtime.string)  -> `String x)
                    arg0.direction))
               :: fields  in
             `Assoc fields)])
  [@ocaml.warning "-A"])

and (vhdl_discrete_range_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_discrete_range_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "SUB_DISCRETE_RANGE")::arg0::[]) ->
          ((fun x  -> vhdl_subtype_indication_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (SubDiscreteRange arg0)))
      | `List ((`String "NAMED_RANGE")::arg0::[]) ->
          ((fun x  -> vhdl_name_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (NamedRange arg0)))
      | `List ((`String "RANGE_WITH_DIRECTION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("direction",x)::xs ->
                      loop xs
                        (((function
                           | `String x -> Result.Ok x
                           | _ ->
                               Result.Error
                                 "Vhdl_ast.vhdl_discrete_range_t.direction")
                            x), arg1, arg2)
                  | ("from",x)::xs ->
                      loop xs
                        (arg0, ((fun x  -> vhdl_expr_t_of_yojson x) x), arg2)
                  | ("_to",x)::xs ->
                      loop xs
                        (arg0, arg1, ((fun x  -> vhdl_expr_t_of_yojson x) x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (DirectedRange
                                           {
                                             direction = arg0;
                                             from = arg1;
                                             _to = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_discrete_range_t.direction"),
                    (Result.Error "Vhdl_ast.vhdl_discrete_range_t.from"),
                    (Result.Error "Vhdl_ast.vhdl_discrete_range_t._to"))
            | _ -> Result.Error "Vhdl_ast.vhdl_discrete_range_t")) arg0
      | _ -> Result.Error "Vhdl_ast.vhdl_discrete_range_t")
  [@ocaml.warning "-A"])

and (vhdl_constraint_t_to_yojson : vhdl_constraint_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | RefConstraint arg0 ->
          `List
            [`String "RefConstraint";
            (let fields = []  in
             let fields =
               ("ref_name",
                 ((fun x  -> vhdl_name_t_to_yojson x) arg0.ref_name))
               :: fields  in
             `Assoc fields)]
      | RangeConstraint arg0 ->
          `List
            [`String "RANGE_CONSTRAINT";
            (let fields = []  in
             let fields =
               ("range",
                 ((fun x  -> vhdl_discrete_range_t_to_yojson x) arg0.range))
               :: fields  in
             `Assoc fields)]
      | IndexConstraint arg0 ->
          `List
            [`String "INDEX_CONSTRAINT";
            (let fields = []  in
             let fields =
               ("ranges",
                 ((fun x  ->
                     `List
                       (List.map
                          (fun x  -> vhdl_discrete_range_t_to_yojson x) x))
                    arg0.ranges))
               :: fields  in
             `Assoc fields)]
      | ArrayConstraint arg0 ->
          `List
            [`String "ARRAY_CONSTRAINT";
            (let fields = []  in
             let fields =
               ("sub", ((fun x  -> vhdl_constraint_t_to_yojson x) arg0.sub))
               :: fields  in
             let fields =
               ("ranges",
                 ((fun x  ->
                     `List
                       (List.map
                          (fun x  -> vhdl_discrete_range_t_to_yojson x) x))
                    arg0.ranges))
               :: fields  in
             `Assoc fields)]
      | RecordConstraint  -> `List [`String "RecordConstraint"]
      | NoConstraint  -> `List [`String "NoConstraint"])
  [@ocaml.warning "-A"])

and (vhdl_constraint_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_constraint_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "RefConstraint")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs (arg0 as _state) =
                  match xs with
                  | ("ref_name",x)::xs ->
                      loop xs ((fun x  -> vhdl_name_t_of_yojson x) x)
                  | [] ->
                      arg0 >>=
                        ((fun arg0  ->
                            Result.Ok (RefConstraint { ref_name = arg0 })))
                  | _::xs -> loop xs _state  in
                loop xs (Result.Error "Vhdl_ast.vhdl_constraint_t.ref_name")
            | _ -> Result.Error "Vhdl_ast.vhdl_constraint_t")) arg0
      | `List ((`String "RANGE_CONSTRAINT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs (arg0 as _state) =
                  match xs with
                  | ("range",x)::xs ->
                      loop xs
                        ((fun x  -> vhdl_discrete_range_t_of_yojson x) x)
                  | [] ->
                      arg0 >>=
                        ((fun arg0  ->
                            Result.Ok (RangeConstraint { range = arg0 })))
                  | _::xs -> loop xs _state  in
                loop xs (Result.Error "Vhdl_ast.vhdl_constraint_t.range")
            | _ -> Result.Error "Vhdl_ast.vhdl_constraint_t")) arg0
      | `List ((`String "INDEX_CONSTRAINT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs (arg0 as _state) =
                  match xs with
                  | ("ranges",x)::xs ->
                      loop xs
                        ((function
                          | `List xs ->
                              map_bind
                                (fun x  -> vhdl_discrete_range_t_of_yojson x)
                                [] xs
                          | _ ->
                              Result.Error
                                "Vhdl_ast.vhdl_constraint_t.ranges") x)
                  | [] ->
                      arg0 >>=
                        ((fun arg0  ->
                            Result.Ok (IndexConstraint { ranges = arg0 })))
                  | _::xs -> loop xs _state  in
                loop xs (Result.Error "Vhdl_ast.vhdl_constraint_t.ranges")
            | _ -> Result.Error "Vhdl_ast.vhdl_constraint_t")) arg0
      | `List ((`String "ARRAY_CONSTRAINT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("ranges",x)::xs ->
                      loop xs
                        (((function
                           | `List xs ->
                               map_bind
                                 (fun x  -> vhdl_discrete_range_t_of_yojson x)
                                 [] xs
                           | _ ->
                               Result.Error
                                 "Vhdl_ast.vhdl_constraint_t.ranges") x),
                          arg1)
                  | ("sub",x)::xs ->
                      loop xs
                        (arg0, ((fun x  -> vhdl_constraint_t_of_yojson x) x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (ArrayConstraint
                                      { ranges = arg0; sub = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_constraint_t.ranges"),
                    (Result.Error "Vhdl_ast.vhdl_constraint_t.sub"))
            | _ -> Result.Error "Vhdl_ast.vhdl_constraint_t")) arg0
      | `List ((`String "RecordConstraint")::[]) ->
          Result.Ok RecordConstraint
      | `List ((`String "NoConstraint")::[]) -> Result.Ok NoConstraint
      | _ -> Result.Error "Vhdl_ast.vhdl_constraint_t")
  [@ocaml.warning "-A"])

and (vhdl_definition_t_to_yojson : vhdl_definition_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | Type arg0 ->
          `List
            [`String "TYPE_DECLARATION";
            (let fields = []  in
             let fields =
               ("definition",
                 ((fun x  -> vhdl_type_t_to_yojson x) arg0.definition))
               :: fields  in
             let fields =
               ("name", ((fun x  -> vhdl_name_t_to_yojson x) arg0.name)) ::
               fields  in
             `Assoc fields)]
      | Subtype arg0 ->
          `List
            [`String "SUBTYPE_DECLARATION";
            (let fields = []  in
             let fields =
               ("typ",
                 ((fun x  -> vhdl_subtype_indication_t_to_yojson x) arg0.typ))
               :: fields  in
             let fields =
               ("name", ((fun x  -> vhdl_name_t_to_yojson x) arg0.name)) ::
               fields  in
             `Assoc fields)])
  [@ocaml.warning "-A"])

and (vhdl_definition_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_definition_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "TYPE_DECLARATION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("name",x)::xs ->
                      loop xs (((fun x  -> vhdl_name_t_of_yojson x) x), arg1)
                  | ("definition",x)::xs ->
                      loop xs (arg0, ((fun x  -> vhdl_type_t_of_yojson x) x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (Type { name = arg0; definition = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_definition_t.name"),
                    (Result.Error "Vhdl_ast.vhdl_definition_t.definition"))
            | _ -> Result.Error "Vhdl_ast.vhdl_definition_t")) arg0
      | `List ((`String "SUBTYPE_DECLARATION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("name",x)::xs ->
                      loop xs (((fun x  -> vhdl_name_t_of_yojson x) x), arg1)
                  | ("typ",x)::xs ->
                      loop xs
                        (arg0,
                          ((fun x  -> vhdl_subtype_indication_t_of_yojson x)
                             x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (Subtype { name = arg0; typ = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_definition_t.name"),
                    (Result.Error "Vhdl_ast.vhdl_definition_t.typ"))
            | _ -> Result.Error "Vhdl_ast.vhdl_definition_t")) arg0
      | _ -> Result.Error "Vhdl_ast.vhdl_definition_t")
  [@ocaml.warning "-A"])

and (vhdl_expr_t_to_yojson : vhdl_expr_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | Call arg0 ->
          `List [`String "CALL"; ((fun x  -> vhdl_name_t_to_yojson x)) arg0]
      | Cst arg0 ->
          `List
            [`String "CONSTANT_VALUE";
            (let fields = []  in
             let fields =
               if arg0.unit_name = None
               then fields
               else
                 ("unit_name",
                   (((function
                      | None  -> `Null
                      | Some x -> ((fun x  -> vhdl_name_t_to_yojson x)) x))
                      arg0.unit_name))
                 :: fields
                in
             let fields =
               ("value", ((fun x  -> vhdl_cst_val_t_to_yojson x) arg0.value))
               :: fields  in
             `Assoc fields)]
      | Op arg0 ->
          `List
            [`String "EXPRESSION";
            (let fields = []  in
             let fields =
               if arg0.args = []
               then fields
               else
                 ("args",
                   (((fun x  ->
                        `List
                          (List.map (fun x  -> vhdl_expr_t_to_yojson x) x)))
                      arg0.args))
                 :: fields
                in
             let fields =
               if arg0.id = ""
               then fields
               else
                 ("id",
                   (((fun (x : Ppx_deriving_runtime.string)  -> `String x))
                      arg0.id))
                 :: fields
                in
             `Assoc fields)]
      | IsNull  -> `List [`String "IsNull"]
      | Time arg0 ->
          `List
            [`String "Time";
            (let fields = []  in
             let fields =
               if arg0.phy_unit = ""
               then fields
               else
                 ("phy_unit",
                   (((fun (x : Ppx_deriving_runtime.string)  -> `String x))
                      arg0.phy_unit))
                 :: fields
                in
             let fields =
               ("value",
                 ((fun (x : Ppx_deriving_runtime.int)  -> `Int x) arg0.value))
               :: fields  in
             `Assoc fields)]
      | Sig arg0 ->
          `List
            [`String "Sig";
            (let fields = []  in
             let fields =
               ("att",
                 ((function
                   | None  -> `Null
                   | Some x ->
                       ((fun x  -> vhdl_signal_attributes_t_to_yojson x)) x)
                    arg0.att))
               :: fields  in
             let fields =
               ("name", ((fun x  -> vhdl_name_t_to_yojson x) arg0.name)) ::
               fields  in
             `Assoc fields)]
      | SuffixMod arg0 ->
          `List
            [`String "SuffixMod";
            (let fields = []  in
             let fields =
               ("selection",
                 ((fun x  -> vhdl_suffix_selection_t_to_yojson x)
                    arg0.selection))
               :: fields  in
             let fields =
               ("expr", ((fun x  -> vhdl_expr_t_to_yojson x) arg0.expr)) ::
               fields  in
             `Assoc fields)]
      | Aggregate arg0 ->
          `List
            [`String "AGGREGATE";
            (let fields = []  in
             let fields =
               ("elems",
                 ((fun x  ->
                     `List
                       (List.map (fun x  -> vhdl_element_assoc_t_to_yojson x)
                          x)) arg0.elems))
               :: fields  in
             `Assoc fields)]
      | Others  -> `List [`String "OTHERS"])
  [@ocaml.warning "-A"])

and (vhdl_expr_t_of_yojson :
      Yojson.Safe.json -> vhdl_expr_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "CALL")::arg0::[]) ->
          ((fun x  -> vhdl_name_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (Call arg0)))
      | `List ((`String "CONSTANT_VALUE")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("value",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_cst_val_t_of_yojson x) x), arg1)
                  | ("unit_name",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `Null -> Result.Ok None
                            | x ->
                                ((fun x  -> vhdl_name_t_of_yojson x) x) >>=
                                  ((fun x  -> Result.Ok (Some x)))) x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (Cst { value = arg0; unit_name = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_expr_t.value"),
                    (Result.Ok None))
            | _ -> Result.Error "Vhdl_ast.vhdl_expr_t")) arg0
      | `List ((`String "EXPRESSION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("id",x)::xs ->
                      loop xs
                        (((function
                           | `String x -> Result.Ok x
                           | _ -> Result.Error "Vhdl_ast.vhdl_expr_t.id") x),
                          arg1)
                  | ("args",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `List xs ->
                                map_bind (fun x  -> vhdl_expr_t_of_yojson x)
                                  [] xs
                            | _ -> Result.Error "Vhdl_ast.vhdl_expr_t.args")
                             x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok (Op { id = arg0; args = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs ((Result.Ok ""), (Result.Ok []))
            | _ -> Result.Error "Vhdl_ast.vhdl_expr_t")) arg0
      | `List ((`String "IsNull")::[]) -> Result.Ok IsNull
      | `List ((`String "Time")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("value",x)::xs ->
                      loop xs
                        (((function
                           | `Int x -> Result.Ok x
                           | _ -> Result.Error "Vhdl_ast.vhdl_expr_t.value")
                            x), arg1)
                  | ("phy_unit",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `String x -> Result.Ok x
                            | _ ->
                                Result.Error "Vhdl_ast.vhdl_expr_t.phy_unit")
                             x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (Time { value = arg0; phy_unit = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_expr_t.value"),
                    (Result.Ok ""))
            | _ -> Result.Error "Vhdl_ast.vhdl_expr_t")) arg0
      | `List ((`String "Sig")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("name",x)::xs ->
                      loop xs (((fun x  -> vhdl_name_t_of_yojson x) x), arg1)
                  | ("att",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `Null -> Result.Ok None
                            | x ->
                                ((fun x  ->
                                    vhdl_signal_attributes_t_of_yojson x) x)
                                  >>= ((fun x  -> Result.Ok (Some x)))) x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok (Sig { name = arg0; att = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_expr_t.name"),
                    (Result.Error "Vhdl_ast.vhdl_expr_t.att"))
            | _ -> Result.Error "Vhdl_ast.vhdl_expr_t")) arg0
      | `List ((`String "SuffixMod")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("expr",x)::xs ->
                      loop xs (((fun x  -> vhdl_expr_t_of_yojson x) x), arg1)
                  | ("selection",x)::xs ->
                      loop xs
                        (arg0,
                          ((fun x  -> vhdl_suffix_selection_t_of_yojson x) x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (SuffixMod
                                      { expr = arg0; selection = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_expr_t.expr"),
                    (Result.Error "Vhdl_ast.vhdl_expr_t.selection"))
            | _ -> Result.Error "Vhdl_ast.vhdl_expr_t")) arg0
      | `List ((`String "AGGREGATE")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs (arg0 as _state) =
                  match xs with
                  | ("elems",x)::xs ->
                      loop xs
                        ((function
                          | `List xs ->
                              map_bind
                                (fun x  -> vhdl_element_assoc_t_of_yojson x)
                                [] xs
                          | _ -> Result.Error "Vhdl_ast.vhdl_expr_t.elems") x)
                  | [] ->
                      arg0 >>=
                        ((fun arg0  -> Result.Ok (Aggregate { elems = arg0 })))
                  | _::xs -> loop xs _state  in
                loop xs (Result.Error "Vhdl_ast.vhdl_expr_t.elems")
            | _ -> Result.Error "Vhdl_ast.vhdl_expr_t")) arg0
      | `List ((`String "OTHERS")::[]) -> Result.Ok Others
      | _ -> Result.Error "Vhdl_ast.vhdl_expr_t")
  [@ocaml.warning "-A"])

and (vhdl_name_t_to_yojson : vhdl_name_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | Simple arg0 ->
          `List
            [`String "SIMPLE_NAME";
            ((fun (x : Ppx_deriving_runtime.string)  -> `String x)) arg0]
      | Identifier arg0 ->
          `List
            [`String "IDENTIFIER";
            ((fun (x : Ppx_deriving_runtime.string)  -> `String x)) arg0]
      | Selected arg0 ->
          `List
            [`String "SELECTED_NAME";
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))) arg0]
      | Index arg0 ->
          `List
            [`String "INDEXED_NAME";
            (let fields = []  in
             let fields =
               ("exprs",
                 ((fun x  ->
                     `List (List.map (fun x  -> vhdl_expr_t_to_yojson x) x))
                    arg0.exprs))
               :: fields  in
             let fields =
               ("id", ((fun x  -> vhdl_name_t_to_yojson x) arg0.id)) ::
               fields  in
             `Assoc fields)]
      | Slice arg0 ->
          `List
            [`String "SLICE_NAME";
            (let fields = []  in
             let fields =
               ("range",
                 ((fun x  -> vhdl_discrete_range_t_to_yojson x) arg0.range))
               :: fields  in
             let fields =
               ("id", ((fun x  -> vhdl_name_t_to_yojson x) arg0.id)) ::
               fields  in
             `Assoc fields)]
      | Attribute arg0 ->
          `List
            [`String "ATTRIBUTE_NAME";
            (let fields = []  in
             let fields =
               if arg0.expr = IsNull
               then fields
               else
                 ("expr", (((fun x  -> vhdl_expr_t_to_yojson x)) arg0.expr))
                 :: fields
                in
             let fields =
               ("designator",
                 ((fun x  -> vhdl_name_t_to_yojson x) arg0.designator))
               :: fields  in
             let fields =
               ("id", ((fun x  -> vhdl_name_t_to_yojson x) arg0.id)) ::
               fields  in
             `Assoc fields)]
      | Function arg0 ->
          `List
            [`String "FUNCTION_CALL";
            (let fields = []  in
             let fields =
               ("assoc_list",
                 ((fun x  ->
                     `List
                       (List.map (fun x  -> vhdl_assoc_element_t_to_yojson x)
                          x)) arg0.assoc_list))
               :: fields  in
             let fields =
               ("id", ((fun x  -> vhdl_name_t_to_yojson x) arg0.id)) ::
               fields  in
             `Assoc fields)]
      | NoName  -> `List [`String "NoName"])
  [@ocaml.warning "-A"])

and (vhdl_name_t_of_yojson :
      Yojson.Safe.json -> vhdl_name_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "SIMPLE_NAME")::arg0::[]) ->
          ((function
            | `String x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_name_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Simple arg0)))
      | `List ((`String "IDENTIFIER")::arg0::[]) ->
          ((function
            | `String x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_name_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Identifier arg0)))
      | `List ((`String "SELECTED_NAME")::arg0::[]) ->
          ((function
            | `List xs -> map_bind (fun x  -> vhdl_name_t_of_yojson x) [] xs
            | _ -> Result.Error "Vhdl_ast.vhdl_name_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Selected arg0)))
      | `List ((`String "INDEXED_NAME")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("id",x)::xs ->
                      loop xs (((fun x  -> vhdl_name_t_of_yojson x) x), arg1)
                  | ("exprs",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `List xs ->
                                map_bind (fun x  -> vhdl_expr_t_of_yojson x)
                                  [] xs
                            | _ -> Result.Error "Vhdl_ast.vhdl_name_t.exprs")
                             x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (Index { id = arg0; exprs = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_name_t.id"),
                    (Result.Error "Vhdl_ast.vhdl_name_t.exprs"))
            | _ -> Result.Error "Vhdl_ast.vhdl_name_t")) arg0
      | `List ((`String "SLICE_NAME")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("id",x)::xs ->
                      loop xs (((fun x  -> vhdl_name_t_of_yojson x) x), arg1)
                  | ("range",x)::xs ->
                      loop xs
                        (arg0,
                          ((fun x  -> vhdl_discrete_range_t_of_yojson x) x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (Slice { id = arg0; range = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_name_t.id"),
                    (Result.Error "Vhdl_ast.vhdl_name_t.range"))
            | _ -> Result.Error "Vhdl_ast.vhdl_name_t")) arg0
      | `List ((`String "ATTRIBUTE_NAME")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("id",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
                  | ("designator",x)::xs ->
                      loop xs
                        (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2)
                  | ("expr",x)::xs ->
                      loop xs
                        (arg0, arg1, ((fun x  -> vhdl_expr_t_of_yojson x) x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (Attribute
                                           {
                                             id = arg0;
                                             designator = arg1;
                                             expr = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_name_t.id"),
                    (Result.Error "Vhdl_ast.vhdl_name_t.designator"),
                    (Result.Ok IsNull))
            | _ -> Result.Error "Vhdl_ast.vhdl_name_t")) arg0
      | `List ((`String "FUNCTION_CALL")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("id",x)::xs ->
                      loop xs (((fun x  -> vhdl_name_t_of_yojson x) x), arg1)
                  | ("assoc_list",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `List xs ->
                                map_bind
                                  (fun x  -> vhdl_assoc_element_t_of_yojson x)
                                  [] xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_name_t.assoc_list") x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (Function { id = arg0; assoc_list = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_name_t.id"),
                    (Result.Error "Vhdl_ast.vhdl_name_t.assoc_list"))
            | _ -> Result.Error "Vhdl_ast.vhdl_name_t")) arg0
      | `List ((`String "NoName")::[]) -> Result.Ok NoName
      | _ -> Result.Error "Vhdl_ast.vhdl_name_t")
  [@ocaml.warning "-A"])

and (vhdl_assoc_element_t_to_yojson :
      vhdl_assoc_element_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.actual_expr = None
          then fields
          else
            ("actual_expr",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_expr_t_to_yojson x)) x))
                 x.actual_expr))
            :: fields
           in
        let fields =
          if x.actual_designator = None
          then fields
          else
            ("actual_designator",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_name_t_to_yojson x)) x))
                 x.actual_designator))
            :: fields
           in
        let fields =
          if x.actual_name = None
          then fields
          else
            ("actual_name",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_name_t_to_yojson x)) x))
                 x.actual_name))
            :: fields
           in
        let fields =
          if x.formal_arg = None
          then fields
          else
            ("formal_arg",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_name_t_to_yojson x)) x))
                 x.formal_arg))
            :: fields
           in
        let fields =
          if x.formal_name = None
          then fields
          else
            ("formal_name",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_name_t_to_yojson x)) x))
                 x.formal_name))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_assoc_element_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_assoc_element_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3,arg4) as _state) =
            match xs with
            | ("formal_name",x)::xs ->
                loop xs
                  (((function
                     | `Null -> Result.Ok None
                     | x ->
                         ((fun x  -> vhdl_name_t_of_yojson x) x) >>=
                           ((fun x  -> Result.Ok (Some x)))) x), arg1, arg2,
                    arg3, arg4)
            | ("formal_arg",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_name_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x), arg2, arg3,
                    arg4)
            | ("actual_name",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_name_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x), arg3, arg4)
            | ("actual_designator",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_name_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x), arg4)
            | ("actual_expr",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_expr_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x))
            | [] ->
                arg4 >>=
                  ((fun arg4  ->
                      arg3 >>=
                        (fun arg3  ->
                           arg2 >>=
                             (fun arg2  ->
                                arg1 >>=
                                  (fun arg1  ->
                                     arg0 >>=
                                       (fun arg0  ->
                                          Result.Ok
                                            {
                                              formal_name = arg0;
                                              formal_arg = arg1;
                                              actual_name = arg2;
                                              actual_designator = arg3;
                                              actual_expr = arg4
                                            }))))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok (Some NoName)), (Result.Ok (Some NoName)),
              (Result.Ok (Some NoName)), (Result.Ok (Some NoName)),
              (Result.Ok (Some IsNull)))
      | _ -> Result.Error "Vhdl_ast.vhdl_assoc_element_t")
  [@ocaml.warning "-A"])

and (vhdl_element_assoc_t_to_yojson :
      vhdl_element_assoc_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields = ("expr", ((fun x  -> vhdl_expr_t_to_yojson x) x.expr))
          :: fields  in
        let fields =
          if x.choices = []
          then fields
          else
            ("choices",
              (((fun x  ->
                   `List (List.map (fun x  -> vhdl_expr_t_to_yojson x) x)))
                 x.choices))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_element_assoc_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_element_assoc_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1) as _state) =
            match xs with
            | ("choices",x)::xs ->
                loop xs
                  (((function
                     | `List xs ->
                         map_bind (fun x  -> vhdl_expr_t_of_yojson x) [] xs
                     | _ ->
                         Result.Error "Vhdl_ast.vhdl_element_assoc_t.choices")
                      x), arg1)
            | ("expr",x)::xs ->
                loop xs (arg0, ((fun x  -> vhdl_expr_t_of_yojson x) x))
            | [] ->
                arg1 >>=
                  ((fun arg1  ->
                      arg0 >>=
                        (fun arg0  ->
                           Result.Ok { choices = arg0; expr = arg1 })))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok []),
              (Result.Error "Vhdl_ast.vhdl_element_assoc_t.expr"))
      | _ -> Result.Error "Vhdl_ast.vhdl_element_assoc_t")
  [@ocaml.warning "-A"])

and (vhdl_array_attributes_t_to_yojson :
      vhdl_array_attributes_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | AAttInt arg0 ->
          `List
            [`String "AAttInt";
            (let fields = []  in
             let fields =
               ("arg",
                 ((fun (x : Ppx_deriving_runtime.int)  -> `Int x) arg0.arg))
               :: fields  in
             let fields =
               ("id",
                 ((fun (x : Ppx_deriving_runtime.string)  -> `String x)
                    arg0.id))
               :: fields  in
             `Assoc fields)]
      | AAttAscending  -> `List [`String "AAttAscending"])
  [@ocaml.warning "-A"])

and (vhdl_array_attributes_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_array_attributes_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "AAttInt")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1) as _state) =
                  match xs with
                  | ("id",x)::xs ->
                      loop xs
                        (((function
                           | `String x -> Result.Ok x
                           | _ ->
                               Result.Error
                                 "Vhdl_ast.vhdl_array_attributes_t.id") x),
                          arg1)
                  | ("arg",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `Int x -> Result.Ok x
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_array_attributes_t.arg") x))
                  | [] ->
                      arg1 >>=
                        ((fun arg1  ->
                            arg0 >>=
                              (fun arg0  ->
                                 Result.Ok
                                   (AAttInt { id = arg0; arg = arg1 }))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_array_attributes_t.id"),
                    (Result.Error "Vhdl_ast.vhdl_array_attributes_t.arg"))
            | _ -> Result.Error "Vhdl_ast.vhdl_array_attributes_t")) arg0
      | `List ((`String "AAttAscending")::[]) -> Result.Ok AAttAscending
      | _ -> Result.Error "Vhdl_ast.vhdl_array_attributes_t")
  [@ocaml.warning "-A"])

and (vhdl_signal_attributes_t_to_yojson :
      vhdl_signal_attributes_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | SigAtt arg0 ->
          `List
            [`String "SigAtt";
            ((fun (x : Ppx_deriving_runtime.string)  -> `String x)) arg0])
  [@ocaml.warning "-A"])

and (vhdl_signal_attributes_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_signal_attributes_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "SigAtt")::arg0::[]) ->
          ((function
            | `String x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_signal_attributes_t") arg0)
            >>= ((fun arg0  -> Result.Ok (SigAtt arg0)))
      | _ -> Result.Error "Vhdl_ast.vhdl_signal_attributes_t")
  [@ocaml.warning "-A"])

and (vhdl_string_attributes_t_to_yojson :
      vhdl_string_attributes_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | StringAtt arg0 ->
          `List
            [`String "StringAtt";
            ((fun (x : Ppx_deriving_runtime.string)  -> `String x)) arg0])
  [@ocaml.warning "-A"])

and (vhdl_string_attributes_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_string_attributes_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "StringAtt")::arg0::[]) ->
          ((function
            | `String x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_string_attributes_t") arg0)
            >>= ((fun arg0  -> Result.Ok (StringAtt arg0)))
      | _ -> Result.Error "Vhdl_ast.vhdl_string_attributes_t")
  [@ocaml.warning "-A"])

and (vhdl_suffix_selection_t_to_yojson :
      vhdl_suffix_selection_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | Idx arg0 ->
          `List
            [`String "Idx";
            ((fun (x : Ppx_deriving_runtime.int)  -> `Int x)) arg0]
      | SuffixRange (arg0,arg1) ->
          `List
            [`String "SuffixRange";
            ((fun (x : Ppx_deriving_runtime.int)  -> `Int x)) arg0;
            ((fun (x : Ppx_deriving_runtime.int)  -> `Int x)) arg1])
  [@ocaml.warning "-A"])

and (vhdl_suffix_selection_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_suffix_selection_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "Idx")::arg0::[]) ->
          ((function
            | `Int x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_suffix_selection_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Idx arg0)))
      | `List ((`String "SuffixRange")::arg0::arg1::[]) ->
          ((function
            | `Int x -> Result.Ok x
            | _ -> Result.Error "Vhdl_ast.vhdl_suffix_selection_t") arg1) >>=
            ((fun arg1  ->
                ((function
                  | `Int x -> Result.Ok x
                  | _ -> Result.Error "Vhdl_ast.vhdl_suffix_selection_t")
                   arg0)
                  >>= (fun arg0  -> Result.Ok (SuffixRange (arg0, arg1)))))
      | _ -> Result.Error "Vhdl_ast.vhdl_suffix_selection_t")
  [@ocaml.warning "-A"])

type 'basetype vhdl_type_attributes_t =
  | TAttNoArg of {
  id: string } 
  | TAttIntArg of {
  id: string ;
  arg: int } 
  | TAttValArg of {
  id: string ;
  arg: 'basetype } 
  | TAttStringArg of {
  id: string ;
  arg: string } 

(* TODO *)
let rec pp_vhdl_type_attributes_t
  =
  ((let open! Ppx_deriving_runtime in
      fun poly_basetype  ->
        fun fmt  ->
          function
          | TAttNoArg { id = aid } ->
              (Format.fprintf fmt "@[<2>TAttNoArg {@,";
               (Format.fprintf fmt "@[%s =@ " "id";
                (Format.fprintf fmt "%S") aid;
                Format.fprintf fmt "@]");
               Format.fprintf fmt "@]}")
          | TAttIntArg { id = aid; arg = aarg } ->
              (Format.fprintf fmt "@[<2>TAttIntArg {@,";
               ((Format.fprintf fmt "@[%s =@ " "id";
                 (Format.fprintf fmt "%S") aid;
                 Format.fprintf fmt "@]");
                Format.fprintf fmt ";@ ";
                Format.fprintf fmt "@[%s =@ " "arg";
                (Format.fprintf fmt "%d") aarg;
                Format.fprintf fmt "@]");
               Format.fprintf fmt "@]}")
          | TAttValArg { id = aid; arg = aarg } ->
              (Format.fprintf fmt "@[<2>TAttValArg {@,";
               ((Format.fprintf fmt "@[%s =@ " "id";
                 (Format.fprintf fmt "%S") aid;
                 Format.fprintf fmt "@]");
                Format.fprintf fmt ";@ ";
                Format.fprintf fmt "@[%s =@ " "arg";
                (poly_basetype fmt) aarg;
                Format.fprintf fmt "@]");
               Format.fprintf fmt "@]}")
          | TAttStringArg { id = aid; arg = aarg } ->
              (Format.fprintf fmt "@[<2>TAttStringArg {@,";
               ((Format.fprintf fmt "@[%s =@ " "id";
                 (Format.fprintf fmt "%S") aid;
                 Format.fprintf fmt "@]");
                Format.fprintf fmt ";@ ";
                Format.fprintf fmt "@[%s =@ " "arg";
                (Format.fprintf fmt "%S") aarg;
                Format.fprintf fmt "@]");
               Format.fprintf fmt "@]}"))
  [@ocaml.warning "-A"])

and show_vhdl_type_attributes_t  =
  fun poly_basetype  ->
    fun x  ->
      Format.asprintf "%a" (pp_vhdl_type_attributes_t poly_basetype) x

let rec vhdl_type_attributes_t_to_yojson :
  'basetype .
    ('basetype -> Yojson.Safe.json) ->
      'basetype vhdl_type_attributes_t -> Yojson.Safe.json
  =
  fun poly_basetype  ->
    ((let open! Ppx_deriving_yojson_runtime in
        function
        | TAttNoArg arg0 ->
            `List
              [`String "TAttNoArg";
              (let fields = []  in
               let fields =
                 ("id",
                   ((fun (x : Ppx_deriving_runtime.string)  -> `String x)
                      arg0.id))
                 :: fields  in
               `Assoc fields)]
        | TAttIntArg arg0 ->
            `List
              [`String "TAttIntArg";
              (let fields = []  in
               let fields =
                 ("arg",
                   ((fun (x : Ppx_deriving_runtime.int)  -> `Int x) arg0.arg))
                 :: fields  in
               let fields =
                 ("id",
                   ((fun (x : Ppx_deriving_runtime.string)  -> `String x)
                      arg0.id))
                 :: fields  in
               `Assoc fields)]
        | TAttValArg arg0 ->
            `List
              [`String "TAttValArg";
              (let fields = []  in
               let fields =
                 ("arg", ((poly_basetype : _ -> Yojson.Safe.json) arg0.arg))
                 :: fields  in
               let fields =
                 ("id",
                   ((fun (x : Ppx_deriving_runtime.string)  -> `String x)
                      arg0.id))
                 :: fields  in
               `Assoc fields)]
        | TAttStringArg arg0 ->
            `List
              [`String "TAttStringArg";
              (let fields = []  in
               let fields =
                 ("arg",
                   ((fun (x : Ppx_deriving_runtime.string)  -> `String x)
                      arg0.arg))
                 :: fields  in
               let fields =
                 ("id",
                   ((fun (x : Ppx_deriving_runtime.string)  -> `String x)
                      arg0.id))
                 :: fields  in
               `Assoc fields)])
    [@ocaml.warning "-A"])

and vhdl_type_attributes_t_of_yojson :
  'basetype .
    (Yojson.Safe.json -> 'basetype Ppx_deriving_yojson_runtime.error_or) ->
      Yojson.Safe.json ->
        'basetype vhdl_type_attributes_t Ppx_deriving_yojson_runtime.error_or
  =
  fun poly_basetype  ->
    ((let open! Ppx_deriving_yojson_runtime in
        function
        | `List ((`String "TAttNoArg")::arg0::[]) ->
            ((function
              | `Assoc xs ->
                  let rec loop xs (arg0 as _state) =
                    match xs with
                    | ("id",x)::xs ->
                        loop xs
                          ((function
                            | `String x -> Result.Ok x
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_type_attributes_t.id") x)
                    | [] ->
                        arg0 >>=
                          ((fun arg0  -> Result.Ok (TAttNoArg { id = arg0 })))
                    | _::xs -> loop xs _state  in
                  loop xs (Result.Error "Vhdl_ast.vhdl_type_attributes_t.id")
              | _ -> Result.Error "Vhdl_ast.vhdl_type_attributes_t")) arg0
        | `List ((`String "TAttIntArg")::arg0::[]) ->
            ((function
              | `Assoc xs ->
                  let rec loop xs ((arg0,arg1) as _state) =
                    match xs with
                    | ("id",x)::xs ->
                        loop xs
                          (((function
                             | `String x -> Result.Ok x
                             | _ ->
                                 Result.Error
                                   "Vhdl_ast.vhdl_type_attributes_t.id") x),
                            arg1)
                    | ("arg",x)::xs ->
                        loop xs
                          (arg0,
                            ((function
                              | `Int x -> Result.Ok x
                              | _ ->
                                  Result.Error
                                    "Vhdl_ast.vhdl_type_attributes_t.arg") x))
                    | [] ->
                        arg1 >>=
                          ((fun arg1  ->
                              arg0 >>=
                                (fun arg0  ->
                                   Result.Ok
                                     (TAttIntArg { id = arg0; arg = arg1 }))))
                    | _::xs -> loop xs _state  in
                  loop xs
                    ((Result.Error "Vhdl_ast.vhdl_type_attributes_t.id"),
                      (Result.Error "Vhdl_ast.vhdl_type_attributes_t.arg"))
              | _ -> Result.Error "Vhdl_ast.vhdl_type_attributes_t")) arg0
        | `List ((`String "TAttValArg")::arg0::[]) ->
            ((function
              | `Assoc xs ->
                  let rec loop xs ((arg0,arg1) as _state) =
                    match xs with
                    | ("id",x)::xs ->
                        loop xs
                          (((function
                             | `String x -> Result.Ok x
                             | _ ->
                                 Result.Error
                                   "Vhdl_ast.vhdl_type_attributes_t.id") x),
                            arg1)
                    | ("arg",x)::xs ->
                        loop xs
                          (arg0,
                            ((poly_basetype : Yojson.Safe.json -> _ error_or)
                               x))
                    | [] ->
                        arg1 >>=
                          ((fun arg1  ->
                              arg0 >>=
                                (fun arg0  ->
                                   Result.Ok
                                     (TAttValArg { id = arg0; arg = arg1 }))))
                    | _::xs -> loop xs _state  in
                  loop xs
                    ((Result.Error "Vhdl_ast.vhdl_type_attributes_t.id"),
                      (Result.Error "Vhdl_ast.vhdl_type_attributes_t.arg"))
              | _ -> Result.Error "Vhdl_ast.vhdl_type_attributes_t")) arg0
        | `List ((`String "TAttStringArg")::arg0::[]) ->
            ((function
              | `Assoc xs ->
                  let rec loop xs ((arg0,arg1) as _state) =
                    match xs with
                    | ("id",x)::xs ->
                        loop xs
                          (((function
                             | `String x -> Result.Ok x
                             | _ ->
                                 Result.Error
                                   "Vhdl_ast.vhdl_type_attributes_t.id") x),
                            arg1)
                    | ("arg",x)::xs ->
                        loop xs
                          (arg0,
                            ((function
                              | `String x -> Result.Ok x
                              | _ ->
                                  Result.Error
                                    "Vhdl_ast.vhdl_type_attributes_t.arg") x))
                    | [] ->
                        arg1 >>=
                          ((fun arg1  ->
                              arg0 >>=
                                (fun arg0  ->
                                   Result.Ok
                                     (TAttStringArg { id = arg0; arg = arg1 }))))
                    | _::xs -> loop xs _state  in
                  loop xs
                    ((Result.Error "Vhdl_ast.vhdl_type_attributes_t.id"),
                      (Result.Error "Vhdl_ast.vhdl_type_attributes_t.arg"))
              | _ -> Result.Error "Vhdl_ast.vhdl_type_attributes_t")) arg0
        | _ -> Result.Error "Vhdl_ast.vhdl_type_attributes_t")
    [@ocaml.warning "-A"])

let typ_att_noarg = ["base"; "left"; "right"; "high"; "low"] 
let typ_att_intarg = ["pos"; "val"; "succ"; "pred"; "leftof"; "rightof"] 
let typ_att_valarg = ["image"] 
let typ_att_stringarg = ["value"] 
let array_att_intarg =
  ["left"; "right"; "high"; "low"; "range"; "reverse_range"; "length"] 
type vhdl_parameter_t =
  {
  names: vhdl_name_t list ;
  mode: string list [@default []];
  typ: vhdl_subtype_indication_t ;
  init_val: vhdl_cst_val_t option [@default None]}

(* TODO *)
let rec pp_vhdl_parameter_t :
  Format.formatter -> vhdl_parameter_t -> Ppx_deriving_runtime.unit =
  let __2 () = pp_vhdl_cst_val_t
  
  and __1 () = pp_vhdl_subtype_indication_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          Format.fprintf fmt "@[<2>{ ";
          ((((Format.fprintf fmt "@[%s =@ " "names";
              ((fun x  ->
                  Format.fprintf fmt "@[<2>[";
                  ignore
                    (List.fold_left
                       (fun sep  ->
                          fun x  ->
                            if sep then Format.fprintf fmt ";@ ";
                            ((__0 ()) fmt) x;
                            true) false x);
                  Format.fprintf fmt "@,]@]")) x.names;
              Format.fprintf fmt "@]");
             Format.fprintf fmt ";@ ";
             Format.fprintf fmt "@[%s =@ " "mode";
             ((fun x  ->
                 Format.fprintf fmt "@[<2>[";
                 ignore
                   (List.fold_left
                      (fun sep  ->
                         fun x  ->
                           if sep then Format.fprintf fmt ";@ ";
                           (Format.fprintf fmt "%S") x;
                           true) false x);
                 Format.fprintf fmt "@,]@]")) x.mode;
             Format.fprintf fmt "@]");
            Format.fprintf fmt ";@ ";
            Format.fprintf fmt "@[%s =@ " "typ";
            ((__1 ()) fmt) x.typ;
            Format.fprintf fmt "@]");
           Format.fprintf fmt ";@ ";
           Format.fprintf fmt "@[%s =@ " "init_val";
           ((function
             | None  -> Format.pp_print_string fmt "None"
             | Some x ->
                 (Format.pp_print_string fmt "(Some ";
                  ((__2 ()) fmt) x;
                  Format.pp_print_string fmt ")"))) x.init_val;
           Format.fprintf fmt "@]");
          Format.fprintf fmt "@ }@]")
    [@ocaml.warning "-A"])

and show_vhdl_parameter_t : vhdl_parameter_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_parameter_t x

let rec (vhdl_parameter_t_to_yojson : vhdl_parameter_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.init_val = None
          then fields
          else
            ("init_val",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_cst_val_t_to_yojson x)) x))
                 x.init_val))
            :: fields
           in
        let fields =
          ("typ", ((fun x  -> vhdl_subtype_indication_t_to_yojson x) x.typ))
          :: fields  in
        let fields =
          if x.mode = []
          then fields
          else
            ("mode",
              (((fun x  ->
                   `List
                     (List.map
                        (fun (x : Ppx_deriving_runtime.string)  -> `String x)
                        x))) x.mode))
            :: fields
           in
        let fields =
          ("names",
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))
               x.names))
          :: fields  in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_parameter_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_parameter_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3) as _state) =
            match xs with
            | ("names",x)::xs ->
                loop xs
                  (((function
                     | `List xs ->
                         map_bind (fun x  -> vhdl_name_t_of_yojson x) [] xs
                     | _ -> Result.Error "Vhdl_ast.vhdl_parameter_t.names") x),
                    arg1, arg2, arg3)
            | ("mode",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `List xs ->
                          map_bind
                            (function
                             | `String x -> Result.Ok x
                             | _ ->
                                 Result.Error
                                   "Vhdl_ast.vhdl_parameter_t.mode") [] xs
                      | _ -> Result.Error "Vhdl_ast.vhdl_parameter_t.mode") x),
                    arg2, arg3)
            | ("typ",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((fun x  -> vhdl_subtype_indication_t_of_yojson x) x),
                    arg3)
            | ("init_val",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_cst_val_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x))
            | [] ->
                arg3 >>=
                  ((fun arg3  ->
                      arg2 >>=
                        (fun arg2  ->
                           arg1 >>=
                             (fun arg1  ->
                                arg0 >>=
                                  (fun arg0  ->
                                     Result.Ok
                                       {
                                         names = arg0;
                                         mode = arg1;
                                         typ = arg2;
                                         init_val = arg3
                                       })))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Error "Vhdl_ast.vhdl_parameter_t.names"),
              (Result.Ok []), (Result.Error "Vhdl_ast.vhdl_parameter_t.typ"),
              (Result.Ok (Some (CstInt 0))))
      | _ -> Result.Error "Vhdl_ast.vhdl_parameter_t")
  [@ocaml.warning "-A"])

type vhdl_subprogram_spec_t =
  {
  name: string [@default ""];
  typeMark: vhdl_name_t [@default NoName];
  parameters: vhdl_parameter_t list ;
  isPure: bool [@default false]}

(* TODO *)
let rec pp_vhdl_subprogram_spec_t :
  Format.formatter -> vhdl_subprogram_spec_t -> Ppx_deriving_runtime.unit =
  let __1 () = pp_vhdl_parameter_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          Format.fprintf fmt "@[<2>{ ";
          ((((Format.fprintf fmt "@[%s =@ " "name";
              (Format.fprintf fmt "%S") x.name;
              Format.fprintf fmt "@]");
             Format.fprintf fmt ";@ ";
             Format.fprintf fmt "@[%s =@ " "typeMark";
             ((__0 ()) fmt) x.typeMark;
             Format.fprintf fmt "@]");
            Format.fprintf fmt ";@ ";
            Format.fprintf fmt "@[%s =@ " "parameters";
            ((fun x  ->
                Format.fprintf fmt "@[<2>[";
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt ";@ ";
                          ((__1 ()) fmt) x;
                          true) false x);
                Format.fprintf fmt "@,]@]")) x.parameters;
            Format.fprintf fmt "@]");
           Format.fprintf fmt ";@ ";
           Format.fprintf fmt "@[%s =@ " "isPure";
           (Format.fprintf fmt "%B") x.isPure;
           Format.fprintf fmt "@]");
          Format.fprintf fmt "@ }@]")
    [@ocaml.warning "-A"])

and show_vhdl_subprogram_spec_t :
  vhdl_subprogram_spec_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_subprogram_spec_t x

let rec (vhdl_subprogram_spec_t_to_yojson :
          vhdl_subprogram_spec_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.isPure = false
          then fields
          else
            ("isPure",
              (((fun (x : Ppx_deriving_runtime.bool)  -> `Bool x)) x.isPure))
            :: fields
           in
        let fields =
          ("parameters",
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_parameter_t_to_yojson x) x))
               x.parameters))
          :: fields  in
        let fields =
          if x.typeMark = NoName
          then fields
          else
            ("typeMark", (((fun x  -> vhdl_name_t_to_yojson x)) x.typeMark))
            :: fields
           in
        let fields =
          if x.name = ""
          then fields
          else
            ("name",
              (((fun (x : Ppx_deriving_runtime.string)  -> `String x)) x.name))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_subprogram_spec_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_subprogram_spec_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3) as _state) =
            match xs with
            | ("name",x)::xs ->
                loop xs
                  (((function
                     | `String x -> Result.Ok x
                     | _ ->
                         Result.Error "Vhdl_ast.vhdl_subprogram_spec_t.name")
                      x), arg1, arg2, arg3)
            | ("typeMark",x)::xs ->
                loop xs
                  (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2, arg3)
            | ("parameters",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `List xs ->
                          map_bind (fun x  -> vhdl_parameter_t_of_yojson x)
                            [] xs
                      | _ ->
                          Result.Error
                            "Vhdl_ast.vhdl_subprogram_spec_t.parameters") x),
                    arg3)
            | ("isPure",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `Bool x -> Result.Ok x
                      | _ ->
                          Result.Error
                            "Vhdl_ast.vhdl_subprogram_spec_t.isPure") x))
            | [] ->
                arg3 >>=
                  ((fun arg3  ->
                      arg2 >>=
                        (fun arg2  ->
                           arg1 >>=
                             (fun arg1  ->
                                arg0 >>=
                                  (fun arg0  ->
                                     Result.Ok
                                       {
                                         name = arg0;
                                         typeMark = arg1;
                                         parameters = arg2;
                                         isPure = arg3
                                       })))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok ""), (Result.Ok NoName),
              (Result.Error "Vhdl_ast.vhdl_subprogram_spec_t.parameters"),
              (Result.Ok false))
      | _ -> Result.Error "Vhdl_ast.vhdl_subprogram_spec_t")
  [@ocaml.warning "-A"])

let arith_funs = ["+"; "-"; "*"; "/"; "mod"; "rem"; "abs"; "**"; "&"] 
let bool_funs = ["and"; "or"; "nand"; "nor"; "xor"; "not"] 
let rel_funs =
  ["<";
  ">";
  "<=";
  ">=";
  "/=";
  "=";
  "?=";
  "?/=";
  "?<";
  "?<=";
  "?>";
  "?>=";
  "??"] 
let shift_funs = ["sll"; "srl"; "sla"; "sra"; "rol"; "ror"] 
type vhdl_sequential_stmt_t =
  | VarAssign of
  {
  label: vhdl_name_t [@default NoName];
  lhs: vhdl_name_t ;
  rhs: vhdl_expr_t } [@name "VARIABLE_ASSIGNMENT_STATEMENT"]
  | SigSeqAssign of
  {
  label: vhdl_name_t [@default NoName];
  lhs: vhdl_name_t ;
  rhs: vhdl_expr_t list } [@name "SIGNAL_ASSIGNMENT_STATEMENT"]
  | If of
  {
  label: vhdl_name_t [@default NoName];
  if_cases: vhdl_if_case_t list ;
  default: vhdl_sequential_stmt_t list [@default []]} [@name "IF_STATEMENT"]
  | Case of
  {
  label: vhdl_name_t [@default NoName];
  guard: vhdl_expr_t ;
  branches: vhdl_case_item_t list } [@name "CASE_STATEMENT_TREE"]
  | Exit of
  {
  label: vhdl_name_t [@default NoName];
  loop_label: string option [@default Some ""];
  condition: vhdl_expr_t option [@default Some IsNull]}
  [@name "EXIT_STATEMENT"]
  | Assert of
  {
  label: vhdl_name_t [@default NoName];
  cond: vhdl_expr_t ;
  report: vhdl_expr_t [@default IsNull];
  severity: vhdl_expr_t [@default IsNull]} [@name "ASSERTION_STATEMENT"]
  | ProcedureCall of
  {
  label: vhdl_name_t [@default NoName];
  name: vhdl_name_t ;
  assocs: vhdl_assoc_element_t list } [@name "PROCEDURE_CALL_STATEMENT"]
  | Wait [@name "WAIT_STATEMENT"]
  | Null of {
  label: vhdl_name_t [@default NoName]} [@name "NULL_STATEMENT"]
  | Return of {
  label: vhdl_name_t [@default NoName]} [@name "RETURN_STATEMENT"]
and vhdl_if_case_t =
  {
  if_cond: vhdl_expr_t ;
  if_block: vhdl_sequential_stmt_t list }
and vhdl_case_item_t =
  {
  when_cond: vhdl_expr_t list ;
  when_stmt: vhdl_sequential_stmt_t list }

let rec pp_vhdl_sequential_stmt_t :
  Format.formatter -> vhdl_sequential_stmt_t -> Ppx_deriving_runtime.unit =
  let __22 () = pp_vhdl_name_t
  
  and __21 () = pp_vhdl_name_t
  
  and __20 () = pp_vhdl_assoc_element_t
  
  and __19 () = pp_vhdl_name_t
  
  and __18 () = pp_vhdl_name_t
  
  and __17 () = pp_vhdl_expr_t
  
  and __16 () = pp_vhdl_expr_t
  
  and __15 () = pp_vhdl_expr_t
  
  and __14 () = pp_vhdl_name_t
  
  and __13 () = pp_vhdl_expr_t
  
  and __12 () = pp_vhdl_name_t
  
  and __11 () = pp_vhdl_case_item_t
  
  and __10 () = pp_vhdl_expr_t
  
  and __9 () = pp_vhdl_name_t
  
  and __8 () = pp_vhdl_sequential_stmt_t
  
  and __7 () = pp_vhdl_if_case_t
  
  and __6 () = pp_vhdl_name_t
  
  and __5 () = pp_vhdl_expr_t
  
  and __4 () = pp_vhdl_name_t
  
  and __3 () = pp_vhdl_name_t
  
  and __2 () = pp_vhdl_expr_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | VarAssign { label = alabel; lhs = alhs; rhs = arhs } ->
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__0 ()) fmt) alabel;
                     Format.fprintf fmt ": ")
            );
            ((__1 ()) fmt) alhs;
            Format.fprintf fmt " := ";
            ((__2 ()) fmt) arhs;
(* TODO: Check
            (Format.fprintf fmt "@[<2>VarAssign {@,";
             (((Format.fprintf fmt "@[%s =@ " "label";
                ((__0 ()) fmt) alabel;
                Format.fprintf fmt "@]");
               Format.fprintf fmt ";@ ";
               Format.fprintf fmt "@[%s =@ " "lhs";
               ((__1 ()) fmt) alhs;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "rhs";
              ((__2 ()) fmt) arhs;
              Format.fprintf fmt "@]");
             Format.fprintf fmt "@]}") *)
        | SigSeqAssign { label = alabel; lhs = alhs; rhs = arhs } ->
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__3 ()) fmt) alabel;
                     Format.fprintf fmt ":@ ")
            );
            Format.fprintf fmt "@[<2>";
            ((__4 ()) fmt) alhs;
            Format.fprintf fmt "@ <=@ ";
            ((fun x  ->
               Format.fprintf fmt "@[";
               ignore
                 (List.fold_left
                   (fun sep  ->
                     fun x  ->
                       if sep then Format.fprintf fmt "";
                        ((__5 ()) fmt) x;
                        true) false x);
            Format.fprintf fmt "@]@]")) arhs;
        | If { label = alabel; if_cases = aif_cases; default = adefault } ->
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__6 ()) fmt) alabel;
                     Format.fprintf fmt ":@ ")
            );
            Format.fprintf fmt "@[<v>if";
            ((fun x ->
               ignore
               (List.fold_left
                 (fun sep  ->
                   fun x  ->
                           if sep then Format.fprintf fmt "@;elseif";
                                ((__7 ()) fmt) x;
                                true
                 ) false x);
             )) aif_cases;
             (match adefault with
              | [] -> Format.fprintf fmt "";
              | _  -> (Format.fprintf fmt "@;else";
                      ((fun x  ->
                          Format.fprintf fmt "@;<0 2>";
                          ignore
                            (List.fold_left
                              (fun sep  ->
                                fun x  ->
                                        if sep then Format.fprintf fmt "";
                          ((__8 ()) fmt) x;
                          true) false x))) adefault));
            Format.fprintf fmt "@;end if;@]"
        | Case { label = alabel; guard = aguard; branches = abranches } ->
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__9 ()) fmt) alabel;
                     Format.fprintf fmt ":@ ")
            );
            Format.fprintf fmt "@[<v>case ";
            ((__10 ()) fmt) aguard;
            Format.fprintf fmt " is";
            ((fun x  ->
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt "";
                          ((__11 ()) fmt) x;
                          true) false x);)) abranches;
            Format.fprintf fmt "@;end case;@]";
        | Exit
            { label = alabel; loop_label = aloop_label;
              condition = acondition }
            ->
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__12 ()) fmt) alabel;
                     Format.fprintf fmt ":@ ")
            );
            Format.fprintf fmt "exit";
            (match aloop_label with
               | None  -> Format.pp_print_string fmt ""
               | Some x -> (Format.fprintf fmt "@ %s@ ") x);
            ((function
               | None  -> Format.pp_print_string fmt ""
               | Some x ->
                   (Format.pp_print_string fmt "when@ ";
                    ((__13 ()) fmt) x;))) acondition;
        | Assert
            { label = alabel; cond = acond; report = areport;
              severity = aseverity }
            ->
            Format.fprintf fmt "@[<v 2>";
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__14 ()) fmt) alabel;
                     Format.fprintf fmt ":@ ")
            );
            Format.fprintf fmt "assert ";
            ((__15 ()) fmt) acond;
            (match areport with
            | IsNull -> Format.fprintf fmt "";
            | _ -> 
                Format.fprintf fmt "@;report ";
                ((__16 ()) fmt) areport);
            (match aseverity with
            | IsNull -> Format.fprintf fmt "";
            | _ -> 
                Format.fprintf fmt "@;severity ";
                ((__17 ()) fmt) aseverity);
            Format.fprintf fmt "@]";
        | ProcedureCall { label = alabel; name = aname; assocs = aassocs } ->
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__18 ()) fmt) alabel;
                     Format.fprintf fmt ":@ ")
            );
            ((__19 ()) fmt) aname;
            ((fun x  ->
                Format.fprintf fmt "(";
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt ",@ ";
                          ((__20 ()) fmt) x;
                          true) false x);
            Format.fprintf fmt ")")) aassocs;
        | Wait  -> Format.pp_print_string fmt "wait"
        | Null { label = alabel } ->
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__18 ()) fmt) alabel;
                     Format.fprintf fmt ":@ ")
            );
            Format.fprintf fmt "null";
        | Return { label = alabel } ->
            (match alabel with
              | NoName -> Format.fprintf fmt "";
              | _ -> (((__19 ()) fmt) alabel;
                     Format.fprintf fmt ":@ ")
            );
            Format.fprintf fmt "return";)
    [@ocaml.warning "-A"])

and show_vhdl_sequential_stmt_t :
  vhdl_sequential_stmt_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_sequential_stmt_t x

and pp_vhdl_if_case_t :
  Format.formatter -> vhdl_if_case_t -> Ppx_deriving_runtime.unit =
  let __1 () = pp_vhdl_sequential_stmt_t
  
  and __0 () = pp_vhdl_expr_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          Format.fprintf fmt " (";
          ((__0 ()) fmt) x.if_cond;
          Format.fprintf fmt ") then@;<0 2>";
          ((fun x  ->
             ignore
               (List.fold_left
                  (fun sep  ->
                     fun x  ->
                             if sep then Format.fprintf fmt "@;<0 2>";
                       ((__1 ()) fmt) x;
                       true) false x);
          )) x.if_block;)
    [@ocaml.warning "-A"])

and show_vhdl_if_case_t : vhdl_if_case_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_if_case_t x

and pp_vhdl_case_item_t :
  Format.formatter -> vhdl_case_item_t -> Ppx_deriving_runtime.unit =
  let __1 () = pp_vhdl_sequential_stmt_t
  
  and __0 () = pp_vhdl_expr_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
                Format.fprintf fmt "@;@[<v 2>when ";
            ((fun x  ->
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt "@ |@ ";
                          ((__0 ()) fmt) x;
                          true) false x);)) x.when_cond;
           Format.fprintf fmt " => ";
           (fun x  ->
               ignore
                 (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         if sep then Format.fprintf fmt "@;";
                         ((__1 ()) fmt) x;
                         Format.fprintf fmt ";";
                         true) ((List.length x) > 1) x);) x.when_stmt;
           Format.fprintf fmt "@]")
    [@ocaml.warning "-A"])

and show_vhdl_case_item_t : vhdl_case_item_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_case_item_t x

let rec (vhdl_sequential_stmt_t_to_yojson :
          vhdl_sequential_stmt_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | VarAssign arg0 ->
          `List
            [`String "VARIABLE_ASSIGNMENT_STATEMENT";
            (let fields = []  in
             let fields =
               ("rhs", ((fun x  -> vhdl_expr_t_to_yojson x) arg0.rhs)) ::
               fields  in
             let fields =
               ("lhs", ((fun x  -> vhdl_name_t_to_yojson x) arg0.lhs)) ::
               fields  in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)]
      | SigSeqAssign arg0 ->
          `List
            [`String "SIGNAL_ASSIGNMENT_STATEMENT";
            (let fields = []  in
             let fields =
               ("rhs",
                 ((fun x  ->
                     `List (List.map (fun x  -> vhdl_expr_t_to_yojson x) x))
                    arg0.rhs))
               :: fields  in
             let fields =
               ("lhs", ((fun x  -> vhdl_name_t_to_yojson x) arg0.lhs)) ::
               fields  in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)]
      | If arg0 ->
          `List
            [`String "IF_STATEMENT";
            (let fields = []  in
             let fields =
               if arg0.default = []
               then fields
               else
                 ("default",
                   (((fun x  ->
                        `List
                          (List.map
                             (fun x  -> vhdl_sequential_stmt_t_to_yojson x) x)))
                      arg0.default))
                 :: fields
                in
             let fields =
               ("if_cases",
                 ((fun x  ->
                     `List
                       (List.map (fun x  -> vhdl_if_case_t_to_yojson x) x))
                    arg0.if_cases))
               :: fields  in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)]
      | Case arg0 ->
          `List
            [`String "CASE_STATEMENT_TREE";
            (let fields = []  in
             let fields =
               ("branches",
                 ((fun x  ->
                     `List
                       (List.map (fun x  -> vhdl_case_item_t_to_yojson x) x))
                    arg0.branches))
               :: fields  in
             let fields =
               ("guard", ((fun x  -> vhdl_expr_t_to_yojson x) arg0.guard)) ::
               fields  in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)]
      | Exit arg0 ->
          `List
            [`String "EXIT_STATEMENT";
            (let fields = []  in
             let fields =
               if arg0.condition = (Some IsNull)
               then fields
               else
                 ("condition",
                   (((function
                      | None  -> `Null
                      | Some x -> ((fun x  -> vhdl_expr_t_to_yojson x)) x))
                      arg0.condition))
                 :: fields
                in
             let fields =
               if arg0.loop_label = (Some "")
               then fields
               else
                 ("loop_label",
                   (((function
                      | None  -> `Null
                      | Some x ->
                          ((fun (x : Ppx_deriving_runtime.string)  ->
                              `String x)) x)) arg0.loop_label))
                 :: fields
                in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)]
      | Assert arg0 ->
          `List
            [`String "ASSERTION_STATEMENT";
            (let fields = []  in
             let fields =
               if arg0.severity = IsNull
               then fields
               else
                 ("severity",
                   (((fun x  -> vhdl_expr_t_to_yojson x)) arg0.severity))
                 :: fields
                in
             let fields =
               if arg0.report = IsNull
               then fields
               else
                 ("report",
                   (((fun x  -> vhdl_expr_t_to_yojson x)) arg0.report))
                 :: fields
                in
             let fields =
               ("cond", ((fun x  -> vhdl_expr_t_to_yojson x) arg0.cond)) ::
               fields  in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)]
      | ProcedureCall arg0 ->
          `List
            [`String "PROCEDURE_CALL_STATEMENT";
            (let fields = []  in
             let fields =
               ("assocs",
                 ((fun x  ->
                     `List
                       (List.map (fun x  -> vhdl_assoc_element_t_to_yojson x)
                          x)) arg0.assocs))
               :: fields  in
             let fields =
               ("name", ((fun x  -> vhdl_name_t_to_yojson x) arg0.name)) ::
               fields  in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)]
      | Wait  -> `List [`String "WAIT_STATEMENT"]
      | Null arg0 ->
          `List
            [`String "NULL_STATEMENT";
            (let fields = []  in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)]
      | Return arg0 ->
          `List
            [`String "RETURN_STATEMENT";
            (let fields = []  in
             let fields =
               if arg0.label = NoName
               then fields
               else
                 ("label",
                   (((fun x  -> vhdl_name_t_to_yojson x)) arg0.label))
                 :: fields
                in
             `Assoc fields)])
  [@ocaml.warning "-A"])

and (vhdl_sequential_stmt_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_sequential_stmt_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "VARIABLE_ASSIGNMENT_STATEMENT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
                  | ("lhs",x)::xs ->
                      loop xs
                        (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2)
                  | ("rhs",x)::xs ->
                      loop xs
                        (arg0, arg1, ((fun x  -> vhdl_expr_t_of_yojson x) x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (VarAssign
                                           {
                                             label = arg0;
                                             lhs = arg1;
                                             rhs = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok NoName),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.lhs"),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.rhs"))
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | `List ((`String "SIGNAL_ASSIGNMENT_STATEMENT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
                  | ("lhs",x)::xs ->
                      loop xs
                        (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2)
                  | ("rhs",x)::xs ->
                      loop xs
                        (arg0, arg1,
                          ((function
                            | `List xs ->
                                map_bind (fun x  -> vhdl_expr_t_of_yojson x)
                                  [] xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_sequential_stmt_t.rhs") x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (SigSeqAssign
                                           {
                                             label = arg0;
                                             lhs = arg1;
                                             rhs = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok NoName),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.lhs"),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.rhs"))
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | `List ((`String "IF_STATEMENT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
                  | ("if_cases",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `List xs ->
                                map_bind
                                  (fun x  -> vhdl_if_case_t_of_yojson x) []
                                  xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_sequential_stmt_t.if_cases")
                             x), arg2)
                  | ("default",x)::xs ->
                      loop xs
                        (arg0, arg1,
                          ((function
                            | `List xs ->
                                map_bind
                                  (fun x  ->
                                     vhdl_sequential_stmt_t_of_yojson x) []
                                  xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_sequential_stmt_t.default")
                             x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (If
                                           {
                                             label = arg0;
                                             if_cases = arg1;
                                             default = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok NoName),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.if_cases"),
                    (Result.Ok []))
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | `List ((`String "CASE_STATEMENT_TREE")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
                  | ("guard",x)::xs ->
                      loop xs
                        (arg0, ((fun x  -> vhdl_expr_t_of_yojson x) x), arg2)
                  | ("branches",x)::xs ->
                      loop xs
                        (arg0, arg1,
                          ((function
                            | `List xs ->
                                map_bind
                                  (fun x  -> vhdl_case_item_t_of_yojson x) []
                                  xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_sequential_stmt_t.branches")
                             x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (Case
                                           {
                                             label = arg0;
                                             guard = arg1;
                                             branches = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok NoName),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.guard"),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.branches"))
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | `List ((`String "EXIT_STATEMENT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
                  | ("loop_label",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `Null -> Result.Ok None
                            | x ->
                                ((function
                                  | `String x -> Result.Ok x
                                  | _ ->
                                      Result.Error
                                        "Vhdl_ast.vhdl_sequential_stmt_t.loop_label")
                                   x)
                                  >>= ((fun x  -> Result.Ok (Some x)))) x),
                          arg2)
                  | ("condition",x)::xs ->
                      loop xs
                        (arg0, arg1,
                          ((function
                            | `Null -> Result.Ok None
                            | x ->
                                ((fun x  -> vhdl_expr_t_of_yojson x) x) >>=
                                  ((fun x  -> Result.Ok (Some x)))) x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (Exit
                                           {
                                             label = arg0;
                                             loop_label = arg1;
                                             condition = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok NoName), (Result.Ok (Some "")),
                    (Result.Ok (Some IsNull)))
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | `List ((`String "ASSERTION_STATEMENT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2,arg3) as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2,
                          arg3)
                  | ("cond",x)::xs ->
                      loop xs
                        (arg0, ((fun x  -> vhdl_expr_t_of_yojson x) x), arg2,
                          arg3)
                  | ("report",x)::xs ->
                      loop xs
                        (arg0, arg1, ((fun x  -> vhdl_expr_t_of_yojson x) x),
                          arg3)
                  | ("severity",x)::xs ->
                      loop xs
                        (arg0, arg1, arg2,
                          ((fun x  -> vhdl_expr_t_of_yojson x) x))
                  | [] ->
                      arg3 >>=
                        ((fun arg3  ->
                            arg2 >>=
                              (fun arg2  ->
                                 arg1 >>=
                                   (fun arg1  ->
                                      arg0 >>=
                                        (fun arg0  ->
                                           Result.Ok
                                             (Assert
                                                {
                                                  label = arg0;
                                                  cond = arg1;
                                                  report = arg2;
                                                  severity = arg3
                                                }))))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok NoName),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.cond"),
                    (Result.Ok IsNull), (Result.Ok IsNull))
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | `List ((`String "PROCEDURE_CALL_STATEMENT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
                  | ("name",x)::xs ->
                      loop xs
                        (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2)
                  | ("assocs",x)::xs ->
                      loop xs
                        (arg0, arg1,
                          ((function
                            | `List xs ->
                                map_bind
                                  (fun x  -> vhdl_assoc_element_t_of_yojson x)
                                  [] xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_sequential_stmt_t.assocs") x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (ProcedureCall
                                           {
                                             label = arg0;
                                             name = arg1;
                                             assocs = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok NoName),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.name"),
                    (Result.Error "Vhdl_ast.vhdl_sequential_stmt_t.assocs"))
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | `List ((`String "WAIT_STATEMENT")::[]) -> Result.Ok Wait
      | `List ((`String "NULL_STATEMENT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs (arg0 as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs ((fun x  -> vhdl_name_t_of_yojson x) x)
                  | [] ->
                      arg0 >>=
                        ((fun arg0  -> Result.Ok (Null { label = arg0 })))
                  | _::xs -> loop xs _state  in
                loop xs (Result.Ok NoName)
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | `List ((`String "RETURN_STATEMENT")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs (arg0 as _state) =
                  match xs with
                  | ("label",x)::xs ->
                      loop xs ((fun x  -> vhdl_name_t_of_yojson x) x)
                  | [] ->
                      arg0 >>=
                        ((fun arg0  -> Result.Ok (Return { label = arg0 })))
                  | _::xs -> loop xs _state  in
                loop xs (Result.Ok NoName)
            | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")) arg0
      | _ -> Result.Error "Vhdl_ast.vhdl_sequential_stmt_t")
  [@ocaml.warning "-A"])

and (vhdl_if_case_t_to_yojson : vhdl_if_case_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          ("if_block",
            ((fun x  ->
                `List
                  (List.map (fun x  -> vhdl_sequential_stmt_t_to_yojson x) x))
               x.if_block))
          :: fields  in
        let fields =
          ("if_cond", ((fun x  -> vhdl_expr_t_to_yojson x) x.if_cond)) ::
          fields  in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_if_case_t_of_yojson :
      Yojson.Safe.json -> vhdl_if_case_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1) as _state) =
            match xs with
            | ("if_cond",x)::xs ->
                loop xs (((fun x  -> vhdl_expr_t_of_yojson x) x), arg1)
            | ("if_block",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_sequential_stmt_t_of_yojson x) []
                            xs
                      | _ -> Result.Error "Vhdl_ast.vhdl_if_case_t.if_block")
                       x))
            | [] ->
                arg1 >>=
                  ((fun arg1  ->
                      arg0 >>=
                        (fun arg0  ->
                           Result.Ok { if_cond = arg0; if_block = arg1 })))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Error "Vhdl_ast.vhdl_if_case_t.if_cond"),
              (Result.Error "Vhdl_ast.vhdl_if_case_t.if_block"))
      | _ -> Result.Error "Vhdl_ast.vhdl_if_case_t")
  [@ocaml.warning "-A"])

and (vhdl_case_item_t_to_yojson : vhdl_case_item_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          ("when_stmt",
            ((fun x  ->
                `List
                  (List.map (fun x  -> vhdl_sequential_stmt_t_to_yojson x) x))
               x.when_stmt))
          :: fields  in
        let fields =
          ("when_cond",
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_expr_t_to_yojson x) x))
               x.when_cond))
          :: fields  in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_case_item_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_case_item_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1) as _state) =
            match xs with
            | ("when_cond",x)::xs ->
                loop xs
                  (((function
                     | `List xs ->
                         map_bind (fun x  -> vhdl_expr_t_of_yojson x) [] xs
                     | _ ->
                         Result.Error "Vhdl_ast.vhdl_case_item_t.when_cond")
                      x), arg1)
            | ("when_stmt",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_sequential_stmt_t_of_yojson x) []
                            xs
                      | _ ->
                          Result.Error "Vhdl_ast.vhdl_case_item_t.when_stmt")
                       x))
            | [] ->
                arg1 >>=
                  ((fun arg1  ->
                      arg0 >>=
                        (fun arg0  ->
                           Result.Ok { when_cond = arg0; when_stmt = arg1 })))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Error "Vhdl_ast.vhdl_case_item_t.when_cond"),
              (Result.Error "Vhdl_ast.vhdl_case_item_t.when_stmt"))
      | _ -> Result.Error "Vhdl_ast.vhdl_case_item_t")
  [@ocaml.warning "-A"])

type vhdl_port_mode_t =
  | InPort [@name "in"]
  | OutPort [@name "out"]
  | InoutPort [@name "inout"]
  | BufferPort [@name "buffer"]

let rec (pp_vhdl_port_mode_t :
          Format.formatter -> vhdl_port_mode_t -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | InPort  -> Format.pp_print_string fmt "in"
        | OutPort  -> Format.pp_print_string fmt "out"
        | InoutPort  -> Format.pp_print_string fmt "inout"
        | BufferPort  -> Format.pp_print_string fmt "buffer")
  [@ocaml.warning "-A"])

and show_vhdl_port_mode_t : vhdl_port_mode_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_port_mode_t x

let rec (vhdl_port_mode_t_to_yojson : vhdl_port_mode_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | InPort  -> `List [`String "in"]
      | OutPort  -> `List [`String "out"]
      | InoutPort  -> `List [`String "inout"]
      | BufferPort  -> `List [`String "buffer"])
  [@ocaml.warning "-A"])

and (vhdl_port_mode_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_port_mode_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "in")::[]) -> Result.Ok InPort
      | `List ((`String "out")::[]) -> Result.Ok OutPort
      | `List ((`String "inout")::[]) -> Result.Ok InoutPort
      | `List ((`String "buffer")::[]) -> Result.Ok BufferPort
      | _ -> Result.Error "Vhdl_ast.vhdl_port_mode_t")
  [@ocaml.warning "-A"])

type vhdl_port_t =
  {
  names: vhdl_name_t list [@default []];
  mode: vhdl_port_mode_t [@default InPort];
  typ: vhdl_subtype_indication_t ;
  expr: vhdl_expr_t [@default IsNull]}

let rec pp_vhdl_port_t :
  Format.formatter -> vhdl_port_t -> Ppx_deriving_runtime.unit =
  let __3 () = pp_vhdl_expr_t
  
  and __2 () = pp_vhdl_subtype_indication_t
  
  and __1 () = pp_vhdl_port_mode_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          Format.fprintf fmt "@[";
          ((((
              ((fun x  ->
                  Format.fprintf fmt "@[";
                  ignore
                    (List.fold_left
                       (fun sep  ->
                          fun x  ->
                            if sep then Format.fprintf fmt ",@ ";
                            ((__0 ()) fmt) x;
                            true) false x);
                  Format.fprintf fmt "@,@]")) x.names;
              );
             Format.fprintf fmt ": ";
             ((__1 ()) fmt) x.mode;
             );
             Format.fprintf fmt " ";
            ((__2 ()) fmt) x.typ;
            );
          (match x.expr with
           | IsNull -> Format.fprintf fmt "";
           | _ -> (Format.fprintf fmt "@[:= ";
                   ((__3 ()) fmt) x.expr;
                   Format.fprintf fmt "@]"));
          Format.fprintf fmt "@]"))
    [@ocaml.warning "-A"])

and show_vhdl_port_t : vhdl_port_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_port_t x

let rec (vhdl_port_t_to_yojson : vhdl_port_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.expr = IsNull
          then fields
          else ("expr", (((fun x  -> vhdl_expr_t_to_yojson x)) x.expr)) ::
            fields
           in
        let fields =
          ("typ", ((fun x  -> vhdl_subtype_indication_t_to_yojson x) x.typ))
          :: fields  in
        let fields =
          if x.mode = InPort
          then fields
          else ("mode", (((fun x  -> vhdl_port_mode_t_to_yojson x)) x.mode))
            :: fields
           in
        let fields =
          if x.names = []
          then fields
          else
            ("names",
              (((fun x  ->
                   `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x)))
                 x.names))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_port_t_of_yojson :
      Yojson.Safe.json -> vhdl_port_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3) as _state) =
            match xs with
            | ("names",x)::xs ->
                loop xs
                  (((function
                     | `List xs ->
                         map_bind (fun x  -> vhdl_name_t_of_yojson x) [] xs
                     | _ -> Result.Error "Vhdl_ast.vhdl_port_t.names") x),
                    arg1, arg2, arg3)
            | ("mode",x)::xs ->
                loop xs
                  (arg0, ((fun x  -> vhdl_port_mode_t_of_yojson x) x), arg2,
                    arg3)
            | ("typ",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((fun x  -> vhdl_subtype_indication_t_of_yojson x) x),
                    arg3)
            | ("expr",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, ((fun x  -> vhdl_expr_t_of_yojson x) x))
            | [] ->
                arg3 >>=
                  ((fun arg3  ->
                      arg2 >>=
                        (fun arg2  ->
                           arg1 >>=
                             (fun arg1  ->
                                arg0 >>=
                                  (fun arg0  ->
                                     Result.Ok
                                       {
                                         names = arg0;
                                         mode = arg1;
                                         typ = arg2;
                                         expr = arg3
                                       })))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok []), (Result.Ok InPort),
              (Result.Error "Vhdl_ast.vhdl_port_t.typ"), (Result.Ok IsNull))
      | _ -> Result.Error "Vhdl_ast.vhdl_port_t")
  [@ocaml.warning "-A"])
type vhdl_declaration_t =
  | VarDecl of
  {
  names: vhdl_name_t list ;
  typ: vhdl_subtype_indication_t ;
  init_val: vhdl_expr_t [@default IsNull]} [@name "VARIABLE_DECLARATION"]
  | CstDecl of
  {
  names: vhdl_name_t list ;
  typ: vhdl_subtype_indication_t ;
  init_val: vhdl_expr_t } [@name "CONSTANT_DECLARATION"]
  | SigDecl of
  {
  names: vhdl_name_t list ;
  typ: vhdl_subtype_indication_t ;
  init_val: vhdl_expr_t [@default IsNull]} [@name "SIGNAL_DECLARATION"]
  | ComponentDecl of
  {
  name: vhdl_name_t [@default NoName];
  generics: vhdl_port_t list [@default []];
  ports: vhdl_port_t list [@default []]} [@name "COMPONENT_DECLARATION"]
  | Subprogram of
  {
  name: string [@default ""];
  kind: string [@default ""];
  spec: vhdl_subprogram_spec_t option [@default None];
  decl_part: vhdl_declaration_t list [@default []];
  stmts: vhdl_sequential_stmt_t list [@default []]} [@name "SUBPROGRAM_BODY"]

(* Needs adaptation for: SubProgram *)
let rec pp_vhdl_declaration_t :
  Format.formatter -> vhdl_declaration_t -> Ppx_deriving_runtime.unit =
  let __14 () = pp_vhdl_sequential_stmt_t
  
  and __13 () = pp_vhdl_declaration_t
  
  and __12 () = pp_vhdl_subprogram_spec_t

  and __11 () = pp_vhdl_port_t
  
  and __10 () = pp_vhdl_port_t
  
  and __9 () = pp_vhdl_name_t
  
  and __8 () = pp_vhdl_expr_t
  
  and __7 () = pp_vhdl_subtype_indication_t
  
  and __6 () = pp_vhdl_name_t
  
  and __5 () = pp_vhdl_expr_t
  
  and __4 () = pp_vhdl_subtype_indication_t
  
  and __3 () = pp_vhdl_name_t
  
  and __2 () = pp_vhdl_expr_t
  
  and __1 () = pp_vhdl_subtype_indication_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | VarDecl { names = anames; typ = atyp; init_val = ainit_val } ->
            (Format.fprintf fmt "variable ";
             ((((fun x  ->
                    ignore
                      (List.fold_left
                         (fun sep  ->
                            fun x  ->
                              if sep then Format.fprintf fmt ",";
                              ((__0 ()) fmt) x;
                              true) false x);)) anames;
               Format.fprintf fmt " : ";
               ((__1 ()) fmt) atyp;
               (match ainit_val with
                | IsNull  -> Format.pp_print_string fmt ""
                | _ ->
                    (Format.fprintf fmt ":=";
                     ((__2 ()) fmt) ainit_val;))));)
        | CstDecl { names = anames; typ = atyp; init_val = ainit_val } ->
            (Format.fprintf fmt "constant ";
             ((((fun x  ->
                    ignore
                      (List.fold_left
                         (fun sep  ->
                            fun x  ->
                              if sep then Format.fprintf fmt ",";
                              ((__3 ()) fmt) x;
                              true) false x);)) anames;
               Format.fprintf fmt " : ";
               ((__4 ()) fmt) atyp;
              Format.fprintf fmt ":=";
              ((__5 ()) fmt) ainit_val)))
        | SigDecl { names = anames; typ = atyp; init_val = ainit_val } ->
            (Format.fprintf fmt "signal ";
            ((fun x  ->
              ignore
              (List.fold_left
                (fun sep  ->
                  fun x  ->
                    if sep then Format.fprintf fmt ",";
                                ((__6 ()) fmt) x;
                                true) false x);
              )) anames;
            Format.fprintf fmt " : ";
            ((__7 ()) fmt) atyp;
            (match ainit_val with
              | IsNull  -> Format.pp_print_string fmt ""
              | _ ->
                  (Format.fprintf fmt ":=";
                  ((__8 ()) fmt) ainit_val;)))
        | ComponentDecl
            { name = aname; generics = agenerics; ports = aports } ->
            Format.fprintf fmt "@[<v 2>component ";
            ((__9 ()) fmt) aname;
            Format.fprintf fmt " is@;";
            ((fun x  ->
              ignore
                (List.fold_left
                  (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt "@;";
                        ((__10 ()) fmt) x;
                        true) false x))) agenerics;
            ((fun x  ->
              ignore
                (List.fold_left
                  (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt "@;";
                        ((__11 ()) fmt) x;
                        true) false x))) aports;
            Format.fprintf fmt "@]@;end component";
        | Subprogram
            { name = aname; kind = akind; spec = aspec;
              decl_part = adecl_part; stmts = astmts }
            ->
            (Format.fprintf fmt "@[<2>Subprogram {@,";
             (((((Format.fprintf fmt "@[%s =@ " "name";
                  (Format.fprintf fmt "%S") aname;
                  Format.fprintf fmt "@]");
                 Format.fprintf fmt ";@ ";
                 Format.fprintf fmt "@[%s =@ " "kind";
                 (Format.fprintf fmt "%S") akind;
                 Format.fprintf fmt "@]");
                Format.fprintf fmt ";@ ";
                Format.fprintf fmt "@[%s =@ " "spec";
                ((function
                  | None  -> Format.pp_print_string fmt "None"
                  | Some x ->
                      (Format.pp_print_string fmt "(Some ";
                       ((__12 ()) fmt) x;
                       Format.pp_print_string fmt ")"))) aspec;
                Format.fprintf fmt "@]");
               Format.fprintf fmt ";@ ";
               Format.fprintf fmt "@[%s =@ " "decl_part";
               ((fun x  ->
                   Format.fprintf fmt "@[<2>[";
                   ignore
                     (List.fold_left
                        (fun sep  ->
                           fun x  ->
                             if sep then Format.fprintf fmt ";@ ";
                             ((__13 ()) fmt) x;
                             true) false x);
                   Format.fprintf fmt "@,]@]")) adecl_part;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "stmts";
              ((fun x  ->
                  Format.fprintf fmt "@[<2>[";
                  ignore
                    (List.fold_left
                       (fun sep  ->
                          fun x  ->
                            if sep then Format.fprintf fmt ";@ ";
                            ((__14 ()) fmt) x;
                            true) false x);
                  Format.fprintf fmt "@,]@]")) astmts;
              Format.fprintf fmt "@]");
             Format.fprintf fmt "@]}"))
    [@ocaml.warning "-A"])

and show_vhdl_declaration_t :
  vhdl_declaration_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_declaration_t x

let rec (vhdl_declaration_t_to_yojson :
          vhdl_declaration_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | VarDecl arg0 ->
          `List
            [`String "VARIABLE_DECLARATION";
            (let fields = []  in
             let fields =
               if arg0.init_val = IsNull
               then fields
               else
                 ("init_val",
                   (((fun x  -> vhdl_expr_t_to_yojson x)) arg0.init_val))
                 :: fields
                in
             let fields =
               ("typ",
                 ((fun x  -> vhdl_subtype_indication_t_to_yojson x) arg0.typ))
               :: fields  in
             let fields =
               ("names",
                 ((fun x  ->
                     `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))
                    arg0.names))
               :: fields  in
             `Assoc fields)]
      | CstDecl arg0 ->
          `List
            [`String "CONSTANT_DECLARATION";
            (let fields = []  in
             let fields =
               ("init_val",
                 ((fun x  -> vhdl_expr_t_to_yojson x) arg0.init_val))
               :: fields  in
             let fields =
               ("typ",
                 ((fun x  -> vhdl_subtype_indication_t_to_yojson x) arg0.typ))
               :: fields  in
             let fields =
               ("names",
                 ((fun x  ->
                     `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))
                    arg0.names))
               :: fields  in
             `Assoc fields)]
      | SigDecl arg0 ->
          `List
            [`String "SIGNAL_DECLARATION";
            (let fields = []  in
             let fields =
               if arg0.init_val = IsNull
               then fields
               else
                 ("init_val",
                   (((fun x  -> vhdl_expr_t_to_yojson x)) arg0.init_val))
                 :: fields
                in
             let fields =
               ("typ",
                 ((fun x  -> vhdl_subtype_indication_t_to_yojson x) arg0.typ))
               :: fields  in
             let fields =
               ("names",
                 ((fun x  ->
                     `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))
                    arg0.names))
               :: fields  in
             `Assoc fields)]
      | ComponentDecl arg0 ->
          `List
            [`String "COMPONENT_DECLARATION";
            (let fields = []  in
             let fields =
               if arg0.ports = []
               then fields
               else
                 ("ports",
                   (((fun x  ->
                        `List
                          (List.map (fun x  -> vhdl_port_t_to_yojson x) x)))
                      arg0.ports))
                 :: fields
                in
             let fields =
               if arg0.generics = []
               then fields
               else
                 ("generics",
                   (((fun x  ->
                        `List
                          (List.map (fun x  -> vhdl_port_t_to_yojson x) x)))
                      arg0.generics))
                 :: fields
                in
             let fields =
               if arg0.name = NoName
               then fields
               else
                 ("name", (((fun x  -> vhdl_name_t_to_yojson x)) arg0.name))
                 :: fields
                in
             `Assoc fields)]
      | Subprogram arg0 ->
          `List
            [`String "SUBPROGRAM_BODY";
            (let fields = []  in
             let fields =
               if arg0.stmts = []
               then fields
               else
                 ("stmts",
                   (((fun x  ->
                        `List
                          (List.map
                             (fun x  -> vhdl_sequential_stmt_t_to_yojson x) x)))
                      arg0.stmts))
                 :: fields
                in
             let fields =
               if arg0.decl_part = []
               then fields
               else
                 ("decl_part",
                   (((fun x  ->
                        `List
                          (List.map
                             (fun x  -> vhdl_declaration_t_to_yojson x) x)))
                      arg0.decl_part))
                 :: fields
                in
             let fields =
               if arg0.spec = None
               then fields
               else
                 ("spec",
                   (((function
                      | None  -> `Null
                      | Some x ->
                          ((fun x  -> vhdl_subprogram_spec_t_to_yojson x)) x))
                      arg0.spec))
                 :: fields
                in
             let fields =
               if arg0.kind = ""
               then fields
               else
                 ("kind",
                   (((fun (x : Ppx_deriving_runtime.string)  -> `String x))
                      arg0.kind))
                 :: fields
                in
             let fields =
               if arg0.name = ""
               then fields
               else
                 ("name",
                   (((fun (x : Ppx_deriving_runtime.string)  -> `String x))
                      arg0.name))
                 :: fields
                in
             `Assoc fields)])
  [@ocaml.warning "-A"])

and (vhdl_declaration_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_declaration_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "VARIABLE_DECLARATION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("names",x)::xs ->
                      loop xs
                        (((function
                           | `List xs ->
                               map_bind (fun x  -> vhdl_name_t_of_yojson x)
                                 [] xs
                           | _ ->
                               Result.Error
                                 "Vhdl_ast.vhdl_declaration_t.names") x),
                          arg1, arg2)
                  | ("typ",x)::xs ->
                      loop xs
                        (arg0,
                          ((fun x  -> vhdl_subtype_indication_t_of_yojson x)
                             x), arg2)
                  | ("init_val",x)::xs ->
                      loop xs
                        (arg0, arg1, ((fun x  -> vhdl_expr_t_of_yojson x) x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (VarDecl
                                           {
                                             names = arg0;
                                             typ = arg1;
                                             init_val = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_declaration_t.names"),
                    (Result.Error "Vhdl_ast.vhdl_declaration_t.typ"),
                    (Result.Ok IsNull))
            | _ -> Result.Error "Vhdl_ast.vhdl_declaration_t")) arg0
      | `List ((`String "CONSTANT_DECLARATION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("names",x)::xs ->
                      loop xs
                        (((function
                           | `List xs ->
                               map_bind (fun x  -> vhdl_name_t_of_yojson x)
                                 [] xs
                           | _ ->
                               Result.Error
                                 "Vhdl_ast.vhdl_declaration_t.names") x),
                          arg1, arg2)
                  | ("typ",x)::xs ->
                      loop xs
                        (arg0,
                          ((fun x  -> vhdl_subtype_indication_t_of_yojson x)
                             x), arg2)
                  | ("init_val",x)::xs ->
                      loop xs
                        (arg0, arg1, ((fun x  -> vhdl_expr_t_of_yojson x) x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (CstDecl
                                           {
                                             names = arg0;
                                             typ = arg1;
                                             init_val = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_declaration_t.names"),
                    (Result.Error "Vhdl_ast.vhdl_declaration_t.typ"),
                    (Result.Error "Vhdl_ast.vhdl_declaration_t.init_val"))
            | _ -> Result.Error "Vhdl_ast.vhdl_declaration_t")) arg0
      | `List ((`String "SIGNAL_DECLARATION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("names",x)::xs ->
                      loop xs
                        (((function
                           | `List xs ->
                               map_bind (fun x  -> vhdl_name_t_of_yojson x)
                                 [] xs
                           | _ ->
                               Result.Error
                                 "Vhdl_ast.vhdl_declaration_t.names") x),
                          arg1, arg2)
                  | ("typ",x)::xs ->
                      loop xs
                        (arg0,
                          ((fun x  -> vhdl_subtype_indication_t_of_yojson x)
                             x), arg2)
                  | ("init_val",x)::xs ->
                      loop xs
                        (arg0, arg1, ((fun x  -> vhdl_expr_t_of_yojson x) x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (SigDecl
                                           {
                                             names = arg0;
                                             typ = arg1;
                                             init_val = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Error "Vhdl_ast.vhdl_declaration_t.names"),
                    (Result.Error "Vhdl_ast.vhdl_declaration_t.typ"),
                    (Result.Ok IsNull))
            | _ -> Result.Error "Vhdl_ast.vhdl_declaration_t")) arg0
      | `List ((`String "COMPONENT_DECLARATION")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2) as _state) =
                  match xs with
                  | ("name",x)::xs ->
                      loop xs
                        (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
                  | ("generics",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `List xs ->
                                map_bind (fun x  -> vhdl_port_t_of_yojson x)
                                  [] xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_declaration_t.generics") x),
                          arg2)
                  | ("ports",x)::xs ->
                      loop xs
                        (arg0, arg1,
                          ((function
                            | `List xs ->
                                map_bind (fun x  -> vhdl_port_t_of_yojson x)
                                  [] xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_declaration_t.ports") x))
                  | [] ->
                      arg2 >>=
                        ((fun arg2  ->
                            arg1 >>=
                              (fun arg1  ->
                                 arg0 >>=
                                   (fun arg0  ->
                                      Result.Ok
                                        (ComponentDecl
                                           {
                                             name = arg0;
                                             generics = arg1;
                                             ports = arg2
                                           })))))
                  | _::xs -> loop xs _state  in
                loop xs ((Result.Ok NoName), (Result.Ok []), (Result.Ok []))
            | _ -> Result.Error "Vhdl_ast.vhdl_declaration_t")) arg0
      | `List ((`String "SUBPROGRAM_BODY")::arg0::[]) ->
          ((function
            | `Assoc xs ->
                let rec loop xs ((arg0,arg1,arg2,arg3,arg4) as _state) =
                  match xs with
                  | ("name",x)::xs ->
                      loop xs
                        (((function
                           | `String x -> Result.Ok x
                           | _ ->
                               Result.Error
                                 "Vhdl_ast.vhdl_declaration_t.name") x),
                          arg1, arg2, arg3, arg4)
                  | ("kind",x)::xs ->
                      loop xs
                        (arg0,
                          ((function
                            | `String x -> Result.Ok x
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_declaration_t.kind") x),
                          arg2, arg3, arg4)
                  | ("spec",x)::xs ->
                      loop xs
                        (arg0, arg1,
                          ((function
                            | `Null -> Result.Ok None
                            | x ->
                                ((fun x  ->
                                    vhdl_subprogram_spec_t_of_yojson x) x)
                                  >>= ((fun x  -> Result.Ok (Some x)))) x),
                          arg3, arg4)
                  | ("decl_part",x)::xs ->
                      loop xs
                        (arg0, arg1, arg2,
                          ((function
                            | `List xs ->
                                map_bind
                                  (fun x  -> vhdl_declaration_t_of_yojson x)
                                  [] xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_declaration_t.decl_part") x),
                          arg4)
                  | ("stmts",x)::xs ->
                      loop xs
                        (arg0, arg1, arg2, arg3,
                          ((function
                            | `List xs ->
                                map_bind
                                  (fun x  ->
                                     vhdl_sequential_stmt_t_of_yojson x) []
                                  xs
                            | _ ->
                                Result.Error
                                  "Vhdl_ast.vhdl_declaration_t.stmts") x))
                  | [] ->
                      arg4 >>=
                        ((fun arg4  ->
                            arg3 >>=
                              (fun arg3  ->
                                 arg2 >>=
                                   (fun arg2  ->
                                      arg1 >>=
                                        (fun arg1  ->
                                           arg0 >>=
                                             (fun arg0  ->
                                                Result.Ok
                                                  (Subprogram
                                                     {
                                                       name = arg0;
                                                       kind = arg1;
                                                       spec = arg2;
                                                       decl_part = arg3;
                                                       stmts = arg4
                                                     })))))))
                  | _::xs -> loop xs _state  in
                loop xs
                  ((Result.Ok ""), (Result.Ok ""), (Result.Ok None),
                    (Result.Ok []), (Result.Ok []))
            | _ -> Result.Error "Vhdl_ast.vhdl_declaration_t")) arg0
      | _ -> Result.Error "Vhdl_ast.vhdl_declaration_t")
  [@ocaml.warning "-A"])

type vhdl_load_t =
  | Library of vhdl_name_t list [@name "LIBRARY_CLAUSE"][@default []]
  | Use of vhdl_name_t list [@name "USE_CLAUSE"][@default []]

(* Adapted. TODO: check indentation *)
let rec pp_vhdl_load_t :
  Format.formatter -> vhdl_load_t -> Ppx_deriving_runtime.unit =
  let __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Library a0 ->
            (Format.fprintf fmt "library ";
             ((fun x  ->
                 ignore
                   (List.fold_left
                      (fun sep  ->
                         fun x  ->
                           if sep then Format.fprintf fmt ".";
                           ((__0 ()) fmt) x;
                           true) false x))) a0;
             Format.fprintf fmt ":")
        | Use a0 ->
            (Format.fprintf fmt "use ";
             ((fun x  ->
                 ignore
                   (List.fold_left
                      (fun sep  ->
                         fun x  ->
                           if sep then Format.fprintf fmt ".";
                           ((__1 ()) fmt) x;
                           true) false x))) a0;
             Format.fprintf fmt ";"))
    [@ocaml.warning "-A"])

and show_vhdl_load_t : vhdl_load_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_load_t x

let rec (vhdl_load_t_to_yojson : vhdl_load_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | Library arg0 ->
          `List
            [`String "LIBRARY_CLAUSE";
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))) arg0]
      | Use arg0 ->
          `List
            [`String "USE_CLAUSE";
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x))) arg0])
  [@ocaml.warning "-A"])

and (vhdl_load_t_of_yojson :
      Yojson.Safe.json -> vhdl_load_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "LIBRARY_CLAUSE")::arg0::[]) ->
          ((function
            | `List xs -> map_bind (fun x  -> vhdl_name_t_of_yojson x) [] xs
            | _ -> Result.Error "Vhdl_ast.vhdl_load_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Library arg0)))
      | `List ((`String "USE_CLAUSE")::arg0::[]) ->
          ((function
            | `List xs -> map_bind (fun x  -> vhdl_name_t_of_yojson x) [] xs
            | _ -> Result.Error "Vhdl_ast.vhdl_load_t") arg0) >>=
            ((fun arg0  -> Result.Ok (Use arg0)))
      | _ -> Result.Error "Vhdl_ast.vhdl_load_t")
  [@ocaml.warning "-A"])

type vhdl_declarative_item_t =
  {
  use_clause: vhdl_load_t option [@default None];
  declaration: vhdl_declaration_t option [@default None];
  definition: vhdl_definition_t option [@default None]}[@@deriving
                                                         ((show
                                                             {
                                                               with_path =
                                                                 false
                                                             }),
                                                           (yojson
                                                              {
                                                                strict =
                                                                  false
                                                              }))]
let rec pp_vhdl_declarative_item_t :
  Format.formatter -> vhdl_declarative_item_t -> Ppx_deriving_runtime.unit =
  let __2 () = pp_vhdl_definition_t
  
  and __1 () = pp_vhdl_declaration_t
  
  and __0 () = pp_vhdl_load_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          (match x.use_clause with
          | None -> Format.fprintf fmt "";
          | Some e -> ((__0 ()) fmt) e);
          (match x.declaration with
          | None -> Format.fprintf fmt "";
          | Some e -> ((__1 ()) fmt) e);
          (match x.definition with
          | None -> Format.fprintf fmt "";
          | Some e -> ((__2 ()) fmt) e);)
    [@ocaml.warning "-A"])

and show_vhdl_declarative_item_t :
  vhdl_declarative_item_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_declarative_item_t x

let rec (vhdl_declarative_item_t_to_yojson :
          vhdl_declarative_item_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.definition = None
          then fields
          else
            ("definition",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_definition_t_to_yojson x)) x))
                 x.definition))
            :: fields
           in
        let fields =
          if x.declaration = None
          then fields
          else
            ("declaration",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_declaration_t_to_yojson x)) x))
                 x.declaration))
            :: fields
           in
        let fields =
          if x.use_clause = None
          then fields
          else
            ("use_clause",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_load_t_to_yojson x)) x))
                 x.use_clause))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_declarative_item_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_declarative_item_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2) as _state) =
            match xs with
            | ("use_clause",x)::xs ->
                loop xs
                  (((function
                     | `Null -> Result.Ok None
                     | x ->
                         ((fun x  -> vhdl_load_t_of_yojson x) x) >>=
                           ((fun x  -> Result.Ok (Some x)))) x), arg1, arg2)
            | ("declaration",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_declaration_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x), arg2)
            | ("definition",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_definition_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x))
            | [] ->
                arg2 >>=
                  ((fun arg2  ->
                      arg1 >>=
                        (fun arg1  ->
                           arg0 >>=
                             (fun arg0  ->
                                Result.Ok
                                  {
                                    use_clause = arg0;
                                    declaration = arg1;
                                    definition = arg2
                                  }))))
            | _::xs -> loop xs _state  in
          loop xs ((Result.Ok None), (Result.Ok None), (Result.Ok None))
      | _ -> Result.Error "Vhdl_ast.vhdl_declarative_item_t")
  [@ocaml.warning "-A"])

type vhdl_signal_condition_t =
  {
  expr: vhdl_expr_t list ;
  cond: vhdl_expr_t [@default IsNull]}

let rec pp_vhdl_signal_condition_t :
  Format.formatter -> vhdl_signal_condition_t -> Ppx_deriving_runtime.unit =
  let __1 () = pp_vhdl_expr_t
  
  and __0 () = pp_vhdl_expr_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          ((fun x  ->
              ignore
                (List.fold_left
                   (fun sep  ->
                      fun x  ->
                        if sep then Format.fprintf fmt ";@ ";
                        ((__0 ()) fmt) x;
                        true) false x))) x.expr;
          (match x.cond with
          | IsNull -> Format.fprintf fmt "";
          | _ -> Format.fprintf fmt "when ";
                 ((__1 ()) fmt) x.cond);)
    [@ocaml.warning "-A"])

and show_vhdl_signal_condition_t :
  vhdl_signal_condition_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_signal_condition_t x

let rec (vhdl_signal_condition_t_to_yojson :
          vhdl_signal_condition_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.cond = IsNull
          then fields
          else ("cond", (((fun x  -> vhdl_expr_t_to_yojson x)) x.cond)) ::
            fields
           in
        let fields =
          ("expr",
            ((fun x  ->
                `List (List.map (fun x  -> vhdl_expr_t_to_yojson x) x))
               x.expr))
          :: fields  in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_signal_condition_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_signal_condition_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1) as _state) =
            match xs with
            | ("expr",x)::xs ->
                loop xs
                  (((function
                     | `List xs ->
                         map_bind (fun x  -> vhdl_expr_t_of_yojson x) [] xs
                     | _ ->
                         Result.Error "Vhdl_ast.vhdl_signal_condition_t.expr")
                      x), arg1)
            | ("cond",x)::xs ->
                loop xs (arg0, ((fun x  -> vhdl_expr_t_of_yojson x) x))
            | [] ->
                arg1 >>=
                  ((fun arg1  ->
                      arg0 >>=
                        (fun arg0  -> Result.Ok { expr = arg0; cond = arg1 })))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Error "Vhdl_ast.vhdl_signal_condition_t.expr"),
              (Result.Ok IsNull))
      | _ -> Result.Error "Vhdl_ast.vhdl_signal_condition_t")
  [@ocaml.warning "-A"])

type vhdl_signal_selection_t =
  {
  expr: vhdl_expr_t ;
  when_sel: vhdl_expr_t list [@default []]}

(* TODO *)
let rec pp_vhdl_signal_selection_t :
  Format.formatter -> vhdl_signal_selection_t -> Ppx_deriving_runtime.unit =
  let __1 () = pp_vhdl_expr_t
  
  and __0 () = pp_vhdl_expr_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          Format.fprintf fmt "@[<2>{ ";
          ((Format.fprintf fmt "@[%s =@ " "expr";
            ((__0 ()) fmt) x.expr;
            Format.fprintf fmt "@]");
           Format.fprintf fmt ";@ ";
           Format.fprintf fmt "@[%s =@ " "when_sel";
           ((fun x  ->
               Format.fprintf fmt "@[<2>[";
               ignore
                 (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         if sep then Format.fprintf fmt ";@ ";
                         ((__1 ()) fmt) x;
                         true) false x);
               Format.fprintf fmt "@,]@]")) x.when_sel;
           Format.fprintf fmt "@]");
          Format.fprintf fmt "@ }@]")
    [@ocaml.warning "-A"])

and show_vhdl_signal_selection_t :
  vhdl_signal_selection_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_signal_selection_t x

let rec (vhdl_signal_selection_t_to_yojson :
          vhdl_signal_selection_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.when_sel = []
          then fields
          else
            ("when_sel",
              (((fun x  ->
                   `List (List.map (fun x  -> vhdl_expr_t_to_yojson x) x)))
                 x.when_sel))
            :: fields
           in
        let fields = ("expr", ((fun x  -> vhdl_expr_t_to_yojson x) x.expr))
          :: fields  in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_signal_selection_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_signal_selection_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1) as _state) =
            match xs with
            | ("expr",x)::xs ->
                loop xs (((fun x  -> vhdl_expr_t_of_yojson x) x), arg1)
            | ("when_sel",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `List xs ->
                          map_bind (fun x  -> vhdl_expr_t_of_yojson x) [] xs
                      | _ ->
                          Result.Error
                            "Vhdl_ast.vhdl_signal_selection_t.when_sel") x))
            | [] ->
                arg1 >>=
                  ((fun arg1  ->
                      arg0 >>=
                        (fun arg0  ->
                           Result.Ok { expr = arg0; when_sel = arg1 })))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Error "Vhdl_ast.vhdl_signal_selection_t.expr"),
              (Result.Ok []))
      | _ -> Result.Error "Vhdl_ast.vhdl_signal_selection_t")
  [@ocaml.warning "-A"])

type vhdl_conditional_signal_t =
  {
  postponed: bool [@default false];
  label: vhdl_name_t [@default NoName];
  lhs: vhdl_name_t ;
  rhs: vhdl_signal_condition_t list ;
  cond: vhdl_expr_t [@default IsNull];
  delay: vhdl_expr_t [@default IsNull]}

let rec pp_vhdl_conditional_signal_t :
  Format.formatter -> vhdl_conditional_signal_t -> Ppx_deriving_runtime.unit
  =
  let __4 () = pp_vhdl_expr_t
  
  and __3 () = pp_vhdl_expr_t
  
  and __2 () = pp_vhdl_signal_condition_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          (match x.label with
            | NoName -> Format.fprintf fmt "";
            | _ -> (((__0 ()) fmt) x.label;
                   Format.fprintf fmt ":@ ")
          );
          if (x.postponed) then Format.fprintf fmt "postponed@ ";
          ((__1 ()) fmt) x.lhs;
          Format.fprintf fmt " <= ";
          (match x.delay with
            | IsNull -> Format.fprintf fmt "";
            | _ -> ((__4 ()) fmt) x.delay;
                   Format.fprintf fmt " ");
          ((fun x  ->
             Format.fprintf fmt "@[";
             ignore
               (List.fold_left
                 (fun sep  ->
                   fun x  ->
                     if sep then Format.fprintf fmt "";
                      ((__2 ()) fmt) x;
                      Format.fprintf fmt ";";
                      true) false x);
          Format.fprintf fmt "@]")) x.rhs;
          (match x.cond with
            | IsNull -> Format.fprintf fmt "";
            | _ -> Format.fprintf fmt "when (";
                   ((__3 ()) fmt) x.cond;
                   Format.fprintf fmt ")"))
   [@ocaml.warning "-A"])

and show_vhdl_conditional_signal_t :
  vhdl_conditional_signal_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_conditional_signal_t x

let rec (vhdl_conditional_signal_t_to_yojson :
          vhdl_conditional_signal_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.delay = IsNull
          then fields
          else ("delay", (((fun x  -> vhdl_expr_t_to_yojson x)) x.delay)) ::
            fields
           in
        let fields =
          if x.cond = IsNull
          then fields
          else ("cond", (((fun x  -> vhdl_expr_t_to_yojson x)) x.cond)) ::
            fields
           in
        let fields =
          ("rhs",
            ((fun x  ->
                `List
                  (List.map (fun x  -> vhdl_signal_condition_t_to_yojson x) x))
               x.rhs))
          :: fields  in
        let fields = ("lhs", ((fun x  -> vhdl_name_t_to_yojson x) x.lhs)) ::
          fields  in
        let fields =
          if x.label = NoName
          then fields
          else ("label", (((fun x  -> vhdl_name_t_to_yojson x)) x.label)) ::
            fields
           in
        let fields =
          if x.postponed = false
          then fields
          else
            ("postponed",
              (((fun (x : Ppx_deriving_runtime.bool)  -> `Bool x))
                 x.postponed))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_conditional_signal_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_conditional_signal_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3,arg4,arg5) as _state) =
            match xs with
            | ("postponed",x)::xs ->
                loop xs
                  (((function
                     | `Bool x -> Result.Ok x
                     | _ ->
                         Result.Error
                           "Vhdl_ast.vhdl_conditional_signal_t.postponed") x),
                    arg1, arg2, arg3, arg4, arg5)
            | ("label",x)::xs ->
                loop xs
                  (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2, arg3,
                    arg4, arg5)
            | ("lhs",x)::xs ->
                loop xs
                  (arg0, arg1, ((fun x  -> vhdl_name_t_of_yojson x) x), arg3,
                    arg4, arg5)
            | ("rhs",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_signal_condition_t_of_yojson x)
                            [] xs
                      | _ ->
                          Result.Error
                            "Vhdl_ast.vhdl_conditional_signal_t.rhs") x),
                    arg4, arg5)
            | ("cond",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3,
                    ((fun x  -> vhdl_expr_t_of_yojson x) x), arg5)
            | ("delay",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3, arg4,
                    ((fun x  -> vhdl_expr_t_of_yojson x) x))
            | [] ->
                arg5 >>=
                  ((fun arg5  ->
                      arg4 >>=
                        (fun arg4  ->
                           arg3 >>=
                             (fun arg3  ->
                                arg2 >>=
                                  (fun arg2  ->
                                     arg1 >>=
                                       (fun arg1  ->
                                          arg0 >>=
                                            (fun arg0  ->
                                               Result.Ok
                                                 {
                                                   postponed = arg0;
                                                   label = arg1;
                                                   lhs = arg2;
                                                   rhs = arg3;
                                                   cond = arg4;
                                                   delay = arg5
                                                 })))))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok false), (Result.Ok NoName),
              (Result.Error "Vhdl_ast.vhdl_conditional_signal_t.lhs"),
              (Result.Error "Vhdl_ast.vhdl_conditional_signal_t.rhs"),
              (Result.Ok IsNull), (Result.Ok IsNull))
      | _ -> Result.Error "Vhdl_ast.vhdl_conditional_signal_t")
  [@ocaml.warning "-A"])

type vhdl_process_t =
  {
  id: vhdl_name_t [@default NoName];
  declarations: vhdl_declarative_item_t list
    [@key "PROCESS_DECLARATIVE_PART"][@default Some []];
  active_sigs: vhdl_name_t list [@default []];
  body: vhdl_sequential_stmt_t list
    [@key "PROCESS_STATEMENT_PART"][@default []]}

let rec pp_vhdl_process_t :
  Format.formatter -> vhdl_process_t -> Ppx_deriving_runtime.unit =
  let __3 () = pp_vhdl_sequential_stmt_t
  
  and __2 () = pp_vhdl_name_t
  
  and __1 () = pp_vhdl_declarative_item_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          Format.fprintf fmt "@[<v>@[<v 2>";
          (match x.id with
          | NoName -> Format.fprintf fmt "";
          | _ -> 
              ((__0 ()) fmt) x.id;
              Format.fprintf fmt ": ");
          Format.fprintf fmt "process ";
          (match x.active_sigs with
          | [] -> Format.fprintf fmt "";
          | _ -> Format.fprintf fmt "(";
                 ((fun x  ->
                    ignore
                      (List.fold_left
                         (fun sep  ->
                            fun x  ->
                              if sep then Format.fprintf fmt ",";
                              ((__2 ()) fmt) x;
                              true) false x))) x.active_sigs;
                 Format.fprintf fmt ")");
          Format.fprintf fmt "@;";
          ((fun x  ->
            ignore
            (List.fold_left
              (fun sep  ->
                fun x  ->
                  if sep then Format.fprintf fmt "@;";
                    ((__1 ()) fmt) x;
                    true) false x))) x.declarations;
          Format.fprintf fmt "@]@;@[<v 2>begin@;";
          ((fun x  ->
               ignore
                 (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         if sep then Format.fprintf fmt "@;";
                         ((__3 ()) fmt) x;
                         true) false x);)) x.body;
          Format.fprintf fmt "@]@;end process;@;";
          Format.fprintf fmt "@]";)
    [@ocaml.warning "-A"])

and show_vhdl_process_t : vhdl_process_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_process_t x

let rec (vhdl_process_t_to_yojson : vhdl_process_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.body = []
          then fields
          else
            ("PROCESS_STATEMENT_PART",
              (((fun x  ->
                   `List
                     (List.map (fun x  -> vhdl_sequential_stmt_t_to_yojson x)
                        x))) x.body))
            :: fields
           in
        let fields =
          if x.active_sigs = []
          then fields
          else
            ("active_sigs",
              (((fun x  ->
                   `List (List.map (fun x  -> vhdl_name_t_to_yojson x) x)))
                 x.active_sigs))
            :: fields
           in
        let fields =
          if x.declarations = []
          then fields
          else
            ("PROCESS_DECLARATIVE_PART",
              (((fun x  ->
                   `List
                     (List.map
                        (fun x  -> vhdl_declarative_item_t_to_yojson x) x)))
                 x.declarations))
            :: fields
           in
        let fields =
          if x.id = NoName
          then fields
          else ("id", (((fun x  -> vhdl_name_t_to_yojson x)) x.id)) :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_process_t_of_yojson :
      Yojson.Safe.json -> vhdl_process_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3) as _state) =
            match xs with
            | ("id",x)::xs ->
                loop xs
                  (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2, arg3)
            | ("PROCESS_DECLARATIVE_PART",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_declarative_item_t_of_yojson x)
                            [] xs
                      | _ ->
                          Result.Error "Vhdl_ast.vhdl_process_t.declarations")
                       x), arg2, arg3)
            | ("active_sigs",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `List xs ->
                          map_bind (fun x  -> vhdl_name_t_of_yojson x) [] xs
                      | _ ->
                          Result.Error "Vhdl_ast.vhdl_process_t.active_sigs")
                       x), arg3)
            | ("PROCESS_STATEMENT_PART",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_sequential_stmt_t_of_yojson x) []
                            xs
                      | _ -> Result.Error "Vhdl_ast.vhdl_process_t.body") x))
            | [] ->
                arg3 >>=
                  ((fun arg3  ->
                      arg2 >>=
                        (fun arg2  ->
                           arg1 >>=
                             (fun arg1  ->
                                arg0 >>=
                                  (fun arg0  ->
                                     Result.Ok
                                       {
                                         id = arg0;
                                         declarations = arg1;
                                         active_sigs = arg2;
                                         body = arg3
                                       })))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok NoName), (Result.Ok []), (Result.Ok []),
              (Result.Ok []))
      | _ -> Result.Error "Vhdl_ast.vhdl_process_t")
  [@ocaml.warning "-A"])

type vhdl_selected_signal_t =
  {
  postponed: bool [@default false];
  label: vhdl_name_t [@default NoName];
  lhs: vhdl_name_t ;
  sel: vhdl_expr_t ;
  branches: vhdl_signal_selection_t list [@default []];
  delay: vhdl_expr_t option }

(* TODO *)
let rec pp_vhdl_selected_signal_t :
  Format.formatter -> vhdl_selected_signal_t -> Ppx_deriving_runtime.unit =
  let __4 () = pp_vhdl_expr_t
  
  and __3 () = pp_vhdl_signal_selection_t
  
  and __2 () = pp_vhdl_expr_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          Format.fprintf fmt "@[<2>{ ";
          ((((((Format.fprintf fmt "@[%s =@ " "postponed";
                (Format.fprintf fmt "%B") x.postponed;
                Format.fprintf fmt "@]");
               Format.fprintf fmt ";@ ";
               Format.fprintf fmt "@[%s =@ " "label";
               ((__0 ()) fmt) x.label;
               Format.fprintf fmt "@]");
              Format.fprintf fmt ";@ ";
              Format.fprintf fmt "@[%s =@ " "lhs";
              ((__1 ()) fmt) x.lhs;
              Format.fprintf fmt "@]");
             Format.fprintf fmt ";@ ";
             Format.fprintf fmt "@[%s =@ " "sel";
             ((__2 ()) fmt) x.sel;
             Format.fprintf fmt "@]");
            Format.fprintf fmt ";@ ";
            Format.fprintf fmt "@[%s =@ " "branches";
            ((fun x  ->
                Format.fprintf fmt "@[<2>[";
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt ";@ ";
                          ((__3 ()) fmt) x;
                          true) false x);
                Format.fprintf fmt "@,]@]")) x.branches;
            Format.fprintf fmt "@]");
           Format.fprintf fmt ";@ ";
           Format.fprintf fmt "@[%s =@ " "delay";
           ((function
             | None  -> Format.pp_print_string fmt "None"
             | Some x ->
                 (Format.pp_print_string fmt "(Some ";
                  ((__4 ()) fmt) x;
                  Format.pp_print_string fmt ")"))) x.delay;
           Format.fprintf fmt "@]");
          Format.fprintf fmt "@ }@]")
    [@ocaml.warning "-A"])

and show_vhdl_selected_signal_t :
  vhdl_selected_signal_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_selected_signal_t x

let rec (vhdl_selected_signal_t_to_yojson :
          vhdl_selected_signal_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          ("delay",
            ((function
              | None  -> `Null
              | Some x -> ((fun x  -> vhdl_expr_t_to_yojson x)) x) x.delay))
          :: fields  in
        let fields =
          if x.branches = []
          then fields
          else
            ("branches",
              (((fun x  ->
                   `List
                     (List.map
                        (fun x  -> vhdl_signal_selection_t_to_yojson x) x)))
                 x.branches))
            :: fields
           in
        let fields = ("sel", ((fun x  -> vhdl_expr_t_to_yojson x) x.sel)) ::
          fields  in
        let fields = ("lhs", ((fun x  -> vhdl_name_t_to_yojson x) x.lhs)) ::
          fields  in
        let fields =
          if x.label = NoName
          then fields
          else ("label", (((fun x  -> vhdl_name_t_to_yojson x)) x.label)) ::
            fields
           in
        let fields =
          if x.postponed = false
          then fields
          else
            ("postponed",
              (((fun (x : Ppx_deriving_runtime.bool)  -> `Bool x))
                 x.postponed))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_selected_signal_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_selected_signal_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3,arg4,arg5) as _state) =
            match xs with
            | ("postponed",x)::xs ->
                loop xs
                  (((function
                     | `Bool x -> Result.Ok x
                     | _ ->
                         Result.Error
                           "Vhdl_ast.vhdl_selected_signal_t.postponed") x),
                    arg1, arg2, arg3, arg4, arg5)
            | ("label",x)::xs ->
                loop xs
                  (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2, arg3,
                    arg4, arg5)
            | ("lhs",x)::xs ->
                loop xs
                  (arg0, arg1, ((fun x  -> vhdl_name_t_of_yojson x) x), arg3,
                    arg4, arg5)
            | ("sel",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, ((fun x  -> vhdl_expr_t_of_yojson x) x),
                    arg4, arg5)
            | ("branches",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_signal_selection_t_of_yojson x)
                            [] xs
                      | _ ->
                          Result.Error
                            "Vhdl_ast.vhdl_selected_signal_t.branches") x),
                    arg5)
            | ("delay",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3, arg4,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_expr_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x))
            | [] ->
                arg5 >>=
                  ((fun arg5  ->
                      arg4 >>=
                        (fun arg4  ->
                           arg3 >>=
                             (fun arg3  ->
                                arg2 >>=
                                  (fun arg2  ->
                                     arg1 >>=
                                       (fun arg1  ->
                                          arg0 >>=
                                            (fun arg0  ->
                                               Result.Ok
                                                 {
                                                   postponed = arg0;
                                                   label = arg1;
                                                   lhs = arg2;
                                                   sel = arg3;
                                                   branches = arg4;
                                                   delay = arg5
                                                 })))))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok false), (Result.Ok NoName),
              (Result.Error "Vhdl_ast.vhdl_selected_signal_t.lhs"),
              (Result.Error "Vhdl_ast.vhdl_selected_signal_t.sel"),
              (Result.Ok []),
              (Result.Error "Vhdl_ast.vhdl_selected_signal_t.delay"))
      | _ -> Result.Error "Vhdl_ast.vhdl_selected_signal_t")
  [@ocaml.warning "-A"])

type vhdl_component_instantiation_t =
  {
  name: vhdl_name_t ;
  inst_unit: vhdl_name_t ;
  archi_name: vhdl_name_t option [@default None];
  generic_map: vhdl_assoc_element_t list [@default []];
  port_map: vhdl_assoc_element_t list [@default []]}[@@deriving
                                                      ((show
                                                          { with_path = false
                                                          }),
                                                        (yojson
                                                           { strict = false }))]

let rec pp_vhdl_component_instantiation_t :
  Format.formatter ->
    vhdl_component_instantiation_t -> Ppx_deriving_runtime.unit
  =
  let __4 () = pp_vhdl_assoc_element_t
  
  and __3 () = pp_vhdl_assoc_element_t
  
  and __2 () = pp_vhdl_name_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          Format.fprintf fmt "@[<v 2>";
          ((__0 ()) fmt) x.name;
          Format.fprintf fmt " : ";
          ((__1 ()) fmt) x.inst_unit;
          ((function
             | None  -> Format.pp_print_string fmt ""
             | Some x ->
                 (Format.fprintf fmt "(";
                 ((__2 ()) fmt) x;
                 Format.fprintf fmt ")@;"))) x.archi_name;
          (match x.generic_map with
          | [] -> Format.fprintf fmt "";
          | _ ->
            (Format.fprintf fmt "@[<v 2>generic map (";
            ((fun x  ->
            ignore
            (List.fold_left
               (fun sep  ->
                 fun x  ->
                   if sep then Format.fprintf fmt ",@,";
                   ((__3 ()) fmt) x;
                   true) false x))) x.generic_map;
            Format.fprintf fmt ")@]@;"));
          (match x.port_map with
          | [] -> Format.fprintf fmt ";";
          | _ ->
            (Format.fprintf fmt "@[<v 2>port map (";
            ((fun x  ->
            ignore
            (List.fold_left
               (fun sep  ->
                 fun x  ->
                   if sep then Format.fprintf fmt ",@,";
                   ((__4 ()) fmt) x;
                   true) false x))) x.port_map;
            Format.fprintf fmt ")@];"));
          Format.fprintf fmt "@]")
    [@ocaml.warning "-A"])

and show_vhdl_component_instantiation_t :
  vhdl_component_instantiation_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_component_instantiation_t x

let rec (vhdl_component_instantiation_t_to_yojson :
          vhdl_component_instantiation_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.port_map = []
          then fields
          else
            ("port_map",
              (((fun x  ->
                   `List
                     (List.map (fun x  -> vhdl_assoc_element_t_to_yojson x) x)))
                 x.port_map))
            :: fields
           in
        let fields =
          if x.generic_map = []
          then fields
          else
            ("generic_map",
              (((fun x  ->
                   `List
                     (List.map (fun x  -> vhdl_assoc_element_t_to_yojson x) x)))
                 x.generic_map))
            :: fields
           in
        let fields =
          if x.archi_name = None
          then fields
          else
            ("archi_name",
              (((function
                 | None  -> `Null
                 | Some x -> ((fun x  -> vhdl_name_t_to_yojson x)) x))
                 x.archi_name))
            :: fields
           in
        let fields =
          ("inst_unit", ((fun x  -> vhdl_name_t_to_yojson x) x.inst_unit)) ::
          fields  in
        let fields = ("name", ((fun x  -> vhdl_name_t_to_yojson x) x.name))
          :: fields  in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_component_instantiation_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_component_instantiation_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3,arg4) as _state) =
            match xs with
            | ("name",x)::xs ->
                loop xs
                  (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2, arg3,
                    arg4)
            | ("inst_unit",x)::xs ->
                loop xs
                  (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2, arg3,
                    arg4)
            | ("archi_name",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `Null -> Result.Ok None
                      | x ->
                          ((fun x  -> vhdl_name_t_of_yojson x) x) >>=
                            ((fun x  -> Result.Ok (Some x)))) x), arg3, arg4)
            | ("generic_map",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_assoc_element_t_of_yojson x) []
                            xs
                      | _ ->
                          Result.Error
                            "Vhdl_ast.vhdl_component_instantiation_t.generic_map")
                       x), arg4)
            | ("port_map",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_assoc_element_t_of_yojson x) []
                            xs
                      | _ ->
                          Result.Error
                            "Vhdl_ast.vhdl_component_instantiation_t.port_map")
                       x))
            | [] ->
                arg4 >>=
                  ((fun arg4  ->
                      arg3 >>=
                        (fun arg3  ->
                           arg2 >>=
                             (fun arg2  ->
                                arg1 >>=
                                  (fun arg1  ->
                                     arg0 >>=
                                       (fun arg0  ->
                                          Result.Ok
                                            {
                                              name = arg0;
                                              inst_unit = arg1;
                                              archi_name = arg2;
                                              generic_map = arg3;
                                              port_map = arg4
                                            }))))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Error "Vhdl_ast.vhdl_component_instantiation_t.name"),
              (Result.Error
                 "Vhdl_ast.vhdl_component_instantiation_t.inst_unit"),
              (Result.Ok None), (Result.Ok []), (Result.Ok []))
      | _ -> Result.Error "Vhdl_ast.vhdl_component_instantiation_t")
  [@ocaml.warning "-A"])

type vhdl_concurrent_stmt_t =
  | SigAssign of vhdl_conditional_signal_t
  [@name "CONDITIONAL_SIGNAL_ASSIGNMENT"]
  | Process of vhdl_process_t [@name "PROCESS_STATEMENT"]
  | SelectedSig of vhdl_selected_signal_t
  [@name "SELECTED_SIGNAL_ASSIGNMENT"]
  | ComponentInst of vhdl_component_instantiation_t
  [@name "COMPONENT_INSTANTIATION_STATEMENT"]

let rec pp_vhdl_concurrent_stmt_t :
  Format.formatter -> vhdl_concurrent_stmt_t -> Ppx_deriving_runtime.unit =
  let __3 () = pp_vhdl_component_instantiation_t
  
  and __2 () = pp_vhdl_selected_signal_t
  
  and __1 () = pp_vhdl_process_t
  
  and __0 () = pp_vhdl_conditional_signal_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | SigAssign a0 ->
             ((__0 ()) fmt) a0;
        | Process a0 ->
             ((__1 ()) fmt) a0;
        | SelectedSig a0 ->
             ((__2 ()) fmt) a0;
        | ComponentInst a0 ->
             ((__3 ()) fmt) a0;
    )
    [@ocaml.warning "-A"])

and show_vhdl_concurrent_stmt_t :
  vhdl_concurrent_stmt_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_concurrent_stmt_t x

let rec (vhdl_concurrent_stmt_t_to_yojson :
          vhdl_concurrent_stmt_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | SigAssign arg0 ->
          `List
            [`String "CONDITIONAL_SIGNAL_ASSIGNMENT";
            ((fun x  -> vhdl_conditional_signal_t_to_yojson x)) arg0]
      | Process arg0 ->
          `List
            [`String "PROCESS_STATEMENT";
            ((fun x  -> vhdl_process_t_to_yojson x)) arg0]
      | SelectedSig arg0 ->
          `List
            [`String "SELECTED_SIGNAL_ASSIGNMENT";
            ((fun x  -> vhdl_selected_signal_t_to_yojson x)) arg0]
      | ComponentInst arg0 ->
          `List
            [`String "COMPONENT_INSTANTIATION_STATEMENT";
            ((fun x  -> vhdl_component_instantiation_t_to_yojson x)) arg0])
  [@ocaml.warning "-A"])

and (vhdl_concurrent_stmt_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_concurrent_stmt_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "CONDITIONAL_SIGNAL_ASSIGNMENT")::arg0::[]) ->
          ((fun x  -> vhdl_conditional_signal_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (SigAssign arg0)))
      | `List ((`String "PROCESS_STATEMENT")::arg0::[]) ->
          ((fun x  -> vhdl_process_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (Process arg0)))
      | `List ((`String "SELECTED_SIGNAL_ASSIGNMENT")::arg0::[]) ->
          ((fun x  -> vhdl_selected_signal_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (SelectedSig arg0)))
      | `List ((`String "COMPONENT_INSTANTIATION_STATEMENT")::arg0::[]) ->
          ((fun x  -> vhdl_component_instantiation_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (ComponentInst arg0)))
      | _ -> Result.Error "Vhdl_ast.vhdl_concurrent_stmt_t")
  [@ocaml.warning "-A"])

type vhdl_entity_t =
  {
  name: vhdl_name_t [@default NoName];
  generics: vhdl_port_t list [@default []];
  ports: vhdl_port_t list [@default []];
  declaration: vhdl_declarative_item_t list
    [@key "ENTITY_DECLARATIVE_PART"][@default []];
  stmts: vhdl_concurrent_stmt_t list
    [@key "ENTITY_STATEMENT_PART"][@default []]}

let rec pp_vhdl_entity_t :
  Format.formatter -> vhdl_entity_t -> Ppx_deriving_runtime.unit =
  let __4 () = pp_vhdl_concurrent_stmt_t
  
  and __3 () = pp_vhdl_declarative_item_t
  
  and __2 () = pp_vhdl_port_t
  
  and __1 () = pp_vhdl_port_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          ((__0 ()) fmt) x.name;
          Format.fprintf fmt " is@;";
          (match x.generics with
          | [] -> Format.fprintf fmt "";
          | _ -> 
              Format.fprintf fmt "generic (@[<v>";
              ((fun x  ->
                ignore
                (List.fold_left
                  (fun sep  ->
                    fun x  ->
                      if sep then Format.fprintf fmt ";@;";
                        ((__1 ()) fmt) x;
                        true) false x))) x.generics;
              Format.fprintf fmt "@]);");
          (match x.ports with
          | [] -> Format.fprintf fmt "";
          | _ -> 
              Format.fprintf fmt "port (@[<v>";
              ((fun x  ->
                 ignore
                   (List.fold_left
                      (fun sep  ->
                         fun x  ->
                           if sep then Format.fprintf fmt ";@;";
                           ((__2 ()) fmt) x;
                           true) false x))) x.ports;
              Format.fprintf fmt "@]);");
          (match x.declaration with
          | [] -> Format.fprintf fmt "";
          | _ ->
              Format.fprintf fmt "@;";
              ((fun x  ->
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt ";@;";
                          ((__3 ()) fmt) x;
                          true) false x))) x.declaration;
              Format.fprintf fmt ";");
          (match x.stmts with
          | [] -> Format.fprintf fmt "";
          | _ ->
              Format.fprintf fmt "@;@[<v 2>begin@;";
              ((fun x  ->
                ignore
                  (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         if sep then Format.fprintf fmt ";@;";
                         ((__4 ()) fmt) x;
                         true) false x))) x.stmts;
              Format.fprintf fmt ";@]");)
    [@ocaml.warning "-A"])

and show_vhdl_entity_t : vhdl_entity_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_entity_t x

let rec (vhdl_entity_t_to_yojson : vhdl_entity_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.stmts = []
          then fields
          else
            ("ENTITY_STATEMENT_PART",
              (((fun x  ->
                   `List
                     (List.map (fun x  -> vhdl_concurrent_stmt_t_to_yojson x)
                        x))) x.stmts))
            :: fields
           in
        let fields =
          if x.declaration = []
          then fields
          else
            ("ENTITY_DECLARATIVE_PART",
              (((fun x  ->
                   `List
                     (List.map
                        (fun x  -> vhdl_declarative_item_t_to_yojson x) x)))
                 x.declaration))
            :: fields
           in
        let fields =
          if x.ports = []
          then fields
          else
            ("ports",
              (((fun x  ->
                   `List (List.map (fun x  -> vhdl_port_t_to_yojson x) x)))
                 x.ports))
            :: fields
           in
        let fields =
          if x.generics = []
          then fields
          else
            ("generics",
              (((fun x  ->
                   `List (List.map (fun x  -> vhdl_port_t_to_yojson x) x)))
                 x.generics))
            :: fields
           in
        let fields =
          if x.name = NoName
          then fields
          else ("name", (((fun x  -> vhdl_name_t_to_yojson x)) x.name)) ::
            fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_entity_t_of_yojson :
      Yojson.Safe.json -> vhdl_entity_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3,arg4) as _state) =
            match xs with
            | ("name",x)::xs ->
                loop xs
                  (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2, arg3,
                    arg4)
            | ("generics",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `List xs ->
                          map_bind (fun x  -> vhdl_port_t_of_yojson x) [] xs
                      | _ -> Result.Error "Vhdl_ast.vhdl_entity_t.generics")
                       x), arg2, arg3, arg4)
            | ("ports",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `List xs ->
                          map_bind (fun x  -> vhdl_port_t_of_yojson x) [] xs
                      | _ -> Result.Error "Vhdl_ast.vhdl_entity_t.ports") x),
                    arg3, arg4)
            | ("ENTITY_DECLARATIVE_PART",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_declarative_item_t_of_yojson x)
                            [] xs
                      | _ ->
                          Result.Error "Vhdl_ast.vhdl_entity_t.declaration")
                       x), arg4)
            | ("ENTITY_STATEMENT_PART",x)::xs ->
                loop xs
                  (arg0, arg1, arg2, arg3,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_concurrent_stmt_t_of_yojson x) []
                            xs
                      | _ -> Result.Error "Vhdl_ast.vhdl_entity_t.stmts") x))
            | [] ->
                arg4 >>=
                  ((fun arg4  ->
                      arg3 >>=
                        (fun arg3  ->
                           arg2 >>=
                             (fun arg2  ->
                                arg1 >>=
                                  (fun arg1  ->
                                     arg0 >>=
                                       (fun arg0  ->
                                          Result.Ok
                                            {
                                              name = arg0;
                                              generics = arg1;
                                              ports = arg2;
                                              declaration = arg3;
                                              stmts = arg4
                                            }))))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok NoName), (Result.Ok []), (Result.Ok []),
              (Result.Ok []), (Result.Ok []))
      | _ -> Result.Error "Vhdl_ast.vhdl_entity_t")
  [@ocaml.warning "-A"])

type vhdl_package_t =
  {
  name: vhdl_name_t [@default NoName];
  shared_defs: vhdl_definition_t list [@default []];
  shared_decls: vhdl_declaration_t list [@default []]}

let rec pp_vhdl_package_t :
  Format.formatter -> vhdl_package_t -> Ppx_deriving_runtime.unit =
  let __2 () = pp_vhdl_declaration_t

  and __1 () = pp_vhdl_definition_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          ((__0 ()) fmt) x.name;
          Format.fprintf fmt " is";
          ((fun x  ->
             ignore
               (List.fold_left
                  (fun sep  ->
                     fun x  ->
                       Format.fprintf fmt "@;";
                       if sep then Format.fprintf fmt "";
                       ((__1 ()) fmt) x;
                       Format.fprintf fmt ";";
                       true) false x))) x.shared_defs;
          ((fun x  ->
               ignore
                 (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         Format.fprintf fmt "@;";
                         if sep then Format.fprintf fmt "";
                         ((__2 ()) fmt) x;
                         Format.fprintf fmt ";";
                         true) false x))) x.shared_decls;)
    [@ocaml.warning "-A"])

and show_vhdl_package_t : vhdl_package_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_package_t x

let rec (vhdl_package_t_to_yojson : vhdl_package_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.shared_decls = []
          then fields
          else
            ("shared_decls",
              (((fun x  ->
                   `List
                     (List.map (fun x  -> vhdl_declaration_t_to_yojson x) x)))
                 x.shared_decls))
            :: fields
           in
        let fields =
          if x.shared_defs = []
          then fields
          else
            ("shared_defs",
              (((fun x  ->
                   `List
                     (List.map (fun x  -> vhdl_definition_t_to_yojson x) x)))
                 x.shared_defs))
            :: fields
           in
        let fields =
          if x.name = NoName
          then fields
          else ("name", (((fun x  -> vhdl_name_t_to_yojson x)) x.name)) ::
            fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_package_t_of_yojson :
      Yojson.Safe.json -> vhdl_package_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2) as _state) =
            match xs with
            | ("name",x)::xs ->
                loop xs (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2)
            | ("shared_defs",x)::xs ->
                loop xs
                  (arg0,
                    ((function
                      | `List xs ->
                          map_bind (fun x  -> vhdl_definition_t_of_yojson x)
                            [] xs
                      | _ ->
                          Result.Error "Vhdl_ast.vhdl_package_t.shared_defs")
                       x), arg2)
            | ("shared_decls",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `List xs ->
                          map_bind (fun x  -> vhdl_declaration_t_of_yojson x)
                            [] xs
                      | _ ->
                          Result.Error "Vhdl_ast.vhdl_package_t.shared_decls")
                       x))
            | [] ->
                arg2 >>=
                  ((fun arg2  ->
                      arg1 >>=
                        (fun arg1  ->
                           arg0 >>=
                             (fun arg0  ->
                                Result.Ok
                                  {
                                    name = arg0;
                                    shared_defs = arg1;
                                    shared_decls = arg2
                                  }))))
            | _::xs -> loop xs _state  in
          loop xs ((Result.Ok NoName), (Result.Ok []), (Result.Ok []))
      | _ -> Result.Error "Vhdl_ast.vhdl_package_t")
  [@ocaml.warning "-A"])

type vhdl_architecture_t =
  {
  name: vhdl_name_t [@default NoName];
  entity: vhdl_name_t [@default NoName];
  declarations: vhdl_declarative_item_t list
    [@key "ARCHITECTURE_DECLARATIVE_PART"][@default []];
  body: vhdl_concurrent_stmt_t list
    [@key "ARCHITECTURE_STATEMENT_PART"][@default []]}

let rec pp_vhdl_architecture_t :
  Format.formatter -> vhdl_architecture_t -> Ppx_deriving_runtime.unit =
  let __3 () = pp_vhdl_concurrent_stmt_t
  
  and __2 () = pp_vhdl_declarative_item_t
  
  and __1 () = pp_vhdl_name_t
  
  and __0 () = pp_vhdl_name_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
          ((__0 ()) fmt) x.name;
          Format.fprintf fmt " of ";
          ((__1 ()) fmt) x.entity;
          Format.fprintf fmt " is@;";
            ((fun x  ->
             ignore
               (List.fold_left
                  (fun sep  ->
                     fun x  ->
                       if sep then Format.fprintf fmt "@;";
                       ((__2 ()) fmt) x;
                       Format.fprintf fmt ";";
                       true) false x))) x.declarations;
          Format.fprintf fmt "@;";
          (match x.body with
            | [] -> Format.fprintf fmt "";
            | _ -> Format.fprintf fmt "@[<v 2>begin@;";
               ((fun x  ->
               ignore
                 (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         if sep then Format.fprintf fmt "@;";
                         ((__3 ()) fmt) x;
                         true) false x))) x.body;
           Format.fprintf fmt "@]@;end;"))
    [@ocaml.warning "-A"])

and show_vhdl_architecture_t :
  vhdl_architecture_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_architecture_t x

let rec (vhdl_architecture_t_to_yojson :
          vhdl_architecture_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.body = []
          then fields
          else
            ("ARCHITECTURE_STATEMENT_PART",
              (((fun x  ->
                   `List
                     (List.map (fun x  -> vhdl_concurrent_stmt_t_to_yojson x)
                        x))) x.body))
            :: fields
           in
        let fields =
          if x.declarations = []
          then fields
          else
            ("ARCHITECTURE_DECLARATIVE_PART",
              (((fun x  ->
                   `List
                     (List.map
                        (fun x  -> vhdl_declarative_item_t_to_yojson x) x)))
                 x.declarations))
            :: fields
           in
        let fields =
          if x.entity = NoName
          then fields
          else ("entity", (((fun x  -> vhdl_name_t_to_yojson x)) x.entity))
            :: fields
           in
        let fields =
          if x.name = NoName
          then fields
          else ("name", (((fun x  -> vhdl_name_t_to_yojson x)) x.name)) ::
            fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_architecture_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_architecture_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1,arg2,arg3) as _state) =
            match xs with
            | ("name",x)::xs ->
                loop xs
                  (((fun x  -> vhdl_name_t_of_yojson x) x), arg1, arg2, arg3)
            | ("entity",x)::xs ->
                loop xs
                  (arg0, ((fun x  -> vhdl_name_t_of_yojson x) x), arg2, arg3)
            | ("ARCHITECTURE_DECLARATIVE_PART",x)::xs ->
                loop xs
                  (arg0, arg1,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_declarative_item_t_of_yojson x)
                            [] xs
                      | _ ->
                          Result.Error
                            "Vhdl_ast.vhdl_architecture_t.declarations") x),
                    arg3)
            | ("ARCHITECTURE_STATEMENT_PART",x)::xs ->
                loop xs
                  (arg0, arg1, arg2,
                    ((function
                      | `List xs ->
                          map_bind
                            (fun x  -> vhdl_concurrent_stmt_t_of_yojson x) []
                            xs
                      | _ -> Result.Error "Vhdl_ast.vhdl_architecture_t.body")
                       x))
            | [] ->
                arg3 >>=
                  ((fun arg3  ->
                      arg2 >>=
                        (fun arg2  ->
                           arg1 >>=
                             (fun arg1  ->
                                arg0 >>=
                                  (fun arg0  ->
                                     Result.Ok
                                       {
                                         name = arg0;
                                         entity = arg1;
                                         declarations = arg2;
                                         body = arg3
                                       })))))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok NoName), (Result.Ok NoName), (Result.Ok []),
              (Result.Ok []))
      | _ -> Result.Error "Vhdl_ast.vhdl_architecture_t")
  [@ocaml.warning "-A"])

type vhdl_configuration_t = unit

let rec (pp_vhdl_configuration_t :
          Format.formatter ->
            vhdl_configuration_t -> Ppx_deriving_runtime.unit)
  =
  ((let open! Ppx_deriving_runtime in
      fun fmt  -> fun ()  -> Format.pp_print_string fmt "()")
  [@ocaml.warning "-A"])

and show_vhdl_configuration_t :
  vhdl_configuration_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_configuration_t x

let rec (vhdl_configuration_t_to_yojson :
          vhdl_configuration_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun (x : Ppx_deriving_runtime.unit)  -> `Null)
  [@ocaml.warning "-A"])

and (vhdl_configuration_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_configuration_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Null -> Result.Ok ()
      | _ -> Result.Error "Vhdl_ast.vhdl_configuration_t")
  [@ocaml.warning "-A"])

type vhdl_library_unit_t =
  | Package of vhdl_package_t [@name "PACKAGE_DECLARATION"]
  | Entities of vhdl_entity_t [@name "ENTITY_DECLARATION"]
  | Architecture of vhdl_architecture_t [@name "ARCHITECTURE_BODY"]
  | Configuration of vhdl_configuration_t [@name "CONFIGURATION_DECLARATION"]

let rec pp_vhdl_library_unit_t :
  Format.formatter -> vhdl_library_unit_t -> Ppx_deriving_runtime.unit =
  let __3 () = pp_vhdl_configuration_t
  
  and __2 () = pp_vhdl_architecture_t
  
  and __1 () = pp_vhdl_entity_t
  
  and __0 () = pp_vhdl_package_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        function
        | Package a0 ->
            (Format.fprintf fmt "@[<v 2>package ";
             ((__0 ()) fmt) a0;
             Format.fprintf fmt "@.end;@]")
        | Entities a0 ->
            (Format.fprintf fmt "@[<v 2>entity ";
             ((__1 ()) fmt) a0;
             Format.fprintf fmt "@.end;@]")
        | Architecture a0 ->
            (Format.fprintf fmt "@[<v 2>architecture ";
             ((__2 ()) fmt) a0;
             Format.fprintf fmt "@.end;@]")
        | Configuration a0 ->
            (Format.fprintf fmt "@[<v 2>configuration ";
             ((__3 ()) fmt) a0;
             Format.fprintf fmt "@.end;@]"))
    [@ocaml.warning "-A"])

and show_vhdl_library_unit_t :
  vhdl_library_unit_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_library_unit_t x

let rec (vhdl_library_unit_t_to_yojson :
          vhdl_library_unit_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | Package arg0 ->
          `List
            [`String "PACKAGE_DECLARATION";
            ((fun x  -> vhdl_package_t_to_yojson x)) arg0]
      | Entities arg0 ->
          `List
            [`String "ENTITY_DECLARATION";
            ((fun x  -> vhdl_entity_t_to_yojson x)) arg0]
      | Architecture arg0 ->
          `List
            [`String "ARCHITECTURE_BODY";
            ((fun x  -> vhdl_architecture_t_to_yojson x)) arg0]
      | Configuration arg0 ->
          `List
            [`String "CONFIGURATION_DECLARATION";
            ((fun x  -> vhdl_configuration_t_to_yojson x)) arg0])
  [@ocaml.warning "-A"])

and (vhdl_library_unit_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_library_unit_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `List ((`String "PACKAGE_DECLARATION")::arg0::[]) ->
          ((fun x  -> vhdl_package_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (Package arg0)))
      | `List ((`String "ENTITY_DECLARATION")::arg0::[]) ->
          ((fun x  -> vhdl_entity_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (Entities arg0)))
      | `List ((`String "ARCHITECTURE_BODY")::arg0::[]) ->
          ((fun x  -> vhdl_architecture_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (Architecture arg0)))
      | `List ((`String "CONFIGURATION_DECLARATION")::arg0::[]) ->
          ((fun x  -> vhdl_configuration_t_of_yojson x) arg0) >>=
            ((fun arg0  -> Result.Ok (Configuration arg0)))
      | _ -> Result.Error "Vhdl_ast.vhdl_library_unit_t")
  [@ocaml.warning "-A"])

type vhdl_design_unit_t =
  {
  contexts: vhdl_load_t list [@default []];
  library: vhdl_library_unit_t }

let rec pp_vhdl_design_unit_t :
  Format.formatter -> vhdl_design_unit_t -> Ppx_deriving_runtime.unit =
  let __1 () = pp_vhdl_library_unit_t
  
  and __0 () = pp_vhdl_load_t
   in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
           (match x.contexts with
           | [] -> Format.fprintf fmt "";
           | _ -> 
            ((fun x  ->
                ignore
                  (List.fold_left
                     (fun sep  ->
                        fun x  ->
                          if sep then Format.fprintf fmt "@.";
                          ((__0 ()) fmt) x;
                          true) false x);
                )) x.contexts;
           Format.fprintf fmt "@.";);
           ((__1 ()) fmt) x.library;)
    [@ocaml.warning "-A"])

and show_vhdl_design_unit_t :
  vhdl_design_unit_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_design_unit_t x

let rec (vhdl_design_unit_t_to_yojson :
          vhdl_design_unit_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          ("library",
            ((fun x  -> vhdl_library_unit_t_to_yojson x) x.library))
          :: fields  in
        let fields =
          if x.contexts = []
          then fields
          else
            ("contexts",
              (((fun x  ->
                   `List (List.map (fun x  -> vhdl_load_t_to_yojson x) x)))
                 x.contexts))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_design_unit_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_design_unit_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1) as _state) =
            match xs with
            | ("contexts",x)::xs ->
                loop xs
                  (((function
                     | `List xs ->
                         map_bind (fun x  -> vhdl_load_t_of_yojson x) [] xs
                     | _ ->
                         Result.Error "Vhdl_ast.vhdl_design_unit_t.contexts")
                      x), arg1)
            | ("library",x)::xs ->
                loop xs
                  (arg0, ((fun x  -> vhdl_library_unit_t_of_yojson x) x))
            | [] ->
                arg1 >>=
                  ((fun arg1  ->
                      arg0 >>=
                        (fun arg0  ->
                           Result.Ok { contexts = arg0; library = arg1 })))
            | _::xs -> loop xs _state  in
          loop xs
            ((Result.Ok []),
              (Result.Error "Vhdl_ast.vhdl_design_unit_t.library"))
      | _ -> Result.Error "Vhdl_ast.vhdl_design_unit_t")
  [@ocaml.warning "-A"])

type vhdl_design_file_t =
  {
  design_units: vhdl_design_unit_t list [@default []]}

let rec pp_vhdl_design_file_t :
  Format.formatter -> vhdl_design_file_t -> Ppx_deriving_runtime.unit =
  let __0 () = pp_vhdl_design_unit_t  in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
           ((fun x  ->
               ignore
                 (List.fold_left
                    (fun sep  ->
                       fun x  ->
                         if sep then Format.fprintf fmt "@.";
                         ((__0 ()) fmt) x;
                         true) false x);
             )) x.design_units)
    [@ocaml.warning "-A"])

and show_vhdl_design_file_t :
  vhdl_design_file_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_design_file_t x

let rec (vhdl_design_file_t_to_yojson :
          vhdl_design_file_t -> Yojson.Safe.json)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.design_units = []
          then fields
          else
            ("design_units",
              (((fun x  ->
                   `List
                     (List.map (fun x  -> vhdl_design_unit_t_to_yojson x) x)))
                 x.design_units))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_design_file_t_of_yojson :
      Yojson.Safe.json ->
        vhdl_design_file_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs (arg0 as _state) =
            match xs with
            | ("design_units",x)::xs ->
                loop xs
                  ((function
                    | `List xs ->
                        map_bind (fun x  -> vhdl_design_unit_t_of_yojson x)
                          [] xs
                    | _ ->
                        Result.Error
                          "Vhdl_ast.vhdl_design_file_t.design_units") x)
            | [] ->
                arg0 >>= ((fun arg0  -> Result.Ok { design_units = arg0 }))
            | _::xs -> loop xs _state  in
          loop xs (Result.Ok [])
      | _ -> Result.Error "Vhdl_ast.vhdl_design_file_t")
  [@ocaml.warning "-A"])

type vhdl_file_t =
  {
  design_file: vhdl_design_file_t
    [@default { design_units = [] }][@key "DESIGN_FILE"]}

let rec pp_vhdl_file_t :
  Format.formatter -> vhdl_file_t -> Ppx_deriving_runtime.unit =
  let __0 () = pp_vhdl_design_file_t  in
  ((let open! Ppx_deriving_runtime in
      fun fmt  ->
        fun x  ->
           ((__0 ()) fmt) x.design_file;
   )
    [@ocaml.warning "-A"])

and show_vhdl_file_t : vhdl_file_t -> Ppx_deriving_runtime.string =
  fun x  -> Format.asprintf "%a" pp_vhdl_file_t x

let rec (vhdl_file_t_to_yojson : vhdl_file_t -> Yojson.Safe.json) =
  ((let open! Ppx_deriving_yojson_runtime in
      fun x  ->
        let fields = []  in
        let fields =
          if x.design_file = { design_units = [] }
          then fields
          else
            ("DESIGN_FILE",
              (((fun x  -> vhdl_design_file_t_to_yojson x)) x.design_file))
            :: fields
           in
        `Assoc fields)
  [@ocaml.warning "-A"])

and (vhdl_file_t_of_yojson :
      Yojson.Safe.json -> vhdl_file_t Ppx_deriving_yojson_runtime.error_or)
  =
  ((let open! Ppx_deriving_yojson_runtime in
      function
      | `Assoc xs ->
          let rec loop xs (arg0 as _state) =
            match xs with
            | ("DESIGN_FILE",x)::xs ->
                loop xs ((fun x  -> vhdl_design_file_t_of_yojson x) x)
            | [] ->
                arg0 >>= ((fun arg0  -> Result.Ok { design_file = arg0 }))
            | _::xs -> Result.Error "Vhdl_ast.vhdl_file_t"  in
          loop xs (Result.Ok { design_units = [] })
      | _ -> Result.Error "Vhdl_ast.vhdl_file_t")
  [@ocaml.warning "-A"])
