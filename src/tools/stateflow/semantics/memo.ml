open Basetypes
  
type ('a, 'b) t = Memo : ('a, 'b) Hashtbl.t -> ('a, 'b) t;;

let create () = Memo (Hashtbl.create 97);;

let reset (Memo hashf) =
  begin
    Hashtbl.reset hashf
  end

let fold (Memo hashf) f e =
  begin
    Hashtbl.fold f hashf e
  end;;

let apply (Memo hashf) f =
  fun x ->
    try
      Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "lookup 1@.");
      Hashtbl.find hashf x
    with Not_found ->
      let res = f x in
      begin
	Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "hashing 1@.");
	Hashtbl.add hashf x res;
	res
      end;;

let apply2 (Memo hashf) f =
  fun x y ->
    try
      Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "lookup 2@.");
      Hashtbl.find hashf (x, y)
    with Not_found ->
      let res = f x y in
      begin
	Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "hashing 2@.");
	Hashtbl.add hashf (x, y) res;
	res
      end;;

let apply3 (Memo hashf) f =
  fun x y z ->
    try
      Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "lookup 3@.");
      Hashtbl.find hashf (x, y, z)
    with Not_found ->
      let res = f x y z in
      begin
	Log.report ~level:sf_level (fun fmt -> Format.fprintf fmt "hashing 3@.");
	Hashtbl.add hashf (x, y, z) res;
	res
      end;;
