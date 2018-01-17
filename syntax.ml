type t = (* MinCamlの構文を表現するデータ型 (caml2html: syntax_t) *)
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

(* type command = 
	| CExp of expr
*)
let rec print_id = print_string

let rec print_expr e =
	match e with
	| Int i ->
		Format.eprintf "Int %d@." i
	| _ -> 
		Format.eprintf "Mahi @."
(*
	| Bool b ->
		(print_string "Bool ";
		print_string (string_of_bool b);)
	| Var x ->
		(print_string "Var ";
		print_id x;)
	| Add (e1,e2) ->
		(print_string "Add ";
		print_newline();
		print_expr e1;
		print_newline();
		print_expr e2;)
	| Eq (e1,e2) ->
                (print_string "Eq ";
                print_newline();
                print_expr e1;
                print_newline();
                print_expr e2;)
*)

let rec print_command p =
	match p with
	| Add (e1,e2) ->
		(Format.eprintf "Add @.";
		print_command e1;
		print_command e2;)
	| _ -> Format.eprintf "hoge @."

