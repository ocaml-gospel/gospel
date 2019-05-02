open Identifier
open Utils

(** type variables *)

type tvsymbol = {
    tv_name : ident;
}

let tv_equal: tvsymbol -> tvsymbol -> bool = (==)

module Tvar = struct
  type t = tvsymbol
  let equal = tv_equal
  let compare = Pervasives.compare
  let hash tv = tv.tv_name.id_tag
end

module Htv = Hashtbl.Make(Tvar)
module Mtv = Map.Make(Tvar)

let create_tv id = { tv_name = id }

let tv_of_string =
  let hs = Hstr.create (1 lsl 5) in fun s ->
  try Hstr.find hs s with Not_found ->
  let tv = create_tv (fresh_id s) in
  Hstr.add hs s tv; tv

(** types *)

type ty = {
    ty_node : ty_node;
}

and ty_node =
  | Tyvar of tvsymbol
  | Tyapp of tysymbol * ty list

and tysymbol = {
  ts_ident : ident;
  ts_arity : int;
}

let ts_equal : tysymbol -> tysymbol -> bool = (==)
(* We cannot use == here because the definition of ty_of_pty in
   typing.ml. For instance, with the current definition of ty_of_pty,
   the following definition will not type check:
   (*@ function f (x:int):int = x *) *)
let ty_equal : ty       -> ty       -> bool = (=)

let ts id ar = {ts_ident = id; ts_arity = ar}
let ts_ident ts = ts.ts_ident
let ts_arity ts = ts.ts_arity

let fresh_ty_var s = {ty_node = Tyvar {tv_name = fresh_id s}}

let ty_of_var tv = {ty_node = Tyvar tv}


(* let ty_app ts tl = {ty_node = Tyapp (ts,tl)} *)

(** smart constructors *)

exception BadTypeArity of tysymbol * int

let ty_app ts tyl =
  if ts_arity ts = List.length tyl then
    {ty_node = Tyapp (ts,tyl)}
  else raise (BadTypeArity (ts,List.length tyl))

(** type matching *)

let rec ty_inst mtv ty = match ty.ty_node with
  | Tyvar n -> begin try Mtv.find n mtv with Not_found -> ty end
  | Tyapp (ts,tyl) ->
     { ty_node = Tyapp (ts, List.map (ty_inst mtv) tyl) }

(* if possible returns the map that allows to instanciate ty1 with ty2 *)
let rec ty_match mtv ty1 ty2 =
  let set = function
    | None -> Some ty2
    | Some ty1 as r when ty_equal ty1 ty2 -> r
    | _ -> raise Exit in
  match ty1.ty_node, ty2.ty_node with
  | Tyvar tv1, _ -> Mtv.update tv1 set mtv
  | Tyapp (ts1,tyl1), Tyapp (ts2,tyl2) when ts_equal ts1 ts2 ->
     List.fold_left2 ty_match mtv tyl1 tyl2
  | _ -> raise Exit

exception TypeMismatch of ty * ty

let ty_match mtv ty1 ty2 =
  try ty_match mtv ty1 ty2 with
  | Exit -> raise (TypeMismatch (ty_inst mtv ty1, ty2))

let ty_equal_check ty1 ty2 =
  if not (ty_equal ty1 ty2) then raise (TypeMismatch (ty1,ty2))

(** Built-in symbols *)

let ts_integer = ts (fresh_id "integer") 0
let ts_int     = ts (fresh_id "int"    ) 0
let ts_bool    = ts (fresh_id "bool"   ) 0
let ts_float   = ts (fresh_id "float"  ) 0
let ts_char    = ts (fresh_id "char"   ) 0
let ts_string  = ts (fresh_id "string" ) 0
let ts_option  = ts (fresh_id "option" ) 1

let ts_tuple =
  let ts_tuples = Hint.create 17 in
  begin fun n ->
  try Hint.find ts_tuples n with | Not_found ->
     let ts_id = fresh_id ("tuple" ^ string_of_int n) in
     let ts = ts ts_id n in
     Hint.add ts_tuples n ts;
     ts
  end

let ts_arrow =
  let id = fresh_id "->" in
  ts id 2

let is_ts_tuple ts = ts_tuple (ts_arity ts) == ts
let is_ts_arrow ts = ts_arrow == ts

let ty_integer = ty_app ts_integer []
let ty_int     = ty_app ts_int     []
let ty_bool    = ty_app ts_bool    []
let ty_float   = ty_app ts_float   []
let ty_char    = ty_app ts_char    []
let ty_string  = ty_app ts_string  []
let ty_option  = ty_app ts_option  [fresh_ty_var "a"]

(** Pretty printers *)

open Opprintast

let print_tv fmt tv =
  pp fmt (if tv.tv_name.id_str = "_" then "%a" else "'%a")
    print_ident tv.tv_name

let print_ts fmt ts =
  pp fmt "@[%a@]" print_ident (ts_ident ts)

let rec print_ty fmt {ty_node} = print_ty_node fmt ty_node

and print_arrow_ty fmt = list ~sep:" -> " print_ty fmt

and print_ty_node fmt = function
  | Tyvar v -> pp fmt "%a" print_tv v
  | Tyapp (ts,[]) -> print_ts fmt ts
  | Tyapp (ts,tys) when is_ts_arrow ts ->
     print_arrow_ty fmt tys
  | Tyapp (ts,[ty]) ->
     pp fmt "%a %a" print_ty ty print_ts ts
  | Tyapp (ts,tyl) when is_ts_tuple ts ->
     pp fmt "(%a)" (list ~sep:"," print_ty) tyl
  | Tyapp (ts,tyl) ->
     pp fmt "(%a) %a" (list ~sep:"," print_ty) tyl print_ts ts

(* register exceptions *)

let () =
  Location.register_error_of_exn (function
      | TypeMismatch (ty1,ty2) ->
         Some (Location.errorf "Type mismatch between %a and %a" print_ty ty1 print_ty ty2)
      | BadTypeArity (ts,i) ->
         Some (Location.errorf
                 "Type %a expects %d arguments as opposed to %d"
                 print_ts ts (ts_arity ts) i)
      | _ -> None)
