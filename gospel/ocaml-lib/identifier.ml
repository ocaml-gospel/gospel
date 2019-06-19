open Utils

(** Attributes  *)

type attr = string

module Sattr = Set.Make(String)

(** Pre identifiers - not unique - used by parsetree and untyped AST *)

type preid = {
  pid_str : string;
  pid_ats : attr list;
  pid_loc : Location.t;
}

module Preid = struct
  type t = preid
  let compare = Pervasives.compare
  let equal = (=)
  let hash = (Hashtbl.hash : preid -> int)
end

module Hpid  = Hashtbl.Make(Preid)

let create_pid s a l = {pid_str = s; pid_ats = a; pid_loc = l}

let pid_of_string s = create_pid s [] Location.none

let pid_add_lab pid l = { pid with pid_ats =  pid.pid_ats @ l }

(** Identifiers *)

type ident = {
  id_str : string;
  id_ats : Sattr.t;
  id_loc : Location.t;
  id_tag : int;
}

module Ident = struct
  type t = ident
  let compare = Pervasives.compare
  let equal = (=)
  let hash = (Hashtbl.hash : ident -> int)
end

module Hid = Hashtbl.Make(Ident)
module Mid = Map.Make(Ident)

let create_id =
  let r = ref 0 in
  fun s a l -> {
      id_str = s;
      id_ats = a;
      id_loc = l;
      id_tag = (incr r; !r)
    }

let id_register pid =
  create_id pid.pid_str (Sattr.of_list pid.pid_ats) pid.pid_loc

let fresh_id s = create_id s Sattr.empty Location.none

let fresh_id_with_loc s l = create_id s Sattr.empty l

let id_add_loc l id = {id with id_loc = l}

let id_add_lab id l = { id with id_ats = Sattr.add l id.id_ats }

(* utils *)

let prefix s = "prefix " ^ s
let infix  s = "infix "  ^ s
let mixfix s = "mixfix " ^ s

let is_somefix f s =
  let sl = String.split_on_char ' ' s in
  List.length sl > 1 && List.hd sl = f

let is_prefix = is_somefix "prefix"
let is_infix  = is_somefix "infix"
let is_mixfix = is_somefix "mixfix"

(* hard-coded ids *)

let eq    = fresh_id (infix "=")
let neq   = fresh_id (infix "<>")
let lt    = fresh_id (infix "<")
let le    = fresh_id (infix "<=")
let gt    = fresh_id (infix ">")
let ge    = fresh_id (infix ">=")
let impl  = fresh_id (infix "->")
let plus  = fresh_id (infix "+")
let minus = fresh_id (infix "-")
let mult  = fresh_id (infix "*")
let none  = fresh_id ("None")
let some  = fresh_id ("Some")
let nil   = fresh_id ("[]")
let cons  = fresh_id (infix "::")

(* pretty-printer *)

type context = {
    current : int Hstr.t;
    output  : string Hint.t;
  }

let default_ctx = {
    current = Hstr.create (1 lsl 8);
    output  = Hint.create (1 lsl 8)
}

let current ctx s =
  try let x = Hstr.find ctx.current s in
      Hstr.replace ctx.current s (x + 1); x with
  | Not_found -> Hstr.add ctx.current s 0; 0

let str_of_id ctx id =
  try Hint.find ctx.output id.id_tag with
  | Not_found ->
     let x = current ctx id.id_str in
     let str = if x = 0 then id.id_str else
                 id.id_str ^ "#" ^ string_of_int x in
     Hint.replace ctx.output id.id_tag str; str

open Opprintast

let print_attr fmt a = pp fmt "[@%s]" a
let print_attrs = list ~sep:" " print_attr

let print_pid fmt pid = pp fmt "%s@ %a" pid.pid_str
                            print_attrs pid.pid_ats

let print_ident fmt id = pp fmt "%s%a" (str_of_id default_ctx id)
                           print_attrs (Sattr.elements id.id_ats)
