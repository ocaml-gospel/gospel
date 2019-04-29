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

let create_id =
  let r = ref 0 in
  fun s a l -> {
      id_str = s;
      id_ats = a;
      id_loc = l;
      id_tag = (incr r; !r)
    }

let id_register = let r = ref 0 in fun pid -> {
  id_str = pid.pid_str;
  id_ats = Sattr.of_list pid.pid_ats;
  id_loc = pid.pid_loc;
  id_tag = (incr r; !r);
}

let fresh_id s = create_id s Sattr.empty Location.none

let id_add_loc l id = {id with id_loc = l}

let id_add_lab id l = { id with id_ats = Sattr.add l id.id_ats }

(* pretty-printer *)

open Opprintast

let print_attr fmt a = pp fmt "[@%s]" a
let print_attrs = list ~sep:" " print_attr

let print_pid fmt pid = pp fmt "%s@ %a" pid.pid_str
                            print_attrs pid.pid_ats

let print_ident fmt id = pp fmt "%s%a" id.id_str
                           print_attrs (Sattr.elements id.id_ats)
