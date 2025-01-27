module W = Warnings
open Ppxlib
open Dterm
open Ttypes
open Uast
open Tast
open Tmodule
open Symbols
open Identifier

type whereami =
  | Axiom
  | Checks
  | Consumes
  | Ensures
  | Function_or_predicate
  | Invariant
  | Modifies
  | Raises
  | Requires
  | Variant

type 'a env = {
  whereami : whereami option;
  global : signature_item Mid.t;
  local : 'a Mstr.t;
  coercions : Coercion.t;
  namespace : namespace;
}

let make whereami global local coercions namespace =
  { whereami; global; local; coercions; namespace }

let make_new global coercions namespace =
  make None global Mstr.empty coercions namespace

let env_of_module_uc muc =
  let global, coercions, namespace =
    (get_known_ids muc, get_coercions muc, get_top_import muc)
  in
  make_new global coercions namespace

let get_coercions env = env.coercions
let get_namespace env = env.namespace
let get_local env = env.local
let mem_local env k = Mstr.mem k env.local
let set_whereami env w = { env with whereami = Some w }
let set_namespace env namespace = { env with namespace }

let whereami env =
  match env.whereami with Some w -> w | None -> invalid_arg "whereami"

let find_local_opt env k = Mstr.find_opt k env.local

let find_local_exn ~loc env k =
  try Mstr.find k env.local
  with Not_found -> W.error ~loc (W.Unbound_variable k)

(* lookup global env *)

let find_constructors env ts =
  match (Mid.find ts.ts_ident env.global).sig_desc with
  | Sig_type (_, tdl, _) -> (
      match (List.find (fun td -> td.td_ts = ts) tdl).td_kind with
      | Pty_record { rd_cs; rd_ldl } ->
          (rd_cs, List.map (fun ld -> ld.ld_field) rd_ldl)
      | _ -> assert false)
  | _ -> assert false

let string_list_of_qualid q =
  let rec fold_q acc = function
    | Qpreid pid -> pid.pid_str :: acc
    | Qdot (q, pid) -> fold_q (pid.pid_str :: acc) q
  in
  fold_q [] q

exception Ns_not_found of location * string

let rec q_loc = function Qpreid pid -> pid.pid_loc | Qdot (q, _) -> q_loc q

let ns_find ~loc f ns sl =
  match sl with
  | s :: _ :: _ when not (ns_exists_ns ns s) ->
      raise (Ns_not_found (loc, s))
      (* this will be caught to try to find file s *)
  | _ -> ( try f ns sl with Not_found -> W.error ~loc (W.Symbol_not_found sl))

let find_ts ~loc env = ns_find ~loc ns_find_ts env.namespace
let find_ls ~loc env = ns_find ~loc ns_find_ls env.namespace
let find_fd ~loc env = ns_find ~loc ns_find_fd env.namespace
let find_xs ~loc env = ns_find ~loc ns_find_xs env.namespace
let find_ns ~loc env = ns_find ~loc ns_find_ns env.namespace

let find_q (f : loc:Location.t -> 'a) ns q =
  let ln = string_list_of_qualid q in
  f ~loc:(q_loc q) ns ln

let find_q_ts env = find_q find_ts env
let find_q_ls env = find_q find_ls env
let find_q_fd env = find_q find_fd env
let find_q_xs env = find_q find_xs env
let find_q_ns env = find_q find_ns env

let dty_env_of_vsymbol_env env =
  let f vs = dty_of_ty vs.vs_ty in
  { env with local = Mstr.map f env.local }

let union_local env local =
  let choose_snd _ _ x = Some x in
  { env with local = Mstr.union choose_snd env.local local }

let add_local_var_no_duplicate env k v =
  let add = function
    | None -> Some v
    | Some _ -> W.error ~loc:v.vs_name.id_loc (W.Duplicated_variable k)
  in
  { env with local = Mstr.update k add env.local }

let add_local_var env k x = { env with local = Mstr.add k x env.local }

(* adds a list of local bindings, checking for duplicates in the list, but
   shadowing previous bindings *)
let add_local_vars env xs =
  let add acc (pid, dty) =
    if Mstr.mem pid.Preid.pid_str acc then
      W.error ~loc:pid.pid_loc (W.Duplicated_variable pid.pid_str)
    else Mstr.add pid.pid_str dty acc
  in
  let xs = List.fold_left add Mstr.empty xs in
  union_local env xs

let add_ns_fd env s ls =
  let ns = ns_add_fd ~allow_duplicate:true env.namespace s ls in
  set_namespace env ns
