(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2019   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Ttypes
open Symbols
module W = Warnings

type coercion_kind =
  | CRCleaf of lsymbol
  | CRCcomp of coercion_kind * coercion_kind

type coercion = {
  crc_kind : coercion_kind;
  crc_src_ts : tysymbol;
  crc_src_tl : ty list;
  crc_tar_ts : tysymbol;
  crc_tar_tl : ty list;
}

let ty_of ls =
  match (ls.ls_args, ls.ls_value) with
  | [ { ty_node = Tyapp (ty1, _) } ], Some { ty_node = Tyapp (ty2, _) } ->
      (ty1.ts_ident.id_str, ty2.ts_ident.id_str)
  | _ -> assert false

let to_string crc =
  let rec aux = function
    | CRCleaf ls ->
        let s1, s2 = ty_of ls in
        [ (ls.ls_name.id_str, s1, s2) ]
    | CRCcomp (k1, k2) -> aux k1 @ aux k2
  in
  aux crc.crc_kind

type t = coercion Mts.t Mts.t
(* invariant: transitively closed *)

let empty = Mts.empty

let create_crc ls =
  match (ls.ls_args, ls.ls_value) with
  | [ { ty_node = Tyapp (ts1, tl1) } ], Some { ty_node = Tyapp (ts2, tl2) }
    when not (ts_equal ts1 ts2) ->
      {
        crc_kind = CRCleaf ls;
        crc_src_ts = ts1;
        crc_src_tl = tl1;
        crc_tar_ts = ts2;
        crc_tar_tl = tl2;
      }
  | _ ->
      W.error ~loc:ls.ls_name.id_loc (W.Invalid_coercion_type ls.ls_name.id_str)

let mem t ts1 ts2 =
  try
    let m = Mts.find ts1 t in
    Mts.mem ts2 m
  with Not_found -> false

let rec may_match ty1 ty2 =
  match (ty1.ty_node, ty2.ty_node) with
  | Tyapp (ts1, tl1), Tyapp (ts2, tl2) ->
      if not (ts_equal ts1 ts2) then raise Not_found;
      List.iter2 may_match tl1 tl2
  | _ -> ()

let find_crc t ts1 ts2 = Mts.find ts1 t |> Mts.find ts2

let find t ty1 ty2 =
  match (ty1, ty2) with
  | { ty_node = Tyapp (ts1, tl1) }, { ty_node = Tyapp (ts2, tl2) } ->
      let rec ls_list_of acc = function
        | CRCleaf ls -> ls :: acc
        | CRCcomp (k1, k2) -> ls_list_of (ls_list_of acc k2) k1
      in
      let crc = find_crc t ts1 ts2 in
      List.iter2 may_match tl1 crc.crc_src_tl;
      List.iter2 may_match tl2 crc.crc_tar_tl;
      ls_list_of [] crc.crc_kind
  | _ -> raise Not_found

(* replace an old coercion by a new one, or fail *)
let rec ck_eq ck_old ck_new =
  match (ck_old, ck_new) with
  | CRCleaf ls_old, CRCleaf ls_new when ls_equal ls_old ls_new -> ()
  | CRCcomp (old_ck1, old_ck2), CRCcomp (new_ck1, new_ck2) ->
      ck_eq old_ck1 new_ck1;
      ck_eq old_ck2 new_ck2
  | _ -> raise Not_found

(* replace an old coercion by a new one, or fail *)
let replace ~loc c_old c_new _m1 m =
  try
    ck_eq c_old.crc_kind c_new.crc_kind;
    m
  with Not_found ->
    W.error ~loc (W.Coercion_already_defined (to_string c_old))

(* add a new coercion c, without making the transitive closure *)
let insert ~loc crc m =
  let put crc m1 m2 =
    Mts.add crc.crc_src_ts (Mts.add crc.crc_tar_ts crc m1) m2
  in
  (if mem m crc.crc_tar_ts crc.crc_src_ts then
     let crc = find_crc m crc.crc_tar_ts crc.crc_src_ts in
     W.error ~loc (W.Coercion_cycle (to_string crc)));
  let m1 = try Mts.find crc.crc_src_ts m with Not_found -> Mts.empty in
  if Mts.mem crc.crc_tar_ts m1 then
    replace ~loc (Mts.find crc.crc_tar_ts m1) crc m1 m
  else put crc m1 m

let compose crc1 crc2 =
  {
    crc_kind = CRCcomp (crc1.crc_kind, crc2.crc_kind);
    crc_src_ts = crc1.crc_src_ts;
    crc_src_tl = crc1.crc_src_tl;
    crc_tar_ts = crc2.crc_tar_ts;
    crc_tar_tl = crc2.crc_tar_tl;
  }

(* add a new coercion crc, and make the transitive closure *)
let add_crc ~loc crcmap crc =
  let close_right c1 _ty c2 macc = insert ~loc (compose c1 c2) macc in
  let close_left_right _ty1 m1 macc =
    if Mts.mem crc.crc_src_ts m1 then
      let c1 = Mts.find crc.crc_src_ts m1 in
      let m2 = try Mts.find crc.crc_tar_ts macc with Not_found -> Mts.empty in
      Mts.fold (close_right c1) (Mts.add crc.crc_tar_ts crc m2) macc
    else macc
  in
  let crcmap_uc1 = insert ~loc crc crcmap in
  let crcmap_uc2 =
    let m1 =
      try Mts.find crc.crc_tar_ts crcmap_uc1 with Not_found -> Mts.empty
    in
    Mts.fold (close_right crc) m1 crcmap_uc1
  in
  Mts.fold close_left_right crcmap_uc2 crcmap_uc2

let add crcmap ls = add_crc ~loc:ls.ls_name.id_loc crcmap (create_crc ls)

let union crcmap1 crcmap2 =
  let add _ty2 crc crcmap =
    match crc.crc_kind with
    | CRCleaf ls -> add_crc ~loc:ls.ls_name.id_loc crcmap crc
    | CRCcomp _ -> crcmap
  in
  Mts.fold (fun _ty1 m1 crcmap -> Mts.fold add m1 crcmap) crcmap2 crcmap1
