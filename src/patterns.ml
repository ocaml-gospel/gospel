open Symbols
open Ttypes
open Tterm
open Tterm_helper

module Pmatrix = struct
  type t = { rows : int; cols : int; mat : pattern list list }

  let mk ?cols mat =
    let cols =
      Option.value cols
        ~default:(match mat with [] -> 0 | e :: _ -> List.length e)
    in
    { rows = List.length mat; cols; mat }

  let from_pat pat =
    if pat = [] then invalid_arg "pmatrix.from_pat";
    let mat =
      List.map
        (fun p -> match p.p_node with Por (p1, p2) -> [ p1; p2 ] | _ -> [ p ])
        pat
      |> List.flatten
      |> List.map (fun e -> [ e ])
    in
    { mat; rows = List.length mat; cols = 1 }

  let empty = { mat = []; rows = 0; cols = 0 }

  let add_col pmat col_elt =
    if pmat = empty then { rows = 1; cols = 1; mat = [ [ col_elt ] ] }
    else
      {
        rows = pmat.rows;
        cols = pmat.cols + 1;
        mat = List.map (fun e -> col_elt :: e) pmat.mat;
      }

  let add_line_hd pmat l =
    let cols = List.length l in
    assert (pmat = empty || pmat.cols = cols);
    { mat = l :: pmat.mat; cols; rows = pmat.rows + 1 }

  let encap_row ck ak pmat =
    let rec split acc i l =
      match l with
      | [] -> (List.rev acc, [])
      | e :: k ->
          if i = ak then (List.rev acc, l) else split (e :: acc) (i + 1) k
    in
    match pmat.mat with
    | [ e ] ->
        let args, l = split [] 0 e in
        let hd = mk_pattern (Papp (ck, args)) (Option.get ck.ls_value) in
        { rows = 1; cols = pmat.cols - ak + 1; mat = [ hd :: l ] }
    | _ -> assert false

  let from_lines l = List.fold_left add_line_hd empty (List.rev l)
  let mk_Pwild = List.map (fun ty -> mk_pattern Pwild ty)
end

module Sigma = struct
  module M = Mls

  let mk =
    List.fold_left
      (fun acc e ->
        match e with
        | { p_node = Papp (c, _); _ } :: _ -> M.add c c.ls_args acc
        | _ -> acc)
      M.empty

  let ts_of_ty = function Tyvar _ -> assert false | Tyapp (ts, _) -> ts

  let get_constructors { ty_node } : lsymbol list =
    let ts = ts_of_ty ty_node in
    let td =
      try Hts.find Tmodule.type_declarations ts with Not_found -> assert false
    in
    match td.td_kind with
    | Pty_variant cdl -> List.map (fun Tast.{ cd_cs; _ } -> cd_cs) cdl
    | _ -> assert false

  let is_full ty ens =
    get_constructors ty |> List.for_all (fun ls -> M.mem ls ens)

  let is_empty = M.is_empty
  let exists = M.exists
  let iter = M.iter

  let other_one ty ens =
    get_constructors ty |> List.find (fun ls -> not (M.mem ls ens))
end

let rec mk_default p =
  let rec mk_line nb_cols = function
    | [] -> []
    | h :: cols -> (
        match h.p_node with
        | Papp _ -> []
        | Pvar _ | Pwild -> [ cols ]
        | Por (t1, t2) ->
            let mat = Pmatrix.from_lines [ t1 :: cols; t2 :: cols ] in
            let r = mk_default mat in
            Pmatrix.(r.mat)
        | Pas (p, _) -> mk_line nb_cols (p :: cols))
  in
  let mat = List.map (mk_line p.cols) p.mat |> List.flatten in
  Pmatrix.mk mat ~cols:(p.cols - 1)

let rec mk_spec constr a p =
  let rec mk_si = function
    | [] -> []
    | h :: cols -> (
        match h.p_node with
        | Papp (c', ti) when ls_equal c' constr -> [ ti @ cols ]
        | Papp _ -> []
        | Pvar _ | Pwild ->
            let ll = Pmatrix.mk_Pwild a in
            [ ll @ cols ]
        | Por (t1, t2) ->
            let mat = Pmatrix.from_lines [ t1 :: cols; t2 :: cols ] in
            let m = mk_spec constr a mat in
            Pmatrix.(m.mat)
        | Pas (p, _) -> mk_si (p :: cols))
  in
  let mat = List.map mk_si p.mat |> List.flatten in
  Pmatrix.mk mat ~cols:(p.cols + List.length a - 1)

let rec usefulness typ_cols (pmat : Pmatrix.t) (qvec : pattern list) : bool =
  assert (List.length qvec = pmat.cols);
  if pmat.cols = 0 then pmat.rows = 0
  else
    match qvec with
    | [] -> assert false
    | { p_node = Papp (c, pl); _ } :: ll ->
        let typ_next = c.ls_args in
        let typ_nl =
          typ_next @ match typ_cols with [] -> assert false | _ :: t -> t
        in
        usefulness typ_nl
          (mk_spec c (List.map (fun p -> p.p_ty) pl) pmat)
          (pl @ ll)
    | { p_node = Pvar _ | Pwild; _ } :: ll ->
        let thd, tl =
          match typ_cols with [] -> assert false | h :: t -> (h, t)
        in
        let sigma = Sigma.mk pmat.mat in
        if Sigma.is_full thd sigma then
          Sigma.exists
            (fun ck tyl ->
              let q = Pmatrix.mk_Pwild ck.ls_args @ ll in
              usefulness (ck.ls_args @ tl) (mk_spec ck tyl pmat) q)
            sigma
        else usefulness tl (mk_default pmat) ll
    | { p_node = Por (v1, v2); _ } :: l ->
        usefulness typ_cols pmat (v1 :: l) || usefulness typ_cols pmat (v2 :: l)
    | { p_node = Pas (_, _); _ } :: _ -> assert false

let rec ui (typ_cols : ty list) (pmat : Pmatrix.t) =
  if pmat.cols = 0 then if pmat.rows = 0 then Some Pmatrix.empty else None
  else
    let thd, tl = match typ_cols with [] -> assert false | h :: t -> (h, t) in
    let sigma = Sigma.mk pmat.mat in
    if Sigma.is_full thd sigma then
      let exception Brk of Pmatrix.t in
      try
        Sigma.iter
          (fun ck ck_args ->
            match ui (ck_args @ tl) (mk_spec ck ck_args pmat) with
            | None -> ()
            | Some pm ->
                let pm = Pmatrix.encap_row ck (List.length ck_args) pm in
                raise (Brk pm))
          sigma;
        None
      with Brk pm -> Some pm
    else
      match ui tl (mk_default pmat) with
      | None -> None
      | Some pm ->
          if Sigma.is_empty sigma then
            Some (Pmatrix.add_col pm (mk_pattern Pwild thd))
          else
            let cc = Sigma.other_one thd sigma in
            Some
              (Pmatrix.add_col pm
                 (mk_pattern
                    (Papp (cc, Pmatrix.mk_Pwild cc.ls_args))
                    (Option.get cc.ls_value)))

open Fmt

let exhaustivite typ pat =
  let pmat = Pmatrix.from_pat pat in
  let q = Pmatrix.mk_Pwild [ (List.hd pat |> fun p -> p.p_ty) ] in
  if usefulness [ typ ] pmat q |> not then true
  else
    match ui [ typ ] pmat with
    | None -> assert false
    | Some p ->
        epr "Contre-ex = %a @."
          (list ~sep:comma Tterm_printer.print_pattern)
          (List.hd p.mat);
        false

(* let redondance tbl tbl2 typ pat = *)
(*   let pmat = Pmatrix.from_pat pat in *)
(*   let res = ref [] in *)
(*   for i = 1 to pmat.rows - 1 do *)
(*     let sub_mat, test_line = Pmatrix.sub_mat_i pmat i in *)
(*     let q = List.map Ast.pattern2value test_line in *)
(*     let u = usefulness tbl tbl2 [ typ ] sub_mat q in *)
(*     if not u then res := i :: !res *)
(*   done; *)
(*   List.rev !res *)

(* let warn ?(debug = false) ?(runtest = false) ~mode cases tbl tbl2 = *)
(*   dbg := debug; *)
(*   let res = ref true in *)
(*   List.iteri *)
(*     (fun ipat (v, t, pat) -> *)
(*       let pat = List.map fst pat in *)
(*       match mode with *)
(*       | Ast.Exh -> *)
(*           let b = exhaustivite tbl tbl2 t pat in *)
(*           res := !res && b; *)
(*           Format.printf "exhaustivite %s = %b@." v b *)
(*       | Ast.Red -> *)
(*           let r = redondance tbl tbl2 t pat in *)
(*           res := !res && r = []; *)
(*           Format.printf "\n-> redondance %s = %a@." v *)
(*             (list ~sep:(any ",") (fun ppf e -> *)
(*                  Ast.get_action ipat e cases |> string ppf)) *)
(*             r) *)
(*     cases; *)
(*   if runtest && not !res then exit 2 *)

let check_exhaustive (ty : ty) (cases : (pattern * term) list) =
  let pat = List.map fst cases in
  if not (exhaustivite ty pat) then failwith "error"
