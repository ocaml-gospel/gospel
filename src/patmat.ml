module W = Warnings
open Ppxlib
open Symbols
open Ttypes
open Tterm
open Tterm_helper
open Fmt

module PMutils : sig
  val mex : int array -> int
  val is_builtin : ty -> bool
  val ts_of_ty : ty -> tysymbol option
end = struct
  exception Brk of int

  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp

  let mex a =
    let n = Array.length a in
    let i = ref 0 in
    while !i < n do
      let x = a.(!i) in
      if x < 0 || x >= n then incr i
      else if x < !i then (
        swap a !i x;
        incr i)
      else if a.(x) = x then incr i
      else swap a !i x
    done;
    try
      for i = 0 to n - 1 do
        if a.(i) <> i then raise (Brk i)
      done;
      n
    with Brk i -> i

  let ts_of_ty { ty_node } =
    match ty_node with Tyvar _ -> None | Tyapp (ts, _) -> Some ts

  let is_builtin ty =
    ty_equal ty ty_int
    || ty_equal ty ty_string
    || ty_equal ty ty_float
    || ty_equal ty ty_integer
    || ty_equal ty ty_char
    ||
    match ts_of_ty ty with
    | Some ts -> ts_equal ts ts_option || ts_equal ts ts_list
    | None -> false
end

module Pmatrix : sig
  type mat = pattern list list
  type t = private { rows : int; cols : int; mat : mat }

  val empty : t
  val from_mat : ?cols:int -> pattern list list -> t
  val from_pat : pattern list -> t
  val add_col : t -> pattern -> t
  val mk_wild : ty list -> pattern list
  val from_lines : pattern list list -> t
  val encap_row : lsymbol -> int -> t -> t
  val remove_or_col1 : t -> t
  val fst : t -> Tterm.pattern
  val pp : Format.formatter -> t -> unit [@@warning "-32"]
  val remove_interval : Tterm.pattern -> Tterm.pattern [@@warning "-32"]
end = struct
  type mat = pattern list list
  type t = { rows : int; cols : int; mat : mat }

  let from_mat ?cols mat =
    let cols =
      Option.value cols
        ~default:(match mat with [] -> 0 | e :: _ -> List.length e)
    in
    { rows = List.length mat; cols; mat }

  let rec remove_as pat =
    match pat.p_node with
    | Por (p1, p2) -> { pat with p_node = Por (remove_as p1, remove_as p2) }
    | Pas (pat, _) -> remove_as pat
    | Papp (l, pl) -> { pat with p_node = Papp (l, List.map remove_as pl) }
    | _ -> pat

  let rec remove_or ?(sub_app = false) pat =
    match pat.p_node with
    | Por (p1, p2) -> remove_or ~sub_app p1 @ remove_or ~sub_app p2
    | Pas (pat, _) -> remove_or ~sub_app pat
    | Papp (l, pl) when sub_app ->
        let pl = List.map (remove_or ~sub_app) pl in
        List.map (fun e -> { pat with p_node = Papp (l, e) }) pl
    | _ -> [ pat ]

  let rec remove_interval pat =
    match pat.p_node with
    | Pinterval (c1, c2) ->
        let rec loop c1 c2 =
          if c1 = c2 then Pconst (Pconst_char c1)
          else
            Por
              ( { pat with p_node = Pconst (Pconst_char c1) },
                { pat with p_node = loop (Char.chr (Char.code c1 + 1)) c2 } )
        in
        let p = if c1 <= c2 then loop c1 c2 else loop c2 c1 in
        { pat with p_node = p }
    | Pas (p, a) -> { pat with p_node = Pas (remove_interval p, a) }
    | Papp (l, pl) ->
        { pat with p_node = Papp (l, List.map remove_interval pl) }
    | Por (p1, p2) ->
        { pat with p_node = Por (remove_interval p1, remove_interval p2) }
    | _ -> pat
    [@@warning "-32"]

  let remove_or_col1 pmat =
    if pmat.cols = 0 then invalid_arg "pmatrix.rm_or_col1";
    let m =
      List.fold_left
        (fun acc e ->
          match e with
          | [] -> assert false
          | h :: t -> (
              match h.p_node with
              | Por _ ->
                  List.map (fun el -> el :: t) (remove_or ~sub_app:true h) @ acc
              | _ -> e :: acc))
        [] pmat.mat
    in
    { mat = m; rows = List.length m; cols = pmat.cols }

  let from_pat pat =
    if pat = [] then invalid_arg "pmatrix.from_pat";
    let mat =
      List.fold_left
        (fun acc e ->
          remove_as e |> remove_or |> List.map (fun e -> [ e ]) |> fun e ->
          e @ acc)
        [] pat
    in
    { mat; rows = List.length mat; cols = 1 }

  let empty = { mat = []; rows = 0; cols = 0 }

  let fst pmat =
    match pmat.mat with
    | [] | [] :: _ :: _ | [ [] ] -> invalid_arg "pmat.fst"
    | (h :: _) :: _ -> h

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
  let mk_wild = List.map (fun ty -> mk_pattern Pwild ty)

  let rec pp_pattern ppf pat =
    match pat.p_node with
    | Pwild -> pf ppf "_"
    | Pvar v -> pf ppf "%s" v.vs_name.id_str
    | Pas (p, a) -> pf ppf "%a as %s" pp_pattern p a.vs_name.id_str
    | Papp (p, []) ->
        let s = p.ls_name.id_str in
        pf ppf "%s" s
    | Papp (p, pl) ->
        let s = p.ls_name.id_str in
        pf ppf "%s(%a)" s (list ~sep:comma pp_pattern) pl
    | Por (p1, p2) -> pf ppf "(%a | %a)" pp_pattern p1 pp_pattern p2
    | Pinterval (c1, c2) -> pf ppf "%C..%C" c1 c2
    | Pconst cc ->
        let v =
          match cc with
          | Pconst_integer (s, _) -> s
          | Pconst_char c -> Format.sprintf "%C" c
          | Pconst_string (s, _, _) -> Format.sprintf "%S" s
          | Pconst_float (s, _) -> s
        in
        pf ppf "%s" v

  let newline = any ",@\n" [@@warning "-32"]
  let line = list ~sep:comma pp_pattern
  let mat = list ~sep:newline line

  let pp ppf { mat = m; rows; cols } =
    pf ppf "%dx%d@\n%a" rows cols (brackets mat) m
end

module Sigma : sig
  type e = ty list
  type t

  val mk : Pmatrix.mat -> t
  val is_full : ty -> t -> Pmatrix.t -> bool
  val is_empty : t -> bool
  val other_one : ty -> t -> Pmatrix.t -> pattern_node
  val exists : (lsymbol -> e -> bool) -> t -> bool
  val iter : (lsymbol -> e -> unit) -> t -> unit
  val pp : Format.formatter -> t -> unit [@@warning "-32"]
  val get_typ_cols : e -> lsymbol -> e
  val cardinal : t -> int [@@warning "-32"]
end = struct
  type e = ty list
  type t = e Mls.t

  let cardinal = Mls.cardinal

  let rec mk p =
    List.fold_left
      (fun acc e ->
        match e with
        | { p_node = Papp (c, _); _ } :: _ -> Mls.add c c.ls_args acc
        | { p_node = Por (p1, p2); _ } :: _ ->
            let map1 = mk [ [ p1 ] ] in
            let map2 = mk [ [ p2 ] ] in
            let f _key e1 e2 =
              match (e1, e2) with
              | None, None -> None
              | Some e, None -> Some e
              | None, Some e -> Some e
              | Some e1, Some e2 when e1 = e2 -> Some e1
              | _ -> assert false
            in
            let map3 = Mls.merge f map1 map2 in
            Mls.merge f acc map3
        | _ -> acc)
      Mls.empty p

  let is_empty = Mls.is_empty
  let exists = Mls.exists
  let iter = Mls.iter

  let pp ppf sigma =
    if is_empty sigma then pf ppf "sigma : ø"
    else (
      pf ppf "sigma : %d@.\t@[" (Mls.cardinal sigma);
      iter
        (fun ls tyl ->
          pf ppf "ls : (%a) -- tyl : (%a)@." pp_lsymbol ls (list pp_ty) tyl)
        sigma;
      pf ppf "@]")

  let get_constructors ty : lsymbol list option =
    try
      match PMutils.ts_of_ty ty with
      | Some ts -> (
          let td = Hts.find Tmodule.type_declarations ts in
          match td.td_kind with
          | Pty_variant cdl ->
              Some (List.map (fun Tast.{ cd_cs; _ } -> cd_cs) cdl)
          | Pty_record { rd_cs; _ } -> Some [ rd_cs ]
          | Pty_abstract -> Some [])
      | None -> Some []
    with Not_found -> None

  let get_typ_cols (typ_cols : ty list) (ls : lsymbol) : ty list =
    match typ_cols with
    | [] -> assert false
    | { ty_node = Tyvar _v } :: _ -> assert false
    | { ty_node = Tyapp (ts, tyl) } :: _ ->
        let subst = ts_match_args ts tyl in
        List.map (fun e -> ty_full_inst subst e) ls.ls_args

  let case f pmat =
    List.fold_left
      (fun acc e ->
        match e with
        | { p_node = Pconst (Pconst_string (s, _, _)); _ } :: _
        | { p_node = Pconst (Pconst_integer (s, _)); _ } :: _
        | { p_node = Pconst (Pconst_float (s, _)); _ } :: _ ->
            f s :: acc
        | _ -> acc)
      []
      Pmatrix.(pmat.mat)
    |> Array.of_list
    |> PMutils.mex

  let case_int pmat mark =
    let pmat = Pmatrix.remove_or_col1 pmat in
    let len = case int_of_string pmat in
    Pconst (Pconst_integer (string_of_int len, mark))

  let case_float pmat =
    let pmat = Pmatrix.remove_or_col1 pmat in
    let len = case (fun x -> float_of_string x |> floor |> int_of_float) pmat in
    Pconst (Pconst_float (Format.sprintf "%d." len, None))

  let case_str pmat =
    let pmat = Pmatrix.remove_or_col1 pmat in
    let len = case String.length pmat in
    Pconst (Pconst_string (String.make len '?', Location.none, None))

  let i = ref ~-1

  let case_char pmat =
    incr i;
    let pmat = Pmatrix.remove_or_col1 pmat in
    let pmat =
      List.map
        (function [] -> [] | h :: t -> Pmatrix.remove_interval h :: t)
        pmat.mat
      |> Pmatrix.from_mat
    in
    let pmat = Pmatrix.remove_or_col1 pmat in
    List.fold_left
      (fun acc e ->
        match e with
        | { p_node = Pconst (Pconst_char c); _ } :: _ -> Char.code c :: acc
        | { p_node = Pinterval (c1, c2); _ } :: _ ->
            if true then assert false;
            let c1 = Char.code c1 in
            let c2 = Char.code c2 in
            let c1, c2 = if c1 <= c2 then (c1, c2) else (c2, c1) in
            let d = c2 - c1 + 1 in
            List.init d (fun i -> c1 + i) @ acc
        | _ -> acc)
      []
      Pmatrix.(pmat.mat)
    |> Array.of_list
    |> PMutils.mex
    |> fun e ->
    if e > 255 then None else Some (Pconst (Pconst_char (Char.chr e)))

  let case_bool pmat =
    let pmat = Pmatrix.remove_or_col1 pmat in
    match Pmatrix.fst pmat with
    | { p_node = Papp (ls, []); _ } when ls_equal ls fs_bool_true ->
        Papp (fs_bool_false, [])
    | { p_node = Papp (ls, []); _ } when ls_equal ls fs_bool_false ->
        Papp (fs_bool_true, [])
    | _ -> assert false

  let other_one ty ens pmat =
    let ts = PMutils.ts_of_ty ty |> Option.get in
    if ts_equal ts ts_int then case_int pmat (Some 'i')
    else if ts_equal ts ts_integer then case_int pmat None
    else if ts_equal ts ts_string then case_str pmat
    else if ts_equal ts ts_char then case_char pmat |> Option.get
    else if ts_equal ts ts_bool then case_bool pmat
    else if ts_equal ts ts_float then case_float pmat
    else
      match get_constructors ty with
      | Some e ->
          let cc = List.find (fun ls -> not (Mls.mem ls ens)) e in
          Papp (cc, Pmatrix.mk_wild cc.ls_args)
      | None -> assert false

  let is_full ty ens pmat =
    match PMutils.ts_of_ty ty with
    | Some ts -> (
        if is_ts_tuple ts then true
        else if ts_equal ts ts_bool then
          Mls.mem fs_bool_true ens && Mls.mem fs_bool_false ens
        else if ts_equal ts ts_char then case_char pmat |> Option.is_none
        else if ts_equal ts ts_int then false
        else if ts_equal ts ts_integer then false
        else if ts_equal ts ts_float then false
        else if ts_equal ts ts_string then false
        else
          match get_constructors ty with
          | Some l -> List.for_all (fun ls -> Mls.mem ls ens) l
          | None -> false)
    | None -> false
end

let rec mk_default p =
  let rec mk_line nb_cols = function
    | [] -> []
    | h :: cols -> (
        match h.p_node with
        | Papp _ | Pconst _ | Pinterval _ -> []
        | Pvar _ | Pwild -> [ cols ]
        | Por (t1, t2) ->
            let mat = Pmatrix.from_lines [ t1 :: cols; t2 :: cols ] in
            let r = mk_default mat in
            Pmatrix.(r.mat)
        | Pas (p, _) -> mk_line nb_cols (p :: cols))
  in
  let mat = List.map (mk_line p.cols) p.mat |> List.flatten in
  Pmatrix.from_mat mat ~cols:(p.cols - 1)

let rec mk_spec ?constr ?char a p =
  let rec mk_si = function
    | [] -> []
    | h :: cols -> (
        match h.p_node with
        | Papp (c', ti)
          when Option.is_some constr && ls_equal c' (Option.get constr) ->
            [ ti @ cols ]
        | Pinterval (c1, c2) when Option.is_some char ->
            let c1 = Char.code c1 in
            let c2 = Char.code c2 in
            let c1, c2 = if c1 <= c2 then (c1, c2) else (c2, c1) in
            let c = Char.code (Option.get char) in
            if c1 <= c && c <= c2 then [ cols ] else []
        | Pconst (Pconst_char c) when Option.is_some char && Option.get char = c
          ->
            [ cols ]
        | Pconst _ | Papp _ -> []
        | Pinterval _ -> []
        | Pvar _ | Pwild ->
            let ll = Pmatrix.mk_wild a in
            [ ll @ cols ]
        | Por (t1, t2) ->
            let mat = Pmatrix.from_lines [ t1 :: cols; t2 :: cols ] in
            let m = mk_spec ?constr ?char a mat in
            Pmatrix.(m.mat)
        | Pas (p, _) -> mk_si (p :: cols))
  in
  let mat = List.map mk_si p.mat |> List.flatten in
  Pmatrix.from_mat mat ~cols:(p.cols + List.length a - 1)

let rec usefulness typ_cols (pmat : Pmatrix.t) (qvec : pattern list) : bool =
  assert (List.length qvec = pmat.cols);
  if pmat.cols = 0 then pmat.rows = 0
  else
    let thd, tl = match typ_cols with [] -> assert false | h :: t -> (h, t) in
    match qvec with
    | [] -> assert false
    | { p_node = Pconst _; _ } :: ll ->
        let typ_nl = tl in
        usefulness typ_nl (mk_spec [] pmat) ll
    | { p_node = Papp (c, pl); _ } :: ll ->
        let typ_next = c.ls_args in
        let typ_nl = typ_next @ tl in
        usefulness typ_nl
          (mk_spec ~constr:c (List.map (fun p -> p.p_ty) pl) pmat)
          (pl @ ll)
    | { p_node = Pvar _ | Pwild; _ } :: ll ->
        let sigma = Sigma.mk pmat.mat in
        if Sigma.is_full thd sigma pmat then
          if ty_equal thd ty_char then
            List.exists
              (fun i -> usefulness tl (mk_spec ~char:(Char.chr i) [] pmat) ll)
              (List.init 256 (fun i -> i))
          else
            Sigma.exists
              (fun ck tyl ->
                let q = Pmatrix.mk_wild ck.ls_args @ ll in
                let typ_cols = Sigma.get_typ_cols typ_cols ck @ tl in
                usefulness typ_cols (mk_spec ~constr:ck tyl pmat) q)
              sigma
        else usefulness tl (mk_default pmat) ll
    | { p_node = Por (p1, p2); _ } :: l ->
        usefulness typ_cols pmat (p1 :: l) || usefulness typ_cols pmat (p2 :: l)
    | { p_node = Pas (p, _); _ } :: l -> usefulness typ_cols pmat (p :: l)
    | _ -> assert false

let rec ui (typ_cols : ty list) (pmat : Pmatrix.t) =
  if pmat.cols = 0 then if pmat.rows = 0 then Some Pmatrix.empty else None
  else
    let thd, tl = match typ_cols with [] -> assert false | h :: t -> (h, t) in
    let sigma = Sigma.mk pmat.mat in
    if Sigma.is_full thd sigma pmat then
      let exception Brk of Pmatrix.t in
      try
        if ty_equal thd ty_char then
          List.iter
            (fun i ->
              match ui tl (mk_spec ~char:(Char.chr i) [] pmat) with
              | None -> ()
              | Some pm ->
                  let c =
                    {
                      p_node = Pconst (Pconst_char (Char.chr i));
                      p_ty = ty_char;
                      p_loc = None;
                    }
                  in
                  let pm = Pmatrix.add_col pm c in
                  raise (Brk pm))
            (List.init 256 (fun i -> i))
        else
          Sigma.iter
            (fun ck ck_args ->
              let typ_cols = Sigma.get_typ_cols typ_cols ck @ tl in
              match ui typ_cols (mk_spec ~constr:ck ck_args pmat) with
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
          if Sigma.is_empty sigma && not (PMutils.is_builtin thd) then
            Some (Pmatrix.add_col pm (mk_pattern Pwild thd))
          else
            let pat_node = Sigma.other_one thd sigma pmat in
            let pat = mk_pattern pat_node thd in
            Some (Pmatrix.add_col pm pat)

let exhaustivite typ pat ~loc =
  let pmat = Pmatrix.from_pat pat in
  let q = Pmatrix.mk_wild [ (List.hd pat |> fun p -> p.p_ty) ] in
  (not (usefulness [ typ ] pmat q))
  || ui [ typ ] pmat |> Option.get |> fun pm ->
     let s = Pmatrix.fst pm |> Fmt.str "@[%a@]" Tterm_printer.print_pattern in
     W.error ~loc (W.Pattern_not_exhaustive s)

let check_exhaustive (ty : ty) (cases : (pattern * term) list)
    ~(loc : Location.t) =
  let pat = List.map fst cases in
  try (exhaustivite ty pat ~loc : bool) |> ignore
  with Not_found -> assert false
