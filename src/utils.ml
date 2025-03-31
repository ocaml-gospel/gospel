(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let rec duplicate p error = function
  | [] -> ()
  | x :: t -> (
      match List.find_opt (p x) t with
      | Some x -> error x
      | None -> duplicate p error t)

(** [keep_until x l] when [l] is of the form [l1 @ x :: l2], where
    [not List.mem x l1], returns the list [List.rev l1]. If the element [x] is
    not in [l], this function fails. *)
let keep_until x =
  let rec keep_until acc x = function
    | [] -> assert false
    | y :: t -> if x = y then acc else keep_until (y :: acc) x t
  in
  keep_until [] x

(** [find_circle graph] returns a circular path in [graph]. The returned path
    will begin and end on the same node. This function expects that all nodes in
    [graph] have at least one path leading from it. *)
let find_circle graph =
  (* [travel x acc] iterates through [graph] starting from node [x]. At each
     step, if [x] is not in [visited], we follow the first adjacent path and add
     it [x] to [visited]. If [x] is already in [visited], then we return the
     cycle that begins in [x]. *)
  let rec travel x visited =
    if List.mem x visited then
      (* If this node has been visited, then we keep only the nodes that form a
         circular path that begins and ends with [x]. *)
      (x, keep_until x visited)
    else
      (* Find the nodes adjacent to [x]. *)
      let _, adj = List.find (fun (y, _) -> x = y) graph in
      (* Get an arbitrary adjacency. *)
      let next = List.hd adj in
      travel next (x :: visited)
  in
  travel (List.hd graph |> fst) []

exception Cycle of string * string list

let rec depends graph =
  if graph = [] then []
  else
    (* Return one of the nodes with no paths starting from it. *)
    match List.find_opt (fun (_, deps) -> deps = []) graph with
    | Some (x, _) ->
        (* We iterate through the list and remove node [x] from the list of
           adjancencies of each node. Additionally, [x] is removed from the
           graph. *)
        let next =
          List.filter_map
            (fun (y, deps) ->
              if x = y then
                (* Remove [x] from the graph. This ensures that each recursive
                   call to [depends] will reduce the size of [graph] by one. *)
                None
              else
                (* Remove [x] from the list of destinations of each node. *)
                Some (y, List.filter (fun z -> not (x = z)) deps))
            graph
        in
        (* Since [x] has no paths leading to it, it does not depend on any node
           in the graph, meaning it can be placed at the top of the list. *)
        x :: depends next
    | None ->
        (* If all nodes have a path starting from it, then there is a circular
           dependency. *)
        let s, l = find_circle graph in
        raise (Cycle (s, l))

module Fmt = struct
  include Fmt

  let list ?(first = nop) ?(last = nop) ?sep pp_v ppf l =
    if List.length l = 0 then ()
    else pf ppf "%a@[%a@]%a" first () (list ?sep pp_v) l last ()

  let pp = pf
  let full ppf _ = pf ppf ".@ "
  let arrow ppf _ = pf ppf " ->@ "
  let star ppf _ = pf ppf " *@ "
  let newline ppf _ = pf ppf "@\n"
  let lparens ppf _ = pf ppf "@[<1>("
  let rparens ppf _ = pf ppf ")@]"
  let lbracket ppf _ = pf ppf "@[<1>["
  let rbracket ppf _ = pf ppf "]@]"
  let lbrace ppf _ = pf ppf "@[<1>{"
  let rbrace ppf _ = pf ppf "}@]"

  let pp_loc ppf loc =
    let open Ppxlib.Location in
    let s = loc.loc_start in
    if s.pos_fname = "_none_" then pf ppf "none"
    else pf ppf "%s:%d:%d" s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol)
end

module Sstr = Set.Make (String)
