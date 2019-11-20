(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let is_none = function
  | None -> true | _ -> false

let is_some o = not (is_none o)

(** Makes a partition at the point in which the property stops to hold
   *)
let rec split_at_f f = function
  | [] -> [],[]
  | x::xs ->
     if f x then let xs',ys' = split_at_f f xs in x::xs',ys'
     else [],x::xs

let rec split_at_i i = function
  | [] -> [],[]
  | l when i <= 0 -> [],l
  | x::xs -> let a,b = split_at_i (i-1) xs in
             x::a, b

(** 'map_until_exc f l = (l1,l2)' applies 'f' to the elements of 'l'
   until an exception is raised. The result of applying 'f'
   successfuly are returned in 'l1' and the remaining in 'l2'. *)
let rec map_until_exc f = function
  | [] -> [],[]
  | x::xs ->
     try let x' = f x in
         let xs',ys' = map_until_exc f xs in
         x'::xs',ys'
     with _ -> ([],x::xs)

let rec map_filter f = function
  | [] -> []
  | x::xs -> match f x with
             | None -> map_filter f xs
             | Some v -> v :: map_filter f xs

let cons_opt o l = match o with | None -> l | Some x -> x :: l

let opt2list = function | None -> [] | Some x -> [x]

let opt2bool = function None -> false | Some _ -> true

let opmap f = function | None -> None | Some v -> Some (f v)

let opiter f = function | None -> () | Some v -> f v

let opget = function | None -> assert false | Some x -> x

let opget_def x = function | None -> x | Some x -> x

let app_pair f g (a,b) = (f a, g b)

let app_triple f g h (a,b,c) = (f a, g b, h c)

let identity x = x

(* TODO: move this to a better place and avoid the translation to list *)
let print_option ?first ?last printer fmt = function
  | None -> ()
  | Some x -> let open Opprintast in
              let to_sf sf = match sf with
                | None -> ("":_ format6)
                | Some sf -> sf in
              pp fmt "%a%a%a" pp (to_sf first) printer x pp (to_sf last)

let print_option_default printer s fmt = function
  | None -> Format.fprintf fmt "%s" s
  | Some x -> printer fmt x

let list_with_first_last : 'a . ?sep:Opprintast.space_formatter ->
  ?first:Opprintast.space_formatter -> ?last:Opprintast.space_formatter ->
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a list -> unit
  = let open Opprintast in
    fun ?sep ?first ?last fu f xs ->
    let first = match first with Some x -> x |None -> ("": _ format6)
    and last = match last with Some x -> x |None -> ("": _ format6)
    and sep = match sep with Some x -> x |None -> ("@ ": _ format6) in
    let aux f = function
      | [] -> ()
      | [x] -> pp f first; fu f x; pp f last
      | xs ->
          let rec loop  f = function
            | [x] -> fu f x
            | x::xs ->  fu f x; pp f sep; loop f xs;
            | _ -> assert false in begin
            pp f first; loop f xs; pp f last;
          end in
    aux f xs

module Int = struct
  type t = int
  let compare (x: int) y = Stdlib.compare x y
  let equal (x: int) y = x = y
  let hash (x: int) = x
end

module Hint = Hashtbl.Make(Int)
module Hstr = Hashtbl.Make(struct
                type t = String.t
                let hash = (Hashtbl.hash : string -> int)
                let equal = ((=) : string -> string -> bool)
                end)
module Mstr = Map.Make(String)
module Sstr = Set.Make(String)

(* to be moved to a better place *)

let get_op_nm s =
  let sl = String.split_on_char ' ' s in
  match sl with
  | [x] -> x
  | [x1;x2] -> x2
  | _ -> assert false

exception TypeCheckingError of string
exception NotSupported of string
exception Located of Location.t * exn

let error ?loc e = match loc with
  | None -> raise e
  | Some loc -> raise (Located (loc,e))

let check ?loc c exn =
  if not c then error ?loc exn

let error_report ?loc s =
  error ?loc (TypeCheckingError s)

let check_report ?loc c s =
  check ?loc c (TypeCheckingError s)

let not_supported ?loc s =
  error ?loc (NotSupported s)

let () =
  let open Location in
  register_error_of_exn (function
      | Located (loc,exn) ->
         begin match error_of_exn exn with
         | None | Some `Already_displayed -> None
         | Some `Ok e -> Some {e with loc = loc}
         end
      | TypeCheckingError s ->
         Some (errorf "Type checking error: %s" s)
      | NotSupported s ->
         Some (errorf "Not supported: %s" s)
      | _ -> None)
