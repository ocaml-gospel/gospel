(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

let rec split_at_f f = function
  | [] -> [], []
  | x::xs as l ->
      if f x then
        let xs', ys' = split_at_f f xs in
        x::xs', ys'
      else [], l

let rec split_at_i i = function
  | [] -> [], []
  | l when i <= 0 -> [], l
  | x::xs ->
      let xs', ys' = split_at_i (i-1) xs in
      x::xs', ys'


module Option = struct
  let value o ~default = match o with
    | Some x -> x
    | None -> default

  let get = function
    | Some x -> x
    | None -> invalid_arg "option is None"

  let map f = function
    | Some v -> Some (f v)
    | None -> None

  let iter f = function
    | Some v -> f v
    | None -> ()

  let is_some = function
    | Some _ -> true
    | None-> false

  let fold ~none ~some = function Some v -> some v | None -> none
end

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

module Sstr = Set.Make(String)

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
