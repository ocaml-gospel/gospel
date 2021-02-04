(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

open Ppxlib

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

module Fmt = struct
  include Fmt

  let list ?(first=nop) ?(last=nop) ?sep pp_v =
    fun ppf l ->
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
end

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
  let open Location.Error in
  register_error_of_exn (function
      | Located (_loc, exn) ->
        of_exn exn
        (*   TODO: wait for the next ppxlib release to get this
         * |> Option.map (fun t -> Location_error.update_loc t loc) *)
      | TypeCheckingError s ->
        Fmt.kstr (fun str -> Some (make ~loc:Location.none ~sub:[] str))
          "Type checking error: %s" s
      | NotSupported s ->
        Fmt.kstr (fun str -> Some (make ~loc:Location.none ~sub:[] str))
          "Not supported: %s" s
      | _ -> None)
