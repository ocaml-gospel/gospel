(* This file is intended to be used with GOSPEL-framework only *)

type int

(*@ function integer_of_int (x: int) : integer *)
(*@ coercion *)

(*@ function abs (x:integer) : integer = if x >= 0 then x else -x *)

(*@ function min (x y : integer) : integer
    = if x <= y then x else y *)

(*@ function max (x y : integer) : integer
    = if x <= y then y else x *)

(*@ function succ (x: integer) : integer = x + 1 *)
(*@ function pred (x: integer) : integer = x - 1 *)

(*@ function max_int : integer *)
(*@ function min_int : integer *)


(* tuples *)

(*@ function fst (p: 'a * 'b) : 'a *)
(*@ function snd (p: 'a * 'b) : 'b *)

type 'a ref
(*@ ephemeral *)
(*@ mutable model contents: 'a *)

(*@ function (!_) (r: 'a ref) : 'a = r.contents *)

type 'a array
(*@ ephemeral *)
(*@ mutable model contents: 'a Seq.seq *)

exception Not_found

module Sys : sig

  (*@ function word_size : integer *)

  (*@ function int_size : integer *)

  (*@ function big_endian : bool *)

  (*@ function max_string_length : integer *)

  (*@ function max_array_length : integer *)

end

(* Type 'a list, [] and (::) constructors are built-in *)

module List : sig

  (*@ open Seq *)

  (*@ function seq_of_list (l: 'a list): 'a seq *)
  (*@ coercion *)

  (* TO BE DISCUSSED - if we want to remove this function length we
   need Seq to be open before *)
  (*@ function length (l: 'a list) : integer = Seq.length l *)

  (*@ function nth (l: 'a list) (i: integer) : 'a =
        l[i] *)

  (*@ function nth_opt (l: 'a list) (i: integer) : 'a option =
        if 0 <= i && i < length l then Some l[i] else None *)

  (*@ function hd (l: 'a list) : 'a = l[0] *)
  (*@ function tl (l: 'a list): 'a list *)
  (*@ axiom tl_def: forall l: 'a list. length l > 0 -> tl l == l[1 ..] *)

  (* TO BE DISCUSSED *)
  (*@ axiom nil_def: [] == empty *)
  (*@ axiom cons_def: forall x: 'a, l: 'a list. x :: l == Seq.cons x l *)


  (* rev, init?, append, *)

end

module Array : sig
  (*@ open Seq *)

  (*@ function seq_of_array (a: 'a array): 'a seq *)
  (*@ coercion *)

  (*@ function length (a: 'a array) : integer = Seq.length a *)
  (*@ function ([_]) (a: 'a array) (i: integer) : 'a = Seq.([_]) a i *)

  (* TODO keep this here? *)
  (*@ predicate permut_sub (a b: 'a array) (i j: integer) *)
  (*@ predicate permut_all (a b: 'a array) *)

end
