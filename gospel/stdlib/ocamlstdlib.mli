
(* built-in

   type unit
   type int
   type string
   type float
   type bool
   type 'a array

   type integer
   function (+)   (x y: integer) : integer
   function (-)   (x y: integer) : integer
   function ( * ) (x y: integer) : integer
   function (/)   (x y: integer) : integer
   function (mod) (x y: integer) : integer (* TODO *)
   function (-_)  (x: integer) : integer (* unary minus *)
   predicate (>)  (x y: integer)
   predicate (>=) (x y: integer)
   predicate (<)  (x y: integer)
   predicate (<=) (x y: integer)

   type 'a option
   function None: 'a option
   function Some (x: 'a) : 'a option

   type 'a list
   function ([]): 'a list
   function (::) (x: 'a) (l: 'a list) : 'a list

   predicate (=) (x y: 'a)

*)

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

(*@ function fst (p: 'a * 'b) : 'a *)
(*@ function snd (p: 'a * 'b) : 'b *)

type 'a ref
(*@ ephemeral *)
(*@ mutable model contents: 'a *)

(*@ function (!_) (r: 'a ref) : 'a = r.contents *)

(*
type 'a array
(*@ ephemeral *)
(*@ mutable model contents: 'a seq *)
(*@ coercion *) (* TODO: coercion for model fields *)
*)

exception Not_found

module Sys : sig

  (*@ function word_size : integer *)

  (*@ function int_size : integer *)

  (*@ function big_endian : bool *)

  (*@ function max_string_length : integer *)

  (*@ function max_array_length : integer *)

end

module List : sig

  (*@ use Seq *)

  (*@ function seq_of_list (l: 'a list): 'a seq *)
  (*@ coercion *)

(* the following require coercions
  (*@ function length (l: 'a list) : integer =
        Seq.length l *)

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
*)

  (* rev, init?, append, *)

end

module Array : sig
  (*@ use Seq *)

  (*@ function seq_of_array (a: 'a array): 'a seq *)
  (*@ coercion *)

  (* TODO: remove the next two when we have coercions *)
  (*@ function length (a: 'a array): integer =
    Seq.length (seq_of_array a) *)

  (*@ function ([_]) (a: 'a array) (i: integer): 'a =
    Seq.([_]) (seq_of_array a) i *)
end
