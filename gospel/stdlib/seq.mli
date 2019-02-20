
(* FIXME add axioms *)

(*@ type 'a seq *)

(*@ function length (s: 'a seq): integer *)

(*@ function ([_]) (s: 'a seq) (i:integer): 'a *)

(*@ predicate (==) (s1 s2: 'a seq) =
      length s1 = length s2 &&
      forall i. 0 <= i < length s1 -> s1[i] = s2[i] *)

(*@ function create (x: integer) (f: integer -> 'a): 'a seq *)
(*@ axiom create_len : forall n, f. n >= 0 ->
      length (create n f) = n *)
(*@ axiom create_def : forall n, f. n >= 0 ->
      forall i. 0 <= i < n -> (create n f)[i] = f i *)

(* TODO : DO WE WANT SOMETHING LIKE THIS ? *)
(*@ function create (n: integer) (f: int -> 'a) : 'a seq *)
(*@ requires 0 <= n
    ensures  length result = n
    ensures  forall i. 0 <= i < n -> result[i] = f i *)

(*@ function empty: 'a seq *)

(*@ function ([<-]) (s: 'a seq) (i: integer) (x: 'a): 'a seq *)

(*@ function cons (x: 'a) (s: 'a seq): 'a seq *)
(*@ function snoc (s: 'a seq) (x: 'a): 'a seq *)

(* FIXME singleton? *)

(*@ function ([_.._]) (s: 'a seq) (i1: integer) (i2: integer): 'a seq *)
(*@ function ([_..]) (s: 'a seq) (i: integer): 'a seq *)
(*@ function ([.._]) (s: 'a seq) (i: integer): 'a seq *)

(*@ function (++) (s1: 'a seq) (s2: 'a seq): 'a seq *)
(* + name append? *)

(*@ predicate mem (s: 'a seq) (x: 'a) =
      exists i. 0 <= i < length s && s[i] = x *)

(*@ predicate distinct (s: 'a seq) =
      forall i j. 0 <= i < length s -> 0 <= j < length s ->
      i <> j -> s[i] <> s[j] *)

(*@ function rev (s: 'a seq) : 'a seq =
      create (length s) (fun i -> s[length s - 1 - i]) *)

(*@ function map (f: 'a -> 'b) (s: 'a seq) : 'b seq =
      create (length s) (fun i -> f s[i]) *)

(*@ function rec fold_left (f: 'a -> 'b -> 'a) (acc: 'a) (s: 'b seq) : 'a =
      if length s = 0 then acc
      else fold_left f (f acc s[0]) s[1 ..] *)

(*@ function rec fold_right (f: 'a -> 'b -> 'b) (s: 'a seq) (acc: 'b) : 'b =
      if length s = 0 then acc
      else f s[0] (fold_right f s[1 ..] acc) *)

(*@ function hd (s: 'a seq) : 'a = s[0] *)
(*@ function tl (s: 'a seq) : 'a seq = s[1 ..] *)

(* hd, tl, rev, mem *)
(* higher-order: map, fold, exists, forall, find, partition *)
(* assoc, mem_assoc? split, combine? *)
