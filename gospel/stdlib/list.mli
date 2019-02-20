
(*@ use Seq *)

(*@ function seq_of_list (l: 'a list): 'a seq *)
(*@ coercion *)

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
(*@ function nil: 'a list *)
(*@ axiom nil_def: nil == empty *)
(*@ function cons (x: 'a) (l: 'a list): 'a list *)
(*@ axiom cons_def: forall x: 'a, l: 'a list. cons x l == Seq.cons x l *)

(*@ function cons (x: 'a) (l: 'a list) : 'a list =
      Seq.cons x l *)

(* rev, init?, append, *)
