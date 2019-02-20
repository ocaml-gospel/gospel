
(*@ use Seq *)

(*@ function seq_of_array (a: 'a array): 'a seq *)
(*@ coercion *)

(*@ function length (a: 'a array): integer =
  Seq.length (seq_of_array a) *)

(*@ function ([_]) (a: 'a array) (i: integer): 'a =
  Seq.([]) (seq_of_array a) i *)
