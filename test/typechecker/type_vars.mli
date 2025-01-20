(*@ function f (f : 'a -> 'b) : 'a * 'b *)

(*@ axiom test1 : forall x. f (fun _ -> x) = (x, x) *)
