module type S = sig
  type 'a t
  (*@ ephemeral 
      model : 'a sequence *)

  val create : unit -> 'a t
  (*@ a = create ()
        produces a @ 'a t
        ensures a = Sequence.empty *)

  val make : int -> 'a -> 'a t
  (*@ a = make n e
        produces a @ 'a t
        ensures a = Sequence.init n (fun _ -> e) *)

  val get : 'a t -> int -> 'a
  (*@ r = get a i
        requires 0 <= i < Sequence.length a
        preserves a @ 'a t
        ensures r = a[i] *)

  val set : 'a t -> int -> 'a -> unit
  (*@ set a i e
        requires 0 <= i < Sequence.length a
        modifies a @ 'a t
        ensures a = Sequence.set (old a) i e *)

  val length : 'a t -> int
  (*@ l = length a
        preserves a @ 'a t
        ensures l = Sequence.length a *)

  val is_empty : 'a t -> bool
  (*@ b = is_empty a
        preserves a @ 'a t
        ensures b <-> (a = Sequence.empty) *)

  val find_last : 'a t -> 'a option
  (*@ r = find_last a
        preserves a @ 'a t
        ensures match r with
        |None -> a = Sequence.empty
        |Some r -> r = a[Sequence.length a - 1] *)

  val copy : 'a t -> 'a t
  (*@ c = copy a
        preserves a @ 'a t
        ensures c = a *)

  val add_last : 'a t -> 'a -> unit
  (*@ add_last a e
        modifies a @ 'a t
        ensures  a = (old a) ++ (Sequence.singleton e) *)

  val append : 'a t -> 'a t -> unit
  (*@ append a1 a2
        modifies a1 @ 'a t
        ensures a1 = (old a1) ++ a2 *)

  val pop_last_opt : 'a t -> 'a option
  (*@ r = pop_last_opt a
        modifies a @  'a t
        ensures match r with
        |None -> a = Sequence.empty && old a = Sequence.empty
        |Some r -> a ++ Sequence.singleton r = old a *)

  val remove_last : 'a t -> unit
  (*@ remove_last a
        modifies a @ 'a t
        ensures old a = Sequence.empty -> a = Sequence.empty
        ensures a <> Sequence.empty ->
                a = old (a[.. Sequence.length a - 1]) *)

  val truncate : 'a t -> int -> unit
  (*@ truncate a n
        modifies a @ 'a t
        requires n >= 0
        ensures n >= Sequence.length a -> a = old a
        ensures n < Sequence.length a -> a = old (a[.. n]) *)

  val clear : 'a t -> unit
  (*@ clear a
        modifies a @ 'a t
        ensures a = Sequence.empty *)
end
