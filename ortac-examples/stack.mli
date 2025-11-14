(*@ open Sequence *)

type 'a t
(*@ mutable model : 'a sequence *)

exception Empty

val create : unit -> 'a t
(*@ s = create ()
    ensures s = empty *)

val push : 'a -> 'a t -> unit
(*@ push a s
    modifies s
    ensures s = cons a (old s) *)

val pop_exn : 'a t -> 'a
(*@ a = pop_exn s
    modifies s

    (* excluding the empty case from the normal behaviour *)
    ensures not (is_empty (old s))

    (* this clause will allow Ortac/QCheck-STM to compute the next state (of `s`)

       it will be used both for normal and exceptional behaviour
       we can't distingusih between normal and exceptional behaviour when
       building the `next_state function
       we need to explicitly define the empty case so that the `next_state`
       function is total
       I may be able to improve this last bit on the Ortac side *)
    ensures s = if decide (is_empty (old s)) then empty else tl (old s)

    (* this clause will be used as a postcondition to check,
       it is also a way to compute the expected value from the model for a
       possible bug report

       if `hd` is called out of domain, the whole formula will be computed as
       false for the postcondition check, and we won't have an expected value
       to show in the bug report -- which is OK, it is optional *)
    ensures a = hd (old s)

    raises Empty
      (* guarantee that if we enter the exceptional behaviour, the stack was
         empty *)
      ensures old s = empty *)

val pop_opt : 'a t -> 'a option
(*@ o = pop_opt s
    modifies s

    (* describing next state *)
    ensures s = if decide (is_empty (old s)) then empty else tl (old s)

    (* describing the expected value *)
    ensures o = if decide (is_empty (old s)) then None else Some (hd (old s)) *)
