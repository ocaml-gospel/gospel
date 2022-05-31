val no_modifies_while_returning_unit : int -> unit
(*@ no_modifies_while_returning_unit i
    requires true *)

(* EXPECTED
   [125] File "no_modifies_while_returning_unit.mli", line 1, characters 0-120:
         Error: The function `no_modifies_while_returning_unit' returns `unit'
                but its specifications does not contain any `modifies' clause.
*)
