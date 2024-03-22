val no_modifies_while_returning_unit : int -> unit
(*@ no_modifies_while_returning_unit i
    requires true *)

(* {gospel_expected|
   [125] File "no_modifies_while_returning_unit.mli", line 1, characters 0-110:
         1 | val no_modifies_while_returning_unit : int -> unit
         2 | (*@ no_modifies_while_returning_unit i
         3 |     requires true *)
         Error: The function no_modifies_while_returning_unit returns unit
                but its specifications does not contain any modifies clause.
   |gospel_expected} *)
