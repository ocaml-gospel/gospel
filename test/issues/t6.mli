include sig
  type t
end

type nonrec t = t

(* {gospel_expected|
   [125] File "t6.mli", line 5, characters 16-17:
         5 | type nonrec t = t
                             ^
         Error: The type declaration for `t' contains a cycle.
   |gospel_expected} *)
