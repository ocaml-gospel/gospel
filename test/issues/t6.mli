(* Fixing this issue properly would require:
   - defining a Gospel [include_description] in [Tast] instead of reusing Ppxlib's
   - adding a [process_include] in [Typing] (adapting [process_open])
*)

include sig
  type t
end

type nonrec t = t

(* {gospel_expected|
   [125] File "t6.mli", line 10, characters 16-17:
         10 | type nonrec t = t
                              ^
         Error: Symbol t not found.
   |gospel_expected} *)
