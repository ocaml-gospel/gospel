(* Fixing this issue properly would require:
   - defining a Gospel [include_description] in [Tast] instead of reusing Ppxlib's
   - adding a [process_include] in [Typing] (adapting [process_open])
*)

include sig
  type t
end

type nonrec t = t

(* {gospel_expected|
   [125] File "include_module.mli", line 6, characters 0-24:
         6 | include sig
         7 |   type t
         8 | end
         Warning: `include`s are currently ignored by the gospel type-checker.
         File "include_module.mli", line 10, characters 16-17:
         10 | type nonrec t = t
                              ^
         Error: Symbol t not found in scope
                (see "Symbols in scope" documentation page).
   |gospel_expected} *)
