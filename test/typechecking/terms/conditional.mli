(*@ function hd_opt (s : 'a sequence) : 'a option =
      if s = Sequence.empty
      then None
      else Some (Sequence.hd s) *)
(* {gospel_expected|
[1] File "conditional.mli", line 2, characters 9-27:
    2 |       if s = Sequence.empty
                 ^^^^^^^^^^^^^^^^^^
    Error: Mismatch between type bool and type prop
    
|gospel_expected} *)
