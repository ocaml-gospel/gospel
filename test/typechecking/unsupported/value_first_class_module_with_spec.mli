module type S = sig
  val s : string
end

val f : (module S) -> string
(*@ s = f m *)
(* {gospel_expected|
[1] File "./value_first_class_module_with_spec.mli", line 5, characters 8-18:
    5 | val f : (module S) -> string
                ^^^^^^^^^^
    Error: Not yet supported: first class module
    
|gospel_expected} *)
