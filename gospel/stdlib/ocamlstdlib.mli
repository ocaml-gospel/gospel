module Sys : sig

  (*@ function word_size : integer *)

  (*@ function int_size : integer *)

  (*@ function big_endian : bool *)

  (*@ function max_string_length : integer *)

  (*@ function max_array_length : integer *)

end


(*@ function integer_of_int (x: int) : integer *)

(*@ function min (x: integer) (y: integer) : integer *)

exception Not_found
