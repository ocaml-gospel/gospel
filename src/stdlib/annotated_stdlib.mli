type annotated_int = int
(*@ model v : integer *)

type 'a annotated_array = 'a array
(*@ mutable model array_content : 'a sequence *)
