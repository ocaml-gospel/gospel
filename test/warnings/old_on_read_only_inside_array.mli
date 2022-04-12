val old_on_read_only : 'a array -> 'a
(*@ r = old_on_read_only a
    ensures r = old a.(0) *)
