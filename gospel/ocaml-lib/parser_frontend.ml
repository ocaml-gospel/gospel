open Oparser
open Uattr2spec

exception Ocaml_syntax_error of Location.t

let () = Location.register_error_of_exn (function
             | Ocaml_syntax_error loc ->
                Some (Location.errorf ~loc "OCaml syntax error")
             | _ -> None )

exception FileNotFound of string

let open_file file =
  let exception Break of in_channel in
  let try_open d = try
      let f = d ^ "/" ^ file in
      let c = open_in f in raise (Break c)
    with Sys_error _ -> () in
  if not (Filename.is_relative file) then open_in file
  else try List.iter try_open !Options.load_path;
           raise (FileNotFound file)
       with Break c -> c

(** Parse the given *.mli file -- it must be an interface.
 Raises FileNotFound if file does not exist. *)
let parse_file file =
  let ch = open_file file in
  let lb = Lexing.from_channel ch in
  Location.init lb file;
  try interface Olexer.token lb with
    Error -> begin
      let spos,fpos = lb.lex_start_p, lb.lex_curr_p in
      let loc = Location.{loc_start=spos; loc_end=fpos;loc_ghost=false}  in
      raise (Ocaml_syntax_error loc) end

(** Parse the attributes as GOSPEL specification. Raises FileNotFound
   if file does not exist. *)
let parse_spec sign = signature sign

(** Raises FileNotFound if file does not exist. *)
let parse_all file = parse_spec (parse_file file)
