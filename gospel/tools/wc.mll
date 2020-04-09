(**************************************************************************)
(*                                                                        *)
(*  GOSPEL -- A Specification Language for OCaml                          *)
(*                                                                        *)
(*  Copyright (c) 2018- The VOCaL Project                                 *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

{
  open Format

  type counter =
    { mutable spec: int; mutable code: int; mutable comment: int }

  let new_counter () =
    { spec = 0; code = 0; comment = 0 }

  let (+=) c1 c2 =
    c1.spec <- c1.spec + c2.spec;
    c1.code <- c1.code + c2.code;
    c1.comment <- c1.comment + c2.comment

  let reset c =  c.spec <- 0; c.code <- 0; c.comment <- 0

  let current_file = new_counter ()
  let grand_total  = new_counter ()

  let update_total () = grand_total += current_file

  type state = Nothing | Spec | Code | Comment
  let state = ref Nothing

  let new_line () = match !state with
    | Nothing -> ()
    | Spec    -> current_file.spec <- current_file.spec + 1
    | Code    -> current_file.code <- current_file.code + 1
    | Comment -> current_file.comment <- current_file.comment + 1

}

let space = [' ' '\t' '\r']
let code  = "val" | "type" | "exception" | "module" | "end" | "sig"

rule scan = parse
  | "(*" space* '\n'?
      { state := Comment; comment lexbuf; scan lexbuf }
  | '\n' space* '\n'
      { new_line (); state := Nothing; scan lexbuf }
  | '\n'
      { new_line (); scan lexbuf }
  | code
      { state := Code; scan lexbuf }
  | "(*@"
      { state := Spec; spec lexbuf; scan lexbuf }
  | _
      { scan lexbuf }
  | eof
      { }

and spec = parse
  | ('\n' | space*) "*)" (* do not count last new_line character *)
           { () }
  | '\n'+  { new_line (); spec lexbuf }
  | _      { spec lexbuf }
  | eof    { failwith "Unterminated specification block.\n" }

and comment = parse
  | ('\n' | space*) "*)"
          { () }
  | "(*"  { comment lexbuf; comment lexbuf }
  | '\n'+ { new_line (); comment lexbuf }
  | _     { comment lexbuf }
  | eof   { failwith "Unterminated comment.\n" }

{
  let legend =
    let first = ref true in
    fun () -> if !first then begin
      printf "    spec     code comments@."; first := false end

  let print_file file c =
    legend ();
    (* print spec, code and comments statistics *)
    printf "%8d" c.spec; printf " %8d" c.code; printf " %8d" c.comment;
    (* print file name *)
    printf " %s@." file

  let print_total () =
    print_file "total" grand_total

  let run_file f =
    let ch = open_in f in
    let lb = Lexing.from_channel ch in
    reset current_file; (* a new file begins *)
    scan lb; (* entry point for file scanning *)
    close_in ch;
    print_file f current_file;
    update_total ()

  let run files =
    List.iter run_file files;
    if List.length files <> 1 then print_total ()
}
