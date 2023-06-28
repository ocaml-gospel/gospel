{
  type t =
    | Ghost of Lexing.position * Lexing.position * string
    | Spec of Lexing.position * Lexing.position * string
    | Documentation of Lexing.position * Lexing.position * string
    | Empty_documentation of Lexing.position
    | Other of string
    | Spaces of string

  let queue = Queue.create ()
  let buf = Buffer.create 1024

  let clear () = Queue.clear queue

  let push () =
    if Buffer.length buf > 0 then (
      Queue.push (Other (Buffer.contents buf)) queue;
      Buffer.clear buf)

  (*  ...(*@ foo *)...

      ~>

      ...[@@@gospel
      # linenb
          {| foo |}
      # linenb
                  ]...
  *)
  let print_gospel (lvl: [`TwoAt | `ThreeAt]) start_p end_p s =
    Fmt.str "[%sgospel\n\
# %d \"%s\"\n\
%s {|%s|}\n\
# %d \"%s\"\n\
%s]"
      (match lvl with `TwoAt -> "@@" | `ThreeAt -> "@@@")
      start_p.Lexing.pos_lnum start_p.pos_fname
      (String.make (start_p.pos_cnum - start_p.pos_bol) ' ') s
      end_p.Lexing.pos_lnum end_p.pos_fname
      (String.make (end_p.pos_cnum - end_p.pos_bol - 1 (*]*)) ' ')

  (* ...(*@ foo *)
     (*@ bar *)...

     ~>

     ...[@@@gospel
     # linenb(foo_start)
         {| foo |}[@@gospel
     # linenb(bar_start)
      {| bar |}]
     # linenb(bar_end)
              ]...
  *)

  let print_nested_gospel start_p inner_start_p end_p outer_s inner_s =
    Fmt.str "[@@@@@@gospel\n\
# %d \"%s\"\n\
%s {|%s|}[@@@@gospel\n\
# %d \"%s\"\n\
%s {|%s|}]\n\
# %d \"%s\"\n\
%s]"
      start_p.Lexing.pos_lnum start_p.pos_fname
      (String.make (start_p.pos_cnum - start_p.pos_bol) ' ') outer_s
      inner_start_p.Lexing.pos_lnum inner_start_p.pos_fname
      (String.make (inner_start_p.pos_cnum - inner_start_p.pos_bol) ' ') inner_s
      end_p.Lexing.pos_lnum end_p.pos_fname
      (String.make (end_p.pos_cnum - end_p.pos_bol - 1 (*]*)) ' ')

  let rec print = function
  | Ghost (start_p, _, g) :: Spec (inner_start_p, end_p, s) :: l ->
    Fmt.str "%s%s"
      (print_nested_gospel start_p inner_start_p end_p g s) (print l)
  | Ghost (start_p, _, g) :: Spaces _ :: Spec (inner_start_p, end_p, s) :: l ->
    Fmt.str "%s%s"
      (print_nested_gospel start_p inner_start_p end_p g s) (print l)
  | Ghost (start_p, end_p, g) :: l ->
    Fmt.str "%s%s" (print_gospel `ThreeAt start_p end_p g) (print l)
  | Other o :: Spec (start_p, end_p, s) :: l ->
    Fmt.str "%s%s%s" o (print_gospel `TwoAt start_p end_p s) (print l)
  | Spec (start_p, end_p, s) :: l ->
    (* FIXME: we could fail right here *)
    Fmt.str "%s%s" (print_gospel `TwoAt start_p end_p s) (print l)
  | Other o :: Spaces sp :: Spec (start_p, end_p, s) :: l ->
    Fmt.str "%s%s%s%s" o sp (print_gospel `TwoAt start_p end_p s) (print l)
  | (Other s | Spaces s) :: l ->
    Fmt.str "%s%s" s (print l)
  | [] -> ""

  let collapse_spaces l =
    let b = Buffer.create 1024 in
    let rec loop = function
      | Spaces s :: l' ->
        Buffer.add_string b s; loop l'
      | elt :: l' ->
        if Buffer.length b > 0 then (
          let sp = Buffer.contents b in
          Buffer.clear b;
          Spaces sp :: elt :: loop l'
        ) else elt :: loop l'
      | [] ->
        if Buffer.length b > 0 then [Spaces (Buffer.contents b)] else []
    in
    loop l

  let flush () =
    push ();
    let l = Queue.fold (fun acc t -> t :: acc) [] queue in
    print (collapse_spaces (List.rev l))
}

let space = [ ' ' '\t' '\r' '\n' ]
let blank = [ ' ' '\t' ]
let newline = ('\n' | "\r\n")
let lowercase = [ 'a'-'z' '_' ]

rule scan = parse
  | blank+ as s
    { push (); Queue.push (Spaces s) queue; scan lexbuf }
  | newline as nl
    { push (); Lexing.new_line lexbuf; Queue.push (Spaces nl) queue; scan lexbuf }
  | "(*@"
    { push (); gospel (Lexing.lexeme_start_p lexbuf) lexbuf }
  | "(**)" {
    push ();
    let end_pos = Lexing.lexeme_end_p lexbuf in
    Queue.push (Empty_documentation end_pos) queue;
    scan lexbuf
    }
  | "(**" {
    push ();
    let start_pos = Lexing.lexeme_start_p lexbuf in
    comment lexbuf;
    let s = Buffer.contents buf in
    Buffer.clear buf;
    let end_pos = Lexing.lexeme_end_p lexbuf in
    Queue.push (Documentation (start_pos, end_pos, s)) queue;
    scan lexbuf
    }
  | "(*"
      {
        Buffer.add_string buf "(*";
        comment lexbuf;
        Buffer.add_string buf "*)";
        scan lexbuf
      }
  | _ as c { Buffer.add_char buf c; scan lexbuf }
  | eof { flush () }

and gospel start_pos = parse
  | blank+ as s   { Buffer.add_string buf s; gospel start_pos lexbuf }
  | newline as nl { Buffer.add_string buf nl; Lexing.new_line lexbuf; gospel start_pos lexbuf }
  | ("function" | "type" | "predicate" | "axiom" | "val" | "open" ) as k {
      Buffer.add_string buf k;
      comment lexbuf;
      let s = Buffer.contents buf in
      Buffer.clear buf;
      let end_pos = Lexing.lexeme_end_p lexbuf in
      Queue.push (Ghost (start_pos, end_pos, s)) queue;
      scan lexbuf
    }
  | "" {
      comment lexbuf;
      let s = Buffer.contents buf in
      Buffer.clear buf;
      let end_pos = Lexing.lexeme_end_p lexbuf in
      Queue.push (Spec (start_pos, end_pos, s)) queue;
      scan lexbuf
    }

and comment = parse
  | "*)" {}
  | "(*"
      {
        Buffer.add_string buf "(*";
        comment lexbuf;
        Buffer.add_string buf "*)";
        comment lexbuf
      }
  | "{" (lowercase* as delim) "|"
      {
        Buffer.add_char buf '{';
        Buffer.add_string buf delim;
        Buffer.add_char buf '|';
        quoted_string delim lexbuf;
        Buffer.add_char buf '|';
        Buffer.add_string buf delim;
        Buffer.add_char buf '}';
        comment lexbuf
      }
   | "\""
      {
        Buffer.add_char buf '\"';
        string lexbuf;
        Buffer.add_char buf '\"';
        comment lexbuf
      }
   | newline as nl { Buffer.add_string buf nl; Lexing.new_line lexbuf; comment lexbuf }
   | _ as c { Buffer.add_char buf c; comment lexbuf }

and string = parse
    '\"'
      { }
  | newline as nl
      {
        Lexing.new_line lexbuf;
        Buffer.add_string buf nl;
        string lexbuf
      }
   | _ as c { Buffer.add_char buf c; string lexbuf }

and quoted_string delim = parse
  | ("|" (lowercase* as edelim) "}" as s)
      { if delim = edelim then ()
        else (Buffer.add_string buf s; quoted_string delim lexbuf) }
  | newline as nl
      {
        Lexing.new_line lexbuf;
        Buffer.add_string buf nl;
        quoted_string delim lexbuf
      }
   | _ as c { Buffer.add_char buf c; quoted_string delim lexbuf }
{
  let run lb =
    clear ();
    scan lb
}
