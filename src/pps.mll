(* Coding-style convention to be able to reindent this file easily:
   - the curly braces that surround OCaml code blocks are written on separate
     lines,
   - OCaml rule code starts with a '{' at the end of a line and preceded by a space
     and ends with 4 spaces and a '}' on a line by themselves,
   - raw OCaml code starts with a '{' and ends with a '}' all by themselves on
     their lines (no indentation, etc.)
*)

{
  (** Kinds of space:
      - [Large] is one that would detach a documentation from a value in the
        OCaml compiler, i.e. one with at least one blank line (a newline
        followed by zero, one or more blanks and then a newline)
      - [Nl] is some space which is not large but ends with a newline
      - [Comment] is some space which is not large but ends with a comment
      - [Blk] is some space which is not large but ends with blanks without a
        newline just before *)
  type space_kind = Nl | Blk | Comment | Large

  type t =
    | Ghost of Lexing.position * Lexing.position * string
    | Spec of Lexing.position * Lexing.position * string
    | Documentation of Lexing.position * Lexing.position * string
    | Empty_documentation of Lexing.position
    | Other of string
    | Spaces of space_kind * string

  let stdlib_file = "stdlib.mli"
  let queue = Queue.create ()
  let buf = Buffer.create 1024
  let clear () = Queue.clear queue

  let push () =
    if Buffer.length buf > 0 then (
      Queue.push (Other (Buffer.contents buf)) queue;
      Buffer.clear buf)

  let print_directive where pos ppf =
    let open Lexing in
    match where with
    | `Open ->
        Fmt.pf ppf "\n# %d \"%s\"\n%s" pos.pos_lnum pos.pos_fname
          (* Align with the first character *)
          (String.make (pos.pos_cnum - pos.pos_bol) ' ')
    | `Close ->
        Fmt.pf ppf "\n# %d \"%s\"\n%s" pos.pos_lnum pos.pos_fname
          (* Align with the last character *)
          (String.make (pos.pos_cnum - pos.pos_bol - 1) ' ')
    | `Beginning -> Fmt.pf ppf "# %d \"%s\"\n" pos.pos_lnum pos.pos_fname

  (* ...(*@ foo *)...

     ~>

     ...[@@@gospel
     # linenb
         {| foo |}
     # linenb
                 ]...
  *)
  let print_gospel (lvl : [ `TwoAt | `ThreeAt ]) start_p end_p s ppf =
    Fmt.pf ppf "[%sgospel%t {|%s|}%t]"
      (match lvl with `TwoAt -> "@@" | `ThreeAt -> "@@@")
      (print_directive `Open start_p)
      s
      (print_directive `Close end_p)
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

  let print_nested_gospel start_p inner_start_p end_p outer_s inner_s ppf =
    Fmt.pf ppf "[@@@@@@gospel%t {|%s|}[@@@@gospel%t {|%s|}]%t]"
      (print_directive `Open start_p)
      outer_s
      (print_directive `Open inner_start_p)
      inner_s
      (print_directive `Close end_p)

  let print_documentation_attribute lvl start_p end_p s ppf =
    Fmt.pf ppf "[%s%t {|%s|}%t]"
      (match lvl with `TwoAt -> "@@ocaml.doc" | `ThreeAt -> "@@@ocaml.text")
      (print_directive `Open start_p)
      s
      (print_directive `Close end_p)

  let print_empty_documentation end_p ppf =
    Fmt.pf ppf "[@@@ocaml.doc%t]" (print_directive `Close end_p)

  let print_triplet o sp doc sp' start_p end_p s ppf =
    let docstring =
      match doc with
      | Documentation (start_p, end_p, d) ->
          print_documentation_attribute `TwoAt start_p end_p d
      | Empty_documentation end_p -> print_empty_documentation end_p
      | _ -> assert false
    in
    Fmt.pf ppf "%s%s%t%s%t" o sp docstring sp'
      (print_gospel `TwoAt start_p end_p s)

  let rec print ppf = function
    (* Documentation-Ghost-Spec interleaved with Spaces *)
    | Documentation (doc_start_p, doc_end_p, d)
      :: Spaces (k, sp)
      :: Ghost (start_p, _, g)
      :: Spaces (k', sp')
      :: Spec (inner_start_p, end_p, s)
      :: l
      when k <> Large && k' <> Large ->
        Fmt.pf ppf "%t%s%t%s"
          (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
          sp
          (print_nested_gospel start_p inner_start_p end_p g s)
          sp';
        print ppf l
    (* Documentation-Ghost interleaved with Spaces *)
    | Documentation (doc_start_p, doc_end_p, d)
      :: Spaces (k, sp)
      :: Ghost (start_p, end_p, g)
      :: l
      when k <> Large ->
        Fmt.pf ppf "%t%s%t"
          (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
          sp
          (print_gospel `ThreeAt start_p end_p g);
        print ppf l
    (* Ghost-Spec-Documentation interleaved with Spaces *)
    | Ghost (start_p, _, g)
      :: Spaces (k, _)
      :: Spec (inner_start_p, end_p, s)
      :: Spaces (k', sp')
      :: Documentation (doc_start_p, doc_end_p, d)
      :: l
      when k <> Large && k' <> Large ->
        Fmt.pf ppf "%t%s%t"
          (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
          sp'
          (print_nested_gospel start_p inner_start_p end_p g s);
        print ppf l
    (* Ghost-Documentation-Spec interleaved with Spaces *)
    | Ghost (start_p, _, g)
      :: Spaces (k, _)
      :: Documentation (doc_start_p, doc_end_p, d)
      :: Spaces (k', _)
      :: Spec (inner_start_p, end_p, s)
      :: l
      when k <> Large && k' <> Large ->
        Fmt.pf ppf "%t%t"
          (print_nested_gospel start_p inner_start_p end_p g s)
          (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d);
        print ppf l
    (* Ghost-Documentation interleaved with Spaces *)
    | Ghost (start_p, end_p, g)
      :: Spaces (k, sp)
      :: Documentation (doc_start_p, doc_end_p, d)
      :: l
      when k <> Large ->
        Fmt.pf ppf "%t%s%t"
          (print_gospel `ThreeAt start_p end_p g)
          sp
          (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d);
        print ppf l
    | Ghost (start_p, _, g)
      :: Spaces (k, _)
      :: Spec (inner_start_p, end_p, s)
      :: l
      when k <> Large ->
        print_nested_gospel start_p inner_start_p end_p g s ppf;
        print ppf l
    | Ghost (start_p, end_p, g) :: l ->
        print_gospel `ThreeAt start_p end_p g ppf;
        print ppf l
    | Spec (start_p, end_p, s) :: l ->
        (* FIXME: we could fail right here *)
        print_gospel `TwoAt start_p end_p s ppf;
        print ppf l
    | Other o :: Spaces (_, sp) :: Spec (start_p, end_p, s) :: l ->
        Fmt.pf ppf "%s%s%t" o sp (print_gospel `TwoAt start_p end_p s);
        print ppf l
    | Other o
      :: Spaces (_, sp)
      :: ((Documentation (_, _, _) | Empty_documentation _) as doc)
      :: Spaces (_, sp')
      :: Spec (start_p, end_p, s)
      :: l ->
        print_triplet o sp doc sp' start_p end_p s ppf;
        print ppf l
    | (Other s | Spaces (_, s)) :: l ->
        Fmt.pf ppf "%s" s;
        print ppf l
    | Documentation (_, _, s) :: l ->
        Fmt.pf ppf "(**%s*)" s;
        print ppf l
    | Empty_documentation _ :: l ->
        Fmt.pf ppf "(**)";
        print ppf l
    | [] -> ()

  (** Collapse spaces and hence computes the space kind *)
  let collapse_spaces l =
    let b = Buffer.create 1024 and curk = ref Blk in
    let update_curk k =
      curk :=
        match (!curk, k) with
        | Large, _ | _, Large -> Large
        | Blk, x | Comment, x -> x
        | Nl, Nl -> Large
        | Nl, Blk -> Nl
        | Nl, Comment -> Comment
    in
    let rec loop acc = function
      | Spaces (k, s) :: l' ->
          Buffer.add_string b s;
          update_curk k;
          loop acc l'
      | elt :: l' ->
          let sp = Buffer.contents b in
          Buffer.clear b;
          let k = !curk in
          curk := Blk;
          loop (elt :: Spaces (k, sp) :: acc) l'
      | [] ->
          List.rev
            (if Buffer.length b > 0 then
               Spaces (!curk, Buffer.contents b) :: acc
             else acc)
    in
    loop [] l

  let flush ppf =
    push ();
    let l = Queue.fold (fun acc t -> t :: acc) [] queue in
    print ppf (collapse_spaces (List.rev l))
}

let space = [ ' ' '\t' '\r' '\n' ]
let blank = [ ' ' '\t' ]
let newline = ('\n' | "\r\n")
let lowercase = [ 'a'-'z' '_' ]

rule scan ppf = parse
  | blank+ as s {
      push ();
      Queue.push (Spaces (Blk, s)) queue;
      scan ppf lexbuf
    }
  | newline as nl {
      push ();
      Lexing.new_line lexbuf;
      Queue.push (Spaces (Nl, nl)) queue;
      scan ppf lexbuf
    }
  | "(*@" {
      push ();
      gospel ppf (Lexing.lexeme_start_p lexbuf) lexbuf
    }
  | "(**)" {
      push ();
      let end_pos = Lexing.lexeme_end_p lexbuf in
      Queue.push (Empty_documentation end_pos) queue;
      scan ppf lexbuf
    }
  | "(**" {
      push ();
      let start_pos = Lexing.lexeme_start_p lexbuf in
      comment lexbuf;
      let s = Buffer.contents buf in
      Buffer.clear buf;
      let end_pos = Lexing.lexeme_end_p lexbuf in
      Queue.push (Documentation (start_pos, end_pos, s)) queue;
      scan ppf lexbuf
    }
  (* When lexing stdlib, we stop here just like ocamldep *)
  | "(*MODULE_ALIASES*)" as s {
      if
        Filename.basename (Lexing.lexeme_start_p lexbuf).pos_fname = stdlib_file
      then flush ppf
      else (
        Buffer.add_string buf s;
        scan ppf lexbuf)
    }
  | "(*" {
      push ();
      Buffer.add_string buf "(*";
      comment lexbuf;
      Buffer.add_string buf "*)";
      Queue.push (Spaces (Comment, Buffer.contents buf)) queue;
      Buffer.clear buf;
      scan ppf lexbuf
    }
  | "#" as c {
      Buffer.add_char buf c;
      let pos = Lexing.lexeme_start_p lexbuf in
      if pos.pos_cnum = pos.pos_bol then directive ppf lexbuf
      else scan ppf lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      scan ppf lexbuf
    }
  | eof {
      flush ppf
    }

and gospel ppf start_pos = parse
  | blank+ as s {
      Buffer.add_string buf s;
      gospel ppf start_pos lexbuf
    }
  | newline as nl {
      Buffer.add_string buf nl;
      Lexing.new_line lexbuf;
      gospel ppf start_pos lexbuf
    }
  | (("function" | "type" | "predicate" | "axiom" | "val" | "open" ) blank+) as k {
      Buffer.add_string buf k;
      comment lexbuf;
      let s = Buffer.contents buf in
      Buffer.clear buf;
      let end_pos = Lexing.lexeme_end_p lexbuf in
      Queue.push (Ghost (start_pos, end_pos, s)) queue;
      scan ppf lexbuf
    }
  | "" {
      comment lexbuf;
      let s = Buffer.contents buf in
      Buffer.clear buf;
      let end_pos = Lexing.lexeme_end_p lexbuf in
      Queue.push (Spec (start_pos, end_pos, s)) queue;
      scan ppf lexbuf
    }

and comment = parse
  | "*)" {}
  | "(*" {
      Buffer.add_string buf "(*";
      comment lexbuf;
      Buffer.add_string buf "*)";
      comment lexbuf
    }
  | "{" (lowercase* as delim) "|" {
      Buffer.add_char buf '{';
      Buffer.add_string buf delim;
      Buffer.add_char buf '|';
      quoted_string delim lexbuf;
      Buffer.add_char buf '|';
      Buffer.add_string buf delim;
      Buffer.add_char buf '}';
      comment lexbuf
    }
  | "\"" {
      Buffer.add_char buf '"';
      string lexbuf;
      Buffer.add_char buf '"';
      comment lexbuf
    }
  | newline as nl {
      Buffer.add_string buf nl;
      Lexing.new_line lexbuf;
      comment lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      comment lexbuf
    }

and directive ppf = parse
  | ( [' ' '\t']*
      (['0'-'9']+ as line)
      [' ' '\t']*
      ('"' ([^ '"' '\010' '\013']* as file) '"')
      [^ '\010' '\013']*
    ) as directive {
      Buffer.add_string buf directive;
      match int_of_string_opt line with
      | Some line ->
          let pos = lexbuf.lex_curr_p in
          let pos =
            {
              pos with
              pos_fname = file;
              pos_lnum = line - 1;
              pos_bol = pos.pos_cnum;
            }
          in
          lexbuf.lex_curr_p <- pos;
          scan ppf lexbuf
      | None -> scan ppf lexbuf
    }
  | "" {
      scan ppf lexbuf
    }

and string = parse
  | '"' {}
  | newline as nl {
      Lexing.new_line lexbuf;
      Buffer.add_string buf nl;
      string lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      string lexbuf
    }

and quoted_string delim = parse
  | ("|" (lowercase* as edelim) "}" as s) {
      if delim = edelim then ()
      else (
        Buffer.add_string buf s;
        quoted_string delim lexbuf)
    }
  | newline as nl {
      Lexing.new_line lexbuf;
      Buffer.add_string buf nl;
      quoted_string delim lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      quoted_string delim lexbuf
    }

{
  let run ppf lb =
    clear ();
    print_directive `Beginning lb.Lexing.lex_curr_p ppf;
    scan ppf lb
}
