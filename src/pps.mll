{
 (** Kinds of space:
     - [Large] is one that would detach a documentation from a value in the OCaml
       compiler, i.e. one with at least one blank line (a newline followed by
       zero, one or more blanks and then a newline)
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

 let directive pos =
   let open Lexing in
   Fmt.str "\n# %d \"%s\"\n" pos.pos_lnum pos.pos_fname

 (* ...(*@ foo *)...

    ~>

    ...[@@@gospel
    # linenb
        {| foo |}
    # linenb
                ]...
 *)
 let print_gospel (lvl : [ `TwoAt | `ThreeAt ]) start_p end_p s =
   Fmt.str "[%sgospel%s%s {|%s|}%s%s]"
     (match lvl with `TwoAt -> "@@" | `ThreeAt -> "@@@")
     (directive start_p)
     (String.make (start_p.pos_cnum - start_p.pos_bol) ' ')
     s (directive end_p)
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
   Fmt.str "[@@@@@@gospel%s%s {|%s|}[@@@@gospel%s%s {|%s|}]%s%s]"
     (directive start_p)
     (String.make (start_p.pos_cnum - start_p.pos_bol) ' ')
     outer_s (directive inner_start_p)
     (String.make (inner_start_p.pos_cnum - inner_start_p.pos_bol) ' ')
     inner_s (directive end_p)
     (String.make (end_p.pos_cnum - end_p.pos_bol - 1 (*]*)) ' ')

 let print_documentation_attribute lvl start_p end_p s =
   Fmt.str "[%s%s%s {|%s|}%s%s]"
     (match lvl with `TwoAt -> "@@ocaml.doc" | `ThreeAt -> "@@@ocaml.text")
     (directive start_p)
     (String.make (start_p.pos_cnum - start_p.pos_bol) ' ')
     s (directive end_p)
     (String.make (end_p.pos_cnum - end_p.pos_bol - 1 (*]*)) ' ')

 let print_empty_documentation end_p =
   Fmt.str "[@@@ocaml.doc%s%s]" (directive end_p)
     (String.make (end_p.pos_cnum - end_p.pos_bol - 1 (*]*)) ' ')

 let print_triplet o sp doc sp' start_p end_p s =
   let docstring =
     match doc with
     | Documentation (start_p, end_p, d) ->
         print_documentation_attribute `TwoAt start_p end_p d
     | Empty_documentation end_p -> print_empty_documentation end_p
     | _ -> assert false
   in
   Fmt.str "%s%s%s%s%s" o sp docstring sp' (print_gospel `TwoAt start_p end_p s)

 let rec print = function
   (* Documentation-Ghost-Spec interleaved with Spaces *)
   | Documentation (doc_start_p, doc_end_p, d)
     :: Spaces (k, sp)
     :: Ghost (start_p, _, g)
     :: Spaces (k', sp')
     :: Spec (inner_start_p, end_p, s)
     :: l
     when k <> Large && k' <> Large ->
       Fmt.str "%s%s%s%s%s"
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         sp
         (print_nested_gospel start_p inner_start_p end_p g s)
         sp' (print l)
   (* Documentation-Ghost interleaved with Spaces *)
   | Documentation (doc_start_p, doc_end_p, d)
     :: Spaces (k, sp)
     :: Ghost (start_p, end_p, g)
     :: l
     when k <> Large ->
       Fmt.str "%s%s%s%s"
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         sp
         (print_gospel `ThreeAt start_p end_p g)
         (print l)
   (* Ghost-Spec-Documentation interleaved with Spaces *)
   | Ghost (start_p, _, g)
     :: Spaces (k, _)
     :: Spec (inner_start_p, end_p, s)
     :: Spaces (k', sp')
     :: Documentation (doc_start_p, doc_end_p, d)
     :: l
     when k <> Large && k' <> Large ->
       Fmt.str "%s%s%s%s"
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         sp'
         (print_nested_gospel start_p inner_start_p end_p g s)
         (print l)
   (* Ghost-Documentation-Spec interleaved with Spaces *)
   | Ghost (start_p, _, g)
     :: Spaces (k, _)
     :: Documentation (doc_start_p, doc_end_p, d)
     :: Spaces (k', _)
     :: Spec (inner_start_p, end_p, s)
     :: l
     when k <> Large && k' <> Large ->
       Fmt.str "%s%s%s"
         (print_nested_gospel start_p inner_start_p end_p g s)
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         (print l)
   (* Ghost-Documentation interleaved with Spaces *)
   | Ghost (start_p, end_p, g)
     :: Spaces (k, sp)
     :: Documentation (doc_start_p, doc_end_p, d)
     :: l
     when k <> Large ->
       Fmt.str "%s%s%s%s"
         (print_gospel `ThreeAt start_p end_p g)
         sp
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         (print l)
   | Ghost (start_p, _, g)
     :: Spaces (k, _)
     :: Spec (inner_start_p, end_p, s)
     :: l
     when k <> Large ->
       Fmt.str "%s%s"
         (print_nested_gospel start_p inner_start_p end_p g s)
         (print l)
   | Ghost (start_p, end_p, g) :: l ->
       Fmt.str "%s%s" (print_gospel `ThreeAt start_p end_p g) (print l)
   | Spec (start_p, end_p, s) :: l ->
       (* FIXME: we could fail right here *)
       Fmt.str "%s%s" (print_gospel `TwoAt start_p end_p s) (print l)
   | Other o :: Spaces (_, sp) :: Spec (start_p, end_p, s) :: l ->
       Fmt.str "%s%s%s%s" o sp (print_gospel `TwoAt start_p end_p s) (print l)
   | Other o
     :: Spaces (_, sp)
     :: ((Documentation (_, _, _) | Empty_documentation _) as doc)
     :: Spaces (_, sp')
     :: Spec (start_p, end_p, s)
     :: l ->
       Fmt.str "%s%s" (print_triplet o sp doc sp' start_p end_p s) (print l)
   | (Other s | Spaces (_, s)) :: l -> Fmt.str "%s%s" s (print l)
   | Documentation (_, _, s) :: l -> Fmt.str "(**%s*)%s" s (print l)
   | Empty_documentation _ :: l -> Fmt.str "(**)%s" (print l)
   | [] -> ""

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
   let rec loop = function
     | Spaces (k, s) :: l' ->
         Buffer.add_string b s;
         update_curk k;
         loop l'
     | elt :: l' ->
         let sp = Buffer.contents b in
         Buffer.clear b;
         let k = !curk in
         curk := Blk;
         Spaces (k, sp) :: elt :: loop l'
     | [] ->
         if Buffer.length b > 0 then [ Spaces (!curk, Buffer.contents b) ]
         else []
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
    { push (); Queue.push (Spaces (Blk,s)) queue; scan lexbuf }
  | newline as nl
    { push (); Lexing.new_line lexbuf; Queue.push (Spaces (Nl,nl)) queue; scan lexbuf }
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
  (* When lexing stdlib, we stop here just like ocamldep *)
  | "(*MODULE_ALIASES*)" as s {
    if Filename.basename (Lexing.lexeme_start_p lexbuf).pos_fname = stdlib_file
    then flush ()
    else (
      Buffer.add_string buf s;
      scan lexbuf)
    }
  | "(*"
      {
        push ();
        Buffer.add_string buf "(*";
        comment lexbuf;
        Buffer.add_string buf "*)";
        Queue.push (Spaces (Comment, Buffer.contents buf)) queue;
        Buffer.clear buf;
        scan lexbuf
      }
  | "#" as c {
    Buffer.add_char buf c;
    let pos = Lexing.lexeme_start_p lexbuf in
    if pos.pos_cnum = pos.pos_bol then directive lexbuf else scan lexbuf
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

and directive = parse
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
          { pos with pos_fname = file; pos_lnum = line - 1; pos_bol = pos.pos_cnum }
        in
        lexbuf.lex_curr_p <- pos;
        scan lexbuf
      | None ->
        scan lexbuf
    }
  | "" { scan lexbuf }

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
