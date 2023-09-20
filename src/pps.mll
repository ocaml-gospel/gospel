{
 type t =
   | Ghost of Lexing.position * Lexing.position * string
   | Spec of Lexing.position * Lexing.position * string
   | Documentation of Lexing.position * Lexing.position * string
   | Empty_documentation of Lexing.position
   | Other of string
   | Spaces of string

 let is_small sp =
   let is_nl = Char.equal '\n' and b = ref false and l = String.length sp in
   try
     for i = 0 to l - 1 do
       if is_nl sp.[i] then if !b then raise Exit else b := true
     done;
     true
   with Exit -> false

 let stdlib_file = "stdlib.mli"
 let queue = Queue.create ()
 let buf = Buffer.create 1024
 let clear () = Queue.clear queue

 let push () =
   if Buffer.length buf > 0 then (
     Queue.push (Other (Buffer.contents buf)) queue;
     Buffer.clear buf)

 (* ...(*@ foo *)...

    ~>

    ...[@@@gospel
    # linenb
        {| foo |}
    # linenb
                ]...
 *)
 let print_gospel (lvl : [ `TwoAt | `ThreeAt ]) start_p end_p s =
   Fmt.str "[%sgospel\n# %d \"%s\"\n%s {|%s|}\n# %d \"%s\"\n%s]"
     (match lvl with `TwoAt -> "@@" | `ThreeAt -> "@@@")
     start_p.Lexing.pos_lnum start_p.pos_fname
     (String.make (start_p.pos_cnum - start_p.pos_bol) ' ')
     s end_p.Lexing.pos_lnum end_p.pos_fname
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
   Fmt.str
     "[@@@@@@gospel\n\
      # %d \"%s\"\n\
      %s {|%s|}[@@@@gospel\n\
      # %d \"%s\"\n\
      %s {|%s|}]\n\
      # %d \"%s\"\n\
      %s]"
     start_p.Lexing.pos_lnum start_p.pos_fname
     (String.make (start_p.pos_cnum - start_p.pos_bol) ' ')
     outer_s inner_start_p.Lexing.pos_lnum inner_start_p.pos_fname
     (String.make (inner_start_p.pos_cnum - inner_start_p.pos_bol) ' ')
     inner_s end_p.Lexing.pos_lnum end_p.pos_fname
     (String.make (end_p.pos_cnum - end_p.pos_bol - 1 (*]*)) ' ')

 let print_documentation_attribute lvl start_p end_p s =
   Fmt.str "[%s\n# %d \"%s\"\n%s {|%s|}\n# %d \"%s\"\n%s]"
     (match lvl with `TwoAt -> "@@ocaml.doc" | `ThreeAt -> "@@@ocaml.text")
     start_p.Lexing.pos_lnum start_p.pos_fname
     (String.make (start_p.pos_cnum - start_p.pos_bol) ' ')
     s end_p.Lexing.pos_lnum end_p.pos_fname
     (String.make (end_p.pos_cnum - end_p.pos_bol - 1 (*]*)) ' ')

 let print_empty_documentation end_p =
   Fmt.str "[@@@ocaml.doc\n# %d \"%s\"\n%s]" end_p.Lexing.pos_lnum
     end_p.pos_fname
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
     :: Spaces sp
     :: Ghost (start_p, _, g)
     :: Spaces sp'
     :: Spec (inner_start_p, end_p, s)
     :: l
     when is_small sp && is_small sp' ->
       Fmt.str "%s%s%s%s%s"
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         sp
         (print_nested_gospel start_p inner_start_p end_p g s)
         sp' (print l)
   (* Documentation-Ghost interleaved with Spaces *)
   | Documentation (doc_start_p, doc_end_p, d)
     :: Spaces sp
     :: Ghost (start_p, end_p, g)
     :: l
     when is_small sp ->
       Fmt.str "%s%s%s%s"
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         sp
         (print_gospel `ThreeAt start_p end_p g)
         (print l)
   (* Ghost-Spec-Documentation interleaved with Spaces *)
   | Ghost (start_p, _, g)
     :: Spaces sp
     :: Spec (inner_start_p, end_p, s)
     :: Spaces sp'
     :: Documentation (doc_start_p, doc_end_p, d)
     :: l
     when is_small sp && is_small sp' ->
       Fmt.str "%s%s%s%s"
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         sp'
         (print_nested_gospel start_p inner_start_p end_p g s)
         (print l)
   (* Ghost-Documentation-Spec interleaved with Spaces *)
   | Ghost (start_p, _, g)
     :: Spaces sp
     :: Documentation (doc_start_p, doc_end_p, d)
     :: Spaces sp'
     :: Spec (inner_start_p, end_p, s)
     :: l
     when is_small sp && is_small sp' ->
       Fmt.str "%s%s%s"
         (print_nested_gospel start_p inner_start_p end_p g s)
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         (print l)
   (* Ghost-Documentation interleaved with Spaces *)
   | Ghost (start_p, end_p, g)
     :: Spaces sp
     :: Documentation (doc_start_p, doc_end_p, d)
     :: l
     when is_small sp ->
       Fmt.str "%s%s%s%s"
         (print_gospel `ThreeAt start_p end_p g)
         sp
         (print_documentation_attribute `ThreeAt doc_start_p doc_end_p d)
         (print l)
   | Ghost (start_p, _, g) :: Spaces sp :: Spec (inner_start_p, end_p, s) :: l
     when is_small sp ->
       Fmt.str "%s%s"
         (print_nested_gospel start_p inner_start_p end_p g s)
         (print l)
   | Ghost (start_p, end_p, g) :: l ->
       Fmt.str "%s%s" (print_gospel `ThreeAt start_p end_p g) (print l)
   | Spec (start_p, end_p, s) :: l ->
       (* FIXME: we could fail right here *)
       Fmt.str "%s%s" (print_gospel `TwoAt start_p end_p s) (print l)
   | Other o :: Spaces sp :: Spec (start_p, end_p, s) :: l ->
       Fmt.str "%s%s%s%s" o sp (print_gospel `TwoAt start_p end_p s) (print l)
   | Other o
     :: Spaces sp
     :: ((Documentation (_, _, _) | Empty_documentation _) as doc)
     :: Spaces sp'
     :: Spec (start_p, end_p, s)
     :: l ->
       Fmt.str "%s%s" (print_triplet o sp doc sp' start_p end_p s) (print l)
   | (Other s | Spaces s) :: l -> Fmt.str "%s%s" s (print l)
   | Documentation (_, _, s) :: l -> Fmt.str "(**%s*)%s" s (print l)
   | Empty_documentation _ :: l -> Fmt.str "(**)%s" (print l)
   | [] -> ""

 let collapse_spaces l =
   let b = Buffer.create 1024 in
   let rec loop = function
     | Spaces s :: l' ->
         Buffer.add_string b s;
         loop l'
     | elt :: l' ->
         let sp = Buffer.contents b in
         Buffer.clear b;
         Spaces sp :: elt :: loop l'
     | [] -> if Buffer.length b > 0 then [ Spaces (Buffer.contents b) ] else []
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
        Queue.push (Spaces (Buffer.contents buf)) queue;
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
