(* $Id: transf.mll,v 1.11 2009-03-03 09:53:08 uid562 Exp $ *)

{ open Lexing;;
  let idx = Buffer.create 5
  let full_kw = Buffer.create 5

  let modern = ref false

  let escape_keyword s =
    let buf = Buffer.create 5 in
    String.iter
      (function
           c when ('A' <= c && c <= 'Z') ||
                  ('a' <= c && c <= 'z') ||
                  ('0' <= c && c <= '9')
             -> Buffer.add_char buf c
         | c -> Buffer.add_string buf
             (Printf.sprintf "\\char%d" (int_of_char c))) s;
    Buffer.contents buf

  let make_keyword () =
    let keyword = Buffer.contents full_kw in
    let index = Buffer.contents idx in
    print_string "\\addspace";
    if !modern then
      Printf.printf
        "\\lstinline$%s$" keyword
    else
      Printf.printf "\\texttt{%s}" (escape_keyword keyword);
    if String.length index > 1 then
      Printf.printf "\\indextt%s{%s}"
        (if keyword.[0] = '\\' then "bs" else "") index;
    print_string "\\spacetrue";
    Buffer.clear idx;
    Buffer.clear full_kw
}

rule main = parse
    "\\begin{syntax}" {
      print_string "\\begin{syntax}";
      syntax lexbuf }
  | "\\@" {
      print_string "@";
      main lexbuf }
  | _ {
      print_char (lexeme_char lexbuf 0); main lexbuf }
  | eof {
      () }

and syntax = parse
    "\\end{syntax}" {
      print_string "\\end{syntax}";
      main lexbuf }
  | ";" ([^ '\n']* as s) '\n' [' ''\t']* '|' {
      print_string "& \\textrm{";
      print_string s;
      print_string "} \\alt";
      syntax lexbuf }
  | ";" ([^ '\n']* as s) '\n' [' ''\t']* '\\' [' ''\t']* '\n' {
      print_string "& \\textrm{";
      print_string s;
      print_string "} \\sep";
      syntax lexbuf }
  | ";" ([^ '\n']* as s) "%\n" {
      print_string "& \\textrm{";
      print_string s;
      print_string "}";
      syntax lexbuf }
  | ";" ([^ '\n']* as s) '\n' {
      print_string "& \\textrm{";
      print_string s;
      print_string "} \\newl";
      syntax lexbuf }
  | "@" {
      print_string "}";
      main lexbuf }
  | '\'' {
      Buffer.clear idx;
      Buffer.clear full_kw;
      inquote lexbuf }
  | '"' {
      Buffer.clear idx;
      Buffer.clear full_kw;
      indoublequote lexbuf }
  | "below" { print_string "\\below"; syntax lexbuf }
  | "epsilon" { print_string "\\emptystring"; syntax lexbuf }
  | ['A'-'Z''a'-'z'] ['A'-'Z''a'-'z''-']* {
      print_string "\\nonterm{";
      print_string (lexeme lexbuf);
      print_string"}";
      check_nonterm_note lexbuf }
  | '\\' ['a'-'z''A'-'Z'] + {
      print_string (lexeme lexbuf);
      syntax lexbuf }
  | ['_' '^'] _ {
      print_string (lexeme lexbuf);
      syntax lexbuf }
  | '-' { print_string "\\interval"; syntax lexbuf }
  | "*" { print_string "\\repetstar"; syntax lexbuf }
  | "+" { print_string "\\repetplus"; syntax lexbuf }
  | "?" { print_string "\\repetone"; syntax lexbuf }
  | "(" { print_string "\\lparen"; syntax lexbuf }
  | ")" { print_string "\\rparen"; syntax lexbuf }
  | "::=" { print_string "\\is"; syntax lexbuf }
  | "|" { print_string "\\orelse"; syntax lexbuf }
  | "\\" { print_string "\\sep"; syntax lexbuf }
  | "{" { print_string "\\begin{notimplementedenv}"; check_implementation_note lexbuf }
  | "}" { print_string "\\end{notimplementedenv}"; syntax lexbuf }
  | [' ''\t'] { syntax lexbuf }
  | _ {
      print_char (lexeme_char lexbuf 0);
      syntax lexbuf }

and inquote = parse
    ['A'-'Z' 'a'-'z' '0'-'9' '?'] as c {
      Buffer.add_char full_kw c;
      Buffer.add_char idx c;
      inquote lexbuf }
  | '\'' {
      make_keyword ();
      syntax lexbuf }
  | '_' {
      Buffer.add_char full_kw '_';
      Buffer.add_string idx "\\_";
      inquote lexbuf
    }
  | _ as c {
      Buffer.add_char full_kw c;
      inquote lexbuf }

and indoublequote = parse
    ['A'-'Z' 'a'-'z' '0'-'9' '?'] as c {
      Buffer.add_char full_kw c;
      Buffer.add_char idx c;
      indoublequote lexbuf }
  | '"' {
      make_keyword();
      syntax lexbuf }
  | '_' {
      Buffer.add_char full_kw '_';
      Buffer.add_string idx "\\_";
      indoublequote lexbuf
    }
  | _ as c {
      Buffer.add_char full_kw c;
      indoublequote lexbuf }
and check_implementation_note = parse
  | "[" { print_string "["; implementation_note lexbuf }
  | "" { syntax lexbuf }
and implementation_note = parse
    "]" { print_string "]"; syntax lexbuf }
  | _  { print_char (lexeme_char lexbuf 0);
           implementation_note lexbuf }
and check_nonterm_note = parse
  | "[" { print_string "{"; nonterm_note lexbuf }
  | ""  { print_string "{}"; syntax lexbuf }
and nonterm_note = parse
    "]" { print_string "}"; syntax lexbuf }
  | _  { print_char (lexeme_char lexbuf 0);
           nonterm_note lexbuf }

{

  let () = Arg.parse
    [ "-modern", Arg.Set modern, "set modern style"; ]
    (fun f ->
       let cin = open_in f in
       let lb = from_channel cin in
       main lb;
       close_in cin)
    "transf [-modern] file"

}
