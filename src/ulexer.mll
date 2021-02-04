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
  open Ppxlib
  open Lexing
  open Uparser

  type error =
    | IllegalCharacter of char
    | UnterminatedComment

  exception Error of error * Location.t

  let prepare_error loc = function
    | IllegalCharacter c ->
      Fmt.kstr (fun str -> Some (Location.Error.make ~loc ~sub:[] str))
        "illegal character %c" c
    | UnterminatedComment ->
      Fmt.kstr (fun str -> Some (Location.Error.make ~loc ~sub:[] str))
        "unterminated comment"

  let () =
    Location.Error.register_error_of_exn (function
       | Error (err, loc) -> prepare_error loc err
       | _ -> None)

  (* let () = Exn_printer.register (fun fmt e -> match e with
   *   | IllegalCharacter c -> fprintf fmt "illegal character %c" c
   *   | UnterminatedComment -> fprintf fmt "unterminated comment"
   *   | _ -> raise e) *)

 let keywords = Hashtbl.create 97
  let () =
    List.iter
      (fun (x,y) -> Hashtbl.add keywords x y)
      [
        "as", AS;
        "axiom", AXIOM;
        "coercion", COERCION;
        "else", ELSE;
        "exists", EXISTS;
        "false", FALSE;
        "forall", FORALL;
        "function", FUNCTION;
        "if", IF;
        "in", IN;
        "invariant", INVARIANT;
        "let", LET;
        "match", MATCH;
        "not", NOT;
        "open", OPEN;
        "predicate", PREDICATE;
        "then", THEN;
        "true", TRUE;
        "type", TYPE;
        "with", WITH;
        "mutable", MUTABLE;
        (* programs *)
        "ensures", ENSURES;
        "consumes", CONSUMES;
        "fun", FUN;
        "old", OLD;
        "raises", RAISES;
        (*"reads", READS;*)
        "rec", REC;
        "requires", REQUIRES;
        "variant", VARIANT;
        "val", VAL;
        "modifies", MODIFIES;
        "equivalent", EQUIVALENT;
        "checks", CHECKS;
        "diverges", DIVERGES;
        (* vocal *)
        "ephemeral", EPHEMERAL;
        "model", MODEL;
      ]

  let char_for_backslash = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | c -> c

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}


let newline = '\n'
let space = [' ' '\t' '\r']
let lalpha = ['a'-'z' '_']
let ualpha = ['A'-'Z']
let alpha = lalpha | ualpha
let digit = ['0'-'9']
let lident = lalpha (alpha | digit | '\'')*
let uident = ualpha (alpha | digit | '\'')*
let hexadigit = ['0'-'9' 'a'-'f' 'A'-'F']

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?
let hex_float_literal =
  '0' ['x' 'X']
  ['0'-'9' 'A'-'F' 'a'-'f'] ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  ('.' ['0'-'9' 'A'-'F' 'a'-'f' '_']* )?
  (['p' 'P'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

let op_char_1 = ['=' '<' '>' '~']
let op_char_2 = ['+' '-']
let op_char_3 = ['*' '/' '\\' '%']
let op_char_4 = ['!' '$' '&' '?' '@' '^' '.' ':' '|' '#']
let op_char_1234 = op_char_1 | op_char_2 | op_char_3 | op_char_4
let op_char_234  = op_char_2 | op_char_3 | op_char_4
let op_char_34   = op_char_3 | op_char_4

let op_char_pref = ['!' '?']


rule token = parse
  | "[@" space* ([^ ' ' '\n' ']']+ (' '+ [^ ' ' '\n' ']']+)* as lbl) space* ']'
      { ATTRIBUTE lbl }
  | newline
      { newline lexbuf; token lexbuf }
  | space+
      { token lexbuf }
  | int_literal as s
      { INTEGER s }
  | (float_literal | hex_float_literal) as s
      { FLOAT s }
  | "(*"
      { comment lexbuf; token lexbuf }
  | ","
      { COMMA }
  | ";"
      { SEMICOLON }
  | "~"
      { TILDA }
  | "?"
      { QUESTION }
  | "."
      { DOT }
  | ":"
      { COLON }
  | "::"
      { COLONCOLON }
  | "("
      { LEFTPAR }
  | ")"
      { RIGHTPAR }
  | "->"
      { ARROW }
  | "<-"
      { LARROW }
  | "<->"
      { LRARROW }
  | "&&"
      { AMPAMP }
  | "||"
      { BARBAR }
  | "/\\"
      { AND }
  | "\\/"
      { OR }
  | ".."
      { DOTDOT }
  | "|"
      { BAR }
  | "="
      { EQUAL }
  | "<>"
      { LTGT }
  | "'" (lident as id)
      { QUOTE_LIDENT id }
  | "`" (lident as id) "`"
      { BACKQUOTE_LIDENT id }
  | '_'
      { UNDERSCORE }
  | lident as id
      { try Hashtbl.find keywords id with Not_found -> LIDENT id }
  | uident as id
            { UIDENT id }
  | "[]"
      { LEFTSQRIGHTSQ }
  | "["
      { LEFTSQ }
  | "]"
      { RIGHTSQ }
  | "{}"
      { LEFTBRCRIGHTBRC }
  | "{"
      { LEFTBRC }
  | "}"
      { RIGHTBRC }
  | "{:"
      { LEFTBRCCOLON }
  | ":}"
      { COLONRIGHTBRC }
  | "*" { STAR }
  | op_char_pref op_char_4* as s
      { OPPREF s }
  | op_char_1234* op_char_1 op_char_1234* as s
      { OP1 s }
  | op_char_234*  op_char_2 op_char_234*  as s
      { OP2 s }
  | op_char_34*   op_char_3 op_char_34*  as s
      { OP3 s }
  | op_char_4+ as s
      { OP4 s }
  | "\""
      { STRING (string (Buffer.create 128) lexbuf) }
  | eof
      { EOF }
  | _ as c
      { raise (Error (IllegalCharacter c, Location.of_lexbuf lexbuf)) }

and comment = parse
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | newline
      { newline lexbuf; comment lexbuf }
  | eof
      { raise (Error (UnterminatedComment, Location.of_lexbuf lexbuf)) }
  | _
      { comment lexbuf }

and string buf = parse
  | "\""
      { Buffer.contents buf }
  | "\\" newline
      { new_line lexbuf;
        string_skip_spaces buf lexbuf }
  | "\\" (_ as c)
      { Buffer.add_char buf (char_for_backslash c);
        string buf lexbuf }
  | newline
      { new_line lexbuf;
        Buffer.add_char buf '\n';
        string buf lexbuf }
  | eof
      { raise Not_found }
  | _ as c
      { Buffer.add_char buf c;
        string buf lexbuf }

and string_skip_spaces buf = parse
  | [' ' '\t']*
      { string buf lexbuf }
