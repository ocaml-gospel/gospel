{
  type t = Ghost of string | Spec of string | Other of string | Spaces of string

  let queue = Queue.create ()
  let buf = Buffer.create 1024

  let push () =
    if Buffer.length buf > 0 then (
      Queue.push (Other (Buffer.contents buf)) queue;
      Buffer.clear buf)

  let rec print = function
  | Ghost g :: Spec s :: l ->
    Format.printf "[@@@@@@gospel {|%s|}[@@@@gospel {|%s|}]]" g s;
    print l
  | Ghost g :: Spaces sp :: Spec s :: l ->
    Format.printf "[@@@@@@gospel {|%s|}%s[@@@@gospel {|%s|}]]" g sp s;
    print l
  | Ghost g :: l ->
    Format.printf "[@@@@@@gospel {|%s|}]" g;
    print l
  | Other o :: Spec s :: l ->
    Format.printf "%s[@@@@gospel {|%s|}]" o s;
    print l
  | Spec s :: l ->
    (* FIXME: we could fail right here *)
    Format.printf "[@@@@gospel {|%s|}]" s;
    print l
  | Other o :: Spaces sp :: Spec s :: l ->
    Format.printf "%s%s[@@@@gospel {|%s|}]" o sp s;
    print l
  | (Other s | Spaces s) :: l ->
    Format.print_string s;
    print l
  | [] ->
    Format.printf "@?"

  let flush () =
    push ();
    let l = Queue.fold (fun acc t -> t :: acc) [] queue in
    print (List.rev l)
}

let space = [ ' ' '\t' '\r' '\n' ]

rule scan = parse
  | space+ as s
    { push (); Queue.push (Spaces s) queue; scan lexbuf }
  | "(*@"
      (space*
       ("function" | "type" | "predicate" | "axiom" | "val" | "open" ) as k)
      {
        push ();
        Buffer.add_string buf k;
        comment lexbuf;
        let s = Buffer.contents buf in
        Buffer.clear buf;
        Queue.push (Ghost s) queue;
        scan lexbuf
      }
  | "(*@"
      {
        push ();
        comment lexbuf;
        let s = Buffer.contents buf in
        Buffer.clear buf;
        Queue.push (Spec s) queue;
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

(* FIXME: Strings in comments. *)
and comment = parse
  | "*)" {}
  | "(*"
      {
        Buffer.add_string buf "(*";
        comment lexbuf;
        Buffer.add_string buf "*)";
        comment lexbuf }
  | _ as c { Buffer.add_char buf c; comment lexbuf }

{
  let () =
    let c =
      if Array.length Sys.argv > 1 then Sys.argv.(1) |> open_in else stdin
    in
    Lexing.from_channel c |> scan;
    close_in c
}
