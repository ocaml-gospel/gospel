{
  type t = Ghost of string | Spec of string | Other of string | Spaces of string

  let queue = Queue.create ()
  let buf = Buffer.create 1024

  let clear () = Queue.clear queue

  let push () =
    if Buffer.length buf > 0 then (
      Queue.push (Other (Buffer.contents buf)) queue;
      Buffer.clear buf)

  let rec print = function
  | Ghost g :: Spec s :: l ->
    Fmt.str "[@@@@@@gospel {|%s|}[@@@@gospel {|%s|}]]%s" g s (print l)
  | Ghost g :: Spaces sp :: Spec s :: l ->
    Fmt.str "[@@@@@@gospel {|%s|}%s[@@@@gospel {|%s|}]]%s" g sp s (print l)
  | Ghost g :: l ->
    Fmt.str "[@@@@@@gospel {|%s|}]%s" g (print l)
  | Other o :: Spec s :: l ->
    Fmt.str "%s[@@@@gospel {|%s|}]%s" o s (print l)
  | Spec s :: l ->
    (* FIXME: we could fail right here *)
    Fmt.str "[@@@@gospel {|%s|}]%s" s (print l)
  | Other o :: Spaces sp :: Spec s :: l ->
    Fmt.str "%s%s[@@@@gospel {|%s|}]%s" o sp s (print l)
  | (Other s | Spaces s) :: l ->
    Fmt.str "%s%s" s (print l)
  | [] -> ""

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
  let run lb =
    clear ();
    scan lb
}
