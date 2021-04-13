{
type t =
  | Ghost of string
  | Spec of string
  | Other of string
  | Spaces of string
  | LoopHead of string

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
  | Other o :: LoopHead h :: Other o' :: Spec s :: l ->
    Fmt.str "%s%s[@@gospel {|%s|}]%s%s" o h s o' (print l)
  | Spec s :: l ->
    (* FIXME: we could fail right here *)
    Fmt.str "[@@@@gospel {|%s|}]%s" s (print l)
  | LoopHead h :: Other o :: Spec s :: l ->
    Fmt.str "%s[@@gospel {|%s|}]%s%s" h s o (print l)
  | Other o :: Spaces sp :: Spec s :: l ->
    Fmt.str "%s%s[@@@@gospel {|%s|}]%s" o sp s (print l)
  | Other o :: Spaces sp :: LoopHead h :: Other o' :: Spec s :: l ->
    Fmt.str "%s%s%s[@@gospel {|%s|}]%s%s" o sp h s o' (print l)
  | (Other s | Spaces s | LoopHead s) :: l ->
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
       ("function" | "type" | "predicate" | "axiom" |
        "lemma"    | "val"  | "open" ) as k)
      {
        push ();
        Buffer.add_string buf k;
        comment lexbuf;
        let s = Buffer.contents buf in
        Buffer.clear buf;
        Queue.push (Ghost s) queue;
        scan lexbuf
      }
  | (("while" | "for") space+) as k
    {
      push ();
      Queue.push (LoopHead k) queue;
      loop lexbuf;
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

and loop = parse
  | ("do" space*) as k "(*@"
    {
      Buffer.add_string buf k;
      push ();
      comment lexbuf;
      let s = Buffer.contents buf in
      Buffer.clear buf;
      Queue.push (Spec s) queue
    }
  | ("do" space*) as k
    {
      Buffer.add_string buf k;
      push ()
    }
  | _ as c { Buffer.add_char buf c; loop lexbuf }
  | eof { failwith "Erro" }

{
  let run lb =
    clear ();
    scan lb
}
