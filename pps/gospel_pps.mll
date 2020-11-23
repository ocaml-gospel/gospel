{
  type t = Gospel of string | Other of string

  let queue = Queue.create ()
  let empty_line = ref true
  let buf = Buffer.create 1024

  let push () =
    if Buffer.length buf > 0 then (
      Queue.push (Other (Buffer.contents buf)) queue;
      Buffer.clear buf)

  let flush () =
    push ();
    let print = function
      | Gospel s ->
          let ats = if !empty_line then "@@@" else "@@" in
          Format.printf "[%sgospel {|%s|}]%!" ats s
      | Other s -> print_string s
    in
    Queue.iter print queue;
    Queue.clear queue
}

let space = [ ' ' '\t' '\r' '\n' ]

rule scan = parse
  | '\n' space* '\n' as s
      { flush (); empty_line := true; print_string s; scan lexbuf }
  | "(*@"
      {
        push ();
        comment lexbuf;
        let s = Buffer.contents buf in
        Buffer.clear buf;
        Queue.push (Gospel s) queue;
        scan lexbuf
      }
  | "(*"
      {
        Buffer.add_string buf "(*";
        comment lexbuf;
        Buffer.add_string buf "*)";
        scan lexbuf
      }
  | space as c { Buffer.add_char buf c; scan lexbuf }
  | _ as c { Buffer.add_char buf c; empty_line := false; scan lexbuf }
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
