{
  type t = Ghost of string | Spec of string | Other of string | Spaces of string

  let queue = Queue.create ()
  let buf = Buffer.create 1024

  let push () =
    if Buffer.length buf > 0 then (
      Queue.push (Other (Buffer.contents buf)) queue;
      Buffer.clear buf)

  let flush () =
    push ();
    (Queue.fold (fun (acc, sp) t -> match acc, t with
         | _, Spaces nsp -> (acc, sp ^ nsp)
         | None, Ghost _ | None, Other _ -> Some t, sp
         | None, Spec _ -> assert false
         | Some (Ghost g), Spec s ->
           Format.printf "%s[@@@@@@gospel {|%s|}[@@@@gospel {|%s|}]]%!" sp g s;
           None, ""
         | Some (Ghost g), _ ->
           Format.printf "%s[@@@@@@gospel {|%s|}]%!" sp g;
           Some t, ""
         | Some (Other o), Spec s ->
           Format.printf "%s%s[@@@@gospel {|%s|}]%!" sp o s;
           None, ""
         | Some (Other o), _ ->
           print_string sp; print_string o;
           Some t, ""
         | Some (Spaces _), _ | Some (Spec _), _ -> assert false
       ) (None, "") queue
     |> fun (acc, sp) ->
     print_string sp;
     match acc with
     | None -> ()
     | Some (Spaces s) -> print_string s
     | Some (Other o) -> print_string o
     | Some (Ghost g) ->
       Format.printf "[@@@@@@gospel {|%s|}]%!" g;
     | Some (Spec _) -> assert false);
    Queue.clear queue
}

let space = [ ' ' '\t' '\r' '\n' ]

rule scan = parse
  | space* as s
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
