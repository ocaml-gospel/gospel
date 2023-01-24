let run_file file =
  if String.equal (Filename.extension file) ".mli"
  then begin
    let ic = open_in file in
    let lexbuf = Lexing.from_channel ic in
    Lexing.set_filename lexbuf file;
    print_endline (Gospel.Pps_doc.run lexbuf);
    close_in ic
  end

let run = List.iter run_file
