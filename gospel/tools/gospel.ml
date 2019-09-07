open Options
open Tmodule
open Typing
open Parser_frontend

let () = Printexc.record_backtrace true

let fmt = Format.std_formatter

let pp = Format.fprintf

let type_check name sigs =
  let md = init_muc name in
  let md = List.fold_left (process_signature !load_path) md sigs in
  wrap_up_muc md

let run_bench () =
  let ok,error = ref 0, ref 0 in
  let parse f =
    try
      let ocaml = parse_ocaml !load_path f in
      let sigs  =  parse_gospel ocaml in
      ok := !ok + 1;
      pp fmt "parse OK - ";
      if !parse_only then raise Exit;
      ignore(type_check f sigs);
      pp fmt "type check OK - ";
      raise Exit
    with
    | Exit -> Format.fprintf fmt "%s\n" f
    | _ -> (error := !error + 1; pp fmt " *** ERROR ***  %s\n" f) in
  List.iter parse !files;
  pp fmt "@[@\n Parsing OK: %d    Typing ERRORs: %d@\n@]@." !ok !error

let run_on_file file =
  try
    let ocaml = parse_ocaml !load_path file in
    if !print_intermediate then begin
        pp fmt "@[@\n ********* Parsed file - %s *********@\n@]@." file;
        pp fmt "@[%a@]@." Opprintast.signature ocaml
      end;

    if !parse_ocaml_only then raise Exit;
    let sigs = parse_gospel ocaml in
    if !print_intermediate || !print_parsed then begin
        pp fmt "@[@\n*******************************@]@.";
        pp fmt    "@[****** GOSPEL translation *****@]@.";
        pp fmt    "@[*******************************@]@.";
        pp fmt "@[%a@]@." Upretty_printer.s_signature sigs
      end;

    if !parse_only then raise Exit;
    let file = type_check file sigs in
    pp fmt "@[@\n*******************************@]@.";
    pp fmt    "@[********* Typed GOSPEL ********@]@.";
    pp fmt    "@[*******************************@]@.";
    pp fmt "@[%a@]@." print_file file;
    pp fmt "@[@\n*** OK ***@\n@]@."
  with
  | Exit -> ()
  | FileNotFound f ->
     let open Format in
     eprintf  "File %s not found.@\nLoad path: @\n%a@\n@."
       f (pp_print_list ~pp_sep:pp_print_newline pp_print_string)
       !Options.load_path
  | e -> Location.report_exception Format.err_formatter e

let run () =
  List.iter run_on_file !files

let () =
  parse ();
  if !bench_mode then run_bench () else run ();
  pp fmt "Done!@."
