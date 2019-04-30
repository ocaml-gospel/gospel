open Options
open Tmodule
open Typing
open Parser_frontend

let () = Printexc.record_backtrace true

let fmt = Format.std_formatter

let pp = Format.fprintf

let type_check name sigs =
  let md = md_with_primitives name in
  let md = List.fold_left process_signature md sigs in
  close_md md

let run_bench () =
  let ok,error = ref 0, ref 0 in
  let parse f =
    try
      let s = parse_file f in
      let sigs = Uattr2spec.signature s in
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
  pp fmt "@[@\n OK: %d    ERROR: %d@\n@]@." !ok !error

let run_on_file file =
  try
    let sigs = parse_file file in
    if !print_intermediate then begin
        pp fmt "@[@\n ********* Parsed file - %s *********@\n@]@." file;
        pp fmt "@[%a@]@." Opprintast.signature sigs
      end;

    if !parse_ocaml_only then raise Exit;
    let sigs = parse_spec sigs in
    if !print_intermediate || !print_parsed then begin
        pp fmt "@[@\n*******************************@]@.";
        pp fmt    "@[****** GOSPEL translation *****@]@.";
        pp fmt    "@[*******************************@]@.";
        pp fmt "@[%a@]@." Upretty_printer.s_signature sigs
      end;

    if !parse_only then raise Exit;
    let md = type_check file sigs in
    pp fmt "@[@\n*******************************@]@.";
    pp fmt    "@[********* Typed GOSPEL ********@]@.";
    pp fmt    "@[*******************************@]@.";
    pp fmt "@[%a@]@." print_mod md;
    pp fmt "@[@\n*** OK ***@\n@]@."
  with
  | Exit -> ()
  | FileNotFound f ->
     let open Format in
     Format.eprintf  "File %s not found.@\nLoad path: @\n%a@\n@."
       f (pp_print_list ~pp_sep:pp_print_newline pp_print_string)
       !Options.load_path
  | e -> Location.report_exception Format.err_formatter e

let run () =
  List.iter run_on_file !files

let () =
  parse ();
  if !bench_mode then run_bench () else run ();
  pp fmt "Done!@."
