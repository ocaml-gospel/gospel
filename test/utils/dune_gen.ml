(* Generates the dune configuration for the current directory of tests

   Takes dependencies as arguments, to be read as a sequence of pairs,
   such that:
     dune_gen.exe a.mli b.mli c.mli "d.mli e.mli"
   will generate the configuration with a.mli depending on b.mli and
   c.mli depending on both d.mli and e.mli.

   Takes also as arguments acceptable exit codes for ocamlc when
   compiling the .mli so that
     dune_gen.exe ... -- a.mli 2
   will generate the configuration where running "ocamlc -c a.mli"
   will be expected to exit with code 2. *)

let usage () =
  Printf.fprintf stderr "Usage: %s [FILE DEPS] ... [-- [FILE EXITS]]"
    Sys.argv.(0);
  exit 1

let dependencies, exit_codes =
  let n = Array.length Sys.argv in
  let deps = Hashtbl.create n and exits = Hashtbl.create n in
  let rec parse tbl i =
    if i >= n then ()
    else
      match Sys.argv.(i) with
      | "--" -> parse exits (i + 1)
      | _ when i = n - 1 -> usage ()
      | x ->
          Hashtbl.add tbl x Sys.argv.(i + 1);
          parse tbl (i + 2)
  in
  parse deps 1;
  (deps, exits)

let print_rule file =
  if Filename.extension file = ".mli" then
    let gospel_file = Filename.remove_extension file ^ ".gospel" in
    let deps =
      match Hashtbl.find_all dependencies file with
      | [] -> ""
      | ds -> "\n  " ^ String.concat "\n  " ds
    in
    let exit_code_open, exit_code_close =
      match Hashtbl.find_opt exit_codes file with
      | None -> ("", "")
      | Some prd -> ("(with-accepted-exit-codes " ^ prd ^ "\n    ", ")")
    in
    Printf.printf
      {|(rule
 (deps
  %%{bin:gospel}
  (:checker %%{project_root}/test/utils/testchecker.exe)%s)
 (targets %s)
 (action
  (with-outputs-to %s.output
   (run %%{checker} %%{dep:%s}))))

(rule
 (alias runtest)
 (action
  (diff %s %s.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %%{project_root}
   ; Syntax sanity check
   %s(run ocamlc -c %%{dep:%s})%s)))

|}
      deps gospel_file file file file file exit_code_open file exit_code_close

let () =
  let files = Filename.current_dir_name |> Sys.readdir in
  Array.sort String.compare files;
  Array.iter print_rule files
