(* Generates the dune configuration for the current directory of tests

   Takes dependencies as arguments, to be read as a sequence of pairs,
   such that:
     gen.exe a.mli b.mli c.mli "d.mli e.mli"
   will generate the configuration with a.mli depending on b.mli and
   c.mli depending on both d.mli and e.mli. *)

let dependencies =
  let n = Array.length Sys.argv in
  assert (n mod 2 = 1);
  let n = n / 2 in
  let tbl = Hashtbl.create n in
  for i = 1 to n do
    Hashtbl.add tbl Sys.argv.((i * 2) - 1) Sys.argv.(i * 2)
  done;
  tbl

let print_rule file =
  if Filename.extension file = ".mli" then
    let deps =
      match Hashtbl.find_all dependencies file with
      | [] -> ""
      | ds -> "\n  " ^ String.concat "\n  " ds
    in
    Printf.printf
      {|(rule
 (deps
  (:checker %%{project_root}/test/gospel_check.exe)%s)
 (action
  (with-outputs-to %s.output
   (run %%{checker} %%{dep:%s}))))

(rule
 (alias runtest)
 (action
  (diff %s %s.output)))

|}
      deps file file file file

let () =
  let files = Filename.current_dir_name |> Sys.readdir in
  Array.sort String.compare files;
  Array.iter print_rule files
