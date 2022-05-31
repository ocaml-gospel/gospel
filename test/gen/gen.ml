let print_rule file =
  if Filename.extension file = ".mli" then
    Printf.printf
      {|(rule
 (target %s.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %%{target}
      (run %%{project_root}/test/gospel_check.exe %%{dep:%s}))))

(rule
 (alias runtest)
 (action (diff %%{dep:%s} %%{dep:%s.output})))

|}
      file file file file

let () =
  let files = Filename.current_dir_name |> Sys.readdir in
  Array.sort String.compare files;
  Array.iter print_rule files
