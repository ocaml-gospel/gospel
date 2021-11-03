let print_rule file =
  if Filename.extension file = ".mli" then
    Printf.printf
      {|(rule
 (targets %s.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %%{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%%{bin:gospel} check --verbose %%{dep:%s}")))))

(rule
 (alias runtest)
 (action (diff %%{dep:%s.expected} %%{dep:%s.output})))

|}
      file file file file

let () =
  let files = Filename.current_dir_name |> Sys.readdir in
  Array.sort String.compare files;
  Array.iter print_rule files
