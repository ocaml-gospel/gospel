let print_rule file =
  if Filename.extension file = ".mli" then
    let pp_file = (Filename.chop_extension file) ^ ".mli.pp" in
    Printf.printf
      {|(rule
 (target %s)
 (action
  (with-outputs-to %%{target}
     (run gospel_pps %%{dep:%s}))))

(rule
 (targets %s.output)
 (action
   (with-outputs-to %%{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%%{bin:gospel} tc --print-intermediate %%{dep:%s}")))))

(rule
 (alias runtest)
 (action (diff %%{dep:%s.expected} %%{dep:%s.output})))

|}
      pp_file file file pp_file file file

let () =
  let files = Filename.current_dir_name |> Sys.readdir in
  Array.sort String.compare files;
  Array.iter print_rule files
