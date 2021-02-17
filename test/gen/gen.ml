let print_rule pp_only file =
  if Filename.extension file = ".mli" then
    let pp_file = (Filename.chop_extension file) ^ ".mli.pp" in
    if pp_only then
      Printf.printf
        {|(rule
 (target %s)
 (action
  (with-outputs-to %%{target}
     (run gospel_pps %%{dep:%s}))))

(rule
 (alias runtest)
 (action (diff %%{dep:%s.expected} %%{dep:%s})))|}
        pp_file file file pp_file
        else
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
  let pp_only = Array.length Sys.argv > 1 &&  Sys.argv.(1) = "--pp-only" in
  let files = Filename.current_dir_name |> Sys.readdir in
  Array.sort String.compare files;
  Array.iter (print_rule pp_only) files
