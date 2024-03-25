(* Given a list of files on the commandline, generates a dune configuration to
   process all those files with [md2mli.awk] and call [gospel check] on the
   resulting .mli *)

(* The [chdir]s ensure that the full paths are fed to the awk script and the
   Gospel checker, so that errors are properly located *)

let print_rule md =
  let base = Filename.remove_extension md in
  let mli = base ^ ".mli" and out = base ^ ".gospel" in
  Printf.printf
    {|(rule
 (enabled_if %%{bin-available:awk})
 (deps
  (:md2mli %%{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   %s
   (chdir %%{project_root}
    (run awk -f %%{md2mli} %%{dep:%s})))))

(rule
 (alias runtest)
 (enabled_if %%{bin-available:awk})
 (deps %%{bin:gospel})
 (target %s)
 (action
  (chdir %%{project_root}
   (run gospel check %%{dep:%s}))))

|}
    mli md out mli

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    print_rule Sys.argv.(i)
  done
