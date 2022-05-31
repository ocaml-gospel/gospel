(rule
 (target pure.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:pure.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:pure.mli} %{dep:pure.mli.output})))

