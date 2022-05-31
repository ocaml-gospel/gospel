(rule
 (target positive-patterns.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:positive-patterns.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:positive-patterns.mli} %{dep:positive-patterns.mli.output})))

