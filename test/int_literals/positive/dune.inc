(rule
 (target ints.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:ints.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:ints.mli} %{dep:ints.mli.output})))

