(rule
 (target basic.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:basic.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:basic.mli} %{dep:basic.mli.output})))

