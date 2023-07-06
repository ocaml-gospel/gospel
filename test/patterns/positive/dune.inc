(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to positive-patterns.mli.output
   (run %{checker} %{dep:positive-patterns.mli}))))

(rule
 (alias runtest)
 (action
  (diff positive-patterns.mli positive-patterns.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:positive-patterns.mli}))))

