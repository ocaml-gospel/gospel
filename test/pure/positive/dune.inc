(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to pure.mli.output
   (run %{checker} %{dep:pure.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure.mli pure.mli.output)))

