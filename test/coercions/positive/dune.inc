(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to basic.mli.output
   (run %{checker} %{dep:basic.mli}))))

(rule
 (alias runtest)
 (action
  (diff basic.mli basic.mli.output)))

