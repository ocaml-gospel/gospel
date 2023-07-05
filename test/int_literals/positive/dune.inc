(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to ints.mli.output
   (run %{checker} %{dep:ints.mli}))))

(rule
 (alias runtest)
 (action
  (diff ints.mli ints.mli.output)))

