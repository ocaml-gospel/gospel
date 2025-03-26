(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to creation.mli.output
   (run %{checker} %{dep:creation.mli}))))

(rule
 (alias runtest)
 (action
  (diff creation.mli creation.mli.output)))

