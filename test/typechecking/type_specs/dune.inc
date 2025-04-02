(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to positive.mli.output
   (run %{checker} %{dep:positive.mli}))))

(rule
 (alias runtest)
 (action
  (diff positive.mli positive.mli.output)))

