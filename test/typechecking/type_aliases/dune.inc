(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to mismatch_vars1.mli.output
   (run %{checker} %{dep:mismatch_vars1.mli}))))

(rule
 (alias runtest)
 (action
  (diff mismatch_vars1.mli mismatch_vars1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to mismatch_vars2.mli.output
   (run %{checker} %{dep:mismatch_vars2.mli}))))

(rule
 (alias runtest)
 (action
  (diff mismatch_vars2.mli mismatch_vars2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to mismatch_vars3.mli.output
   (run %{checker} %{dep:mismatch_vars3.mli}))))

(rule
 (alias runtest)
 (action
  (diff mismatch_vars3.mli mismatch_vars3.mli.output)))

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

