(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret1.mli.output
   (run %{checker} %{dep:invalid_ret1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret1.mli invalid_ret1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret2.mli.output
   (run %{checker} %{dep:invalid_ret2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret2.mli invalid_ret2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret3.mli.output
   (run %{checker} %{dep:invalid_ret3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret3.mli invalid_ret3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret4.mli.output
   (run %{checker} %{dep:invalid_ret4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret4.mli invalid_ret4.mli.output)))

