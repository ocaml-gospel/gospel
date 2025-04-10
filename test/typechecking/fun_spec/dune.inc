(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to fun_name_scope.mli.output
   (run %{checker} %{dep:fun_name_scope.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_name_scope.mli fun_name_scope.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_invariant.mli.output
   (run %{checker} %{dep:invalid_invariant.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_invariant.mli invalid_invariant.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_post.mli.output
   (run %{checker} %{dep:invalid_post.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_post.mli invalid_post.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_pre.mli.output
   (run %{checker} %{dep:invalid_pre.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_pre.mli invalid_pre.mli.output)))

