(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to neq_vars1.mli.output
   (run %{checker} %{dep:neq_vars1.mli}))))

(rule
 (alias runtest)
 (action
  (diff neq_vars1.mli neq_vars1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to neq_vars2.mli.output
   (run %{checker} %{dep:neq_vars2.mli}))))

(rule
 (alias runtest)
 (action
  (diff neq_vars2.mli neq_vars2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to neq_vars3.mli.output
   (run %{checker} %{dep:neq_vars3.mli}))))

(rule
 (alias runtest)
 (action
  (diff neq_vars3.mli neq_vars3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to neq_vars4.mli.output
   (run %{checker} %{dep:neq_vars4.mli}))))

(rule
 (alias runtest)
 (action
  (diff neq_vars4.mli neq_vars4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to neq_vars5.mli.output
   (run %{checker} %{dep:neq_vars5.mli}))))

(rule
 (alias runtest)
 (action
  (diff neq_vars5.mli neq_vars5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to neq_vars6.mli.output
   (run %{checker} %{dep:neq_vars6.mli}))))

(rule
 (alias runtest)
 (action
  (diff neq_vars6.mli neq_vars6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to neq_vars7.mli.output
   (run %{checker} %{dep:neq_vars7.mli}))))

(rule
 (alias runtest)
 (action
  (diff neq_vars7.mli neq_vars7.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to neq_vars8.mli.output
   (run %{checker} %{dep:neq_vars8.mli}))))

(rule
 (alias runtest)
 (action
  (diff neq_vars8.mli neq_vars8.mli.output)))

