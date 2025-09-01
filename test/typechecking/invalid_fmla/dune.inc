(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to fmla_not_fun1.mli.output
   (run %{checker} %{dep:fmla_not_fun1.mli}))))

(rule
 (alias runtest)
 (action
  (diff fmla_not_fun1.mli fmla_not_fun1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to fmla_not_fun2.mli.output
   (run %{checker} %{dep:fmla_not_fun2.mli}))))

(rule
 (alias runtest)
 (action
  (diff fmla_not_fun2.mli fmla_not_fun2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to fmla_not_fun3.mli.output
   (run %{checker} %{dep:fmla_not_fun3.mli}))))

(rule
 (alias runtest)
 (action
  (diff fmla_not_fun3.mli fmla_not_fun3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to fmla_not_integer.mli.output
   (run %{checker} %{dep:fmla_not_integer.mli}))))

(rule
 (alias runtest)
 (action
  (diff fmla_not_integer.mli fmla_not_integer.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to fmla_not_record.mli.output
   (run %{checker} %{dep:fmla_not_record.mli}))))

(rule
 (alias runtest)
 (action
  (diff fmla_not_record.mli fmla_not_record.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to fmla_not_set.mli.output
   (run %{checker} %{dep:fmla_not_set.mli}))))

(rule
 (alias runtest)
 (action
  (diff fmla_not_set.mli fmla_not_set.mli.output)))

