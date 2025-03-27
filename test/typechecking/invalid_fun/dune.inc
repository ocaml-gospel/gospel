(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_fun1.mli.output
   (run %{checker} %{dep:not_fun1.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_fun1.mli not_fun1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_fun2.mli.output
   (run %{checker} %{dep:not_fun2.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_fun2.mli not_fun2.mli.output)))

