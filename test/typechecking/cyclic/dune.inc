(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_fun1.mli.output
   (run %{checker} %{dep:cyclic_fun1.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_fun1.mli cyclic_fun1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_fun2.mli.output
   (run %{checker} %{dep:cyclic_fun2.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_fun2.mli cyclic_fun2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_fun3.mli.output
   (run %{checker} %{dep:cyclic_fun3.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_fun3.mli cyclic_fun3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_type1.mli.output
   (run %{checker} %{dep:cyclic_type1.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_type1.mli cyclic_type1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_type2.mli.output
   (run %{checker} %{dep:cyclic_type2.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_type2.mli cyclic_type2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_type3.mli.output
   (run %{checker} %{dep:cyclic_type3.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_type3.mli cyclic_type3.mli.output)))

