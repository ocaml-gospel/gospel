(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found1.mli.output
   (run %{checker} %{dep:module_not_found1.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found1.mli module_not_found1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found2.mli.output
   (run %{checker} %{dep:module_not_found2.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found2.mli module_not_found2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found3.mli.output
   (run %{checker} %{dep:module_not_found3.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found3.mli module_not_found3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found4.mli.output
   (run %{checker} %{dep:module_not_found4.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found4.mli module_not_found4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found5.mli.output
   (run %{checker} %{dep:module_not_found5.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found5.mli module_not_found5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found6.mli.output
   (run %{checker} %{dep:module_not_found6.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found6.mli module_not_found6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found7.mli.output
   (run %{checker} %{dep:module_not_found7.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found7.mli module_not_found7.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found8.mli.output
   (run %{checker} %{dep:module_not_found8.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found8.mli module_not_found8.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to module_not_found9.mli.output
   (run %{checker} %{dep:module_not_found9.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_not_found9.mli module_not_found9.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to scope_not_found1.mli.output
   (run %{checker} %{dep:scope_not_found1.mli}))))

(rule
 (alias runtest)
 (action
  (diff scope_not_found1.mli scope_not_found1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to scope_not_found2.mli.output
   (run %{checker} %{dep:scope_not_found2.mli}))))

(rule
 (alias runtest)
 (action
  (diff scope_not_found2.mli scope_not_found2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to scope_not_found3.mli.output
   (run %{checker} %{dep:scope_not_found3.mli}))))

(rule
 (alias runtest)
 (action
  (diff scope_not_found3.mli scope_not_found3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to scope_not_found4.mli.output
   (run %{checker} %{dep:scope_not_found4.mli}))))

(rule
 (alias runtest)
 (action
  (diff scope_not_found4.mli scope_not_found4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to scope_not_found5.mli.output
   (run %{checker} %{dep:scope_not_found5.mli}))))

(rule
 (alias runtest)
 (action
  (diff scope_not_found5.mli scope_not_found5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to scope_not_found7.mli.output
   (run %{checker} %{dep:scope_not_found7.mli}))))

(rule
 (alias runtest)
 (action
  (diff scope_not_found7.mli scope_not_found7.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to scope_not_found8.mli.output
   (run %{checker} %{dep:scope_not_found8.mli}))))

(rule
 (alias runtest)
 (action
  (diff scope_not_found8.mli scope_not_found8.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to scope_not_found9.mli.output
   (run %{checker} %{dep:scope_not_found9.mli}))))

(rule
 (alias runtest)
 (action
  (diff scope_not_found9.mli scope_not_found9.mli.output)))

