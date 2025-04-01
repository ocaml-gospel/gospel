(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias1.mli.output
   (run %{checker} %{dep:cyclic_alias1.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias1.mli cyclic_alias1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias10.mli.output
   (run %{checker} %{dep:cyclic_alias10.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias10.mli cyclic_alias10.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias11.mli.output
   (run %{checker} %{dep:cyclic_alias11.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias11.mli cyclic_alias11.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias2.mli.output
   (run %{checker} %{dep:cyclic_alias2.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias2.mli cyclic_alias2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias3.mli.output
   (run %{checker} %{dep:cyclic_alias3.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias3.mli cyclic_alias3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias4.mli.output
   (run %{checker} %{dep:cyclic_alias4.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias4.mli cyclic_alias4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias5.mli.output
   (run %{checker} %{dep:cyclic_alias5.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias5.mli cyclic_alias5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias6.mli.output
   (run %{checker} %{dep:cyclic_alias6.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias6.mli cyclic_alias6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias7.mli.output
   (run %{checker} %{dep:cyclic_alias7.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias7.mli cyclic_alias7.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias8.mli.output
   (run %{checker} %{dep:cyclic_alias8.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias8.mli cyclic_alias8.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cyclic_alias9.mli.output
   (run %{checker} %{dep:cyclic_alias9.mli}))))

(rule
 (alias runtest)
 (action
  (diff cyclic_alias9.mli cyclic_alias9.mli.output)))

