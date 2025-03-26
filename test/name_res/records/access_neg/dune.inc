(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to field_not_axiom.mli.output
   (run %{checker} %{dep:field_not_axiom.mli}))))

(rule
 (alias runtest)
 (action
  (diff field_not_axiom.mli field_not_axiom.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to field_not_type.mli.output
   (run %{checker} %{dep:field_not_type.mli}))))

(rule
 (alias runtest)
 (action
  (diff field_not_type.mli field_not_type.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to field_not_var.mli.output
   (run %{checker} %{dep:field_not_var.mli}))))

(rule
 (alias runtest)
 (action
  (diff field_not_var.mli field_not_var.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_field1.mli.output
   (run %{checker} %{dep:unbound_field1.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_field1.mli unbound_field1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_field2.mli.output
   (run %{checker} %{dep:unbound_field2.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_field2.mli unbound_field2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_field3.mli.output
   (run %{checker} %{dep:unbound_field3.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_field3.mli unbound_field3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_field4.mli.output
   (run %{checker} %{dep:unbound_field4.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_field4.mli unbound_field4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_field5.mli.output
   (run %{checker} %{dep:unbound_field5.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_field5.mli unbound_field5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_module1.mli.output
   (run %{checker} %{dep:unbound_module1.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_module1.mli unbound_module1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_module2.mli.output
   (run %{checker} %{dep:unbound_module2.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_module2.mli unbound_module2.mli.output)))

