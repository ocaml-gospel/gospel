(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to incompatible_fields1.mli.output
   (run %{checker} %{dep:incompatible_fields1.mli}))))

(rule
 (alias runtest)
 (action
  (diff incompatible_fields1.mli incompatible_fields1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to incompatible_fields2.mli.output
   (run %{checker} %{dep:incompatible_fields2.mli}))))

(rule
 (alias runtest)
 (action
  (diff incompatible_fields2.mli incompatible_fields2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to incompatible_fields3.mli.output
   (run %{checker} %{dep:incompatible_fields3.mli}))))

(rule
 (alias runtest)
 (action
  (diff incompatible_fields3.mli incompatible_fields3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to incompatible_fields4.mli.output
   (run %{checker} %{dep:incompatible_fields4.mli}))))

(rule
 (alias runtest)
 (action
  (diff incompatible_fields4.mli incompatible_fields4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to incompatible_fields5.mli.output
   (run %{checker} %{dep:incompatible_fields5.mli}))))

(rule
 (alias runtest)
 (action
  (diff incompatible_fields5.mli incompatible_fields5.mli.output)))

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
  (with-outputs-to unbound_field6.mli.output
   (run %{checker} %{dep:unbound_field6.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_field6.mli unbound_field6.mli.output)))

