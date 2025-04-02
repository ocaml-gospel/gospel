(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to axiom_name_invalid.mli.output
   (run %{checker} %{dep:axiom_name_invalid.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom_name_invalid.mli axiom_name_invalid.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to escape_scope1.mli.output
   (run %{checker} %{dep:escape_scope1.mli}))))

(rule
 (alias runtest)
 (action
  (diff escape_scope1.mli escape_scope1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to escape_scope2.mli.output
   (run %{checker} %{dep:escape_scope2.mli}))))

(rule
 (alias runtest)
 (action
  (diff escape_scope2.mli escape_scope2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to escape_scope3.mli.output
   (run %{checker} %{dep:escape_scope3.mli}))))

(rule
 (alias runtest)
 (action
  (diff escape_scope3.mli escape_scope3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to escape_scope4.mli.output
   (run %{checker} %{dep:escape_scope4.mli}))))

(rule
 (alias runtest)
 (action
  (diff escape_scope4.mli escape_scope4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to escape_scope5.mli.output
   (run %{checker} %{dep:escape_scope5.mli}))))

(rule
 (alias runtest)
 (action
  (diff escape_scope5.mli escape_scope5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to escape_scope6.mli.output
   (run %{checker} %{dep:escape_scope6.mli}))))

(rule
 (alias runtest)
 (action
  (diff escape_scope6.mli escape_scope6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to non_recursive1.mli.output
   (run %{checker} %{dep:non_recursive1.mli}))))

(rule
 (alias runtest)
 (action
  (diff non_recursive1.mli non_recursive1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to non_recursive2.mli.output
   (run %{checker} %{dep:non_recursive2.mli}))))

(rule
 (alias runtest)
 (action
  (diff non_recursive2.mli non_recursive2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to non_recursive3.mli.output
   (run %{checker} %{dep:non_recursive3.mli}))))

(rule
 (alias runtest)
 (action
  (diff non_recursive3.mli non_recursive3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to non_recursive4.mli.output
   (run %{checker} %{dep:non_recursive4.mli}))))

(rule
 (alias runtest)
 (action
  (diff non_recursive4.mli non_recursive4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to non_recursive5.mli.output
   (run %{checker} %{dep:non_recursive5.mli}))))

(rule
 (alias runtest)
 (action
  (diff non_recursive5.mli non_recursive5.mli.output)))

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

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_module3.mli.output
   (run %{checker} %{dep:unbound_module3.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_module3.mli unbound_module3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_module4.mli.output
   (run %{checker} %{dep:unbound_module4.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_module4.mli unbound_module4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var1.mli.output
   (run %{checker} %{dep:unbound_var1.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var1.mli unbound_var1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var2.mli.output
   (run %{checker} %{dep:unbound_var2.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var2.mli unbound_var2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to var_not_field.mli.output
   (run %{checker} %{dep:var_not_field.mli}))))

(rule
 (alias runtest)
 (action
  (diff var_not_field.mli var_not_field.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to var_not_module.mli.output
   (run %{checker} %{dep:var_not_module.mli}))))

(rule
 (alias runtest)
 (action
  (diff var_not_module.mli var_not_module.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to var_not_type.mli.output
   (run %{checker} %{dep:var_not_type.mli}))))

(rule
 (alias runtest)
 (action
  (diff var_not_type.mli var_not_type.mli.output)))

