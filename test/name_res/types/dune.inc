(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_not_axiom.mli.output
   (run %{checker} %{dep:type_not_axiom.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_not_axiom.mli type_not_axiom.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_not_field.mli.output
   (run %{checker} %{dep:type_not_field.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_not_field.mli type_not_field.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_not_var.mli.output
   (run %{checker} %{dep:type_not_var.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_not_var.mli type_not_var.mli.output)))

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
  (with-outputs-to unbound_type1.mli.output
   (run %{checker} %{dep:unbound_type1.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type1.mli unbound_type1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type10.mli.output
   (run %{checker} %{dep:unbound_type10.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type10.mli unbound_type10.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type2.mli.output
   (run %{checker} %{dep:unbound_type2.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type2.mli unbound_type2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type3.mli.output
   (run %{checker} %{dep:unbound_type3.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type3.mli unbound_type3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type4.mli.output
   (run %{checker} %{dep:unbound_type4.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type4.mli unbound_type4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type5.mli.output
   (run %{checker} %{dep:unbound_type5.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type5.mli unbound_type5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type6.mli.output
   (run %{checker} %{dep:unbound_type6.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type6.mli unbound_type6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type7.mli.output
   (run %{checker} %{dep:unbound_type7.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type7.mli unbound_type7.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type8.mli.output
   (run %{checker} %{dep:unbound_type8.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type8.mli unbound_type8.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_type9.mli.output
   (run %{checker} %{dep:unbound_type9.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_type9.mli unbound_type9.mli.output)))

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
  (with-outputs-to unbound_var10.mli.output
   (run %{checker} %{dep:unbound_var10.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var10.mli unbound_var10.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var19.mli.output
   (run %{checker} %{dep:unbound_var19.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var19.mli unbound_var19.mli.output)))

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
  (with-outputs-to unbound_var3.mli.output
   (run %{checker} %{dep:unbound_var3.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var3.mli unbound_var3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var4.mli.output
   (run %{checker} %{dep:unbound_var4.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var4.mli unbound_var4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var5.mli.output
   (run %{checker} %{dep:unbound_var5.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var5.mli unbound_var5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var6.mli.output
   (run %{checker} %{dep:unbound_var6.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var6.mli unbound_var6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var7.mli.output
   (run %{checker} %{dep:unbound_var7.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var7.mli unbound_var7.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var8.mli.output
   (run %{checker} %{dep:unbound_var8.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var8.mli unbound_var8.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unbound_var9.mli.output
   (run %{checker} %{dep:unbound_var9.mli}))))

(rule
 (alias runtest)
 (action
  (diff unbound_var9.mli unbound_var9.mli.output)))

