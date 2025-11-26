(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to polymorphic_variant.mli.output
   (run %{checker} %{dep:polymorphic_variant.mli}))))

(rule
 (alias runtest)
 (action
  (diff polymorphic_variant.mli polymorphic_variant.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_extension.mli.output
   (run %{checker} %{dep:type_extension.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_extension.mli type_extension.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_extension_with_spec.mli.output
   (run %{checker} %{dep:type_extension_with_spec.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_extension_with_spec.mli type_extension_with_spec.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_parameter.mli.output
   (run %{checker} %{dep:type_parameter.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_parameter.mli type_parameter.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_parameter_with_spec.mli.output
   (run %{checker} %{dep:type_parameter_with_spec.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_parameter_with_spec.mli type_parameter_with_spec.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to value_first_class_module.mli.output
   (run %{checker} %{dep:value_first_class_module.mli}))))

(rule
 (alias runtest)
 (action
  (diff value_first_class_module.mli value_first_class_module.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to value_first_class_module_with_spec.mli.output
   (run %{checker} %{dep:value_first_class_module_with_spec.mli}))))

(rule
 (alias runtest)
 (action
  (diff value_first_class_module_with_spec.mli value_first_class_module_with_spec.mli.output)))

