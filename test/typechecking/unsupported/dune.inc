(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
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

