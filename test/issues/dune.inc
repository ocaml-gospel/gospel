(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets attribute_syntax.gospel)
 (action
  (with-outputs-to attribute_syntax.mli.output
   (run %{checker} %{dep:attribute_syntax.mli}))))

(rule
 (alias runtest)
 (action
  (diff attribute_syntax.mli attribute_syntax.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:attribute_syntax.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets include_module.gospel)
 (action
  (with-outputs-to include_module.mli.output
   (run %{checker} %{dep:include_module.mli}))))

(rule
 (alias runtest)
 (action
  (diff include_module.mli include_module.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:include_module.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets model_field_not_found.gospel)
 (action
  (with-outputs-to model_field_not_found.mli.output
   (run %{checker} %{dep:model_field_not_found.mli}))))

(rule
 (alias runtest)
 (action
  (diff model_field_not_found.mli model_field_not_found.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:model_field_not_found.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets model_is_not_record.gospel)
 (action
  (with-outputs-to model_is_not_record.mli.output
   (run %{checker} %{dep:model_is_not_record.mli}))))

(rule
 (alias runtest)
 (action
  (diff model_is_not_record.mli model_is_not_record.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:model_is_not_record.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets model_shadows_field.gospel)
 (action
  (with-outputs-to model_shadows_field.mli.output
   (run %{checker} %{dep:model_shadows_field.mli}))))

(rule
 (alias runtest)
 (action
  (diff model_shadows_field.mli model_shadows_field.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:model_shadows_field.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets no_return.gospel)
 (action
  (with-outputs-to no_return.mli.output
   (run %{checker} %{dep:no_return.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_return.mli no_return.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:no_return.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets return_unit_and_ghost.gospel)
 (action
  (with-outputs-to return_unit_and_ghost.mli.output
   (run %{checker} %{dep:return_unit_and_ghost.mli}))))

(rule
 (alias runtest)
 (action
  (diff return_unit_and_ghost.mli return_unit_and_ghost.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:return_unit_and_ghost.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets stdlib_float_array_not_found.gospel)
 (action
  (with-outputs-to stdlib_float_array_not_found.mli.output
   (run %{checker} %{dep:stdlib_float_array_not_found.mli}))))

(rule
 (alias runtest)
 (action
  (diff stdlib_float_array_not_found.mli stdlib_float_array_not_found.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:stdlib_float_array_not_found.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets stdlib_float_array_t_not_found.gospel)
 (action
  (with-outputs-to stdlib_float_array_t_not_found.mli.output
   (run %{checker} %{dep:stdlib_float_array_t_not_found.mli}))))

(rule
 (alias runtest)
 (action
  (diff stdlib_float_array_t_not_found.mli stdlib_float_array_t_not_found.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:stdlib_float_array_t_not_found.mli}))))

