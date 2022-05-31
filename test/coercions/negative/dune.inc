(rule
 (target bad_type_id.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:bad_type_id.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:bad_type_id.mli} %{dep:bad_type_id.mli.output})))

(rule
 (target bad_type_multiple_args.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:bad_type_multiple_args.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:bad_type_multiple_args.mli} %{dep:bad_type_multiple_args.mli.output})))

(rule
 (target complex_cycle.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:complex_cycle.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:complex_cycle.mli} %{dep:complex_cycle.mli.output})))

(rule
 (target double_definition.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:double_definition.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:double_definition.mli} %{dep:double_definition.mli.output})))

(rule
 (target simple_cycle.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:simple_cycle.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:simple_cycle.mli} %{dep:simple_cycle.mli.output})))

