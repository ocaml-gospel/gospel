(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to bad_type_id.mli.output
   (run %{checker} %{dep:bad_type_id.mli}))))

(rule
 (alias runtest)
 (action
  (diff bad_type_id.mli bad_type_id.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to bad_type_multiple_args.mli.output
   (run %{checker} %{dep:bad_type_multiple_args.mli}))))

(rule
 (alias runtest)
 (action
  (diff bad_type_multiple_args.mli bad_type_multiple_args.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to complex_cycle.mli.output
   (run %{checker} %{dep:complex_cycle.mli}))))

(rule
 (alias runtest)
 (action
  (diff complex_cycle.mli complex_cycle.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to double_definition.mli.output
   (run %{checker} %{dep:double_definition.mli}))))

(rule
 (alias runtest)
 (action
  (diff double_definition.mli double_definition.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to simple_cycle.mli.output
   (run %{checker} %{dep:simple_cycle.mli}))))

(rule
 (alias runtest)
 (action
  (diff simple_cycle.mli simple_cycle.mli.output)))

