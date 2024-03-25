(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets bad_type_id.gospel)
 (action
  (with-outputs-to bad_type_id.mli.output
   (run %{checker} %{dep:bad_type_id.mli}))))

(rule
 (alias runtest)
 (action
  (diff bad_type_id.mli bad_type_id.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:bad_type_id.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets bad_type_multiple_args.gospel)
 (action
  (with-outputs-to bad_type_multiple_args.mli.output
   (run %{checker} %{dep:bad_type_multiple_args.mli}))))

(rule
 (alias runtest)
 (action
  (diff bad_type_multiple_args.mli bad_type_multiple_args.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:bad_type_multiple_args.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets basic.gospel)
 (action
  (with-outputs-to basic.mli.output
   (run %{checker} %{dep:basic.mli}))))

(rule
 (alias runtest)
 (action
  (diff basic.mli basic.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:basic.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets complex_cycle.gospel)
 (action
  (with-outputs-to complex_cycle.mli.output
   (run %{checker} %{dep:complex_cycle.mli}))))

(rule
 (alias runtest)
 (action
  (diff complex_cycle.mli complex_cycle.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:complex_cycle.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets constr_literal.gospel)
 (action
  (with-outputs-to constr_literal.mli.output
   (run %{checker} %{dep:constr_literal.mli}))))

(rule
 (alias runtest)
 (action
  (diff constr_literal.mli constr_literal.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constr_literal.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets double_definition.gospel)
 (action
  (with-outputs-to double_definition.mli.output
   (run %{checker} %{dep:double_definition.mli}))))

(rule
 (alias runtest)
 (action
  (diff double_definition.mli double_definition.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:double_definition.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets simple_cycle.gospel)
 (action
  (with-outputs-to simple_cycle.mli.output
   (run %{checker} %{dep:simple_cycle.mli}))))

(rule
 (alias runtest)
 (action
  (diff simple_cycle.mli simple_cycle.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:simple_cycle.mli}))))

