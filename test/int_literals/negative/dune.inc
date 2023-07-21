(rule
 (deps
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to bad_constr_arg.mli.output
   (run %{checker} %{dep:bad_constr_arg.mli}))))

(rule
 (alias runtest)
 (action
  (diff bad_constr_arg.mli bad_constr_arg.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:bad_constr_arg.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to invalid_literal.mli.output
   (run %{checker} %{dep:invalid_literal.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_literal.mli invalid_literal.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invalid_literal.mli}))))

