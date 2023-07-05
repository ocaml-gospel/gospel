(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to bad_constr_arg.mli.output
   (run %{checker} %{dep:bad_constr_arg.mli}))))

(rule
 (alias runtest)
 (action
  (diff bad_constr_arg.mli bad_constr_arg.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invalid_literal.mli.output
   (run %{checker} %{dep:invalid_literal.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_literal.mli invalid_literal.mli.output)))

