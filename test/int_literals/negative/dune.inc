(rule
 (target bad_constr_arg.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:bad_constr_arg.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:bad_constr_arg.mli} %{dep:bad_constr_arg.mli.output})))

(rule
 (target invalid_literal.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:invalid_literal.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:invalid_literal.mli} %{dep:invalid_literal.mli.output})))

