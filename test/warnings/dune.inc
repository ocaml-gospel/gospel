(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to no_modifies_value_of_unit.mli.output
   (run %{checker} %{dep:no_modifies_value_of_unit.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_modifies_value_of_unit.mli no_modifies_value_of_unit.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:no_modifies_value_of_unit.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to no_modifies_while_returning_unit.mli.output
   (run %{checker} %{dep:no_modifies_while_returning_unit.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_modifies_while_returning_unit.mli no_modifies_while_returning_unit.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:no_modifies_while_returning_unit.mli}))))

