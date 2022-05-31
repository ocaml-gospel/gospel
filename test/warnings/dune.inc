(rule
 (target no_modifies_while_returning_unit.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:no_modifies_while_returning_unit.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:no_modifies_while_returning_unit.mli} %{dep:no_modifies_while_returning_unit.mli.output})))

