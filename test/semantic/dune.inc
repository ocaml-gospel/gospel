(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets invariant1.gospel)
 (action
  (with-outputs-to invariant1.mli.output
   (run %{checker} %{dep:invariant1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant1.mli invariant1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets invariant2.gospel)
 (action
  (with-outputs-to invariant2.mli.output
   (run %{checker} %{dep:invariant2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant2.mli invariant2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets invariant3.gospel)
 (action
  (with-outputs-to invariant3.mli.output
   (run %{checker} %{dep:invariant3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant3.mli invariant3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant3.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets no_modifies_while_returning_unit.gospel)
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

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets old_in_precond.gospel)
 (action
  (with-outputs-to old_in_precond.mli.output
   (run %{checker} %{dep:old_in_precond.mli}))))

(rule
 (alias runtest)
 (action
  (diff old_in_precond.mli old_in_precond.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:old_in_precond.mli}))))

