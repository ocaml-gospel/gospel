(rule
 (target impure1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:impure1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:impure1.mli} %{dep:impure1.mli.output})))

(rule
 (target impure2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:impure2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:impure2.mli} %{dep:impure2.mli.output})))

(rule
 (target impure3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:impure3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:impure3.mli} %{dep:impure3.mli.output})))

(rule
 (target impure4.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:impure4.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:impure4.mli} %{dep:impure4.mli.output})))

(rule
 (target not_pure.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:not_pure.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:not_pure.mli} %{dep:not_pure.mli.output})))

