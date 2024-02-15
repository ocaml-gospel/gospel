(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets impure_not_promoted.gospel)
 (action
  (with-outputs-to impure_not_promoted.mli.output
   (run %{checker} %{dep:impure_not_promoted.mli}))))

(rule
 (alias runtest)
 (action
  (diff impure_not_promoted.mli impure_not_promoted.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:impure_not_promoted.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets pure_no_diverges.gospel)
 (action
  (with-outputs-to pure_no_diverges.mli.output
   (run %{checker} %{dep:pure_no_diverges.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_no_diverges.mli pure_no_diverges.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pure_no_diverges.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets pure_no_exceptions1.gospel)
 (action
  (with-outputs-to pure_no_exceptions1.mli.output
   (run %{checker} %{dep:pure_no_exceptions1.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_no_exceptions1.mli pure_no_exceptions1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pure_no_exceptions1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets pure_no_exceptions2.gospel)
 (action
  (with-outputs-to pure_no_exceptions2.mli.output
   (run %{checker} %{dep:pure_no_exceptions2.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_no_exceptions2.mli pure_no_exceptions2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pure_no_exceptions2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets pure_no_modifies.gospel)
 (action
  (with-outputs-to pure_no_modifies.mli.output
   (run %{checker} %{dep:pure_no_modifies.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_no_modifies.mli pure_no_modifies.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pure_no_modifies.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets pure_promoted.gospel)
 (action
  (with-outputs-to pure_promoted.mli.output
   (run %{checker} %{dep:pure_promoted.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_promoted.mli pure_promoted.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pure_promoted.mli}))))

