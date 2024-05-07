(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Stack.gospel)
 (action
  (with-outputs-to Stack.mli.output
   (run %{checker} %{dep:Stack.mli}))))

(rule
 (alias runtest)
 (action
  (diff Stack.mli Stack.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Stack.mli}))))

