(rule
 (deps
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to pure.mli.output
   (run %{checker} %{dep:pure.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure.mli pure.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pure.mli}))))

