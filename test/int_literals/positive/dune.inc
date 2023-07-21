(rule
 (deps
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to ints.mli.output
   (run %{checker} %{dep:ints.mli}))))

(rule
 (alias runtest)
 (action
  (diff ints.mli ints.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:ints.mli}))))

