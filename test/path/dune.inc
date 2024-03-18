(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to path_test.mli.output
   (run %{checker} %{dep:path_test.mli} --p))))

(rule
 (alias runtest)
 (action
  (diff path_test.mli path_test.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:path_test.mli}))))

