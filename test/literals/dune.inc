(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets char1.gospel)
 (action
  (with-outputs-to char1.mli.output
   (run %{checker} %{dep:char1.mli}))))

(rule
 (alias runtest)
 (action
  (diff char1.mli char1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:char1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets ints.gospel)
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

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets invalid_literal.gospel)
 (action
  (with-outputs-to invalid_literal.mli.output
   (run %{checker} %{dep:invalid_literal.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_literal.mli invalid_literal.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invalid_literal.mli}))))

