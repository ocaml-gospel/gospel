(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t1.mli.output
   (run %{checker} %{dep:t1.mli}))))

(rule
 (alias runtest)
 (action
  (diff t1.mli t1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t2.mli.output
   (run %{checker} %{dep:t2.mli}))))

(rule
 (alias runtest)
 (action
  (diff t2.mli t2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t3.mli.output
   (run %{checker} %{dep:t3.mli}))))

(rule
 (alias runtest)
 (action
  (diff t3.mli t3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t4.mli.output
   (run %{checker} %{dep:t4.mli}))))

(rule
 (alias runtest)
 (action
  (diff t4.mli t4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t4.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t5.mli.output
   (run %{checker} %{dep:t5.mli}))))

(rule
 (alias runtest)
 (action
  (diff t5.mli t5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t5.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t6.mli.output
   (run %{checker} %{dep:t6.mli}))))

(rule
 (alias runtest)
 (action
  (diff t6.mli t6.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t6.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t7.mli.output
   (run %{checker} %{dep:t7.mli}))))

(rule
 (alias runtest)
 (action
  (diff t7.mli t7.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t7.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t8.mli.output
   (run %{checker} %{dep:t8.mli}))))

(rule
 (alias runtest)
 (action
  (diff t8.mli t8.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t8.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t9.mli.output
   (run %{checker} %{dep:t9.mli}))))

(rule
 (alias runtest)
 (action
  (diff t9.mli t9.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t9.mli}))))

