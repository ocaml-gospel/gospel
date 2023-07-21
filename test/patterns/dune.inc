(rule
 (deps
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun.mli.output
   (run %{checker} %{dep:fun.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun.mli fun.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun_list.mli.output
   (run %{checker} %{dep:fun_list.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_list.mli fun_list.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun_list.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun_opt.mli.output
   (run %{checker} %{dep:fun_opt.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_opt.mli fun_opt.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun_opt.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun_opt_incomplete.mli.output
   (run %{checker} %{dep:fun_opt_incomplete.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_opt_incomplete.mli fun_opt_incomplete.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun_opt_incomplete.mli}))))

