(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to axiom_no_def1.mli.output
   (run %{checker} %{dep:axiom_no_def1.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom_no_def1.mli axiom_no_def1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to axiom_no_def2.mli.output
   (run %{checker} %{dep:axiom_no_def2.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom_no_def2.mli axiom_no_def2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to axiom_no_name.mli.output
   (run %{checker} %{dep:axiom_no_name.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom_no_name.mli axiom_no_name.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to axiom_num_name.mli.output
   (run %{checker} %{dep:axiom_num_name.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom_num_name.mli axiom_num_name.mli.output)))

