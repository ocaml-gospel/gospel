(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to axioms.mli.output
   (run %{checker} %{dep:axioms.mli}))))

(rule
 (alias runtest)
 (action
  (diff axioms.mli axioms.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to functions.mli.output
   (run %{checker} %{dep:functions.mli}))))

(rule
 (alias runtest)
 (action
  (diff functions.mli functions.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to gospel_types.mli.output
   (run %{checker} %{dep:gospel_types.mli}))))

(rule
 (alias runtest)
 (action
  (diff gospel_types.mli gospel_types.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to modules.mli.output
   (run %{checker} %{dep:modules.mli}))))

(rule
 (alias runtest)
 (action
  (diff modules.mli modules.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to operators.mli.output
   (run %{checker} %{dep:operators.mli}))))

(rule
 (alias runtest)
 (action
  (diff operators.mli operators.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to terms.mli.output
   (run %{checker} %{dep:terms.mli}))))

(rule
 (alias runtest)
 (action
  (diff terms.mli terms.mli.output)))

