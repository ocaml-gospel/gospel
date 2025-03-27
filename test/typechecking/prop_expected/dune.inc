(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to axiom.mli.output
   (run %{checker} %{dep:axiom.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom.mli axiom.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to if.mli.output
   (run %{checker} %{dep:if.mli}))))

(rule
 (alias runtest)
 (action
  (diff if.mli if.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to pred.mli.output
   (run %{checker} %{dep:pred.mli}))))

(rule
 (alias runtest)
 (action
  (diff pred.mli pred.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to quant.mli.output
   (run %{checker} %{dep:quant.mli}))))

(rule
 (alias runtest)
 (action
  (diff quant.mli quant.mli.output)))

