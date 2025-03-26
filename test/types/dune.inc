(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_arity1.mli.output
   (run %{checker} %{dep:type_arity1.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity1.mli type_arity1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_arity2.mli.output
   (run %{checker} %{dep:type_arity2.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity2.mli type_arity2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_arity3.mli.output
   (run %{checker} %{dep:type_arity3.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity3.mli type_arity3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_arity4.mli.output
   (run %{checker} %{dep:type_arity4.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity4.mli type_arity4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_arity5.mli.output
   (run %{checker} %{dep:type_arity5.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity5.mli type_arity5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to type_arity6.mli.output
   (run %{checker} %{dep:type_arity6.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity6.mli type_arity6.mli.output)))

