(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to wrong_arity1.mli.output
   (run %{checker} %{dep:wrong_arity1.mli}))))

(rule
 (alias runtest)
 (action
  (diff wrong_arity1.mli wrong_arity1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to wrong_arity2.mli.output
   (run %{checker} %{dep:wrong_arity2.mli}))))

(rule
 (alias runtest)
 (action
  (diff wrong_arity2.mli wrong_arity2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to wrong_arity3.mli.output
   (run %{checker} %{dep:wrong_arity3.mli}))))

(rule
 (alias runtest)
 (action
  (diff wrong_arity3.mli wrong_arity3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to wrong_arity4.mli.output
   (run %{checker} %{dep:wrong_arity4.mli}))))

(rule
 (alias runtest)
 (action
  (diff wrong_arity4.mli wrong_arity4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to wrong_arity5.mli.output
   (run %{checker} %{dep:wrong_arity5.mli}))))

(rule
 (alias runtest)
 (action
  (diff wrong_arity5.mli wrong_arity5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to wrong_arity6.mli.output
   (run %{checker} %{dep:wrong_arity6.mli}))))

(rule
 (alias runtest)
 (action
  (diff wrong_arity6.mli wrong_arity6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to wrong_arity7.mli.output
   (run %{checker} %{dep:wrong_arity7.mli}))))

(rule
 (alias runtest)
 (action
  (diff wrong_arity7.mli wrong_arity7.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to wrong_arity8.mli.output
   (run %{checker} %{dep:wrong_arity8.mli}))))

(rule
 (alias runtest)
 (action
  (diff wrong_arity8.mli wrong_arity8.mli.output)))

