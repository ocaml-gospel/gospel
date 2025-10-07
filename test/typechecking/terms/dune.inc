(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ho1.mli.output
   (run %{checker} %{dep:ho1.mli}))))

(rule
 (alias runtest)
 (action
  (diff ho1.mli ho1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ho2.mli.output
   (run %{checker} %{dep:ho2.mli}))))

(rule
 (alias runtest)
 (action
  (diff ho2.mli ho2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ho3.mli.output
   (run %{checker} %{dep:ho3.mli}))))

(rule
 (alias runtest)
 (action
  (diff ho3.mli ho3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ho4.mli.output
   (run %{checker} %{dep:ho4.mli}))))

(rule
 (alias runtest)
 (action
  (diff ho4.mli ho4.mli.output)))

