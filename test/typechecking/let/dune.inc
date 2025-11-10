(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to let_destruct1.mli.output
   (run %{checker} %{dep:let_destruct1.mli}))))

(rule
 (alias runtest)
 (action
  (diff let_destruct1.mli let_destruct1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to let_destruct2.mli.output
   (run %{checker} %{dep:let_destruct2.mli}))))

(rule
 (alias runtest)
 (action
  (diff let_destruct2.mli let_destruct2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to let_destruct3.mli.output
   (run %{checker} %{dep:let_destruct3.mli}))))

(rule
 (alias runtest)
 (action
  (diff let_destruct3.mli let_destruct3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to let_destruct4.mli.output
   (run %{checker} %{dep:let_destruct4.mli}))))

(rule
 (alias runtest)
 (action
  (diff let_destruct4.mli let_destruct4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to let_destruct5.mli.output
   (run %{checker} %{dep:let_destruct5.mli}))))

(rule
 (alias runtest)
 (action
  (diff let_destruct5.mli let_destruct5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to let_destruct6.mli.output
   (run %{checker} %{dep:let_destruct6.mli}))))

(rule
 (alias runtest)
 (action
  (diff let_destruct6.mli let_destruct6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to let_destruct7.mli.output
   (run %{checker} %{dep:let_destruct7.mli}))))

(rule
 (alias runtest)
 (action
  (diff let_destruct7.mli let_destruct7.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to let_destruct8.mli.output
   (run %{checker} %{dep:let_destruct8.mli}))))

(rule
 (alias runtest)
 (action
  (diff let_destruct8.mli let_destruct8.mli.output)))

