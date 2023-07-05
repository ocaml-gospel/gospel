(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to impure1.mli.output
   (run %{checker} %{dep:impure1.mli}))))

(rule
 (alias runtest)
 (action
  (diff impure1.mli impure1.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to impure2.mli.output
   (run %{checker} %{dep:impure2.mli}))))

(rule
 (alias runtest)
 (action
  (diff impure2.mli impure2.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to impure3.mli.output
   (run %{checker} %{dep:impure3.mli}))))

(rule
 (alias runtest)
 (action
  (diff impure3.mli impure3.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to impure4.mli.output
   (run %{checker} %{dep:impure4.mli}))))

(rule
 (alias runtest)
 (action
  (diff impure4.mli impure4.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to not_pure.mli.output
   (run %{checker} %{dep:not_pure.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_pure.mli not_pure.mli.output)))

