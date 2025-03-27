(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_cast1.mli.output
   (run %{checker} %{dep:invalid_cast1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_cast1.mli invalid_cast1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_cast2.mli.output
   (run %{checker} %{dep:invalid_cast2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_cast2.mli invalid_cast2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_cast3.mli.output
   (run %{checker} %{dep:invalid_cast3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_cast3.mli invalid_cast3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_cast4.mli.output
   (run %{checker} %{dep:invalid_cast4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_cast4.mli invalid_cast4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_cast_tvar1.mli.output
   (run %{checker} %{dep:invalid_cast_tvar1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_cast_tvar1.mli invalid_cast_tvar1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_cast_tvar2.mli.output
   (run %{checker} %{dep:invalid_cast_tvar2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_cast_tvar2.mli invalid_cast_tvar2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_cast_tvar3.mli.output
   (run %{checker} %{dep:invalid_cast_tvar3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_cast_tvar3.mli invalid_cast_tvar3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to positive.mli.output
   (run %{checker} %{dep:positive.mli}))))

(rule
 (alias runtest)
 (action
  (diff positive.mli positive.mli.output)))

