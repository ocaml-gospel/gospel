(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_field_access.mli.output
   (run %{checker} %{dep:invalid_field_access.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_field_access.mli invalid_field_access.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_infix.mli.output
   (run %{checker} %{dep:invalid_infix.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_infix.mli invalid_infix.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_let1.mli.output
   (run %{checker} %{dep:invalid_let1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_let1.mli invalid_let1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_let2.mli.output
   (run %{checker} %{dep:invalid_let2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_let2.mli invalid_let2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_let3.mli.output
   (run %{checker} %{dep:invalid_let3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_let3.mli invalid_let3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_let4.mli.output
   (run %{checker} %{dep:invalid_let4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_let4.mli invalid_let4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_quant1.mli.output
   (run %{checker} %{dep:invalid_quant1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_quant1.mli invalid_quant1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_quant2.mli.output
   (run %{checker} %{dep:invalid_quant2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_quant2.mli invalid_quant2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_quant3.mli.output
   (run %{checker} %{dep:invalid_quant3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_quant3.mli invalid_quant3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_quant4.mli.output
   (run %{checker} %{dep:invalid_quant4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_quant4.mli invalid_quant4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_record1.mli.output
   (run %{checker} %{dep:invalid_record1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_record1.mli invalid_record1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_record2.mli.output
   (run %{checker} %{dep:invalid_record2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_record2.mli invalid_record2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_record3.mli.output
   (run %{checker} %{dep:invalid_record3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_record3.mli invalid_record3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unmatched_paren1.mli.output
   (run %{checker} %{dep:unmatched_paren1.mli}))))

(rule
 (alias runtest)
 (action
  (diff unmatched_paren1.mli unmatched_paren1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to unmatched_paren2.mli.output
   (run %{checker} %{dep:unmatched_paren2.mli}))))

(rule
 (alias runtest)
 (action
  (diff unmatched_paren2.mli unmatched_paren2.mli.output)))

