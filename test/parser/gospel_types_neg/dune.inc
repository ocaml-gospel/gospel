(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to empty_record1.mli.output
   (run %{checker} %{dep:empty_record1.mli}))))

(rule
 (alias runtest)
 (action
  (diff empty_record1.mli empty_record1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to empty_record2.mli.output
   (run %{checker} %{dep:empty_record2.mli}))))

(rule
 (alias runtest)
 (action
  (diff empty_record2.mli empty_record2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_name1.mli.output
   (run %{checker} %{dep:invalid_name1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_name1.mli invalid_name1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_name2.mli.output
   (run %{checker} %{dep:invalid_name2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_name2.mli invalid_name2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_record_field1.mli.output
   (run %{checker} %{dep:invalid_record_field1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_record_field1.mli invalid_record_field1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_record_field2.mli.output
   (run %{checker} %{dep:invalid_record_field2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_record_field2.mli invalid_record_field2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_record_field3.mli.output
   (run %{checker} %{dep:invalid_record_field3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_record_field3.mli invalid_record_field3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_record_field4.mli.output
   (run %{checker} %{dep:invalid_record_field4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_record_field4.mli invalid_record_field4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_annot1.mli.output
   (run %{checker} %{dep:invalid_type_annot1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_annot1.mli invalid_type_annot1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_annot2.mli.output
   (run %{checker} %{dep:invalid_type_annot2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_annot2.mli invalid_type_annot2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_annot3.mli.output
   (run %{checker} %{dep:invalid_type_annot3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_annot3.mli invalid_type_annot3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_annot4.mli.output
   (run %{checker} %{dep:invalid_type_annot4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_annot4.mli invalid_type_annot4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_annot5.mli.output
   (run %{checker} %{dep:invalid_type_annot5.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_annot5.mli invalid_type_annot5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_params1.mli.output
   (run %{checker} %{dep:invalid_type_params1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_params1.mli invalid_type_params1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_params2.mli.output
   (run %{checker} %{dep:invalid_type_params2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_params2.mli invalid_type_params2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_params3.mli.output
   (run %{checker} %{dep:invalid_type_params3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_params3.mli invalid_type_params3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_type_params4.mli.output
   (run %{checker} %{dep:invalid_type_params4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_type_params4.mli invalid_type_params4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to no_name.mli.output
   (run %{checker} %{dep:no_name.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_name.mli no_name.mli.output)))

