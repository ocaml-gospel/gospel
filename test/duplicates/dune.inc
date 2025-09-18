(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_consumes_top_level1.mli.output
   (run %{checker} %{dep:duplicate_consumes_top_level1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_consumes_top_level1.mli duplicate_consumes_top_level1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_consumes_top_level2.mli.output
   (run %{checker} %{dep:duplicate_consumes_top_level2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_consumes_top_level2.mli duplicate_consumes_top_level2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_consumes_top_level3.mli.output
   (run %{checker} %{dep:duplicate_consumes_top_level3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_consumes_top_level3.mli duplicate_consumes_top_level3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_exn1.mli.output
   (run %{checker} %{dep:duplicate_exn1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_exn1.mli duplicate_exn1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_exn2.mli.output
   (run %{checker} %{dep:duplicate_exn2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_exn2.mli duplicate_exn2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_exn3.mli.output
   (run %{checker} %{dep:duplicate_exn3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_exn3.mli duplicate_exn3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_exn4.mli.output
   (run %{checker} %{dep:duplicate_exn4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_exn4.mli duplicate_exn4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_exn_ret1.mli.output
   (run %{checker} %{dep:duplicate_exn_ret1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_exn_ret1.mli duplicate_exn_ret1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_exn_ret2.mli.output
   (run %{checker} %{dep:duplicate_exn_ret2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_exn_ret2.mli duplicate_exn_ret2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_exn_ret3.mli.output
   (run %{checker} %{dep:duplicate_exn_ret3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_exn_ret3.mli duplicate_exn_ret3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_exn_ret4.mli.output
   (run %{checker} %{dep:duplicate_exn_ret4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_exn_ret4.mli duplicate_exn_ret4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_fun_args1.mli.output
   (run %{checker} %{dep:duplicate_fun_args1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_fun_args1.mli duplicate_fun_args1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_fun_args2.mli.output
   (run %{checker} %{dep:duplicate_fun_args2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_fun_args2.mli duplicate_fun_args2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_fun_args3.mli.output
   (run %{checker} %{dep:duplicate_fun_args3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_fun_args3.mli duplicate_fun_args3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_fun_args4.mli.output
   (run %{checker} %{dep:duplicate_fun_args4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_fun_args4.mli duplicate_fun_args4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_args1.mli.output
   (run %{checker} %{dep:duplicate_header_args1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_args1.mli duplicate_header_args1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_args2.mli.output
   (run %{checker} %{dep:duplicate_header_args2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_args2.mli duplicate_header_args2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_args3.mli.output
   (run %{checker} %{dep:duplicate_header_args3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_args3.mli duplicate_header_args3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_args4.mli.output
   (run %{checker} %{dep:duplicate_header_args4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_args4.mli duplicate_header_args4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_rets1.mli.output
   (run %{checker} %{dep:duplicate_header_rets1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_rets1.mli duplicate_header_rets1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_rets2.mli.output
   (run %{checker} %{dep:duplicate_header_rets2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_rets2.mli duplicate_header_rets2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_rets3.mli.output
   (run %{checker} %{dep:duplicate_header_rets3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_rets3.mli duplicate_header_rets3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_rets4.mli.output
   (run %{checker} %{dep:duplicate_header_rets4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_rets4.mli duplicate_header_rets4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_vars1.mli.output
   (run %{checker} %{dep:duplicate_header_vars1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_vars1.mli duplicate_header_vars1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_vars2.mli.output
   (run %{checker} %{dep:duplicate_header_vars2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_vars2.mli duplicate_header_vars2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_vars3.mli.output
   (run %{checker} %{dep:duplicate_header_vars3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_vars3.mli duplicate_header_vars3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_vars4.mli.output
   (run %{checker} %{dep:duplicate_header_vars4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_vars4.mli duplicate_header_vars4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_header_vars5.mli.output
   (run %{checker} %{dep:duplicate_header_vars5.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_header_vars5.mli duplicate_header_vars5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_label1.mli.output
   (run %{checker} %{dep:duplicate_label1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_label1.mli duplicate_label1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_label2.mli.output
   (run %{checker} %{dep:duplicate_label2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_label2.mli duplicate_label2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_label3.mli.output
   (run %{checker} %{dep:duplicate_label3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_label3.mli duplicate_label3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_label4.mli.output
   (run %{checker} %{dep:duplicate_label4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_label4.mli duplicate_label4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_label_creation1.mli.output
   (run %{checker} %{dep:duplicate_label_creation1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_label_creation1.mli duplicate_label_creation1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_label_creation2.mli.output
   (run %{checker} %{dep:duplicate_label_creation2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_label_creation2.mli duplicate_label_creation2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_label_creation3.mli.output
   (run %{checker} %{dep:duplicate_label_creation3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_label_creation3.mli duplicate_label_creation3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_label_creation4.mli.output
   (run %{checker} %{dep:duplicate_label_creation4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_label_creation4.mli duplicate_label_creation4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_modifies_top_level1.mli.output
   (run %{checker} %{dep:duplicate_modifies_top_level1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_modifies_top_level1.mli duplicate_modifies_top_level1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_modifies_top_level2.mli.output
   (run %{checker} %{dep:duplicate_modifies_top_level2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_modifies_top_level2.mli duplicate_modifies_top_level2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_modifies_top_level3.mli.output
   (run %{checker} %{dep:duplicate_modifies_top_level3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_modifies_top_level3.mli duplicate_modifies_top_level3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_pred_args1.mli.output
   (run %{checker} %{dep:duplicate_pred_args1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_pred_args1.mli duplicate_pred_args1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_pred_args2.mli.output
   (run %{checker} %{dep:duplicate_pred_args2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_pred_args2.mli duplicate_pred_args2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_pred_args3.mli.output
   (run %{checker} %{dep:duplicate_pred_args3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_pred_args3.mli duplicate_pred_args3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_pred_args4.mli.output
   (run %{checker} %{dep:duplicate_pred_args4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_pred_args4.mli duplicate_pred_args4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_preserves_top_level1.mli.output
   (run %{checker} %{dep:duplicate_preserves_top_level1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_preserves_top_level1.mli duplicate_preserves_top_level1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_preserves_top_level2.mli.output
   (run %{checker} %{dep:duplicate_preserves_top_level2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_preserves_top_level2.mli duplicate_preserves_top_level2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_preserves_top_level3.mli.output
   (run %{checker} %{dep:duplicate_preserves_top_level3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_preserves_top_level3.mli duplicate_preserves_top_level3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_type_vars1.mli.output
   (run %{checker} %{dep:duplicate_type_vars1.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_type_vars1.mli duplicate_type_vars1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_type_vars2.mli.output
   (run %{checker} %{dep:duplicate_type_vars2.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_type_vars2.mli duplicate_type_vars2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_type_vars3.mli.output
   (run %{checker} %{dep:duplicate_type_vars3.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_type_vars3.mli duplicate_type_vars3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to duplicate_type_vars4.mli.output
   (run %{checker} %{dep:duplicate_type_vars4.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_type_vars4.mli duplicate_type_vars4.mli.output)))

