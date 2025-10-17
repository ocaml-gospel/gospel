(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_invalid_let.mli.output
   (run %{checker} %{dep:function_invalid_let.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_invalid_let.mli function_invalid_let.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_invalid_operator1.mli.output
   (run %{checker} %{dep:function_invalid_operator1.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_invalid_operator1.mli function_invalid_operator1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_invalid_operator2.mli.output
   (run %{checker} %{dep:function_invalid_operator2.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_invalid_operator2.mli function_invalid_operator2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_invalid_operator3.mli.output
   (run %{checker} %{dep:function_invalid_operator3.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_invalid_operator3.mli function_invalid_operator3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_invalid_parameter1.mli.output
   (run %{checker} %{dep:function_invalid_parameter1.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_invalid_parameter1.mli function_invalid_parameter1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_invalid_parameter2.mli.output
   (run %{checker} %{dep:function_invalid_parameter2.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_invalid_parameter2.mli function_invalid_parameter2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_invalid_parameter3.mli.output
   (run %{checker} %{dep:function_invalid_parameter3.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_invalid_parameter3.mli function_invalid_parameter3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_invalid_parameter4.mli.output
   (run %{checker} %{dep:function_invalid_parameter4.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_invalid_parameter4.mli function_invalid_parameter4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_no_name.mli.output
   (run %{checker} %{dep:function_no_name.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_no_name.mli function_no_name.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_no_ret1.mli.output
   (run %{checker} %{dep:function_no_ret1.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_no_ret1.mli function_no_ret1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_no_ret2.mli.output
   (run %{checker} %{dep:function_no_ret2.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_no_ret2.mli function_no_ret2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_no_ret3.mli.output
   (run %{checker} %{dep:function_no_ret3.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_no_ret3.mli function_no_ret3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_no_ret4.mli.output
   (run %{checker} %{dep:function_no_ret4.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_no_ret4.mli function_no_ret4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_no_ret5.mli.output
   (run %{checker} %{dep:function_no_ret5.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_no_ret5.mli function_no_ret5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_no_ret6.mli.output
   (run %{checker} %{dep:function_no_ret6.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_no_ret6.mli function_no_ret6.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to function_num_name.mli.output
   (run %{checker} %{dep:function_num_name.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_num_name.mli function_num_name.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_invalid_operator1.mli.output
   (run %{checker} %{dep:predicate_invalid_operator1.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_invalid_operator1.mli predicate_invalid_operator1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_invalid_operator2.mli.output
   (run %{checker} %{dep:predicate_invalid_operator2.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_invalid_operator2.mli predicate_invalid_operator2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_invalid_operator3.mli.output
   (run %{checker} %{dep:predicate_invalid_operator3.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_invalid_operator3.mli predicate_invalid_operator3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_invalid_parameter1.mli.output
   (run %{checker} %{dep:predicate_invalid_parameter1.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_invalid_parameter1.mli predicate_invalid_parameter1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_invalid_parameter2.mli.output
   (run %{checker} %{dep:predicate_invalid_parameter2.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_invalid_parameter2.mli predicate_invalid_parameter2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_invalid_parameter3.mli.output
   (run %{checker} %{dep:predicate_invalid_parameter3.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_invalid_parameter3.mli predicate_invalid_parameter3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_invalid_parameter4.mli.output
   (run %{checker} %{dep:predicate_invalid_parameter4.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_invalid_parameter4.mli predicate_invalid_parameter4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_no_name.mli.output
   (run %{checker} %{dep:predicate_no_name.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_no_name.mli predicate_no_name.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_num_name.mli.output
   (run %{checker} %{dep:predicate_num_name.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_num_name.mli predicate_num_name.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_ret1.mli.output
   (run %{checker} %{dep:predicate_ret1.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_ret1.mli predicate_ret1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_ret2.mli.output
   (run %{checker} %{dep:predicate_ret2.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_ret2.mli predicate_ret2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_ret3.mli.output
   (run %{checker} %{dep:predicate_ret3.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_ret3.mli predicate_ret3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_ret4.mli.output
   (run %{checker} %{dep:predicate_ret4.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_ret4.mli predicate_ret4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_ret5.mli.output
   (run %{checker} %{dep:predicate_ret5.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_ret5.mli predicate_ret5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to predicate_ret6.mli.output
   (run %{checker} %{dep:predicate_ret6.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicate_ret6.mli predicate_ret6.mli.output)))

