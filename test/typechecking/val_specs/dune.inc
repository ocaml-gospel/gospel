(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cant_return_unit1.mli.output
   (run %{checker} %{dep:cant_return_unit1.mli}))))

(rule
 (alias runtest)
 (action
  (diff cant_return_unit1.mli cant_return_unit1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cant_return_unit2.mli.output
   (run %{checker} %{dep:cant_return_unit2.mli}))))

(rule
 (alias runtest)
 (action
  (diff cant_return_unit2.mli cant_return_unit2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cant_return_unit3.mli.output
   (run %{checker} %{dep:cant_return_unit3.mli}))))

(rule
 (alias runtest)
 (action
  (diff cant_return_unit3.mli cant_return_unit3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to cant_return_unit4.mli.output
   (run %{checker} %{dep:cant_return_unit4.mli}))))

(rule
 (alias runtest)
 (action
  (diff cant_return_unit4.mli cant_return_unit4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to higher_order.mli.output
   (run %{checker} %{dep:higher_order.mli}))))

(rule
 (alias runtest)
 (action
  (diff higher_order.mli higher_order.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number1.mli.output
   (run %{checker} %{dep:invalid_arg_number1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number1.mli invalid_arg_number1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number2.mli.output
   (run %{checker} %{dep:invalid_arg_number2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number2.mli invalid_arg_number2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number3.mli.output
   (run %{checker} %{dep:invalid_arg_number3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number3.mli invalid_arg_number3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number4.mli.output
   (run %{checker} %{dep:invalid_arg_number4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number4.mli invalid_arg_number4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_arg_number5.mli.output
   (run %{checker} %{dep:invalid_arg_number5.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_arg_number5.mli invalid_arg_number5.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret_number1.mli.output
   (run %{checker} %{dep:invalid_ret_number1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret_number1.mli invalid_ret_number1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret_number2.mli.output
   (run %{checker} %{dep:invalid_ret_number2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret_number2.mli invalid_ret_number2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_ret_number3.mli.output
   (run %{checker} %{dep:invalid_ret_number3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_ret_number3.mli invalid_ret_number3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_unit1.mli.output
   (run %{checker} %{dep:invalid_unit1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_unit1.mli invalid_unit1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_unit2.mli.output
   (run %{checker} %{dep:invalid_unit2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_unit2.mli invalid_unit2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to pure_cant_consume.mli.output
   (run %{checker} %{dep:pure_cant_consume.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_cant_consume.mli pure_cant_consume.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to pure_cant_diverge.mli.output
   (run %{checker} %{dep:pure_cant_diverge.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_cant_diverge.mli pure_cant_diverge.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to pure_cant_modify.mli.output
   (run %{checker} %{dep:pure_cant_modify.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_cant_modify.mli pure_cant_modify.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to pure_cant_raise.mli.output
   (run %{checker} %{dep:pure_cant_raise.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure_cant_raise.mli pure_cant_raise.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to return_option.mli.output
   (run %{checker} %{dep:return_option.mli}))))

(rule
 (alias runtest)
 (action
  (diff return_option.mli return_option.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to valid_exn_match.mli.output
   (run %{checker} %{dep:valid_exn_match.mli}))))

(rule
 (alias runtest)
 (action
  (diff valid_exn_match.mli valid_exn_match.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to valid_ghost_ret.mli.output
   (run %{checker} %{dep:valid_ghost_ret.mli}))))

(rule
 (alias runtest)
 (action
  (diff valid_ghost_ret.mli valid_ghost_ret.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to valid_ret_number.mli.output
   (run %{checker} %{dep:valid_ret_number.mli}))))

(rule
 (alias runtest)
 (action
  (diff valid_ret_number.mli valid_ret_number.mli.output)))

