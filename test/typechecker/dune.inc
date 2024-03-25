(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets alpha_not_beta.gospel)
 (action
  (with-outputs-to alpha_not_beta.mli.output
   (run %{checker} %{dep:alpha_not_beta.mli}))))

(rule
 (alias runtest)
 (action
  (diff alpha_not_beta.mli alpha_not_beta.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:alpha_not_beta.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets any_not_bool.gospel)
 (action
  (with-outputs-to any_not_bool.mli.output
   (run %{checker} %{dep:any_not_bool.mli}))))

(rule
 (alias runtest)
 (action
  (diff any_not_bool.mli any_not_bool.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:any_not_bool.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets axiom.gospel)
 (action
  (with-outputs-to axiom.mli.output
   (run %{checker} %{dep:axiom.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom.mli axiom.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:axiom.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets axiom_float_not_integer.gospel)
 (action
  (with-outputs-to axiom_float_not_integer.mli.output
   (run %{checker} %{dep:axiom_float_not_integer.mli}))))

(rule
 (alias runtest)
 (action
  (diff axiom_float_not_integer.mli axiom_float_not_integer.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:axiom_float_not_integer.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets constructor_arity5.gospel)
 (action
  (with-outputs-to constructor_arity5.mli.output
   (run %{checker} %{dep:constructor_arity5.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity5.mli constructor_arity5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity5.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets duplicate_declaration.gospel)
 (action
  (with-outputs-to duplicate_declaration.mli.output
   (run %{checker} %{dep:duplicate_declaration.mli}))))

(rule
 (alias runtest)
 (action
  (diff duplicate_declaration.mli duplicate_declaration.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:duplicate_declaration.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets exception_arity.gospel)
 (action
  (with-outputs-to exception_arity.mli.output
   (run %{checker} %{dep:exception_arity.mli}))))

(rule
 (alias runtest)
 (action
  (diff exception_arity.mli exception_arity.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:exception_arity.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets exception_no_pattern.gospel)
 (action
  (with-outputs-to exception_no_pattern.mli.output
   (run %{checker} %{dep:exception_no_pattern.mli}))))

(rule
 (alias runtest)
 (action
  (diff exception_no_pattern.mli exception_no_pattern.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:exception_no_pattern.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets exn_arity.gospel)
 (action
  (with-outputs-to exn_arity.mli.output
   (run %{checker} %{dep:exn_arity.mli}))))

(rule
 (alias runtest)
 (action
  (diff exn_arity.mli exn_arity.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:exn_arity.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets faulty_header1.gospel)
 (action
  (with-outputs-to faulty_header1.mli.output
   (run %{checker} %{dep:faulty_header1.mli}))))

(rule
 (alias runtest)
 (action
  (diff faulty_header1.mli faulty_header1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:faulty_header1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets faulty_header2.gospel)
 (action
  (with-outputs-to faulty_header2.mli.output
   (run %{checker} %{dep:faulty_header2.mli}))))

(rule
 (alias runtest)
 (action
  (diff faulty_header2.mli faulty_header2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:faulty_header2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets faulty_header3.gospel)
 (action
  (with-outputs-to faulty_header3.mli.output
   (run %{checker} %{dep:faulty_header3.mli}))))

(rule
 (alias runtest)
 (action
  (diff faulty_header3.mli faulty_header3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:faulty_header3.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets float_not_int.gospel)
 (action
  (with-outputs-to float_not_int.mli.output
   (run %{checker} %{dep:float_not_int.mli}))))

(rule
 (alias runtest)
 (action
  (diff float_not_int.mli float_not_int.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:float_not_int.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets float_not_integer.gospel)
 (action
  (with-outputs-to float_not_integer.mli.output
   (run %{checker} %{dep:float_not_integer.mli}))))

(rule
 (alias runtest)
 (action
  (diff float_not_integer.mli float_not_integer.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:float_not_integer.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets function_float_not_integer.gospel)
 (action
  (with-outputs-to function_float_not_integer.mli.output
   (run %{checker} %{dep:function_float_not_integer.mli}))))

(rule
 (alias runtest)
 (action
  (diff function_float_not_integer.mli function_float_not_integer.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:function_float_not_integer.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets int_not_bool1.gospel)
 (action
  (with-outputs-to int_not_bool1.mli.output
   (run %{checker} %{dep:int_not_bool1.mli}))))

(rule
 (alias runtest)
 (action
  (diff int_not_bool1.mli int_not_bool1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:int_not_bool1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets int_not_bool2.gospel)
 (action
  (with-outputs-to int_not_bool2.mli.output
   (run %{checker} %{dep:int_not_bool2.mli}))))

(rule
 (alias runtest)
 (action
  (diff int_not_bool2.mli int_not_bool2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:int_not_bool2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets integer_not_bool1.gospel)
 (action
  (with-outputs-to integer_not_bool1.mli.output
   (run %{checker} %{dep:integer_not_bool1.mli}))))

(rule
 (alias runtest)
 (action
  (diff integer_not_bool1.mli integer_not_bool1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:integer_not_bool1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets integer_not_bool2.gospel)
 (action
  (with-outputs-to integer_not_bool2.mli.output
   (run %{checker} %{dep:integer_not_bool2.mli}))))

(rule
 (alias runtest)
 (action
  (diff integer_not_bool2.mli integer_not_bool2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:integer_not_bool2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets integer_not_bool3.gospel)
 (action
  (with-outputs-to integer_not_bool3.mli.output
   (run %{checker} %{dep:integer_not_bool3.mli}))))

(rule
 (alias runtest)
 (action
  (diff integer_not_bool3.mli integer_not_bool3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:integer_not_bool3.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets integer_not_bool4.gospel)
 (action
  (with-outputs-to integer_not_bool4.mli.output
   (run %{checker} %{dep:integer_not_bool4.mli}))))

(rule
 (alias runtest)
 (action
  (diff integer_not_bool4.mli integer_not_bool4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:integer_not_bool4.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets integer_not_int.gospel)
 (action
  (with-outputs-to integer_not_int.mli.output
   (run %{checker} %{dep:integer_not_int.mli}))))

(rule
 (alias runtest)
 (action
  (diff integer_not_int.mli integer_not_int.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:integer_not_int.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets invariant4.gospel)
 (action
  (with-outputs-to invariant4.mli.output
   (run %{checker} %{dep:invariant4.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant4.mli invariant4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant4.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets labeled_arg1.gospel)
 (action
  (with-outputs-to labeled_arg1.mli.output
   (run %{checker} %{dep:labeled_arg1.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg1.mli labeled_arg1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets labeled_arg2.gospel)
 (action
  (with-outputs-to labeled_arg2.mli.output
   (run %{checker} %{dep:labeled_arg2.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg2.mli labeled_arg2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets labeled_arg3.gospel)
 (action
  (with-outputs-to labeled_arg3.mli.output
   (run %{checker} %{dep:labeled_arg3.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg3.mli labeled_arg3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg3.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets labeled_arg4.gospel)
 (action
  (with-outputs-to labeled_arg4.mli.output
   (run %{checker} %{dep:labeled_arg4.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg4.mli labeled_arg4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg4.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets labeled_arg5.gospel)
 (action
  (with-outputs-to labeled_arg5.mli.output
   (run %{checker} %{dep:labeled_arg5.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg5.mli labeled_arg5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg5.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets labeled_arg6.gospel)
 (action
  (with-outputs-to labeled_arg6.mli.output
   (run %{checker} %{dep:labeled_arg6.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg6.mli labeled_arg6.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg6.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets labeled_arg7.gospel)
 (action
  (with-outputs-to labeled_arg7.mli.output
   (run %{checker} %{dep:labeled_arg7.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg7.mli labeled_arg7.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg7.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets labeled_arg8.gospel)
 (action
  (with-outputs-to labeled_arg8.mli.output
   (run %{checker} %{dep:labeled_arg8.mli}))))

(rule
 (alias runtest)
 (action
  (diff labeled_arg8.mli labeled_arg8.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:labeled_arg8.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
  int_not_bool1.mli)
 (targets open.gospel)
 (action
  (with-outputs-to open.mli.output
   (run %{checker} %{dep:open.mli}))))

(rule
 (alias runtest)
 (action
  (diff open.mli open.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:open.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets partial_application.gospel)
 (action
  (with-outputs-to partial_application.mli.output
   (run %{checker} %{dep:partial_application.mli}))))

(rule
 (alias runtest)
 (action
  (diff partial_application.mli partial_application.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:partial_application.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets raises_float_not_integer.gospel)
 (action
  (with-outputs-to raises_float_not_integer.mli.output
   (run %{checker} %{dep:raises_float_not_integer.mli}))))

(rule
 (alias runtest)
 (action
  (diff raises_float_not_integer.mli raises_float_not_integer.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:raises_float_not_integer.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets record_pattern4.gospel)
 (action
  (with-outputs-to record_pattern4.mli.output
   (run %{checker} %{dep:record_pattern4.mli}))))

(rule
 (alias runtest)
 (action
  (diff record_pattern4.mli record_pattern4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:record_pattern4.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets returned_tuple.gospel)
 (action
  (with-outputs-to returned_tuple.mli.output
   (run %{checker} %{dep:returned_tuple.mli}))))

(rule
 (alias runtest)
 (action
  (diff returned_tuple.mli returned_tuple.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:returned_tuple.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets t1_not_t2.gospel)
 (action
  (with-outputs-to t1_not_t2.mli.output
   (run %{checker} %{dep:t1_not_t2.mli}))))

(rule
 (alias runtest)
 (action
  (diff t1_not_t2.mli t1_not_t2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t1_not_t2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets tuple_arity1.gospel)
 (action
  (with-outputs-to tuple_arity1.mli.output
   (run %{checker} %{dep:tuple_arity1.mli}))))

(rule
 (alias runtest)
 (action
  (diff tuple_arity1.mli tuple_arity1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:tuple_arity1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets tuple_arity2.gospel)
 (action
  (with-outputs-to tuple_arity2.mli.output
   (run %{checker} %{dep:tuple_arity2.mli}))))

(rule
 (alias runtest)
 (action
  (diff tuple_arity2.mli tuple_arity2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:tuple_arity2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets type_arity1.gospel)
 (action
  (with-outputs-to type_arity1.mli.output
   (run %{checker} %{dep:type_arity1.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity1.mli type_arity1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:type_arity1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets type_arity2.gospel)
 (action
  (with-outputs-to type_arity2.mli.output
   (run %{checker} %{dep:type_arity2.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity2.mli type_arity2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:type_arity2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets type_arity3.gospel)
 (action
  (with-outputs-to type_arity3.mli.output
   (run %{checker} %{dep:type_arity3.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity3.mli type_arity3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:type_arity3.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets type_arity4.gospel)
 (action
  (with-outputs-to type_arity4.mli.output
   (run %{checker} %{dep:type_arity4.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity4.mli type_arity4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:type_arity4.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets type_arity5.gospel)
 (action
  (with-outputs-to type_arity5.mli.output
   (run %{checker} %{dep:type_arity5.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_arity5.mli type_arity5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:type_arity5.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets variant_integer.gospel)
 (action
  (with-outputs-to variant_integer.mli.output
   (run %{checker} %{dep:variant_integer.mli}))))

(rule
 (alias runtest)
 (action
  (diff variant_integer.mli variant_integer.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:variant_integer.mli}))))

