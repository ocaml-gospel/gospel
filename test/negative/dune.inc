(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to char1.mli.output
   (run %{checker} %{dep:char1.mli}))))

(rule
 (alias runtest)
 (action
  (diff char1.mli char1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:char1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constants1.mli.output
   (run %{checker} %{dep:constants1.mli}))))

(rule
 (alias runtest)
 (action
  (diff constants1.mli constants1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constants1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity1.mli.output
   (run %{checker} %{dep:constructor_arity1.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity1.mli constructor_arity1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity2.mli.output
   (run %{checker} %{dep:constructor_arity2.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity2.mli constructor_arity2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity3.mli.output
   (run %{checker} %{dep:constructor_arity3.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity3.mli constructor_arity3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constructor_arity4.mli.output
   (run %{checker} %{dep:constructor_arity4.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_arity4.mli constructor_arity4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_arity4.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
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
  (:checker %{project_root}/test/gospel_check.exe))
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
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to empty_match.mli.output
   (run %{checker} %{dep:empty_match.mli}))))

(rule
 (alias runtest)
 (action
  (diff empty_match.mli empty_match.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:empty_match.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
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
  (:checker %{project_root}/test/gospel_check.exe))
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
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to field_application.mli.output
   (run %{checker} %{dep:field_application.mli}))))

(rule
 (alias runtest)
 (action
  (diff field_application.mli field_application.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:field_application.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant1.mli.output
   (run %{checker} %{dep:invariant1.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant1.mli invariant1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant2.mli.output
   (run %{checker} %{dep:invariant2.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant2.mli invariant2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant3.mli.output
   (run %{checker} %{dep:invariant3.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant3.mli invariant3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
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
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariant5.mli.output
   (run %{checker} %{dep:invariant5.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariant5.mli invariant5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariant5.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t1.mli.output
   (run %{checker} %{dep:t1.mli}))))

(rule
 (alias runtest)
 (action
  (diff t1.mli t1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe)
  t9.mli)
 (action
  (with-outputs-to t10.mli.output
   (run %{checker} %{dep:t10.mli}))))

(rule
 (alias runtest)
 (action
  (diff t10.mli t10.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t10.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t11.mli.output
   (run %{checker} %{dep:t11.mli}))))

(rule
 (alias runtest)
 (action
  (diff t11.mli t11.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t11.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t12.mli.output
   (run %{checker} %{dep:t12.mli}))))

(rule
 (alias runtest)
 (action
  (diff t12.mli t12.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t12.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t13.mli.output
   (run %{checker} %{dep:t13.mli}))))

(rule
 (alias runtest)
 (action
  (diff t13.mli t13.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t13.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t14.mli.output
   (run %{checker} %{dep:t14.mli}))))

(rule
 (alias runtest)
 (action
  (diff t14.mli t14.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t14.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t15.mli.output
   (run %{checker} %{dep:t15.mli}))))

(rule
 (alias runtest)
 (action
  (diff t15.mli t15.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t15.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t16.mli.output
   (run %{checker} %{dep:t16.mli}))))

(rule
 (alias runtest)
 (action
  (diff t16.mli t16.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t16.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t17.mli.output
   (run %{checker} %{dep:t17.mli}))))

(rule
 (alias runtest)
 (action
  (diff t17.mli t17.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t17.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t18.mli.output
   (run %{checker} %{dep:t18.mli}))))

(rule
 (alias runtest)
 (action
  (diff t18.mli t18.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t18.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t19.mli.output
   (run %{checker} %{dep:t19.mli}))))

(rule
 (alias runtest)
 (action
  (diff t19.mli t19.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t19.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t2.mli.output
   (run %{checker} %{dep:t2.mli}))))

(rule
 (alias runtest)
 (action
  (diff t2.mli t2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t20.mli.output
   (run %{checker} %{dep:t20.mli}))))

(rule
 (alias runtest)
 (action
  (diff t20.mli t20.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t20.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t21.mli.output
   (run %{checker} %{dep:t21.mli}))))

(rule
 (alias runtest)
 (action
  (diff t21.mli t21.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t21.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t22.mli.output
   (run %{checker} %{dep:t22.mli}))))

(rule
 (alias runtest)
 (action
  (diff t22.mli t22.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t22.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t23.mli.output
   (run %{checker} %{dep:t23.mli}))))

(rule
 (alias runtest)
 (action
  (diff t23.mli t23.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t23.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t24.mli.output
   (run %{checker} %{dep:t24.mli}))))

(rule
 (alias runtest)
 (action
  (diff t24.mli t24.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t24.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t25.mli.output
   (run %{checker} %{dep:t25.mli}))))

(rule
 (alias runtest)
 (action
  (diff t25.mli t25.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t25.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t26.mli.output
   (run %{checker} %{dep:t26.mli}))))

(rule
 (alias runtest)
 (action
  (diff t26.mli t26.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t26.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t27.mli.output
   (run %{checker} %{dep:t27.mli}))))

(rule
 (alias runtest)
 (action
  (diff t27.mli t27.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t27.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t28.mli.output
   (run %{checker} %{dep:t28.mli}))))

(rule
 (alias runtest)
 (action
  (diff t28.mli t28.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t28.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t29.mli.output
   (run %{checker} %{dep:t29.mli}))))

(rule
 (alias runtest)
 (action
  (diff t29.mli t29.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t29.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t3.mli.output
   (run %{checker} %{dep:t3.mli}))))

(rule
 (alias runtest)
 (action
  (diff t3.mli t3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t30.mli.output
   (run %{checker} %{dep:t30.mli}))))

(rule
 (alias runtest)
 (action
  (diff t30.mli t30.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t30.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t31.mli.output
   (run %{checker} %{dep:t31.mli}))))

(rule
 (alias runtest)
 (action
  (diff t31.mli t31.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t31.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t32.mli.output
   (run %{checker} %{dep:t32.mli}))))

(rule
 (alias runtest)
 (action
  (diff t32.mli t32.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t32.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t33.mli.output
   (run %{checker} %{dep:t33.mli}))))

(rule
 (alias runtest)
 (action
  (diff t33.mli t33.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t33.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t34.mli.output
   (run %{checker} %{dep:t34.mli}))))

(rule
 (alias runtest)
 (action
  (diff t34.mli t34.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t34.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t35.mli.output
   (run %{checker} %{dep:t35.mli}))))

(rule
 (alias runtest)
 (action
  (diff t35.mli t35.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t35.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t36.mli.output
   (run %{checker} %{dep:t36.mli}))))

(rule
 (alias runtest)
 (action
  (diff t36.mli t36.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t36.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t37.mli.output
   (run %{checker} %{dep:t37.mli}))))

(rule
 (alias runtest)
 (action
  (diff t37.mli t37.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t37.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t38.mli.output
   (run %{checker} %{dep:t38.mli}))))

(rule
 (alias runtest)
 (action
  (diff t38.mli t38.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t38.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t39.mli.output
   (run %{checker} %{dep:t39.mli}))))

(rule
 (alias runtest)
 (action
  (diff t39.mli t39.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:t39.mli})))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t4.mli.output
   (run %{checker} %{dep:t4.mli}))))

(rule
 (alias runtest)
 (action
  (diff t4.mli t4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t4.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t5.mli.output
   (run %{checker} %{dep:t5.mli}))))

(rule
 (alias runtest)
 (action
  (diff t5.mli t5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t5.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t6.mli.output
   (run %{checker} %{dep:t6.mli}))))

(rule
 (alias runtest)
 (action
  (diff t6.mli t6.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t6.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t7.mli.output
   (run %{checker} %{dep:t7.mli}))))

(rule
 (alias runtest)
 (action
  (diff t7.mli t7.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t7.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t8.mli.output
   (run %{checker} %{dep:t8.mli}))))

(rule
 (alias runtest)
 (action
  (diff t8.mli t8.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t8.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to t9.mli.output
   (run %{checker} %{dep:t9.mli}))))

(rule
 (alias runtest)
 (action
  (diff t9.mli t9.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:t9.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
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
  (:checker %{project_root}/test/gospel_check.exe))
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
  (:checker %{project_root}/test/gospel_check.exe))
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

