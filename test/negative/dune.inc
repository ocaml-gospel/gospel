(rule
 (target char1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:char1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:char1.mli} %{dep:char1.mli.output})))

(rule
 (target constants1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:constants1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:constants1.mli} %{dep:constants1.mli.output})))

(rule
 (target constructor_arity1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:constructor_arity1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:constructor_arity1.mli} %{dep:constructor_arity1.mli.output})))

(rule
 (target constructor_arity2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:constructor_arity2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:constructor_arity2.mli} %{dep:constructor_arity2.mli.output})))

(rule
 (target constructor_arity3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:constructor_arity3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:constructor_arity3.mli} %{dep:constructor_arity3.mli.output})))

(rule
 (target constructor_arity4.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:constructor_arity4.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:constructor_arity4.mli} %{dep:constructor_arity4.mli.output})))

(rule
 (target empty_match.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:empty_match.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:empty_match.mli} %{dep:empty_match.mli.output})))

(rule
 (target exception_no_pattern.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:exception_no_pattern.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:exception_no_pattern.mli} %{dep:exception_no_pattern.mli.output})))

(rule
 (target exn_arity.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:exn_arity.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:exn_arity.mli} %{dep:exn_arity.mli.output})))

(rule
 (target field_application.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:field_application.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:field_application.mli} %{dep:field_application.mli.output})))

(rule
 (target invariant1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:invariant1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:invariant1.mli} %{dep:invariant1.mli.output})))

(rule
 (target invariant2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:invariant2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:invariant2.mli} %{dep:invariant2.mli.output})))

(rule
 (target invariant3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:invariant3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:invariant3.mli} %{dep:invariant3.mli.output})))

(rule
 (target invariant4.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:invariant4.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:invariant4.mli} %{dep:invariant4.mli.output})))

(rule
 (target invariant5.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:invariant5.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:invariant5.mli} %{dep:invariant5.mli.output})))

(rule
 (target t1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t1.mli} %{dep:t1.mli.output})))

(rule
 (target t10.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t10.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t10.mli} %{dep:t10.mli.output})))

(rule
 (target t11.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t11.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t11.mli} %{dep:t11.mli.output})))

(rule
 (target t12.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t12.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t12.mli} %{dep:t12.mli.output})))

(rule
 (target t13.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t13.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t13.mli} %{dep:t13.mli.output})))

(rule
 (target t14.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t14.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t14.mli} %{dep:t14.mli.output})))

(rule
 (target t15.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t15.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t15.mli} %{dep:t15.mli.output})))

(rule
 (target t16.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t16.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t16.mli} %{dep:t16.mli.output})))

(rule
 (target t17.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t17.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t17.mli} %{dep:t17.mli.output})))

(rule
 (target t18.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t18.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t18.mli} %{dep:t18.mli.output})))

(rule
 (target t19.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t19.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t19.mli} %{dep:t19.mli.output})))

(rule
 (target t2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t2.mli} %{dep:t2.mli.output})))

(rule
 (target t20.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t20.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t20.mli} %{dep:t20.mli.output})))

(rule
 (target t21.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t21.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t21.mli} %{dep:t21.mli.output})))

(rule
 (target t22.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t22.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t22.mli} %{dep:t22.mli.output})))

(rule
 (target t23.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t23.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t23.mli} %{dep:t23.mli.output})))

(rule
 (target t24.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t24.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t24.mli} %{dep:t24.mli.output})))

(rule
 (target t25.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t25.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t25.mli} %{dep:t25.mli.output})))

(rule
 (target t26.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t26.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t26.mli} %{dep:t26.mli.output})))

(rule
 (target t27.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t27.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t27.mli} %{dep:t27.mli.output})))

(rule
 (target t28.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t28.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t28.mli} %{dep:t28.mli.output})))

(rule
 (target t29.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t29.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t29.mli} %{dep:t29.mli.output})))

(rule
 (target t3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t3.mli} %{dep:t3.mli.output})))

(rule
 (target t30.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t30.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t30.mli} %{dep:t30.mli.output})))

(rule
 (target t31.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t31.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t31.mli} %{dep:t31.mli.output})))

(rule
 (target t32.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t32.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t32.mli} %{dep:t32.mli.output})))

(rule
 (target t33.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t33.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t33.mli} %{dep:t33.mli.output})))

(rule
 (target t34.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t34.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t34.mli} %{dep:t34.mli.output})))

(rule
 (target t35.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t35.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t35.mli} %{dep:t35.mli.output})))

(rule
 (target t36.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t36.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t36.mli} %{dep:t36.mli.output})))

(rule
 (target t37.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t37.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t37.mli} %{dep:t37.mli.output})))

(rule
 (target t38.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t38.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t38.mli} %{dep:t38.mli.output})))

(rule
 (target t39.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t39.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t39.mli} %{dep:t39.mli.output})))

(rule
 (target t4.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t4.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t4.mli} %{dep:t4.mli.output})))

(rule
 (target t5.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t5.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t5.mli} %{dep:t5.mli.output})))

(rule
 (target t6.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t6.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t6.mli} %{dep:t6.mli.output})))

(rule
 (target t7.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t7.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t7.mli} %{dep:t7.mli.output})))

(rule
 (target t8.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t8.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t8.mli} %{dep:t8.mli.output})))

(rule
 (target t9.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:t9.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:t9.mli} %{dep:t9.mli.output})))

