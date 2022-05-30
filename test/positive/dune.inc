(rule
 (target FM19.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:FM19.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:FM19.mli} %{dep:FM19.mli.output})))

(rule
 (target a.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:a.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:a.mli} %{dep:a.mli.output})))

(rule
 (target a1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:a1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:a1.mli} %{dep:a1.mli.output})))

(rule
 (target a2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:a2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:a2.mli} %{dep:a2.mli.output})))

(rule
 (target a3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:a3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:a3.mli} %{dep:a3.mli.output})))

(rule
 (target abstract_functions.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:abstract_functions.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:abstract_functions.mli} %{dep:abstract_functions.mli.output})))

(rule
 (target b.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:b.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:b.mli} %{dep:b.mli.output})))

(rule
 (target basic_functions_axioms.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:basic_functions_axioms.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:basic_functions_axioms.mli} %{dep:basic_functions_axioms.mli.output})))

(rule
 (target bitvector.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:bitvector.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:bitvector.mli} %{dep:bitvector.mli.output})))

(rule
 (target c.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:c.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:c.mli} %{dep:c.mli.output})))

(rule
 (target complex_vals.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:complex_vals.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:complex_vals.mli} %{dep:complex_vals.mli.output})))

(rule
 (target constants.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:constants.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:constants.mli} %{dep:constants.mli.output})))

(rule
 (target exceptions.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:exceptions.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:exceptions.mli} %{dep:exceptions.mli.output})))

(rule
 (target fib.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:fib.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:fib.mli} %{dep:fib.mli.output})))

(rule
 (target infix.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:infix.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:infix.mli} %{dep:infix.mli.output})))

(rule
 (target invariants.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:invariants.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:invariants.mli} %{dep:invariants.mli.output})))

(rule
 (target literals.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:literals.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:literals.mli} %{dep:literals.mli.output})))

(rule
 (target log2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:log2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:log2.mli} %{dep:log2.mli.output})))

(rule
 (target logical.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:logical.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:logical.mli} %{dep:logical.mli.output})))

(rule
 (target modules.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:modules.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:modules.mli} %{dep:modules.mli.output})))

(rule
 (target more_types.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:more_types.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:more_types.mli} %{dep:more_types.mli.output})))

(rule
 (target no_header.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:no_header.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:no_header.mli} %{dep:no_header.mli.output})))

(rule
 (target open_existing_type.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:open_existing_type.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:open_existing_type.mli} %{dep:open_existing_type.mli.output})))

(rule
 (target pattern_matching.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:pattern_matching.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:pattern_matching.mli} %{dep:pattern_matching.mli.output})))

(rule
 (target record_functions.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:record_functions.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:record_functions.mli} %{dep:record_functions.mli.output})))

(rule
 (target recursive_type_invariant.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:recursive_type_invariant.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:recursive_type_invariant.mli} %{dep:recursive_type_invariant.mli.output})))

(rule
 (target stdlib_exceptions.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:stdlib_exceptions.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:stdlib_exceptions.mli} %{dep:stdlib_exceptions.mli.output})))

(rule
 (target test.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:test.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:test.mli} %{dep:test.mli.output})))

(rule
 (target test1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:test1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:test1.mli} %{dep:test1.mli.output})))

(rule
 (target test2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:test2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:test2.mli} %{dep:test2.mli.output})))

(rule
 (target tuple_result.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:tuple_result.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:tuple_result.mli} %{dep:tuple_result.mli.output})))

(rule
 (target type_decl.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:type_decl.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:type_decl.mli} %{dep:type_decl.mli.output})))

(rule
 (target vals.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:vals.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:vals.mli} %{dep:vals.mli.output})))

(rule
 (target variant_patterns.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:variant_patterns.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:variant_patterns.mli} %{dep:variant_patterns.mli.output})))

