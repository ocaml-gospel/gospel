(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to FM19.mli.output
   (run %{checker} %{dep:FM19.mli}))))

(rule
 (alias runtest)
 (action
  (diff FM19.mli FM19.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to a.mli.output
   (run %{checker} %{dep:a.mli}))))

(rule
 (alias runtest)
 (action
  (diff a.mli a.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to a1.mli.output
   (run %{checker} %{dep:a1.mli}))))

(rule
 (alias runtest)
 (action
  (diff a1.mli a1.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe)
  a1.mli)
 (action
  (with-outputs-to a2.mli.output
   (run %{checker} %{dep:a2.mli}))))

(rule
 (alias runtest)
 (action
  (diff a2.mli a2.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe)
  a1.mli a2.mli)
 (action
  (with-outputs-to a3.mli.output
   (run %{checker} %{dep:a3.mli}))))

(rule
 (alias runtest)
 (action
  (diff a3.mli a3.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to abstract_functions.mli.output
   (run %{checker} %{dep:abstract_functions.mli}))))

(rule
 (alias runtest)
 (action
  (diff abstract_functions.mli abstract_functions.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe)
  a.mli)
 (action
  (with-outputs-to b.mli.output
   (run %{checker} %{dep:b.mli}))))

(rule
 (alias runtest)
 (action
  (diff b.mli b.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to basic_functions_axioms.mli.output
   (run %{checker} %{dep:basic_functions_axioms.mli}))))

(rule
 (alias runtest)
 (action
  (diff basic_functions_axioms.mli basic_functions_axioms.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to bitvector.mli.output
   (run %{checker} %{dep:bitvector.mli}))))

(rule
 (alias runtest)
 (action
  (diff bitvector.mli bitvector.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe)
  a.mli b.mli)
 (action
  (with-outputs-to c.mli.output
   (run %{checker} %{dep:c.mli}))))

(rule
 (alias runtest)
 (action
  (diff c.mli c.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to complex_vals.mli.output
   (run %{checker} %{dep:complex_vals.mli}))))

(rule
 (alias runtest)
 (action
  (diff complex_vals.mli complex_vals.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to constants.mli.output
   (run %{checker} %{dep:constants.mli}))))

(rule
 (alias runtest)
 (action
  (diff constants.mli constants.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to exceptions.mli.output
   (run %{checker} %{dep:exceptions.mli}))))

(rule
 (alias runtest)
 (action
  (diff exceptions.mli exceptions.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to fib.mli.output
   (run %{checker} %{dep:fib.mli}))))

(rule
 (alias runtest)
 (action
  (diff fib.mli fib.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to infix.mli.output
   (run %{checker} %{dep:infix.mli}))))

(rule
 (alias runtest)
 (action
  (diff infix.mli infix.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to invariants.mli.output
   (run %{checker} %{dep:invariants.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariants.mli invariants.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to literals.mli.output
   (run %{checker} %{dep:literals.mli}))))

(rule
 (alias runtest)
 (action
  (diff literals.mli literals.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to log2.mli.output
   (run %{checker} %{dep:log2.mli}))))

(rule
 (alias runtest)
 (action
  (diff log2.mli log2.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to logical.mli.output
   (run %{checker} %{dep:logical.mli}))))

(rule
 (alias runtest)
 (action
  (diff logical.mli logical.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to modules.mli.output
   (run %{checker} %{dep:modules.mli}))))

(rule
 (alias runtest)
 (action
  (diff modules.mli modules.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to more_types.mli.output
   (run %{checker} %{dep:more_types.mli}))))

(rule
 (alias runtest)
 (action
  (diff more_types.mli more_types.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to no_header.mli.output
   (run %{checker} %{dep:no_header.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_header.mli no_header.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to open_existing_type.mli.output
   (run %{checker} %{dep:open_existing_type.mli}))))

(rule
 (alias runtest)
 (action
  (diff open_existing_type.mli open_existing_type.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to pattern_matching.mli.output
   (run %{checker} %{dep:pattern_matching.mli}))))

(rule
 (alias runtest)
 (action
  (diff pattern_matching.mli pattern_matching.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to record_functions.mli.output
   (run %{checker} %{dep:record_functions.mli}))))

(rule
 (alias runtest)
 (action
  (diff record_functions.mli record_functions.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to recursive_type_invariant.mli.output
   (run %{checker} %{dep:recursive_type_invariant.mli}))))

(rule
 (alias runtest)
 (action
  (diff recursive_type_invariant.mli recursive_type_invariant.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to stdlib_exceptions.mli.output
   (run %{checker} %{dep:stdlib_exceptions.mli}))))

(rule
 (alias runtest)
 (action
  (diff stdlib_exceptions.mli stdlib_exceptions.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to test.mli.output
   (run %{checker} %{dep:test.mli}))))

(rule
 (alias runtest)
 (action
  (diff test.mli test.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe)
  test.mli)
 (action
  (with-outputs-to test1.mli.output
   (run %{checker} %{dep:test1.mli}))))

(rule
 (alias runtest)
 (action
  (diff test1.mli test1.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to test2.mli.output
   (run %{checker} %{dep:test2.mli}))))

(rule
 (alias runtest)
 (action
  (diff test2.mli test2.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to tuple_result.mli.output
   (run %{checker} %{dep:tuple_result.mli}))))

(rule
 (alias runtest)
 (action
  (diff tuple_result.mli tuple_result.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to type_decl.mli.output
   (run %{checker} %{dep:type_decl.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_decl.mli type_decl.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to vals.mli.output
   (run %{checker} %{dep:vals.mli}))))

(rule
 (alias runtest)
 (action
  (diff vals.mli vals.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to variant_patterns.mli.output
   (run %{checker} %{dep:variant_patterns.mli}))))

(rule
 (alias runtest)
 (action
  (diff variant_patterns.mli variant_patterns.mli.output)))

