(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
  type_decl.mli)
 (targets abstract_functions.gospel)
 (action
  (with-outputs-to abstract_functions.mli.output
   (run %{checker} %{dep:abstract_functions.mli}))))

(rule
 (alias runtest)
 (action
  (diff abstract_functions.mli abstract_functions.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:abstract_functions.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets allsorts_labels.gospel)
 (action
  (with-outputs-to allsorts_labels.mli.output
   (run %{checker} %{dep:allsorts_labels.mli}))))

(rule
 (alias runtest)
 (action
  (diff allsorts_labels.mli allsorts_labels.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:allsorts_labels.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets basic_functions_axioms.gospel)
 (action
  (with-outputs-to basic_functions_axioms.mli.output
   (run %{checker} %{dep:basic_functions_axioms.mli}))))

(rule
 (alias runtest)
 (action
  (diff basic_functions_axioms.mli basic_functions_axioms.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:basic_functions_axioms.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets bitvector.gospel)
 (action
  (with-outputs-to bitvector.mli.output
   (run %{checker} %{dep:bitvector.mli}))))

(rule
 (alias runtest)
 (action
  (diff bitvector.mli bitvector.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:bitvector.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets constants.gospel)
 (action
  (with-outputs-to constants.mli.output
   (run %{checker} %{dep:constants.mli}))))

(rule
 (alias runtest)
 (action
  (diff constants.mli constants.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constants.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets constructor_args.gospel)
 (action
  (with-outputs-to constructor_args.mli.output
   (run %{checker} %{dep:constructor_args.mli}))))

(rule
 (alias runtest)
 (action
  (diff constructor_args.mli constructor_args.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:constructor_args.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets constructor_arity1.gospel)
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
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets constructor_arity2.gospel)
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
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets constructor_arity3.gospel)
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
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets constructor_arity4.gospel)
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
 (targets empty_match.gospel)
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
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets exceptions.gospel)
 (action
  (with-outputs-to exceptions.mli.output
   (run %{checker} %{dep:exceptions.mli}))))

(rule
 (alias runtest)
 (action
  (diff exceptions.mli exceptions.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:exceptions.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets expressions.gospel)
 (action
  (with-outputs-to expressions.mli.output
   (run %{checker} %{dep:expressions.mli}))))

(rule
 (alias runtest)
 (action
  (diff expressions.mli expressions.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:expressions.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets field_application.gospel)
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
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets ghost_arg1.gospel)
 (action
  (with-outputs-to ghost_arg1.mli.output
   (run %{checker} %{dep:ghost_arg1.mli}))))

(rule
 (alias runtest)
 (action
  (diff ghost_arg1.mli ghost_arg1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:ghost_arg1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets ghost_arg2.gospel)
 (action
  (with-outputs-to ghost_arg2.mli.output
   (run %{checker} %{dep:ghost_arg2.mli}))))

(rule
 (alias runtest)
 (action
  (diff ghost_arg2.mli ghost_arg2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:ghost_arg2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets guard.gospel)
 (action
  (with-outputs-to guard.mli.output
   (run %{checker} %{dep:guard.mli}))))

(rule
 (alias runtest)
 (action
  (diff guard.mli guard.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:guard.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets infix.gospel)
 (action
  (with-outputs-to infix.mli.output
   (run %{checker} %{dep:infix.mli}))))

(rule
 (alias runtest)
 (action
  (diff infix.mli infix.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:infix.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets invalid_modifies.gospel)
 (action
  (with-outputs-to invalid_modifies.mli.output
   (run %{checker} %{dep:invalid_modifies.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_modifies.mli invalid_modifies.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invalid_modifies.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets invariant5.gospel)
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
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets invariants.gospel)
 (action
  (with-outputs-to invariants.mli.output
   (run %{checker} %{dep:invariants.mli}))))

(rule
 (alias runtest)
 (action
  (diff invariants.mli invariants.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:invariants.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets keyword_as_prefix.gospel)
 (action
  (with-outputs-to keyword_as_prefix.mli.output
   (run %{checker} %{dep:keyword_as_prefix.mli}))))

(rule
 (alias runtest)
 (action
  (diff keyword_as_prefix.mli keyword_as_prefix.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:keyword_as_prefix.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets literals.gospel)
 (action
  (with-outputs-to literals.mli.output
   (run %{checker} %{dep:literals.mli}))))

(rule
 (alias runtest)
 (action
  (diff literals.mli literals.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:literals.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets match_guard.gospel)
 (action
  (with-outputs-to match_guard.mli.output
   (run %{checker} %{dep:match_guard.mli}))))

(rule
 (alias runtest)
 (action
  (diff match_guard.mli match_guard.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:match_guard.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets module_with.gospel)
 (action
  (with-outputs-to module_with.mli.output
   (run %{checker} %{dep:module_with.mli}))))

(rule
 (alias runtest)
 (action
  (diff module_with.mli module_with.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:module_with.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets modules1.gospel)
 (action
  (with-outputs-to modules1.mli.output
   (run %{checker} %{dep:modules1.mli}))))

(rule
 (alias runtest)
 (action
  (diff modules1.mli modules1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:modules1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets modules2.gospel)
 (action
  (with-outputs-to modules2.mli.output
   (run %{checker} %{dep:modules2.mli}))))

(rule
 (alias runtest)
 (action
  (diff modules2.mli modules2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:modules2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets no_specification.gospel)
 (action
  (with-outputs-to no_specification.mli.output
   (run %{checker} %{dep:no_specification.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_specification.mli no_specification.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:no_specification.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
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
  (:checker %{project_root}/test/utils/testchecker.exe)
  type.mli)
 (targets open2.gospel)
 (action
  (with-outputs-to open2.mli.output
   (run %{checker} %{dep:open2.mli}))))

(rule
 (alias runtest)
 (action
  (diff open2.mli open2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:open2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
  types_functions.mli)
 (targets open3.gospel)
 (action
  (with-outputs-to open3.mli.output
   (run %{checker} %{dep:open3.mli}))))

(rule
 (alias runtest)
 (action
  (diff open3.mli open3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:open3.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets open_gospelstdlib_module.gospel)
 (action
  (with-outputs-to open_gospelstdlib_module.mli.output
   (run %{checker} %{dep:open_gospelstdlib_module.mli}))))

(rule
 (alias runtest)
 (action
  (diff open_gospelstdlib_module.mli open_gospelstdlib_module.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:open_gospelstdlib_module.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
  open3.mli types_functions.mli)
 (targets open_qualified.gospel)
 (action
  (with-outputs-to open_qualified.mli.output
   (run %{checker} %{dep:open_qualified.mli}))))

(rule
 (alias runtest)
 (action
  (diff open_qualified.mli open_qualified.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (with-accepted-exit-codes 2
    (run ocamlc -c %{dep:open_qualified.mli})))))

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
 (targets pattern_binding.gospel)
 (action
  (with-outputs-to pattern_binding.mli.output
   (run %{checker} %{dep:pattern_binding.mli}))))

(rule
 (alias runtest)
 (action
  (diff pattern_binding.mli pattern_binding.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pattern_binding.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets predicates.gospel)
 (action
  (with-outputs-to predicates.mli.output
   (run %{checker} %{dep:predicates.mli}))))

(rule
 (alias runtest)
 (action
  (diff predicates.mli predicates.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:predicates.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets pure.gospel)
 (action
  (with-outputs-to pure.mli.output
   (run %{checker} %{dep:pure.mli}))))

(rule
 (alias runtest)
 (action
  (diff pure.mli pure.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pure.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
  type.mli open2.mli)
 (targets qualified.gospel)
 (action
  (with-outputs-to qualified.mli.output
   (run %{checker} %{dep:qualified.mli}))))

(rule
 (alias runtest)
 (action
  (diff qualified.mli qualified.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:qualified.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets record_pattern1.gospel)
 (action
  (with-outputs-to record_pattern1.mli.output
   (run %{checker} %{dep:record_pattern1.mli}))))

(rule
 (alias runtest)
 (action
  (diff record_pattern1.mli record_pattern1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:record_pattern1.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets record_pattern2.gospel)
 (action
  (with-outputs-to record_pattern2.mli.output
   (run %{checker} %{dep:record_pattern2.mli}))))

(rule
 (alias runtest)
 (action
  (diff record_pattern2.mli record_pattern2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:record_pattern2.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets record_pattern3.gospel)
 (action
  (with-outputs-to record_pattern3.mli.output
   (run %{checker} %{dep:record_pattern3.mli}))))

(rule
 (alias runtest)
 (action
  (diff record_pattern3.mli record_pattern3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:record_pattern3.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets recursive_invariants.gospel)
 (action
  (with-outputs-to recursive_invariants.mli.output
   (run %{checker} %{dep:recursive_invariants.mli}))))

(rule
 (alias runtest)
 (action
  (diff recursive_invariants.mli recursive_invariants.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:recursive_invariants.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets stdlib_exceptions.gospel)
 (action
  (with-outputs-to stdlib_exceptions.mli.output
   (run %{checker} %{dep:stdlib_exceptions.mli}))))

(rule
 (alias runtest)
 (action
  (diff stdlib_exceptions.mli stdlib_exceptions.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:stdlib_exceptions.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets type.gospel)
 (action
  (with-outputs-to type.mli.output
   (run %{checker} %{dep:type.mli}))))

(rule
 (alias runtest)
 (action
  (diff type.mli type.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:type.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets type_decl.gospel)
 (action
  (with-outputs-to type_decl.mli.output
   (run %{checker} %{dep:type_decl.mli}))))

(rule
 (alias runtest)
 (action
  (diff type_decl.mli type_decl.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:type_decl.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets types.gospel)
 (action
  (with-outputs-to types.mli.output
   (run %{checker} %{dep:types.mli}))))

(rule
 (alias runtest)
 (action
  (diff types.mli types.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:types.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets types_functions.gospel)
 (action
  (with-outputs-to types_functions.mli.output
   (run %{checker} %{dep:types_functions.mli}))))

(rule
 (alias runtest)
 (action
  (diff types_functions.mli types_functions.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:types_functions.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets types_functions2.gospel)
 (action
  (with-outputs-to types_functions2.mli.output
   (run %{checker} %{dep:types_functions2.mli}))))

(rule
 (alias runtest)
 (action
  (diff types_functions2.mli types_functions2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:types_functions2.mli}))))

