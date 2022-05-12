(rule
 (targets FM19.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:FM19.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:FM19.mli.expected} %{dep:FM19.mli.output})))

(rule
 (targets a.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:a.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:a.mli.expected} %{dep:a.mli.output})))

(rule
 (targets a1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:a1.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:a1.mli.expected} %{dep:a1.mli.output})))

(rule
 (targets a2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:a2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:a2.mli.expected} %{dep:a2.mli.output})))

(rule
 (targets a3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:a3.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:a3.mli.expected} %{dep:a3.mli.output})))

(rule
 (targets abstract_functions.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:abstract_functions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:abstract_functions.mli.expected} %{dep:abstract_functions.mli.output})))

(rule
 (targets b.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:b.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:b.mli.expected} %{dep:b.mli.output})))

(rule
 (targets basic_functions_axioms.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:basic_functions_axioms.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:basic_functions_axioms.mli.expected} %{dep:basic_functions_axioms.mli.output})))

(rule
 (targets bitvector.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:bitvector.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bitvector.mli.expected} %{dep:bitvector.mli.output})))

(rule
 (targets c.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:c.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:c.mli.expected} %{dep:c.mli.output})))

(rule
 (targets complex_vals.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:complex_vals.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:complex_vals.mli.expected} %{dep:complex_vals.mli.output})))

(rule
 (targets constants.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:constants.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:constants.mli.expected} %{dep:constants.mli.output})))

(rule
 (targets exceptions.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:exceptions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:exceptions.mli.expected} %{dep:exceptions.mli.output})))

(rule
 (targets fib.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:fib.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:fib.mli.expected} %{dep:fib.mli.output})))

(rule
 (targets infix.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:infix.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:infix.mli.expected} %{dep:infix.mli.output})))

(rule
 (targets invariants.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:invariants.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:invariants.mli.expected} %{dep:invariants.mli.output})))

(rule
 (targets log2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:log2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:log2.mli.expected} %{dep:log2.mli.output})))

(rule
 (targets logical.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:logical.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:logical.mli.expected} %{dep:logical.mli.output})))

(rule
 (targets modules.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:modules.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:modules.mli.expected} %{dep:modules.mli.output})))

(rule
 (targets more_types.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:more_types.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:more_types.mli.expected} %{dep:more_types.mli.output})))

(rule
 (targets no_header.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:no_header.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:no_header.mli.expected} %{dep:no_header.mli.output})))

(rule
 (targets open_existing_type.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:open_existing_type.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:open_existing_type.mli.expected} %{dep:open_existing_type.mli.output})))

(rule
 (targets pattern_matching.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:pattern_matching.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pattern_matching.mli.expected} %{dep:pattern_matching.mli.output})))

(rule
 (targets record_functions.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:record_functions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:record_functions.mli.expected} %{dep:record_functions.mli.output})))

(rule
 (targets stdlib_exceptions.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:stdlib_exceptions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:stdlib_exceptions.mli.expected} %{dep:stdlib_exceptions.mli.output})))

(rule
 (targets test.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:test.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:test.mli.expected} %{dep:test.mli.output})))

(rule
 (targets test1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:test1.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:test1.mli.expected} %{dep:test1.mli.output})))

(rule
 (targets test2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:test2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:test2.mli.expected} %{dep:test2.mli.output})))

(rule
 (targets tuple_result.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:tuple_result.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tuple_result.mli.expected} %{dep:tuple_result.mli.output})))

(rule
 (targets type_decl.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:type_decl.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:type_decl.mli.expected} %{dep:type_decl.mli.output})))

(rule
 (targets vals.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:vals.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:vals.mli.expected} %{dep:vals.mli.output})))

(rule
 (targets variant_patterns.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:variant_patterns.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:variant_patterns.mli.expected} %{dep:variant_patterns.mli.output})))

