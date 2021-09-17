(rule
 (targets FM19.mli.output)
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
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:basic_functions_axioms.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:basic_functions_axioms.mli.expected} %{dep:basic_functions_axioms.mli.output})))

(rule
 (targets c.mli.output)
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
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:fib.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:fib.mli.expected} %{dep:fib.mli.output})))

(rule
 (targets log2.mli.output)
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
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:pattern_matching.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pattern_matching.mli.expected} %{dep:pattern_matching.mli.output})))

(rule
 (targets test.mli.output)
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
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:test2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:test2.mli.expected} %{dep:test2.mli.output})))

(rule
 (targets type_decl.mli.output)
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
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:vals.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:vals.mli.expected} %{dep:vals.mli.output})))

