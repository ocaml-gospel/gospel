(rule
 (target FM19.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:FM19.mli}))))

(rule
 (targets FM19.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:FM19.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:FM19.mli.expected} %{dep:FM19.mli.output})))

(rule
 (target a.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:a.mli}))))

(rule
 (targets a.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:a.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:a.mli.expected} %{dep:a.mli.output})))

(rule
 (target a1.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:a1.mli}))))

(rule
 (targets a1.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:a1.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:a1.mli.expected} %{dep:a1.mli.output})))

(rule
 (target a2.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:a2.mli}))))

(rule
 (targets a2.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:a2.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:a2.mli.expected} %{dep:a2.mli.output})))

(rule
 (target a3.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:a3.mli}))))

(rule
 (targets a3.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:a3.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:a3.mli.expected} %{dep:a3.mli.output})))

(rule
 (target abstract_functions.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:abstract_functions.mli}))))

(rule
 (targets abstract_functions.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:abstract_functions.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:abstract_functions.mli.expected} %{dep:abstract_functions.mli.output})))

(rule
 (target b.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:b.mli}))))

(rule
 (targets b.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:b.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:b.mli.expected} %{dep:b.mli.output})))

(rule
 (target basic_functions_axioms.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:basic_functions_axioms.mli}))))

(rule
 (targets basic_functions_axioms.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:basic_functions_axioms.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:basic_functions_axioms.mli.expected} %{dep:basic_functions_axioms.mli.output})))

(rule
 (target c.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:c.mli}))))

(rule
 (targets c.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:c.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:c.mli.expected} %{dep:c.mli.output})))

(rule
 (target complex_vals.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:complex_vals.mli}))))

(rule
 (targets complex_vals.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:complex_vals.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:complex_vals.mli.expected} %{dep:complex_vals.mli.output})))

(rule
 (target exceptions.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:exceptions.mli}))))

(rule
 (targets exceptions.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:exceptions.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:exceptions.mli.expected} %{dep:exceptions.mli.output})))

(rule
 (target modules.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:modules.mli}))))

(rule
 (targets modules.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:modules.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:modules.mli.expected} %{dep:modules.mli.output})))

(rule
 (target more_types.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:more_types.mli}))))

(rule
 (targets more_types.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:more_types.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:more_types.mli.expected} %{dep:more_types.mli.output})))

(rule
 (target pattern_matching.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:pattern_matching.mli}))))

(rule
 (targets pattern_matching.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pattern_matching.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pattern_matching.mli.expected} %{dep:pattern_matching.mli.output})))

(rule
 (target test.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:test.mli}))))

(rule
 (targets test.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:test.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:test.mli.expected} %{dep:test.mli.output})))

(rule
 (target test1.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:test1.mli}))))

(rule
 (targets test1.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:test1.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:test1.mli.expected} %{dep:test1.mli.output})))

(rule
 (target test2.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:test2.mli}))))

(rule
 (targets test2.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:test2.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:test2.mli.expected} %{dep:test2.mli.output})))

(rule
 (target type_decl.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:type_decl.mli}))))

(rule
 (targets type_decl.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:type_decl.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:type_decl.mli.expected} %{dep:type_decl.mli.output})))

(rule
 (target vals.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:vals.mli}))))

(rule
 (targets vals.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:vals.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:vals.mli.expected} %{dep:vals.mli.output})))

