
(rule
 (targets FM19.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:FM19.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:FM19.mli.expected} %{dep:FM19.mli.output})))

(rule
 (targets abstract_functions.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:abstract_functions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:abstract_functions.mli.expected} %{dep:abstract_functions.mli.output})))

(rule
 (targets basic_functions_axioms.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:basic_functions_axioms.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:basic_functions_axioms.mli.expected} %{dep:basic_functions_axioms.mli.output})))

(rule
 (targets complex_vals.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:complex_vals.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:complex_vals.mli.expected} %{dep:complex_vals.mli.output})))

(rule
 (targets exceptions.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:exceptions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:exceptions.mli.expected} %{dep:exceptions.mli.output})))

(rule
 (targets modules.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:modules.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:modules.mli.expected} %{dep:modules.mli.output})))

(rule
 (targets more_types.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:more_types.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:more_types.mli.expected} %{dep:more_types.mli.output})))

(rule
 (targets type_decl.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:type_decl.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:type_decl.mli.expected} %{dep:type_decl.mli.output})))

(rule
 (targets vals.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:vals.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:vals.mli.expected} %{dep:vals.mli.output})))
