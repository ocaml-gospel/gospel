(rule
 (target bad_type_id.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:bad_type_id.mli}))))

(rule
 (targets bad_type_id.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bad_type_id.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bad_type_id.mli.expected} %{dep:bad_type_id.mli.output})))

(rule
 (target bad_type_multiple_args.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:bad_type_multiple_args.mli}))))

(rule
 (targets bad_type_multiple_args.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bad_type_multiple_args.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bad_type_multiple_args.mli.expected} %{dep:bad_type_multiple_args.mli.output})))

(rule
 (target complex_cycle.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:complex_cycle.mli}))))

(rule
 (targets complex_cycle.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:complex_cycle.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:complex_cycle.mli.expected} %{dep:complex_cycle.mli.output})))

(rule
 (target double_definition.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:double_definition.mli}))))

(rule
 (targets double_definition.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:double_definition.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:double_definition.mli.expected} %{dep:double_definition.mli.output})))

(rule
 (target simple_cycle.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:simple_cycle.mli}))))

(rule
 (targets simple_cycle.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:simple_cycle.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:simple_cycle.mli.expected} %{dep:simple_cycle.mli.output})))

