(rule
 (targets bad_type_id.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:bad_type_id.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bad_type_id.mli.expected} %{dep:bad_type_id.mli.output})))

(rule
 (targets bad_type_multiple_args.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:bad_type_multiple_args.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bad_type_multiple_args.mli.expected} %{dep:bad_type_multiple_args.mli.output})))

(rule
 (targets complex_cycle.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:complex_cycle.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:complex_cycle.mli.expected} %{dep:complex_cycle.mli.output})))

(rule
 (targets double_definition.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:double_definition.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:double_definition.mli.expected} %{dep:double_definition.mli.output})))

(rule
 (targets simple_cycle.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:simple_cycle.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:simple_cycle.mli.expected} %{dep:simple_cycle.mli.output})))

