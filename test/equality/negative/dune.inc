(rule
 (targets equality_order.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:equality_order.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:equality_order.mli.expected} %{dep:equality_order.mli.output})))

(rule
 (targets extra_arg.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:extra_arg.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:extra_arg.mli.expected} %{dep:extra_arg.mli.output})))

(rule
 (targets inner_equality.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:inner_equality.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inner_equality.mli.expected} %{dep:inner_equality.mli.output})))

(rule
 (targets polymorphic_equality.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:polymorphic_equality.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:polymorphic_equality.mli.expected} %{dep:polymorphic_equality.mli.output})))

(rule
 (targets too_few_args.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:too_few_args.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:too_few_args.mli.expected} %{dep:too_few_args.mli.output})))

(rule
 (targets type_arg.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:type_arg.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:type_arg.mli.expected} %{dep:type_arg.mli.output})))

(rule
 (targets wrong_arg_number.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:wrong_arg_number.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:wrong_arg_number.mli.expected} %{dep:wrong_arg_number.mli.output})))

(rule
 (targets wrong_args.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:wrong_args.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:wrong_args.mli.expected} %{dep:wrong_args.mli.output})))

(rule
 (targets wrong_order.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:wrong_order.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:wrong_order.mli.expected} %{dep:wrong_order.mli.output})))

(rule
 (targets wrong_ret.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:wrong_ret.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:wrong_ret.mli.expected} %{dep:wrong_ret.mli.output})))

