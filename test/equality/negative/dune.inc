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

