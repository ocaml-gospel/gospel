(rule
 (targets bad_constr_arg.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:bad_constr_arg.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bad_constr_arg.mli.expected} %{dep:bad_constr_arg.mli.output})))

(rule
 (targets invalid_literal.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:invalid_literal.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:invalid_literal.mli.expected} %{dep:invalid_literal.mli.output})))

