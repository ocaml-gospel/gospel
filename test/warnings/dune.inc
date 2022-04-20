(rule
 (targets no_modifies_while_returning_unit.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:no_modifies_while_returning_unit.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:no_modifies_while_returning_unit.mli.expected} %{dep:no_modifies_while_returning_unit.mli.output})))

