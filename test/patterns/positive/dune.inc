(rule
 (targets positive-patterns.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:positive-patterns.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:positive-patterns.mli.expected} %{dep:positive-patterns.mli.output})))

