(rule
 (targets basic.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:basic.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:basic.mli.expected} %{dep:basic.mli.output})))

