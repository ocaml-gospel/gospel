(rule
 (targets equality.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:equality.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:equality.mli.expected} %{dep:equality.mli.output})))

