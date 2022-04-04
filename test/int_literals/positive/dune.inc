(rule
 (targets ints.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:ints.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ints.mli.expected} %{dep:ints.mli.output})))

