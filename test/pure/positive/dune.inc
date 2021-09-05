(rule
 (targets pure.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:pure.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pure.mli.expected} %{dep:pure.mli.output})))

