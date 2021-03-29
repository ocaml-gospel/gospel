(rule
 (targets basic.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:basic.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:basic.mli.expected} %{dep:basic.mli.output})))

