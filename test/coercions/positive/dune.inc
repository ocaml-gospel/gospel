(rule
 (target basic.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:basic.mli}))))

(rule
 (targets basic.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:basic.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:basic.mli.expected} %{dep:basic.mli.output})))

