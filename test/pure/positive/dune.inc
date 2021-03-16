(rule
 (target pure.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:pure.mli}))))

(rule
 (targets pure.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pure.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pure.mli.expected} %{dep:pure.mli.output})))

