(rule
 (target impure1.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:impure1.mli}))))

(rule
 (targets impure1.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:impure1.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:impure1.mli.expected} %{dep:impure1.mli.output})))

(rule
 (target impure2.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:impure2.mli}))))

(rule
 (targets impure2.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:impure2.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:impure2.mli.expected} %{dep:impure2.mli.output})))

(rule
 (target impure3.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:impure3.mli}))))

(rule
 (targets impure3.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:impure3.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:impure3.mli.expected} %{dep:impure3.mli.output})))

(rule
 (target impure4.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:impure4.mli}))))

(rule
 (targets impure4.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:impure4.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:impure4.mli.expected} %{dep:impure4.mli.output})))

(rule
 (target not_pure.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:not_pure.mli}))))

(rule
 (targets not_pure.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:not_pure.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:not_pure.mli.expected} %{dep:not_pure.mli.output})))

