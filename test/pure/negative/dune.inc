(rule
 (targets impure1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:impure1.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:impure1.mli.expected} %{dep:impure1.mli.output})))

(rule
 (targets impure2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:impure2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:impure2.mli.expected} %{dep:impure2.mli.output})))

(rule
 (targets impure3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:impure3.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:impure3.mli.expected} %{dep:impure3.mli.output})))

(rule
 (targets impure4.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:impure4.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:impure4.mli.expected} %{dep:impure4.mli.output})))

(rule
 (targets not_pure.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:not_pure.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:not_pure.mli.expected} %{dep:not_pure.mli.output})))

