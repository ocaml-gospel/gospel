(rule
 (targets base.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:base.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:base.mli.expected} %{dep:base.mli.output})))

(rule
 (targets booleans.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:booleans.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:booleans.mli.expected} %{dep:booleans.mli.output})))

(rule
 (targets char2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:char2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:char2.mli.expected} %{dep:char2.mli.output})))

(rule
 (targets etuple.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:etuple.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:etuple.mli.expected} %{dep:etuple.mli.output})))

(rule
 (targets float.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:float.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:float.mli.expected} %{dep:float.mli.output})))

(rule
 (targets int.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:int.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:int.mli.expected} %{dep:int.mli.output})))

(rule
 (targets int2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:int2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:int2.mli.expected} %{dep:int2.mli.output})))

(rule
 (targets int3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:int3.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:int3.mli.expected} %{dep:int3.mli.output})))

(rule
 (targets interval.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:interval.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:interval.mli.expected} %{dep:interval.mli.output})))

(rule
 (targets list1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:list1.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:list1.mli.expected} %{dep:list1.mli.output})))

(rule
 (targets list2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:list2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:list2.mli.expected} %{dep:list2.mli.output})))

(rule
 (targets neg2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:neg2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:neg2.mli.expected} %{dep:neg2.mli.output})))

(rule
 (targets neg3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:neg3.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:neg3.mli.expected} %{dep:neg3.mli.output})))

(rule
 (targets pair.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:pair.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pair.mli.expected} %{dep:pair.mli.output})))

(rule
 (targets record.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:record.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:record.mli.expected} %{dep:record.mli.output})))

(rule
 (targets str.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:str.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:str.mli.expected} %{dep:str.mli.output})))

(rule
 (targets tuple.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:tuple.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tuple.mli.expected} %{dep:tuple.mli.output})))

(rule
 (targets tuple2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:tuple2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tuple2.mli.expected} %{dep:tuple2.mli.output})))

