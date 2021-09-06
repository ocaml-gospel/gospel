(rule
 (targets Arrays.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:Arrays.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Arrays.mli.expected} %{dep:Arrays.mli.output})))

(rule
 (targets CountingSort.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:CountingSort.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:CountingSort.mli.expected} %{dep:CountingSort.mli.output})))

(rule
 (targets HashTable.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:HashTable.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:HashTable.mli.expected} %{dep:HashTable.mli.output})))

(rule
 (targets Lists.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:Lists.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Lists.mli.expected} %{dep:Lists.mli.output})))

(rule
 (targets Mjrty.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:Mjrty.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Mjrty.mli.expected} %{dep:Mjrty.mli.output})))

(rule
 (targets PairingHeap.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:PairingHeap.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:PairingHeap.mli.expected} %{dep:PairingHeap.mli.output})))

(rule
 (targets PriorityQueue.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:PriorityQueue.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:PriorityQueue.mli.expected} %{dep:PriorityQueue.mli.output})))

(rule
 (targets Queue.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:Queue.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Queue.mli.expected} %{dep:Queue.mli.output})))

(rule
 (targets RingBuffer.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:RingBuffer.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:RingBuffer.mli.expected} %{dep:RingBuffer.mli.output})))

(rule
 (targets UnionFind.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:UnionFind.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:UnionFind.mli.expected} %{dep:UnionFind.mli.output})))

(rule
 (targets Vector.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:Vector.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Vector.mli.expected} %{dep:Vector.mli.output})))

(rule
 (targets ZipperList.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:ZipperList.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ZipperList.mli.expected} %{dep:ZipperList.mli.output})))

