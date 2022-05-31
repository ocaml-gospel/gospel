(rule
 (target Arrays.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:Arrays.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:Arrays.mli} %{dep:Arrays.mli.output})))

(rule
 (target CountingSort.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:CountingSort.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:CountingSort.mli} %{dep:CountingSort.mli.output})))

(rule
 (target HashTable.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:HashTable.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:HashTable.mli} %{dep:HashTable.mli.output})))

(rule
 (target Lists.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:Lists.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:Lists.mli} %{dep:Lists.mli.output})))

(rule
 (target Mjrty.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:Mjrty.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:Mjrty.mli} %{dep:Mjrty.mli.output})))

(rule
 (target PairingHeap.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:PairingHeap.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:PairingHeap.mli} %{dep:PairingHeap.mli.output})))

(rule
 (target PriorityQueue.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:PriorityQueue.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:PriorityQueue.mli} %{dep:PriorityQueue.mli.output})))

(rule
 (target Queue.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:Queue.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:Queue.mli} %{dep:Queue.mli.output})))

(rule
 (target RingBuffer.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:RingBuffer.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:RingBuffer.mli} %{dep:RingBuffer.mli.output})))

(rule
 (target UnionFind.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:UnionFind.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:UnionFind.mli} %{dep:UnionFind.mli.output})))

(rule
 (target Vector.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:Vector.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:Vector.mli} %{dep:Vector.mli.output})))

(rule
 (target ZipperList.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:ZipperList.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:ZipperList.mli} %{dep:ZipperList.mli.output})))

