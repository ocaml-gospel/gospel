(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to Arrays.mli.output
   (run %{checker} %{dep:Arrays.mli}))))

(rule
 (alias runtest)
 (action
  (diff Arrays.mli Arrays.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to CountingSort.mli.output
   (run %{checker} %{dep:CountingSort.mli}))))

(rule
 (alias runtest)
 (action
  (diff CountingSort.mli CountingSort.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to HashTable.mli.output
   (run %{checker} %{dep:HashTable.mli}))))

(rule
 (alias runtest)
 (action
  (diff HashTable.mli HashTable.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to Lists.mli.output
   (run %{checker} %{dep:Lists.mli}))))

(rule
 (alias runtest)
 (action
  (diff Lists.mli Lists.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to Mjrty.mli.output
   (run %{checker} %{dep:Mjrty.mli}))))

(rule
 (alias runtest)
 (action
  (diff Mjrty.mli Mjrty.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to PairingHeap.mli.output
   (run %{checker} %{dep:PairingHeap.mli}))))

(rule
 (alias runtest)
 (action
  (diff PairingHeap.mli PairingHeap.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to PriorityQueue.mli.output
   (run %{checker} %{dep:PriorityQueue.mli}))))

(rule
 (alias runtest)
 (action
  (diff PriorityQueue.mli PriorityQueue.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to Queue.mli.output
   (run %{checker} %{dep:Queue.mli}))))

(rule
 (alias runtest)
 (action
  (diff Queue.mli Queue.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to RingBuffer.mli.output
   (run %{checker} %{dep:RingBuffer.mli}))))

(rule
 (alias runtest)
 (action
  (diff RingBuffer.mli RingBuffer.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to UnionFind.mli.output
   (run %{checker} %{dep:UnionFind.mli}))))

(rule
 (alias runtest)
 (action
  (diff UnionFind.mli UnionFind.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to Vector.mli.output
   (run %{checker} %{dep:Vector.mli}))))

(rule
 (alias runtest)
 (action
  (diff Vector.mli Vector.mli.output)))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to ZipperList.mli.output
   (run %{checker} %{dep:ZipperList.mli}))))

(rule
 (alias runtest)
 (action
  (diff ZipperList.mli ZipperList.mli.output)))

