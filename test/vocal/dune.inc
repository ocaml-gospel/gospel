(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Arrays.gospel)
 (action
  (with-outputs-to Arrays.mli.output
   (run %{checker} %{dep:Arrays.mli}))))

(rule
 (alias runtest)
 (action
  (diff Arrays.mli Arrays.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Arrays.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets CountingSort.gospel)
 (action
  (with-outputs-to CountingSort.mli.output
   (run %{checker} %{dep:CountingSort.mli}))))

(rule
 (alias runtest)
 (action
  (diff CountingSort.mli CountingSort.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:CountingSort.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets FM19.gospel)
 (action
  (with-outputs-to FM19.mli.output
   (run %{checker} %{dep:FM19.mli}))))

(rule
 (alias runtest)
 (action
  (diff FM19.mli FM19.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:FM19.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets HashTable.gospel)
 (action
  (with-outputs-to HashTable.mli.output
   (run %{checker} %{dep:HashTable.mli}))))

(rule
 (alias runtest)
 (action
  (diff HashTable.mli HashTable.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:HashTable.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Lists.gospel)
 (action
  (with-outputs-to Lists.mli.output
   (run %{checker} %{dep:Lists.mli}))))

(rule
 (alias runtest)
 (action
  (diff Lists.mli Lists.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Lists.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Mjrty.gospel)
 (action
  (with-outputs-to Mjrty.mli.output
   (run %{checker} %{dep:Mjrty.mli}))))

(rule
 (alias runtest)
 (action
  (diff Mjrty.mli Mjrty.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Mjrty.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets PairingHeap.gospel)
 (action
  (with-outputs-to PairingHeap.mli.output
   (run %{checker} %{dep:PairingHeap.mli}))))

(rule
 (alias runtest)
 (action
  (diff PairingHeap.mli PairingHeap.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:PairingHeap.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets PriorityQueue.gospel)
 (action
  (with-outputs-to PriorityQueue.mli.output
   (run %{checker} %{dep:PriorityQueue.mli}))))

(rule
 (alias runtest)
 (action
  (diff PriorityQueue.mli PriorityQueue.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:PriorityQueue.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Queue.gospel)
 (action
  (with-outputs-to Queue.mli.output
   (run %{checker} %{dep:Queue.mli}))))

(rule
 (alias runtest)
 (action
  (diff Queue.mli Queue.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Queue.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets RingBuffer.gospel)
 (action
  (with-outputs-to RingBuffer.mli.output
   (run %{checker} %{dep:RingBuffer.mli}))))

(rule
 (alias runtest)
 (action
  (diff RingBuffer.mli RingBuffer.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:RingBuffer.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets UnionFind.gospel)
 (action
  (with-outputs-to UnionFind.mli.output
   (run %{checker} %{dep:UnionFind.mli}))))

(rule
 (alias runtest)
 (action
  (diff UnionFind.mli UnionFind.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:UnionFind.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Vector.gospel)
 (action
  (with-outputs-to Vector.mli.output
   (run %{checker} %{dep:Vector.mli}))))

(rule
 (alias runtest)
 (action
  (diff Vector.mli Vector.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Vector.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets ZipperList.gospel)
 (action
  (with-outputs-to ZipperList.mli.output
   (run %{checker} %{dep:ZipperList.mli}))))

(rule
 (alias runtest)
 (action
  (diff ZipperList.mli ZipperList.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:ZipperList.mli}))))

