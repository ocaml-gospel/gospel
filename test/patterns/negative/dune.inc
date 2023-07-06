(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to ambiguous.mli.output
   (run %{checker} %{dep:ambiguous.mli}))))

(rule
 (alias runtest)
 (action
  (diff ambiguous.mli ambiguous.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:ambiguous.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to base.mli.output
   (run %{checker} %{dep:base.mli}))))

(rule
 (alias runtest)
 (action
  (diff base.mli base.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:base.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to booleans.mli.output
   (run %{checker} %{dep:booleans.mli}))))

(rule
 (alias runtest)
 (action
  (diff booleans.mli booleans.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:booleans.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to char2.mli.output
   (run %{checker} %{dep:char2.mli}))))

(rule
 (alias runtest)
 (action
  (diff char2.mli char2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:char2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to etuple.mli.output
   (run %{checker} %{dep:etuple.mli}))))

(rule
 (alias runtest)
 (action
  (diff etuple.mli etuple.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:etuple.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to float.mli.output
   (run %{checker} %{dep:float.mli}))))

(rule
 (alias runtest)
 (action
  (diff float.mli float.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:float.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to guard1.mli.output
   (run %{checker} %{dep:guard1.mli}))))

(rule
 (alias runtest)
 (action
  (diff guard1.mli guard1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:guard1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to guard2.mli.output
   (run %{checker} %{dep:guard2.mli}))))

(rule
 (alias runtest)
 (action
  (diff guard2.mli guard2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:guard2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to guard3.mli.output
   (run %{checker} %{dep:guard3.mli}))))

(rule
 (alias runtest)
 (action
  (diff guard3.mli guard3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:guard3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to guard4.mli.output
   (run %{checker} %{dep:guard4.mli}))))

(rule
 (alias runtest)
 (action
  (diff guard4.mli guard4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:guard4.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to guard5.mli.output
   (run %{checker} %{dep:guard5.mli}))))

(rule
 (alias runtest)
 (action
  (diff guard5.mli guard5.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:guard5.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to int.mli.output
   (run %{checker} %{dep:int.mli}))))

(rule
 (alias runtest)
 (action
  (diff int.mli int.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:int.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to int2.mli.output
   (run %{checker} %{dep:int2.mli}))))

(rule
 (alias runtest)
 (action
  (diff int2.mli int2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:int2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to int3.mli.output
   (run %{checker} %{dep:int3.mli}))))

(rule
 (alias runtest)
 (action
  (diff int3.mli int3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:int3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to interval.mli.output
   (run %{checker} %{dep:interval.mli}))))

(rule
 (alias runtest)
 (action
  (diff interval.mli interval.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:interval.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to list1.mli.output
   (run %{checker} %{dep:list1.mli}))))

(rule
 (alias runtest)
 (action
  (diff list1.mli list1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:list1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to list2.mli.output
   (run %{checker} %{dep:list2.mli}))))

(rule
 (alias runtest)
 (action
  (diff list2.mli list2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:list2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to neg2.mli.output
   (run %{checker} %{dep:neg2.mli}))))

(rule
 (alias runtest)
 (action
  (diff neg2.mli neg2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:neg2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to neg3.mli.output
   (run %{checker} %{dep:neg3.mli}))))

(rule
 (alias runtest)
 (action
  (diff neg3.mli neg3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:neg3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to pair.mli.output
   (run %{checker} %{dep:pair.mli}))))

(rule
 (alias runtest)
 (action
  (diff pair.mli pair.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:pair.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to record.mli.output
   (run %{checker} %{dep:record.mli}))))

(rule
 (alias runtest)
 (action
  (diff record.mli record.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:record.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to redundant1.mli.output
   (run %{checker} %{dep:redundant1.mli}))))

(rule
 (alias runtest)
 (action
  (diff redundant1.mli redundant1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:redundant1.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to redundant2.mli.output
   (run %{checker} %{dep:redundant2.mli}))))

(rule
 (alias runtest)
 (action
  (diff redundant2.mli redundant2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:redundant2.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to redundant3.mli.output
   (run %{checker} %{dep:redundant3.mli}))))

(rule
 (alias runtest)
 (action
  (diff redundant3.mli redundant3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:redundant3.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to str.mli.output
   (run %{checker} %{dep:str.mli}))))

(rule
 (alias runtest)
 (action
  (diff str.mli str.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:str.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to tuple.mli.output
   (run %{checker} %{dep:tuple.mli}))))

(rule
 (alias runtest)
 (action
  (diff tuple.mli tuple.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:tuple.mli}))))

(rule
 (deps
  (:checker %{project_root}/test/gospel_check.exe))
 (action
  (with-outputs-to tuple2.mli.output
   (run %{checker} %{dep:tuple2.mli}))))

(rule
 (alias runtest)
 (action
  (diff tuple2.mli tuple2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:tuple2.mli}))))

