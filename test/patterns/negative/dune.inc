(rule
 (target ambiguous.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:ambiguous.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:ambiguous.mli} %{dep:ambiguous.mli.output})))

(rule
 (target base.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:base.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:base.mli} %{dep:base.mli.output})))

(rule
 (target booleans.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:booleans.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:booleans.mli} %{dep:booleans.mli.output})))

(rule
 (target char2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:char2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:char2.mli} %{dep:char2.mli.output})))

(rule
 (target etuple.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:etuple.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:etuple.mli} %{dep:etuple.mli.output})))

(rule
 (target float.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:float.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:float.mli} %{dep:float.mli.output})))

(rule
 (target guard1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:guard1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:guard1.mli} %{dep:guard1.mli.output})))

(rule
 (target guard2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:guard2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:guard2.mli} %{dep:guard2.mli.output})))

(rule
 (target guard3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:guard3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:guard3.mli} %{dep:guard3.mli.output})))

(rule
 (target guard4.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:guard4.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:guard4.mli} %{dep:guard4.mli.output})))

(rule
 (target guard5.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:guard5.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:guard5.mli} %{dep:guard5.mli.output})))

(rule
 (target int.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:int.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:int.mli} %{dep:int.mli.output})))

(rule
 (target int2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:int2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:int2.mli} %{dep:int2.mli.output})))

(rule
 (target int3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:int3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:int3.mli} %{dep:int3.mli.output})))

(rule
 (target interval.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:interval.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:interval.mli} %{dep:interval.mli.output})))

(rule
 (target list1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:list1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:list1.mli} %{dep:list1.mli.output})))

(rule
 (target list2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:list2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:list2.mli} %{dep:list2.mli.output})))

(rule
 (target neg2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:neg2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:neg2.mli} %{dep:neg2.mli.output})))

(rule
 (target neg3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:neg3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:neg3.mli} %{dep:neg3.mli.output})))

(rule
 (target pair.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:pair.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:pair.mli} %{dep:pair.mli.output})))

(rule
 (target record.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:record.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:record.mli} %{dep:record.mli.output})))

(rule
 (target redundant1.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:redundant1.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:redundant1.mli} %{dep:redundant1.mli.output})))

(rule
 (target redundant2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:redundant2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:redundant2.mli} %{dep:redundant2.mli.output})))

(rule
 (target redundant3.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:redundant3.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:redundant3.mli} %{dep:redundant3.mli.output})))

(rule
 (target str.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:str.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:str.mli} %{dep:str.mli.output})))

(rule
 (target tuple.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:tuple.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:tuple.mli} %{dep:tuple.mli.output})))

(rule
 (target tuple2.mli.output)
 (deps (source_tree .))
 (action
   (with-outputs-to %{target}
      (run %{project_root}/test/gospel_check.exe %{dep:tuple2.mli}))))

(rule
 (alias runtest)
 (action (diff %{dep:tuple2.mli} %{dep:tuple2.mli.output})))

