(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets BitSet.gospel)
 (action
  (with-outputs-to BitSet.mli.output
   (run %{checker} %{dep:BitSet.mli}))))

(rule
 (alias runtest)
 (action
  (diff BitSet.mli BitSet.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:BitSet.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Eq.gospel)
 (action
  (with-outputs-to Eq.mli.output
   (run %{checker} %{dep:Eq.mli}))))

(rule
 (alias runtest)
 (action
  (diff Eq.mli Eq.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Eq.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Hashtbl.gospel)
 (action
  (with-outputs-to Hashtbl.mli.output
   (run %{checker} %{dep:Hashtbl.mli}))))

(rule
 (alias runtest)
 (action
  (diff Hashtbl.mli Hashtbl.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Hashtbl.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Ref.gospel)
 (action
  (with-outputs-to Ref.mli.output
   (run %{checker} %{dep:Ref.mli}))))

(rule
 (alias runtest)
 (action
  (diff Ref.mli Ref.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Ref.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Stack.gospel)
 (action
  (with-outputs-to Stack.mli.output
   (run %{checker} %{dep:Stack.mli}))))

(rule
 (alias runtest)
 (action
  (diff Stack.mli Stack.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Stack.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets Stack_cfml.gospel)
 (action
  (with-outputs-to Stack_cfml.mli.output
   (run %{checker} %{dep:Stack_cfml.mli}))))

(rule
 (alias runtest)
 (action
  (diff Stack_cfml.mli Stack_cfml.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:Stack_cfml.mli}))))

