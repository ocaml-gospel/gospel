(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets alias_type.gospel)
 (action
  (with-outputs-to alias_type.mli.output
   (run %{checker} %{dep:alias_type.mli}))))

(rule
 (alias runtest)
 (action
  (diff alias_type.mli alias_type.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:alias_type.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets extensible_type.gospel)
 (action
  (with-outputs-to extensible_type.mli.output
   (run %{checker} %{dep:extensible_type.mli}))))

(rule
 (alias runtest)
 (action
  (diff extensible_type.mli extensible_type.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:extensible_type.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets first_class_module.gospel)
 (action
  (with-outputs-to first_class_module.mli.output
   (run %{checker} %{dep:first_class_module.mli}))))

(rule
 (alias runtest)
 (action
  (diff first_class_module.mli first_class_module.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:first_class_module.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets object.gospel)
 (action
  (with-outputs-to object.mli.output
   (run %{checker} %{dep:object.mli}))))

(rule
 (alias runtest)
 (action
  (diff object.mli object.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:object.mli}))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (targets polymorphic_variants.gospel)
 (action
  (with-outputs-to polymorphic_variants.mli.output
   (run %{checker} %{dep:polymorphic_variants.mli}))))

(rule
 (alias runtest)
 (action
  (diff polymorphic_variants.mli polymorphic_variants.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:polymorphic_variants.mli}))))

