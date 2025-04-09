(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_record1.mli.output
   (run %{checker} %{dep:ocaml_record1.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_record1.mli ocaml_record1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_record2.mli.output
   (run %{checker} %{dep:ocaml_record2.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_record2.mli ocaml_record2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_record3.mli.output
   (run %{checker} %{dep:ocaml_record3.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_record3.mli ocaml_record3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_record4.mli.output
   (run %{checker} %{dep:ocaml_record4.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_record4.mli ocaml_record4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_type1.mli.output
   (run %{checker} %{dep:ocaml_type1.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_type1.mli ocaml_type1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_type2.mli.output
   (run %{checker} %{dep:ocaml_type2.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_type2.mli ocaml_type2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_type3.mli.output
   (run %{checker} %{dep:ocaml_type3.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_type3.mli ocaml_type3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_type4.mli.output
   (run %{checker} %{dep:ocaml_type4.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_type4.mli ocaml_type4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to ocaml_type5.mli.output
   (run %{checker} %{dep:ocaml_type5.mli}))))

(rule
 (alias runtest)
 (action
  (diff ocaml_type5.mli ocaml_type5.mli.output)))

