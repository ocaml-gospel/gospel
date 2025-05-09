(rule
(targets _gospel)
   (action (run mkdir -p _gospel))) ; create the compilation directory.
   (rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to allowed_top_level.mli.output
   (run %{checker} %{dep:allowed_top_level.mli}))))

(rule
 (alias runtest)
 (action
  (diff allowed_top_level.mli allowed_top_level.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to consume_ret.mli.output
   (run %{checker} %{dep:consume_ret.mli}))))

(rule
 (alias runtest)
 (action
  (diff consume_ret.mli consume_ret.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to invalid_header_nm.mli.output
   (run %{checker} %{dep:invalid_header_nm.mli}))))

(rule
 (alias runtest)
 (action
  (diff invalid_header_nm.mli invalid_header_nm.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to no_gospel_rep1.mli.output
   (run %{checker} %{dep:no_gospel_rep1.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_gospel_rep1.mli no_gospel_rep1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to no_gospel_rep2.mli.output
   (run %{checker} %{dep:no_gospel_rep2.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_gospel_rep2.mli no_gospel_rep2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to no_gospel_rep3.mli.output
   (run %{checker} %{dep:no_gospel_rep3.mli}))))

(rule
 (alias runtest)
 (action
  (diff no_gospel_rep3.mli no_gospel_rep3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_produced.mli.output
   (run %{checker} %{dep:not_produced.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_produced.mli not_produced.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_produced_top_level1.mli.output
   (run %{checker} %{dep:not_produced_top_level1.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_produced_top_level1.mli not_produced_top_level1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_produced_top_level2.mli.output
   (run %{checker} %{dep:not_produced_top_level2.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_produced_top_level2.mli not_produced_top_level2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_produced_top_level3.mli.output
   (run %{checker} %{dep:not_produced_top_level3.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_produced_top_level3.mli not_produced_top_level3.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to not_produced_top_level4.mli.output
   (run %{checker} %{dep:not_produced_top_level4.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_produced_top_level4.mli not_produced_top_level4.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to own_ghost1.mli.output
   (run %{checker} %{dep:own_ghost1.mli}))))

(rule
 (alias runtest)
 (action
  (diff own_ghost1.mli own_ghost1.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to own_ghost2.mli.output
   (run %{checker} %{dep:own_ghost2.mli}))))

(rule
 (alias runtest)
 (action
  (diff own_ghost2.mli own_ghost2.mli.output)))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe)
       _gospel)
 (action
  (with-outputs-to own_ghost3.mli.output
   (run %{checker} %{dep:own_ghost3.mli}))))

(rule
 (alias runtest)
 (action
  (diff own_ghost3.mli own_ghost3.mli.output)))

