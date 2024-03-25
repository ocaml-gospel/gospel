(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   faq.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:faq.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target faq.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:faq.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   stdlib.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:stdlib.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target stdlib.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:stdlib.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   welcome.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:welcome.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target welcome.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:welcome.mli}))))

