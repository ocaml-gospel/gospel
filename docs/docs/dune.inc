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
 (action
  (progn
   (with-stdout-to faq.output
    (chdir %{project_root}
     (run gospel check %{dep:faq.mli})))
   (diff? %{project_root}/test/utils/check_success faq.output))))

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
 (action
  (progn
   (with-stdout-to stdlib.output
    (chdir %{project_root}
     (run gospel check %{dep:stdlib.mli})))
   (diff? %{project_root}/test/utils/check_success stdlib.output))))

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
 (action
  (progn
   (with-stdout-to welcome.output
    (chdir %{project_root}
     (run gospel check %{dep:welcome.mli})))
   (diff? %{project_root}/test/utils/check_success welcome.output))))

