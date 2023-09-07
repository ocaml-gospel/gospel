(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   good-practices.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:good-practices.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to good-practices.output
    (chdir %{project_root}
     (run gospel check %{dep:good-practices.mli})))
   (diff? %{project_root}/test/utils/check_success good-practices.output))))

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

