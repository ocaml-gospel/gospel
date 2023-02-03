(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   CHANGES.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:CHANGES.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to CHANGES.output
    (chdir %{project_root}
     (run gospel check %{dep:CHANGES.mli})))
   (diff? %{project_root}/test/utils/check_success CHANGES.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   README.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:README.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to README.output
    (chdir %{project_root}
     (run gospel check %{dep:README.mli})))
   (diff? %{project_root}/test/utils/check_success README.output))))

