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
 (target CHANGES.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:CHANGES.mli}))))

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
 (target README.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:README.mli}))))

