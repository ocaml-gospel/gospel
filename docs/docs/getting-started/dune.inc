(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   first-spec.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:first-spec.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target first-spec.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:first-spec.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   going-further.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:going-further.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target going-further.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:going-further.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   installation.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:installation.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target installation.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:installation.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   tools.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:tools.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target tools.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:tools.mli}))))

