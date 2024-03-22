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
 (action
  (progn
   (with-stdout-to first-spec.output
    (chdir %{project_root}
     (run gospel check %{dep:first-spec.mli})))
   (diff? %{project_root}/test/utils/check_success first-spec.output))))

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
 (action
  (progn
   (with-stdout-to going-further.output
    (chdir %{project_root}
     (run gospel check %{dep:going-further.mli})))
   (diff? %{project_root}/test/utils/check_success going-further.output))))

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
 (action
  (progn
   (with-stdout-to installation.output
    (chdir %{project_root}
     (run gospel check %{dep:installation.mli})))
   (diff? %{project_root}/test/utils/check_success installation.output))))

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
 (action
  (progn
   (with-stdout-to tools.output
    (chdir %{project_root}
     (run gospel check %{dep:tools.mli})))
   (diff? %{project_root}/test/utils/check_success tools.output))))

