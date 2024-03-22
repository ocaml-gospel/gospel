(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   fibonacci.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:fibonacci.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to fibonacci.output
    (chdir %{project_root}
     (run gospel check %{dep:fibonacci.mli})))
   (diff? %{project_root}/test/utils/check_success fibonacci.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   introduction.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:introduction.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to introduction.output
    (chdir %{project_root}
     (run gospel check %{dep:introduction.mli})))
   (diff? %{project_root}/test/utils/check_success introduction.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   mutable-queue.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:mutable-queue.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to mutable-queue.output
    (chdir %{project_root}
     (run gospel check %{dep:mutable-queue.mli})))
   (diff? %{project_root}/test/utils/check_success mutable-queue.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   union-find.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:union-find.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to union-find.output
    (chdir %{project_root}
     (run gospel check %{dep:union-find.mli})))
   (diff? %{project_root}/test/utils/check_success union-find.output))))

