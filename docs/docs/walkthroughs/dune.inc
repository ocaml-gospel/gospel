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
 (target fibonacci.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:fibonacci.mli}))))

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
 (target introduction.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:introduction.mli}))))

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
 (target mutable-queue.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:mutable-queue.mli}))))

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
 (target union-find.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:union-find.mli}))))

