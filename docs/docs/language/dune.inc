(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   constants-specifications.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:constants-specifications.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to constants-specifications.output
    (chdir %{project_root}
     (run gospel check %{dep:constants-specifications.mli})))
   (diff? %{project_root}/test/utils/check_success constants-specifications.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   formulae.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:formulae.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to formulae.output
    (chdir %{project_root}
     (run gospel check %{dep:formulae.mli})))
   (diff? %{project_root}/test/utils/check_success formulae.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   function-contracts.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:function-contracts.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to function-contracts.output
    (chdir %{project_root}
     (run gospel check %{dep:function-contracts.mli})))
   (diff? %{project_root}/test/utils/check_success function-contracts.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   lexical-conventions.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:lexical-conventions.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to lexical-conventions.output
    (chdir %{project_root}
     (run gospel check %{dep:lexical-conventions.mli})))
   (diff? %{project_root}/test/utils/check_success lexical-conventions.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   locations.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:locations.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to locations.output
    (chdir %{project_root}
     (run gospel check %{dep:locations.mli})))
   (diff? %{project_root}/test/utils/check_success locations.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   logical.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:logical.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to logical.output
    (chdir %{project_root}
     (run gospel check %{dep:logical.mli})))
   (diff? %{project_root}/test/utils/check_success logical.output))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   type-specifications.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:type-specifications.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to type-specifications.output
    (chdir %{project_root}
     (run gospel check %{dep:type-specifications.mli})))
   (diff? %{project_root}/test/utils/check_success type-specifications.output))))

