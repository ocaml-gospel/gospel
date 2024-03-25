(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   attributes.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:attributes.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target attributes.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:attributes.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   constant-specifications.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:constant-specifications.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target constant-specifications.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:constant-specifications.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   expressions.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:expressions.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target expressions.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:expressions.mli}))))

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
 (target function-contracts.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:function-contracts.mli}))))

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
 (target lexical-conventions.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:lexical-conventions.mli}))))

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
 (target logical.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:logical.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   scope.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:scope.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target scope.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:scope.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   semantics.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:semantics.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target semantics.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:semantics.mli}))))

(rule
 (enabled_if %{bin-available:awk})
 (deps
  (:md2mli %{project_root}/test/utils/md2mli.awk))
 (action
  (with-stdout-to
   syntax.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:syntax.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (target syntax.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:syntax.mli}))))

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
 (target type-specifications.gospel)
 (action
  (chdir %{project_root}
   (run gospel check %{dep:type-specifications.mli}))))

