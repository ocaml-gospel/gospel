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
 (action
  (progn
   (with-stdout-to attributes.output
    (chdir %{project_root}
     (run gospel check %{dep:attributes.mli})))
   (diff? %{project_root}/test/utils/check_success attributes.output))))

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
 (action
  (progn
   (with-stdout-to constant-specifications.output
    (chdir %{project_root}
     (run gospel check %{dep:constant-specifications.mli})))
   (diff? %{project_root}/test/utils/check_success constant-specifications.output))))

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
 (action
  (progn
   (with-stdout-to expressions.output
    (chdir %{project_root}
     (run gospel check %{dep:expressions.mli})))
   (diff? %{project_root}/test/utils/check_success expressions.output))))

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
   scope.mli
   (chdir %{project_root}
    (run awk -f %{md2mli} %{dep:scope.md})))))

(rule
 (alias runtest)
 (enabled_if %{bin-available:awk})
 (deps %{bin:gospel})
 (action
  (progn
   (with-stdout-to scope.output
    (chdir %{project_root}
     (run gospel check %{dep:scope.mli})))
   (diff? %{project_root}/test/utils/check_success scope.output))))

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
 (action
  (progn
   (with-stdout-to semantics.output
    (chdir %{project_root}
     (run gospel check %{dep:semantics.mli})))
   (diff? %{project_root}/test/utils/check_success semantics.output))))

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
 (action
  (progn
   (with-stdout-to syntax.output
    (chdir %{project_root}
     (run gospel check %{dep:syntax.mli})))
   (diff? %{project_root}/test/utils/check_success syntax.output))))

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

