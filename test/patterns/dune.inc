(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to allsorts.mli.output
   (run %{checker} %{dep:allsorts.mli}))))

(rule
 (alias runtest)
 (action
  (diff allsorts.mli allsorts.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:allsorts.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:allsorts.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to allsorts2.mli.output
   (run %{checker} %{dep:allsorts2.mli}))))

(rule
 (alias runtest)
 (action
  (diff allsorts2.mli allsorts2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:allsorts2.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:allsorts2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to ambiguous.mli.output
   (run %{checker} %{dep:ambiguous.mli}))))

(rule
 (alias runtest)
 (action
  (diff ambiguous.mli ambiguous.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:ambiguous.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:ambiguous.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to expressions_raises.mli.output
   (run %{checker} %{dep:expressions_raises.mli}))))

(rule
 (alias runtest)
 (action
  (diff expressions_raises.mli expressions_raises.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:expressions_raises.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:expressions_raises.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun_list.mli.output
   (run %{checker} %{dep:fun_list.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_list.mli fun_list.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun_list.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:fun_list.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun_option.mli.output
   (run %{checker} %{dep:fun_option.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_option.mli fun_option.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun_option.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:fun_option.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun_option_partial.mli.output
   (run %{checker} %{dep:fun_option_partial.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_option_partial.mli fun_option_partial.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun_option_partial.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:fun_option_partial.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun_pair.mli.output
   (run %{checker} %{dep:fun_pair.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_pair.mli fun_pair.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun_pair.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:fun_pair.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to fun_triple.mli.output
   (run %{checker} %{dep:fun_triple.mli}))))

(rule
 (alias runtest)
 (action
  (diff fun_triple.mli fun_triple.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:fun_triple.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:fun_triple.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to inner_cast.mli.output
   (run %{checker} %{dep:inner_cast.mli}))))

(rule
 (alias runtest)
 (action
  (diff inner_cast.mli inner_cast.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:inner_cast.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:inner_cast.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_bool.mli.output
   (run %{checker} %{dep:nonexhaustive_bool.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_bool.mli nonexhaustive_bool.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_bool.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_bool.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_char.mli.output
   (run %{checker} %{dep:nonexhaustive_char.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_char.mli nonexhaustive_char.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_char.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_char.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_float.mli.output
   (run %{checker} %{dep:nonexhaustive_float.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_float.mli nonexhaustive_float.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_float.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_float.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_guard.mli.output
   (run %{checker} %{dep:nonexhaustive_guard.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_guard.mli nonexhaustive_guard.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_guard.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_guard.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_char.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_char.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_char.mli nonexhaustive_inner_char.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_char.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_char.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_int1.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_int1.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_int1.mli nonexhaustive_inner_int1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_int1.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_int1.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_int2.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_int2.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_int2.mli nonexhaustive_inner_int2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_int2.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_int2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_int3.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_int3.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_int3.mli nonexhaustive_inner_int3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_int3.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_int3.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_int4.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_int4.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_int4.mli nonexhaustive_inner_int4.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_int4.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_int4.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_list.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_list.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_list.mli nonexhaustive_inner_list.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_list.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_list.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_pair1.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_pair1.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_pair1.mli nonexhaustive_inner_pair1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_pair1.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_pair1.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_pair2.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_pair2.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_pair2.mli nonexhaustive_inner_pair2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_pair2.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_pair2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_string1.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_string1.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_string1.mli nonexhaustive_inner_string1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_string1.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_string1.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_inner_string2.mli.output
   (run %{checker} %{dep:nonexhaustive_inner_string2.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_inner_string2.mli nonexhaustive_inner_string2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_inner_string2.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_inner_string2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_int.mli.output
   (run %{checker} %{dep:nonexhaustive_int.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_int.mli nonexhaustive_int.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_int.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_int.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_list.mli.output
   (run %{checker} %{dep:nonexhaustive_list.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_list.mli nonexhaustive_list.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_list.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_list.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_list_list.mli.output
   (run %{checker} %{dep:nonexhaustive_list_list.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_list_list.mli nonexhaustive_list_list.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_list_list.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_list_list.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_pair1.mli.output
   (run %{checker} %{dep:nonexhaustive_pair1.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_pair1.mli nonexhaustive_pair1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_pair1.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_pair1.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_pair2.mli.output
   (run %{checker} %{dep:nonexhaustive_pair2.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_pair2.mli nonexhaustive_pair2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_pair2.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_pair2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to nonexhaustive_type.mli.output
   (run %{checker} %{dep:nonexhaustive_type.mli}))))

(rule
 (alias runtest)
 (action
  (diff nonexhaustive_type.mli nonexhaustive_type.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:nonexhaustive_type.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:nonexhaustive_type.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to not_all_guarded1.mli.output
   (run %{checker} %{dep:not_all_guarded1.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_all_guarded1.mli not_all_guarded1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:not_all_guarded1.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:not_all_guarded1.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to not_all_guarded2.mli.output
   (run %{checker} %{dep:not_all_guarded2.mli}))))

(rule
 (alias runtest)
 (action
  (diff not_all_guarded2.mli not_all_guarded2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:not_all_guarded2.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:not_all_guarded2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to redundant1.mli.output
   (run %{checker} %{dep:redundant1.mli}))))

(rule
 (alias runtest)
 (action
  (diff redundant1.mli redundant1.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:redundant1.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:redundant1.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to redundant2.mli.output
   (run %{checker} %{dep:redundant2.mli}))))

(rule
 (alias runtest)
 (action
  (diff redundant2.mli redundant2.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:redundant2.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:redundant2.mli})))))

(rule
 (deps
  %{bin:gospel}
  (:checker %{project_root}/test/utils/testchecker.exe))
 (action
  (with-outputs-to redundant3.mli.output
   (run %{checker} %{dep:redundant3.mli}))))

(rule
 (alias runtest)
 (action
  (diff redundant3.mli redundant3.mli.output)))

(rule
 (alias test-cmis)
 (action
  (chdir %{project_root}
   ; Syntax sanity check
   (run ocamlc -c %{dep:redundant3.mli}))))

; A good alias to use in a pre-push hook
(rule
 (alias pre-push)
 (action
  (chdir %{project_root}
   (ignore-stdout
    (run git rev-parse --verify --quiet @:%{dep:redundant3.mli})))))

