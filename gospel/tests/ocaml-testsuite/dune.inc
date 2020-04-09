
(rule
 (targets Documentation_tags.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Documentation_tags.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Documentation_tags.mli.expected} %{dep:Documentation_tags.mli.output})))

(rule
 (targets Include_module_type_of.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Include_module_type_of.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Include_module_type_of.mli.expected} %{dep:Include_module_type_of.mli.output})))

(rule
 (targets Inline_records.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Inline_records.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Inline_records.mli.expected} %{dep:Inline_records.mli.output})))

(rule
 (targets Item_ids.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Item_ids.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Item_ids.mli.expected} %{dep:Item_ids.mli.output})))

(rule
 (targets Level_0.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Level_0.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Level_0.mli.expected} %{dep:Level_0.mli.output})))

(rule
 (targets Linebreaks.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Linebreaks.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Linebreaks.mli.expected} %{dep:Linebreaks.mli.output})))

(rule
 (targets No_preamble.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:No_preamble.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:No_preamble.mli.expected} %{dep:No_preamble.mli.output})))

(rule
 (targets Paragraph.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Paragraph.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Paragraph.mli.expected} %{dep:Paragraph.mli.output})))

(rule
 (targets Test.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Test.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Test.mli.expected} %{dep:Test.mli.output})))

(rule
 (targets Variants.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:Variants.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:Variants.mli.expected} %{dep:Variants.mli.output})))

(rule
 (targets api.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:api.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:api.mli.expected} %{dep:api.mli.output})))

(rule
 (targets bar.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bar.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bar.mli.expected} %{dep:bar.mli.output})))

(rule
 (targets c.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:c.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:c.mli.expected} %{dep:c.mli.output})))

(rule
 (targets common.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:common.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:common.mli.expected} %{dep:common.mli.output})))

(rule
 (targets d.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:d.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:d.mli.expected} %{dep:d.mli.output})))

(rule
 (targets deprecated_module.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:deprecated_module.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:deprecated_module.mli.expected} %{dep:deprecated_module.mli.output})))

(rule
 (targets equations.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:equations.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:equations.mli.expected} %{dep:equations.mli.output})))

(rule
 (targets external.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:external.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:external.mli.expected} %{dep:external.mli.output})))

(rule
 (targets external_for_pack.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:external_for_pack.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:external_for_pack.mli.expected} %{dep:external_for_pack.mli.output})))

(rule
 (targets foo.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:foo.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:foo.mli.expected} %{dep:foo.mli.output})))

(rule
 (targets gpr1223_bar.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:gpr1223_bar.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:gpr1223_bar.mli.expected} %{dep:gpr1223_bar.mli.output})))

(rule
 (targets gpr1223_foo.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:gpr1223_foo.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:gpr1223_foo.mli.expected} %{dep:gpr1223_foo.mli.output})))

(rule
 (targets kb.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:kb.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:kb.mli.expected} %{dep:kb.mli.output})))

(rule
 (targets latex_ref.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:latex_ref.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:latex_ref.mli.expected} %{dep:latex_ref.mli.output})))

(rule
 (targets length.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:length.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:length.mli.expected} %{dep:length.mli.output})))

(rule
 (targets lexcmm.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lexcmm.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lexcmm.mli.expected} %{dep:lexcmm.mli.output})))

(rule
 (targets lib.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lib.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lib.mli.expected} %{dep:lib.mli.output})))

(rule
 (targets main.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:main.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:main.mli.expected} %{dep:main.mli.output})))

(rule
 (targets module_without_cmx.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:module_without_cmx.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:module_without_cmx.mli.expected} %{dep:module_without_cmx.mli.output})))

(rule
 (targets multdef.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:multdef.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:multdef.mli.expected} %{dep:multdef.mli.output})))

(rule
 (targets mylib.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:mylib.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:mylib.mli.expected} %{dep:mylib.mli.output})))

(rule
 (targets opaque_impl.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:opaque_impl.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:opaque_impl.mli.expected} %{dep:opaque_impl.mli.output})))

(rule
 (targets opaque_intf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:opaque_intf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:opaque_intf.mli.expected} %{dep:opaque_intf.mli.output})))

(rule
 (targets orderings.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:orderings.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:orderings.mli.expected} %{dep:orderings.mli.output})))

(rule
 (targets parsecmmaux.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parsecmmaux.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parsecmmaux.mli.expected} %{dep:parsecmmaux.mli.output})))

(rule
 (targets plugin.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:plugin.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:plugin.mli.expected} %{dep:plugin.mli.output})))

(rule
 (targets pr3918a.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pr3918a.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pr3918a.mli.expected} %{dep:pr3918a.mli.output})))

(rule
 (targets pr3918b.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pr3918b.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pr3918b.mli.expected} %{dep:pr3918b.mli.output})))

(rule
 (targets regular.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:regular.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:regular.mli.expected} %{dep:regular.mli.output})))

(rule
 (targets stop_after_parsing_intf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:stop_after_parsing_intf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:stop_after_parsing_intf.mli.expected} %{dep:stop_after_parsing_intf.mli.output})))

(rule
 (targets t22ok.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t22ok.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t22ok.mli.expected} %{dep:t22ok.mli.output})))

(rule
 (targets terms.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:terms.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:terms.mli.expected} %{dep:terms.mli.output})))

(rule
 (targets testing.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:testing.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:testing.mli.expected} %{dep:testing.mli.output})))

(rule
 (targets unclosed_class_signature.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unclosed_class_signature.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unclosed_class_signature.mli.expected} %{dep:unclosed_class_signature.mli.output})))

(rule
 (targets unclosed_paren_module_type.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unclosed_paren_module_type.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unclosed_paren_module_type.mli.expected} %{dep:unclosed_paren_module_type.mli.output})))

(rule
 (targets unclosed_sig.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unclosed_sig.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unclosed_sig.mli.expected} %{dep:unclosed_sig.mli.output})))

(rule
 (targets w32.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:w32.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:w32.mli.expected} %{dep:w32.mli.output})))

(rule
 (targets w60.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:w60.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:w60.mli.expected} %{dep:w60.mli.output})))
