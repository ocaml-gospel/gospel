
(rule
 (targets CSEgen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:CSEgen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:CSEgen.mli.expected} %{dep:CSEgen.mli.output})))

(rule
 (targets actions.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:actions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:actions.mli.expected} %{dep:actions.mli.output})))

(rule
 (targets actions_helpers.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:actions_helpers.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:actions_helpers.mli.expected} %{dep:actions_helpers.mli.output})))

(rule
 (targets afl_instrument.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:afl_instrument.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:afl_instrument.mli.expected} %{dep:afl_instrument.mli.output})))

(rule
 (targets alias_analysis.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:alias_analysis.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:alias_analysis.mli.expected} %{dep:alias_analysis.mli.output})))

(rule
 (targets allocated_const.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:allocated_const.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:allocated_const.mli.expected} %{dep:allocated_const.mli.output})))

(rule
 (targets annot.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:annot.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:annot.mli.expected} %{dep:annot.mli.output})))

(rule
 (targets arg.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:arg.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:arg.mli.expected} %{dep:arg.mli.output})))

(rule
 (targets arg_helper.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:arg_helper.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:arg_helper.mli.expected} %{dep:arg_helper.mli.output})))

(rule
 (targets array.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:array.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:array.mli.expected} %{dep:array.mli.output})))

(rule
 (targets arrayLabels.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:arrayLabels.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:arrayLabels.mli.expected} %{dep:arrayLabels.mli.output})))

(rule
 (targets asmgen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:asmgen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:asmgen.mli.expected} %{dep:asmgen.mli.output})))

(rule
 (targets asmlibrarian.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:asmlibrarian.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:asmlibrarian.mli.expected} %{dep:asmlibrarian.mli.output})))

(rule
 (targets asmlink.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:asmlink.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:asmlink.mli.expected} %{dep:asmlink.mli.output})))

(rule
 (targets asmpackager.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:asmpackager.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:asmpackager.mli.expected} %{dep:asmpackager.mli.output})))

(rule
 (targets ast_helper.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ast_helper.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ast_helper.mli.expected} %{dep:ast_helper.mli.output})))

(rule
 (targets ast_invariants.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ast_invariants.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ast_invariants.mli.expected} %{dep:ast_invariants.mli.output})))

(rule
 (targets ast_iterator.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ast_iterator.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ast_iterator.mli.expected} %{dep:ast_iterator.mli.output})))

(rule
 (targets ast_mapper.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ast_mapper.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ast_mapper.mli.expected} %{dep:ast_mapper.mli.output})))

(rule
 (targets asttypes.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:asttypes.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:asttypes.mli.expected} %{dep:asttypes.mli.output})))

(rule
 (targets attr_helper.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:attr_helper.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:attr_helper.mli.expected} %{dep:attr_helper.mli.output})))

(rule
 (targets augment_specialised_args.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:augment_specialised_args.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:augment_specialised_args.mli.expected} %{dep:augment_specialised_args.mli.output})))

(rule
 (targets available_regs.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:available_regs.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:available_regs.mli.expected} %{dep:available_regs.mli.output})))

(rule
 (targets backend_intf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:backend_intf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:backend_intf.mli.expected} %{dep:backend_intf.mli.output})))

(rule
 (targets bigarray.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bigarray.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bigarray.mli.expected} %{dep:bigarray.mli.output})))

(rule
 (targets branch_relaxation.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:branch_relaxation.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:branch_relaxation.mli.expected} %{dep:branch_relaxation.mli.output})))

(rule
 (targets breakpoints.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:breakpoints.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:breakpoints.mli.expected} %{dep:breakpoints.mli.output})))

(rule
 (targets btype.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:btype.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:btype.mli.expected} %{dep:btype.mli.output})))

(rule
 (targets buffer.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:buffer.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:buffer.mli.expected} %{dep:buffer.mli.output})))

(rule
 (targets build_export_info.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:build_export_info.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:build_export_info.mli.expected} %{dep:build_export_info.mli.output})))

(rule
 (targets build_path_prefix_map.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:build_path_prefix_map.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:build_path_prefix_map.mli.expected} %{dep:build_path_prefix_map.mli.output})))

(rule
 (targets builtin_actions.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:builtin_actions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:builtin_actions.mli.expected} %{dep:builtin_actions.mli.output})))

(rule
 (targets builtin_attributes.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:builtin_attributes.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:builtin_attributes.mli.expected} %{dep:builtin_attributes.mli.output})))

(rule
 (targets builtin_variables.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:builtin_variables.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:builtin_variables.mli.expected} %{dep:builtin_variables.mli.output})))

(rule
 (targets bytegen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bytegen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bytegen.mli.expected} %{dep:bytegen.mli.output})))

(rule
 (targets bytelibrarian.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bytelibrarian.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bytelibrarian.mli.expected} %{dep:bytelibrarian.mli.output})))

(rule
 (targets bytelink.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bytelink.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bytelink.mli.expected} %{dep:bytelink.mli.output})))

(rule
 (targets bytepackager.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bytepackager.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bytepackager.mli.expected} %{dep:bytepackager.mli.output})))

(rule
 (targets bytes.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bytes.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bytes.mli.expected} %{dep:bytes.mli.output})))

(rule
 (targets bytesLabels.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bytesLabels.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bytesLabels.mli.expected} %{dep:bytesLabels.mli.output})))

(rule
 (targets bytesections.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:bytesections.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:bytesections.mli.expected} %{dep:bytesections.mli.output})))

(rule
 (targets callback.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:callback.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:callback.mli.expected} %{dep:callback.mli.output})))

(rule
 (targets camlinternalFormat.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:camlinternalFormat.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:camlinternalFormat.mli.expected} %{dep:camlinternalFormat.mli.output})))

(rule
 (targets camlinternalFormatBasics.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:camlinternalFormatBasics.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:camlinternalFormatBasics.mli.expected} %{dep:camlinternalFormatBasics.mli.output})))

(rule
 (targets camlinternalLazy.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:camlinternalLazy.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:camlinternalLazy.mli.expected} %{dep:camlinternalLazy.mli.output})))

(rule
 (targets camlinternalMod.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:camlinternalMod.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:camlinternalMod.mli.expected} %{dep:camlinternalMod.mli.output})))

(rule
 (targets camlinternalOO.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:camlinternalOO.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:camlinternalOO.mli.expected} %{dep:camlinternalOO.mli.output})))

(rule
 (targets ccomp.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ccomp.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ccomp.mli.expected} %{dep:ccomp.mli.output})))

(rule
 (targets char.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:char.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:char.mli.expected} %{dep:char.mli.output})))

(rule
 (targets checkpoints.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:checkpoints.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:checkpoints.mli.expected} %{dep:checkpoints.mli.output})))

(rule
 (targets clambda.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:clambda.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:clambda.mli.expected} %{dep:clambda.mli.output})))

(rule
 (targets clflags.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:clflags.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:clflags.mli.expected} %{dep:clflags.mli.output})))

(rule
 (targets closure.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:closure.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:closure.mli.expected} %{dep:closure.mli.output})))

(rule
 (targets closure_conversion.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:closure_conversion.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:closure_conversion.mli.expected} %{dep:closure_conversion.mli.output})))

(rule
 (targets closure_conversion_aux.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:closure_conversion_aux.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:closure_conversion_aux.mli.expected} %{dep:closure_conversion_aux.mli.output})))

(rule
 (targets closure_element.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:closure_element.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:closure_element.mli.expected} %{dep:closure_element.mli.output})))

(rule
 (targets closure_id.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:closure_id.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:closure_id.mli.expected} %{dep:closure_id.mli.output})))

(rule
 (targets closure_offsets.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:closure_offsets.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:closure_offsets.mli.expected} %{dep:closure_offsets.mli.output})))

(rule
 (targets closure_origin.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:closure_origin.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:closure_origin.mli.expected} %{dep:closure_origin.mli.output})))

(rule
 (targets cmi_format.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:cmi_format.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:cmi_format.mli.expected} %{dep:cmi_format.mli.output})))

(rule
 (targets cmm.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:cmm.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:cmm.mli.expected} %{dep:cmm.mli.output})))

(rule
 (targets cmmgen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:cmmgen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:cmmgen.mli.expected} %{dep:cmmgen.mli.output})))

(rule
 (targets cmo_format.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:cmo_format.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:cmo_format.mli.expected} %{dep:cmo_format.mli.output})))

(rule
 (targets cmt_format.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:cmt_format.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:cmt_format.mli.expected} %{dep:cmt_format.mli.output})))

(rule
 (targets cmx_format.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:cmx_format.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:cmx_format.mli.expected} %{dep:cmx_format.mli.output})))

(rule
 (targets coloring.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:coloring.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:coloring.mli.expected} %{dep:coloring.mli.output})))

(rule
 (targets comballoc.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:comballoc.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:comballoc.mli.expected} %{dep:comballoc.mli.output})))

(rule
 (targets command_line.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:command_line.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:command_line.mli.expected} %{dep:command_line.mli.output})))

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
 (targets compact.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:compact.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:compact.mli.expected} %{dep:compact.mli.output})))

(rule
 (targets compenv.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:compenv.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:compenv.mli.expected} %{dep:compenv.mli.output})))

(rule
 (targets compilation_unit.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:compilation_unit.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:compilation_unit.mli.expected} %{dep:compilation_unit.mli.output})))

(rule
 (targets compile.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:compile.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:compile.mli.expected} %{dep:compile.mli.output})))

(rule
 (targets compile_common.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:compile_common.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:compile_common.mli.expected} %{dep:compile_common.mli.output})))

(rule
 (targets compilenv.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:compilenv.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:compilenv.mli.expected} %{dep:compilenv.mli.output})))

(rule
 (targets complex.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:complex.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:complex.mli.expected} %{dep:complex.mli.output})))

(rule
 (targets compmisc.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:compmisc.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:compmisc.mli.expected} %{dep:compmisc.mli.output})))

(rule
 (targets compplugin.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:compplugin.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:compplugin.mli.expected} %{dep:compplugin.mli.output})))

(rule
 (targets condition.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:condition.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:condition.mli.expected} %{dep:condition.mli.output})))

(rule
 (targets config.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:config.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:config.mli.expected} %{dep:config.mli.output})))

(rule
 (targets consistbl.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:consistbl.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:consistbl.mli.expected} %{dep:consistbl.mli.output})))

(rule
 (targets cset.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:cset.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:cset.mli.expected} %{dep:cset.mli.output})))

(rule
 (targets ctype.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ctype.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ctype.mli.expected} %{dep:ctype.mli.output})))

(rule
 (targets datarepr.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:datarepr.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:datarepr.mli.expected} %{dep:datarepr.mli.output})))

(rule
 (targets deadcode.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:deadcode.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:deadcode.mli.expected} %{dep:deadcode.mli.output})))

(rule
 (targets debugcom.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:debugcom.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:debugcom.mli.expected} %{dep:debugcom.mli.output})))

(rule
 (targets debugger_config.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:debugger_config.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:debugger_config.mli.expected} %{dep:debugger_config.mli.output})))

(rule
 (targets debuginfo.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:debuginfo.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:debuginfo.mli.expected} %{dep:debuginfo.mli.output})))

(rule
 (targets depend.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:depend.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:depend.mli.expected} %{dep:depend.mli.output})))

(rule
 (targets digest.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:digest.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:digest.mli.expected} %{dep:digest.mli.output})))

(rule
 (targets dll.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:dll.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:dll.mli.expected} %{dep:dll.mli.output})))

(rule
 (targets docstrings.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:docstrings.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:docstrings.mli.expected} %{dep:docstrings.mli.output})))

(rule
 (targets dynlink.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:dynlink.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:dynlink.mli.expected} %{dep:dynlink.mli.output})))

(rule
 (targets effect_analysis.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:effect_analysis.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:effect_analysis.mli.expected} %{dep:effect_analysis.mli.output})))

(rule
 (targets emit.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:emit.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:emit.mli.expected} %{dep:emit.mli.output})))

(rule
 (targets emitaux.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:emitaux.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:emitaux.mli.expected} %{dep:emitaux.mli.output})))

(rule
 (targets emitcode.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:emitcode.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:emitcode.mli.expected} %{dep:emitcode.mli.output})))

(rule
 (targets env.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:env.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:env.mli.expected} %{dep:env.mli.output})))

(rule
 (targets envaux.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:envaux.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:envaux.mli.expected} %{dep:envaux.mli.output})))

(rule
 (targets environments.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:environments.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:environments.mli.expected} %{dep:environments.mli.output})))

(rule
 (targets ephemeron.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ephemeron.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ephemeron.mli.expected} %{dep:ephemeron.mli.output})))

(rule
 (targets errors.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:errors.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:errors.mli.expected} %{dep:errors.mli.output})))

(rule
 (targets eval.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:eval.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:eval.mli.expected} %{dep:eval.mli.output})))

(rule
 (targets event.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:event.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:event.mli.expected} %{dep:event.mli.output})))

(rule
 (targets events.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:events.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:events.mli.expected} %{dep:events.mli.output})))

(rule
 (targets exec.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:exec.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:exec.mli.expected} %{dep:exec.mli.output})))

(rule
 (targets export_id.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:export_id.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:export_id.mli.expected} %{dep:export_id.mli.output})))

(rule
 (targets export_info.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:export_info.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:export_info.mli.expected} %{dep:export_info.mli.output})))

(rule
 (targets export_info_for_pack.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:export_info_for_pack.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:export_info_for_pack.mli.expected} %{dep:export_info_for_pack.mli.output})))

(rule
 (targets extract_projections.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:extract_projections.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:extract_projections.mli.expected} %{dep:extract_projections.mli.output})))

(rule
 (targets filecompare.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:filecompare.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:filecompare.mli.expected} %{dep:filecompare.mli.output})))

(rule
 (targets filename.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:filename.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:filename.mli.expected} %{dep:filename.mli.output})))

(rule
 (targets find_recursive_functions.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:find_recursive_functions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:find_recursive_functions.mli.expected} %{dep:find_recursive_functions.mli.output})))

(rule
 (targets flambda.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:flambda.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:flambda.mli.expected} %{dep:flambda.mli.output})))

(rule
 (targets flambda_invariants.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:flambda_invariants.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:flambda_invariants.mli.expected} %{dep:flambda_invariants.mli.output})))

(rule
 (targets flambda_iterators.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:flambda_iterators.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:flambda_iterators.mli.expected} %{dep:flambda_iterators.mli.output})))

(rule
 (targets flambda_to_clambda.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:flambda_to_clambda.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:flambda_to_clambda.mli.expected} %{dep:flambda_to_clambda.mli.output})))

(rule
 (targets flambda_utils.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:flambda_utils.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:flambda_utils.mli.expected} %{dep:flambda_utils.mli.output})))

(rule
 (targets float.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:float.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:float.mli.expected} %{dep:float.mli.output})))

(rule
 (targets format.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:format.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:format.mli.expected} %{dep:format.mli.output})))

(rule
 (targets frames.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:frames.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:frames.mli.expected} %{dep:frames.mli.output})))

(rule
 (targets freshening.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:freshening.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:freshening.mli.expected} %{dep:freshening.mli.output})))

(rule
 (targets gc.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:gc.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:gc.mli.expected} %{dep:gc.mli.output})))

(rule
 (targets genlex.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:genlex.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:genlex.mli.expected} %{dep:genlex.mli.output})))

(rule
 (targets genprintval.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:genprintval.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:genprintval.mli.expected} %{dep:genprintval.mli.output})))

(rule
 (targets graphics.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:graphics.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:graphics.mli.expected} %{dep:graphics.mli.output})))

(rule
 (targets graphicsX11.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:graphicsX11.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:graphicsX11.mli.expected} %{dep:graphicsX11.mli.output})))

(rule
 (targets hashtbl.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:hashtbl.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:hashtbl.mli.expected} %{dep:hashtbl.mli.output})))

(rule
 (targets history.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:history.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:history.mli.expected} %{dep:history.mli.output})))

(rule
 (targets id_types.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:id_types.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:id_types.mli.expected} %{dep:id_types.mli.output})))

(rule
 (targets ident.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ident.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ident.mli.expected} %{dep:ident.mli.output})))

(rule
 (targets identifiable.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:identifiable.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:identifiable.mli.expected} %{dep:identifiable.mli.output})))

(rule
 (targets import_approx.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:import_approx.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:import_approx.mli.expected} %{dep:import_approx.mli.output})))

(rule
 (targets includeclass.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:includeclass.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:includeclass.mli.expected} %{dep:includeclass.mli.output})))

(rule
 (targets includecore.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:includecore.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:includecore.mli.expected} %{dep:includecore.mli.output})))

(rule
 (targets includemod.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:includemod.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:includemod.mli.expected} %{dep:includemod.mli.output})))

(rule
 (targets inconstant_idents.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inconstant_idents.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inconstant_idents.mli.expected} %{dep:inconstant_idents.mli.output})))

(rule
 (targets initialize_symbol_to_let_symbol.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:initialize_symbol_to_let_symbol.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:initialize_symbol_to_let_symbol.mli.expected} %{dep:initialize_symbol_to_let_symbol.mli.output})))

(rule
 (targets inline_and_simplify.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inline_and_simplify.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inline_and_simplify.mli.expected} %{dep:inline_and_simplify.mli.output})))

(rule
 (targets inline_and_simplify_aux.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inline_and_simplify_aux.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inline_and_simplify_aux.mli.expected} %{dep:inline_and_simplify_aux.mli.output})))

(rule
 (targets inlining_cost.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inlining_cost.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inlining_cost.mli.expected} %{dep:inlining_cost.mli.output})))

(rule
 (targets inlining_decision.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inlining_decision.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inlining_decision.mli.expected} %{dep:inlining_decision.mli.output})))

(rule
 (targets inlining_decision_intf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inlining_decision_intf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inlining_decision_intf.mli.expected} %{dep:inlining_decision_intf.mli.output})))

(rule
 (targets inlining_stats.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inlining_stats.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inlining_stats.mli.expected} %{dep:inlining_stats.mli.output})))

(rule
 (targets inlining_stats_types.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inlining_stats_types.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inlining_stats_types.mli.expected} %{dep:inlining_stats_types.mli.output})))

(rule
 (targets inlining_transforms.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:inlining_transforms.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:inlining_transforms.mli.expected} %{dep:inlining_transforms.mli.output})))

(rule
 (targets input_handling.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:input_handling.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:input_handling.mli.expected} %{dep:input_handling.mli.output})))

(rule
 (targets instruct.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:instruct.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:instruct.mli.expected} %{dep:instruct.mli.output})))

(rule
 (targets int32.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:int32.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:int32.mli.expected} %{dep:int32.mli.output})))

(rule
 (targets int64.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:int64.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:int64.mli.expected} %{dep:int64.mli.output})))

(rule
 (targets int64ops.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:int64ops.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:int64ops.mli.expected} %{dep:int64ops.mli.output})))

(rule
 (targets int_replace_polymorphic_compare.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:int_replace_polymorphic_compare.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:int_replace_polymorphic_compare.mli.expected} %{dep:int_replace_polymorphic_compare.mli.output})))

(rule
 (targets interf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:interf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:interf.mli.expected} %{dep:interf.mli.output})))

(rule
 (targets internal_variable_names.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:internal_variable_names.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:internal_variable_names.mli.expected} %{dep:internal_variable_names.mli.output})))

(rule
 (targets interval.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:interval.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:interval.mli.expected} %{dep:interval.mli.output})))

(rule
 (targets invariant_params.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:invariant_params.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:invariant_params.mli.expected} %{dep:invariant_params.mli.output})))

(rule
 (targets lambda.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lambda.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lambda.mli.expected} %{dep:lambda.mli.output})))

(rule
 (targets lazy.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lazy.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lazy.mli.expected} %{dep:lazy.mli.output})))

(rule
 (targets lexer.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lexer.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lexer.mli.expected} %{dep:lexer.mli.output})))

(rule
 (targets lexgen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lexgen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lexgen.mli.expected} %{dep:lexgen.mli.output})))

(rule
 (targets lexing.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lexing.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lexing.mli.expected} %{dep:lexing.mli.output})))

(rule
 (targets lift_code.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lift_code.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lift_code.mli.expected} %{dep:lift_code.mli.output})))

(rule
 (targets lift_constants.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lift_constants.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lift_constants.mli.expected} %{dep:lift_constants.mli.output})))

(rule
 (targets lift_let_to_initialize_symbol.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:lift_let_to_initialize_symbol.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:lift_let_to_initialize_symbol.mli.expected} %{dep:lift_let_to_initialize_symbol.mli.output})))

(rule
 (targets linearize.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:linearize.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:linearize.mli.expected} %{dep:linearize.mli.output})))

(rule
 (targets linkage_name.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:linkage_name.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:linkage_name.mli.expected} %{dep:linkage_name.mli.output})))

(rule
 (targets linscan.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:linscan.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:linscan.mli.expected} %{dep:linscan.mli.output})))

(rule
 (targets list.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:list.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:list.mli.expected} %{dep:list.mli.output})))

(rule
 (targets listLabels.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:listLabels.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:listLabels.mli.expected} %{dep:listLabels.mli.output})))

(rule
 (targets liveness.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:liveness.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:liveness.mli.expected} %{dep:liveness.mli.output})))

(rule
 (targets loadprinter.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:loadprinter.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:loadprinter.mli.expected} %{dep:loadprinter.mli.output})))

(rule
 (targets location.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:location.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:location.mli.expected} %{dep:location.mli.output})))

(rule
 (targets longident.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:longident.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:longident.mli.expected} %{dep:longident.mli.output})))

(rule
 (targets mach.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:mach.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:mach.mli.expected} %{dep:mach.mli.output})))

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
 (targets main_args.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:main_args.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:main_args.mli.expected} %{dep:main_args.mli.output})))

(rule
 (targets makedepend.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:makedepend.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:makedepend.mli.expected} %{dep:makedepend.mli.output})))

(rule
 (targets map.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:map.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:map.mli.expected} %{dep:map.mli.output})))

(rule
 (targets marshal.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:marshal.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:marshal.mli.expected} %{dep:marshal.mli.output})))

(rule
 (targets matching.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:matching.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:matching.mli.expected} %{dep:matching.mli.output})))

(rule
 (targets menhirLib.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:menhirLib.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:menhirLib.mli.expected} %{dep:menhirLib.mli.output})))

(rule
 (targets meta.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:meta.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:meta.mli.expected} %{dep:meta.mli.output})))

(rule
 (targets middle_end.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:middle_end.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:middle_end.mli.expected} %{dep:middle_end.mli.output})))

(rule
 (targets misc.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:misc.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:misc.mli.expected} %{dep:misc.mli.output})))

(rule
 (targets moreLabels.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:moreLabels.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:moreLabels.mli.expected} %{dep:moreLabels.mli.output})))

(rule
 (targets mtype.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:mtype.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:mtype.mli.expected} %{dep:mtype.mli.output})))

(rule
 (targets mutable_variable.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:mutable_variable.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:mutable_variable.mli.expected} %{dep:mutable_variable.mli.output})))

(rule
 (targets mutex.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:mutex.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:mutex.mli.expected} %{dep:mutex.mli.output})))

(rule
 (targets nativeint.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:nativeint.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:nativeint.mli.expected} %{dep:nativeint.mli.output})))

(rule
 (targets numbers.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:numbers.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:numbers.mli.expected} %{dep:numbers.mli.output})))

(rule
 (targets obj.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:obj.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:obj.mli.expected} %{dep:obj.mli.output})))

(rule
 (targets ocaml_actions.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_actions.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_actions.mli.expected} %{dep:ocaml_actions.mli.output})))

(rule
 (targets ocaml_backends.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_backends.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_backends.mli.expected} %{dep:ocaml_backends.mli.output})))

(rule
 (targets ocaml_commands.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_commands.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_commands.mli.expected} %{dep:ocaml_commands.mli.output})))

(rule
 (targets ocaml_compilers.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_compilers.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_compilers.mli.expected} %{dep:ocaml_compilers.mli.output})))

(rule
 (targets ocaml_directories.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_directories.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_directories.mli.expected} %{dep:ocaml_directories.mli.output})))

(rule
 (targets ocaml_files.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_files.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_files.mli.expected} %{dep:ocaml_files.mli.output})))

(rule
 (targets ocaml_filetypes.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_filetypes.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_filetypes.mli.expected} %{dep:ocaml_filetypes.mli.output})))

(rule
 (targets ocaml_flags.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_flags.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_flags.mli.expected} %{dep:ocaml_flags.mli.output})))

(rule
 (targets ocaml_modifiers.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_modifiers.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_modifiers.mli.expected} %{dep:ocaml_modifiers.mli.output})))

(rule
 (targets ocaml_tests.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_tests.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_tests.mli.expected} %{dep:ocaml_tests.mli.output})))

(rule
 (targets ocaml_tools.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_tools.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_tools.mli.expected} %{dep:ocaml_tools.mli.output})))

(rule
 (targets ocaml_toplevels.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_toplevels.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_toplevels.mli.expected} %{dep:ocaml_toplevels.mli.output})))

(rule
 (targets ocaml_variables.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocaml_variables.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocaml_variables.mli.expected} %{dep:ocaml_variables.mli.output})))

(rule
 (targets ocamltest_config.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocamltest_config.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocamltest_config.mli.expected} %{dep:ocamltest_config.mli.output})))

(rule
 (targets ocamltest_stdlib.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ocamltest_stdlib.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ocamltest_stdlib.mli.expected} %{dep:ocamltest_stdlib.mli.output})))

(rule
 (targets odoc_analyse.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_analyse.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_analyse.mli.expected} %{dep:odoc_analyse.mli.output})))

(rule
 (targets odoc_args.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_args.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_args.mli.expected} %{dep:odoc_args.mli.output})))

(rule
 (targets odoc_ast.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_ast.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_ast.mli.expected} %{dep:odoc_ast.mli.output})))

(rule
 (targets odoc_comments.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_comments.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_comments.mli.expected} %{dep:odoc_comments.mli.output})))

(rule
 (targets odoc_comments_global.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_comments_global.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_comments_global.mli.expected} %{dep:odoc_comments_global.mli.output})))

(rule
 (targets odoc_config.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_config.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_config.mli.expected} %{dep:odoc_config.mli.output})))

(rule
 (targets odoc_cross.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_cross.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_cross.mli.expected} %{dep:odoc_cross.mli.output})))

(rule
 (targets odoc_dag2html.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_dag2html.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_dag2html.mli.expected} %{dep:odoc_dag2html.mli.output})))

(rule
 (targets odoc_env.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_env.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_env.mli.expected} %{dep:odoc_env.mli.output})))

(rule
 (targets odoc_gen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_gen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_gen.mli.expected} %{dep:odoc_gen.mli.output})))

(rule
 (targets odoc_global.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_global.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_global.mli.expected} %{dep:odoc_global.mli.output})))

(rule
 (targets odoc_info.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_info.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_info.mli.expected} %{dep:odoc_info.mli.output})))

(rule
 (targets odoc_merge.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_merge.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_merge.mli.expected} %{dep:odoc_merge.mli.output})))

(rule
 (targets odoc_misc.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_misc.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_misc.mli.expected} %{dep:odoc_misc.mli.output})))

(rule
 (targets odoc_name.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_name.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_name.mli.expected} %{dep:odoc_name.mli.output})))

(rule
 (targets odoc_print.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_print.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_print.mli.expected} %{dep:odoc_print.mli.output})))

(rule
 (targets odoc_search.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_search.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_search.mli.expected} %{dep:odoc_search.mli.output})))

(rule
 (targets odoc_sig.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_sig.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_sig.mli.expected} %{dep:odoc_sig.mli.output})))

(rule
 (targets odoc_str.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_str.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_str.mli.expected} %{dep:odoc_str.mli.output})))

(rule
 (targets odoc_text.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_text.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_text.mli.expected} %{dep:odoc_text.mli.output})))

(rule
 (targets odoc_types.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:odoc_types.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:odoc_types.mli.expected} %{dep:odoc_types.mli.output})))

(rule
 (targets oo.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:oo.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:oo.mli.expected} %{dep:oo.mli.output})))

(rule
 (targets oprint.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:oprint.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:oprint.mli.expected} %{dep:oprint.mli.output})))

(rule
 (targets optcompile.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:optcompile.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:optcompile.mli.expected} %{dep:optcompile.mli.output})))

(rule
 (targets opterrors.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:opterrors.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:opterrors.mli.expected} %{dep:opterrors.mli.output})))

(rule
 (targets option.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:option.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:option.mli.expected} %{dep:option.mli.output})))

(rule
 (targets options.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:options.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:options.mli.expected} %{dep:options.mli.output})))

(rule
 (targets optmain.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:optmain.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:optmain.mli.expected} %{dep:optmain.mli.output})))

(rule
 (targets opttopdirs.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:opttopdirs.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:opttopdirs.mli.expected} %{dep:opttopdirs.mli.output})))

(rule
 (targets opttoploop.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:opttoploop.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:opttoploop.mli.expected} %{dep:opttoploop.mli.output})))

(rule
 (targets opttopmain.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:opttopmain.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:opttopmain.mli.expected} %{dep:opttopmain.mli.output})))

(rule
 (targets outcometree.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:outcometree.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:outcometree.mli.expected} %{dep:outcometree.mli.output})))

(rule
 (targets output.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:output.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:output.mli.expected} %{dep:output.mli.output})))

(rule
 (targets outputbis.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:outputbis.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:outputbis.mli.expected} %{dep:outputbis.mli.output})))

(rule
 (targets parameter.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parameter.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parameter.mli.expected} %{dep:parameter.mli.output})))

(rule
 (targets parameters.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parameters.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parameters.mli.expected} %{dep:parameters.mli.output})))

(rule
 (targets parmatch.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parmatch.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parmatch.mli.expected} %{dep:parmatch.mli.output})))

(rule
 (targets parse.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parse.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parse.mli.expected} %{dep:parse.mli.output})))

(rule
 (targets parser.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parser.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parser.mli.expected} %{dep:parser.mli.output})))

(rule
 (targets parser_aux.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parser_aux.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parser_aux.mli.expected} %{dep:parser_aux.mli.output})))

(rule
 (targets parsetree.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parsetree.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parsetree.mli.expected} %{dep:parsetree.mli.output})))

(rule
 (targets parsing.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:parsing.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:parsing.mli.expected} %{dep:parsing.mli.output})))

(rule
 (targets pass_wrapper.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pass_wrapper.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pass_wrapper.mli.expected} %{dep:pass_wrapper.mli.output})))

(rule
 (targets path.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:path.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:path.mli.expected} %{dep:path.mli.output})))

(rule
 (targets pattern_matching.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pattern_matching.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pattern_matching.mli.expected} %{dep:pattern_matching.mli.output})))

(rule
 (targets pos.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pos.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pos.mli.expected} %{dep:pos.mli.output})))

(rule
 (targets pparse.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pparse.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pparse.mli.expected} %{dep:pparse.mli.output})))

(rule
 (targets pprintast.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:pprintast.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:pprintast.mli.expected} %{dep:pprintast.mli.output})))

(rule
 (targets predef.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:predef.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:predef.mli.expected} %{dep:predef.mli.output})))

(rule
 (targets primitive.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:primitive.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:primitive.mli.expected} %{dep:primitive.mli.output})))

(rule
 (targets primitives.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:primitives.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:primitives.mli.expected} %{dep:primitives.mli.output})))

(rule
 (targets printast.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printast.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printast.mli.expected} %{dep:printast.mli.output})))

(rule
 (targets printclambda.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printclambda.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printclambda.mli.expected} %{dep:printclambda.mli.output})))

(rule
 (targets printcmm.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printcmm.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printcmm.mli.expected} %{dep:printcmm.mli.output})))

(rule
 (targets printexc.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printexc.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printexc.mli.expected} %{dep:printexc.mli.output})))

(rule
 (targets printf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printf.mli.expected} %{dep:printf.mli.output})))

(rule
 (targets printinstr.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printinstr.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printinstr.mli.expected} %{dep:printinstr.mli.output})))

(rule
 (targets printlambda.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printlambda.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printlambda.mli.expected} %{dep:printlambda.mli.output})))

(rule
 (targets printlinear.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printlinear.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printlinear.mli.expected} %{dep:printlinear.mli.output})))

(rule
 (targets printmach.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printmach.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printmach.mli.expected} %{dep:printmach.mli.output})))

(rule
 (targets printpat.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printpat.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printpat.mli.expected} %{dep:printpat.mli.output})))

(rule
 (targets printtyp.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printtyp.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printtyp.mli.expected} %{dep:printtyp.mli.output})))

(rule
 (targets printtyped.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printtyped.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printtyped.mli.expected} %{dep:printtyped.mli.output})))

(rule
 (targets printval.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:printval.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:printval.mli.expected} %{dep:printval.mli.output})))

(rule
 (targets proc.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:proc.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:proc.mli.expected} %{dep:proc.mli.output})))

(rule
 (targets profile.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:profile.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:profile.mli.expected} %{dep:profile.mli.output})))

(rule
 (targets profiling.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:profiling.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:profiling.mli.expected} %{dep:profiling.mli.output})))

(rule
 (targets program_loading.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:program_loading.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:program_loading.mli.expected} %{dep:program_loading.mli.output})))

(rule
 (targets program_management.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:program_management.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:program_management.mli.expected} %{dep:program_management.mli.output})))

(rule
 (targets projection.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:projection.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:projection.mli.expected} %{dep:projection.mli.output})))

(rule
 (targets question.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:question.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:question.mli.expected} %{dep:question.mli.output})))

(rule
 (targets queue.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:queue.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:queue.mli.expected} %{dep:queue.mli.output})))

(rule
 (targets random.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:random.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:random.mli.expected} %{dep:random.mli.output})))

(rule
 (targets raw_spacetime_lib.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:raw_spacetime_lib.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:raw_spacetime_lib.mli.expected} %{dep:raw_spacetime_lib.mli.output})))

(rule
 (targets rec_check.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:rec_check.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:rec_check.mli.expected} %{dep:rec_check.mli.output})))

(rule
 (targets ref_to_variables.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:ref_to_variables.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:ref_to_variables.mli.expected} %{dep:ref_to_variables.mli.output})))

(rule
 (targets reg.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:reg.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:reg.mli.expected} %{dep:reg.mli.output})))

(rule
 (targets reg_availability_set.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:reg_availability_set.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:reg_availability_set.mli.expected} %{dep:reg_availability_set.mli.output})))

(rule
 (targets reg_with_debug_info.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:reg_with_debug_info.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:reg_with_debug_info.mli.expected} %{dep:reg_with_debug_info.mli.output})))

(rule
 (targets reload.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:reload.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:reload.mli.expected} %{dep:reload.mli.output})))

(rule
 (targets reloadgen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:reloadgen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:reloadgen.mli.expected} %{dep:reloadgen.mli.output})))

(rule
 (targets remove_free_vars_equal_to_args.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:remove_free_vars_equal_to_args.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:remove_free_vars_equal_to_args.mli.expected} %{dep:remove_free_vars_equal_to_args.mli.output})))

(rule
 (targets remove_unused_arguments.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:remove_unused_arguments.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:remove_unused_arguments.mli.expected} %{dep:remove_unused_arguments.mli.output})))

(rule
 (targets remove_unused_closure_vars.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:remove_unused_closure_vars.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:remove_unused_closure_vars.mli.expected} %{dep:remove_unused_closure_vars.mli.output})))

(rule
 (targets remove_unused_program_constructs.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:remove_unused_program_constructs.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:remove_unused_program_constructs.mli.expected} %{dep:remove_unused_program_constructs.mli.output})))

(rule
 (targets result.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:result.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:result.mli.expected} %{dep:result.mli.output})))

(rule
 (targets run_command.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:run_command.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:run_command.mli.expected} %{dep:run_command.mli.output})))

(rule
 (targets runtimedef.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:runtimedef.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:runtimedef.mli.expected} %{dep:runtimedef.mli.output})))

(rule
 (targets scanf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:scanf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:scanf.mli.expected} %{dep:scanf.mli.output})))

(rule
 (targets schedgen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:schedgen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:schedgen.mli.expected} %{dep:schedgen.mli.output})))

(rule
 (targets scheduling.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:scheduling.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:scheduling.mli.expected} %{dep:scheduling.mli.output})))

(rule
 (targets selectgen.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:selectgen.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:selectgen.mli.expected} %{dep:selectgen.mli.output})))

(rule
 (targets selection.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:selection.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:selection.mli.expected} %{dep:selection.mli.output})))

(rule
 (targets semantics_of_primitives.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:semantics_of_primitives.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:semantics_of_primitives.mli.expected} %{dep:semantics_of_primitives.mli.output})))

(rule
 (targets seq.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:seq.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:seq.mli.expected} %{dep:seq.mli.output})))

(rule
 (targets set.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:set.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:set.mli.expected} %{dep:set.mli.output})))

(rule
 (targets set_of_closures_id.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:set_of_closures_id.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:set_of_closures_id.mli.expected} %{dep:set_of_closures_id.mli.output})))

(rule
 (targets set_of_closures_origin.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:set_of_closures_origin.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:set_of_closures_origin.mli.expected} %{dep:set_of_closures_origin.mli.output})))

(rule
 (targets share_constants.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:share_constants.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:share_constants.mli.expected} %{dep:share_constants.mli.output})))

(rule
 (targets show_information.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:show_information.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:show_information.mli.expected} %{dep:show_information.mli.output})))

(rule
 (targets show_source.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:show_source.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:show_source.mli.expected} %{dep:show_source.mli.output})))

(rule
 (targets simple_value_approx.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:simple_value_approx.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:simple_value_approx.mli.expected} %{dep:simple_value_approx.mli.output})))

(rule
 (targets simplif.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:simplif.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:simplif.mli.expected} %{dep:simplif.mli.output})))

(rule
 (targets simplify_boxed_integer_ops.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:simplify_boxed_integer_ops.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:simplify_boxed_integer_ops.mli.expected} %{dep:simplify_boxed_integer_ops.mli.output})))

(rule
 (targets simplify_boxed_integer_ops_intf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:simplify_boxed_integer_ops_intf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:simplify_boxed_integer_ops_intf.mli.expected} %{dep:simplify_boxed_integer_ops_intf.mli.output})))

(rule
 (targets simplify_common.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:simplify_common.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:simplify_common.mli.expected} %{dep:simplify_common.mli.output})))

(rule
 (targets simplify_primitives.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:simplify_primitives.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:simplify_primitives.mli.expected} %{dep:simplify_primitives.mli.output})))

(rule
 (targets source.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:source.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:source.mli.expected} %{dep:source.mli.output})))

(rule
 (targets spacetime.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:spacetime.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:spacetime.mli.expected} %{dep:spacetime.mli.output})))

(rule
 (targets spacetime_profiling.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:spacetime_profiling.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:spacetime_profiling.mli.expected} %{dep:spacetime_profiling.mli.output})))

(rule
 (targets spill.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:spill.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:spill.mli.expected} %{dep:spill.mli.output})))

(rule
 (targets split.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:split.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:split.mli.expected} %{dep:split.mli.output})))

(rule
 (targets stack.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:stack.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:stack.mli.expected} %{dep:stack.mli.output})))

(rule
 (targets static_exception.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:static_exception.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:static_exception.mli.expected} %{dep:static_exception.mli.output})))

(rule
 (targets stdLabels.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:stdLabels.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:stdLabels.mli.expected} %{dep:stdLabels.mli.output})))

(rule
 (targets stdlib.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:stdlib.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:stdlib.mli.expected} %{dep:stdlib.mli.output})))

(rule
 (targets str.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:str.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:str.mli.expected} %{dep:str.mli.output})))

(rule
 (targets stream.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:stream.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:stream.mli.expected} %{dep:stream.mli.output})))

(rule
 (targets string.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:string.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:string.mli.expected} %{dep:string.mli.output})))

(rule
 (targets stringLabels.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:stringLabels.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:stringLabels.mli.expected} %{dep:stringLabels.mli.output})))

(rule
 (targets strmatch.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:strmatch.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:strmatch.mli.expected} %{dep:strmatch.mli.output})))

(rule
 (targets strongly_connected_components.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:strongly_connected_components.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:strongly_connected_components.mli.expected} %{dep:strongly_connected_components.mli.output})))

(rule
 (targets stypes.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:stypes.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:stypes.mli.expected} %{dep:stypes.mli.output})))

(rule
 (targets subst.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:subst.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:subst.mli.expected} %{dep:subst.mli.output})))

(rule
 (targets switch.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:switch.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:switch.mli.expected} %{dep:switch.mli.output})))

(rule
 (targets symbol.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:symbol.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:symbol.mli.expected} %{dep:symbol.mli.output})))

(rule
 (targets symbols.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:symbols.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:symbols.mli.expected} %{dep:symbols.mli.output})))

(rule
 (targets symtable.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:symtable.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:symtable.mli.expected} %{dep:symtable.mli.output})))

(rule
 (targets syntax.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:syntax.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:syntax.mli.expected} %{dep:syntax.mli.output})))

(rule
 (targets syntaxerr.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:syntaxerr.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:syntaxerr.mli.expected} %{dep:syntaxerr.mli.output})))

(rule
 (targets sys.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:sys.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:sys.mli.expected} %{dep:sys.mli.output})))

(rule
 (targets table.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:table.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:table.mli.expected} %{dep:table.mli.output})))

(rule
 (targets tag.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:tag.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tag.mli.expected} %{dep:tag.mli.output})))

(rule
 (targets targetint.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:targetint.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:targetint.mli.expected} %{dep:targetint.mli.output})))

(rule
 (targets tast_mapper.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:tast_mapper.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tast_mapper.mli.expected} %{dep:tast_mapper.mli.output})))

(rule
 (targets terminfo.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:terminfo.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:terminfo.mli.expected} %{dep:terminfo.mli.output})))

(rule
 (targets tests.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:tests.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tests.mli.expected} %{dep:tests.mli.output})))

(rule
 (targets thread.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:thread.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:thread.mli.expected} %{dep:thread.mli.output})))

(rule
 (targets threadUnix.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:threadUnix.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:threadUnix.mli.expected} %{dep:threadUnix.mli.output})))

(rule
 (targets time_travel.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:time_travel.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:time_travel.mli.expected} %{dep:time_travel.mli.output})))

(rule
 (targets topdirs.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:topdirs.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:topdirs.mli.expected} %{dep:topdirs.mli.output})))

(rule
 (targets toploop.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:toploop.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:toploop.mli.expected} %{dep:toploop.mli.output})))

(rule
 (targets topmain.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:topmain.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:topmain.mli.expected} %{dep:topmain.mli.output})))

(rule
 (targets trace.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:trace.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:trace.mli.expected} %{dep:trace.mli.output})))

(rule
 (targets translattribute.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:translattribute.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:translattribute.mli.expected} %{dep:translattribute.mli.output})))

(rule
 (targets translclass.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:translclass.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:translclass.mli.expected} %{dep:translclass.mli.output})))

(rule
 (targets translcore.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:translcore.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:translcore.mli.expected} %{dep:translcore.mli.output})))

(rule
 (targets translmod.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:translmod.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:translmod.mli.expected} %{dep:translmod.mli.output})))

(rule
 (targets translobj.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:translobj.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:translobj.mli.expected} %{dep:translobj.mli.output})))

(rule
 (targets translprim.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:translprim.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:translprim.mli.expected} %{dep:translprim.mli.output})))

(rule
 (targets trap_barrier.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:trap_barrier.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:trap_barrier.mli.expected} %{dep:trap_barrier.mli.output})))

(rule
 (targets traverse_for_exported_symbols.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:traverse_for_exported_symbols.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:traverse_for_exported_symbols.mli.expected} %{dep:traverse_for_exported_symbols.mli.output})))

(rule
 (targets tsl_ast.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:tsl_ast.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tsl_ast.mli.expected} %{dep:tsl_ast.mli.output})))

(rule
 (targets tsl_lexer.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:tsl_lexer.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tsl_lexer.mli.expected} %{dep:tsl_lexer.mli.output})))

(rule
 (targets tsl_semantics.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:tsl_semantics.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:tsl_semantics.mli.expected} %{dep:tsl_semantics.mli.output})))

(rule
 (targets typeclass.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typeclass.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typeclass.mli.expected} %{dep:typeclass.mli.output})))

(rule
 (targets typecore.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typecore.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typecore.mli.expected} %{dep:typecore.mli.output})))

(rule
 (targets typedecl.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typedecl.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typedecl.mli.expected} %{dep:typedecl.mli.output})))

(rule
 (targets typedtree.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typedtree.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typedtree.mli.expected} %{dep:typedtree.mli.output})))

(rule
 (targets typedtreeIter.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typedtreeIter.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typedtreeIter.mli.expected} %{dep:typedtreeIter.mli.output})))

(rule
 (targets typedtreeMap.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typedtreeMap.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typedtreeMap.mli.expected} %{dep:typedtreeMap.mli.output})))

(rule
 (targets typemod.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typemod.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typemod.mli.expected} %{dep:typemod.mli.output})))

(rule
 (targets typeopt.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typeopt.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typeopt.mli.expected} %{dep:typeopt.mli.output})))

(rule
 (targets types.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:types.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:types.mli.expected} %{dep:types.mli.output})))

(rule
 (targets typetexp.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:typetexp.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:typetexp.mli.expected} %{dep:typetexp.mli.output})))

(rule
 (targets uchar.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:uchar.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:uchar.mli.expected} %{dep:uchar.mli.output})))

(rule
 (targets un_anf.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:un_anf.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:un_anf.mli.expected} %{dep:un_anf.mli.output})))

(rule
 (targets unbox_closures.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unbox_closures.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unbox_closures.mli.expected} %{dep:unbox_closures.mli.output})))

(rule
 (targets unbox_free_vars_of_closures.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unbox_free_vars_of_closures.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unbox_free_vars_of_closures.mli.expected} %{dep:unbox_free_vars_of_closures.mli.output})))

(rule
 (targets unbox_specialised_args.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unbox_specialised_args.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unbox_specialised_args.mli.expected} %{dep:unbox_specialised_args.mli.output})))

(rule
 (targets unix.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unix.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unix.mli.expected} %{dep:unix.mli.output})))

(rule
 (targets unixLabels.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unixLabels.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unixLabels.mli.expected} %{dep:unixLabels.mli.output})))

(rule
 (targets unix_tools.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:unix_tools.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:unix_tools.mli.expected} %{dep:unix_tools.mli.output})))

(rule
 (targets untypeast.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:untypeast.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:untypeast.mli.expected} %{dep:untypeast.mli.output})))

(rule
 (targets var_within_closure.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:var_within_closure.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:var_within_closure.mli.expected} %{dep:var_within_closure.mli.output})))

(rule
 (targets variable.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:variable.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:variable.mli.expected} %{dep:variable.mli.output})))

(rule
 (targets variables.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:variables.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:variables.mli.expected} %{dep:variables.mli.output})))

(rule
 (targets warnings.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:warnings.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:warnings.mli.expected} %{dep:warnings.mli.output})))

(rule
 (targets weak.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:weak.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:weak.mli.expected} %{dep:weak.mli.output})))

(rule
 (targets x86_ast.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:x86_ast.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:x86_ast.mli.expected} %{dep:x86_ast.mli.output})))

(rule
 (targets x86_dsl.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:x86_dsl.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:x86_dsl.mli.expected} %{dep:x86_dsl.mli.output})))

(rule
 (targets x86_gas.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:x86_gas.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:x86_gas.mli.expected} %{dep:x86_gas.mli.output})))

(rule
 (targets x86_masm.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:x86_masm.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:x86_masm.mli.expected} %{dep:x86_masm.mli.output})))

(rule
 (targets x86_proc.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:x86_proc.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:x86_proc.mli.expected} %{dep:x86_proc.mli.output})))
