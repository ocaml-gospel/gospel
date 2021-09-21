(rule
 (targets char1.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:char1.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:char1.mli.expected} %{dep:char1.mli.output})))

(rule
 (targets constructor_arity1.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:constructor_arity1.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:constructor_arity1.mli.expected} %{dep:constructor_arity1.mli.output})))

(rule
 (targets constructor_arity2.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:constructor_arity2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:constructor_arity2.mli.expected} %{dep:constructor_arity2.mli.output})))

(rule
 (targets exn_arity.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:exn_arity.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:exn_arity.mli.expected} %{dep:exn_arity.mli.output})))

(rule
 (targets t1.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t1.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t1.mli.expected} %{dep:t1.mli.output})))

(rule
 (targets t10.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t10.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t10.mli.expected} %{dep:t10.mli.output})))

(rule
 (targets t11.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t11.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t11.mli.expected} %{dep:t11.mli.output})))

(rule
 (targets t12.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t12.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t12.mli.expected} %{dep:t12.mli.output})))

(rule
 (targets t13.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t13.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t13.mli.expected} %{dep:t13.mli.output})))

(rule
 (targets t14.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t14.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t14.mli.expected} %{dep:t14.mli.output})))

(rule
 (targets t15.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t15.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t15.mli.expected} %{dep:t15.mli.output})))

(rule
 (targets t16.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t16.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t16.mli.expected} %{dep:t16.mli.output})))

(rule
 (targets t17.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t17.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t17.mli.expected} %{dep:t17.mli.output})))

(rule
 (targets t18.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t18.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t18.mli.expected} %{dep:t18.mli.output})))

(rule
 (targets t19.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t19.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t19.mli.expected} %{dep:t19.mli.output})))

(rule
 (targets t2.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t2.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t2.mli.expected} %{dep:t2.mli.output})))

(rule
 (targets t20.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t20.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t20.mli.expected} %{dep:t20.mli.output})))

(rule
 (targets t21.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t21.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t21.mli.expected} %{dep:t21.mli.output})))

(rule
 (targets t22.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t22.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t22.mli.expected} %{dep:t22.mli.output})))

(rule
 (targets t23.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t23.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t23.mli.expected} %{dep:t23.mli.output})))

(rule
 (targets t24.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t24.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t24.mli.expected} %{dep:t24.mli.output})))

(rule
 (targets t25.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t25.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t25.mli.expected} %{dep:t25.mli.output})))

(rule
 (targets t26.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t26.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t26.mli.expected} %{dep:t26.mli.output})))

(rule
 (targets t27.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t27.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t27.mli.expected} %{dep:t27.mli.output})))

(rule
 (targets t28.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t28.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t28.mli.expected} %{dep:t28.mli.output})))

(rule
 (targets t29.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t29.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t29.mli.expected} %{dep:t29.mli.output})))

(rule
 (targets t3.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t3.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t3.mli.expected} %{dep:t3.mli.output})))

(rule
 (targets t30.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t30.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t30.mli.expected} %{dep:t30.mli.output})))

(rule
 (targets t31.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t31.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t31.mli.expected} %{dep:t31.mli.output})))

(rule
 (targets t32.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t32.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t32.mli.expected} %{dep:t32.mli.output})))

(rule
 (targets t33.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t33.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t33.mli.expected} %{dep:t33.mli.output})))

(rule
 (targets t34.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t34.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t34.mli.expected} %{dep:t34.mli.output})))

(rule
 (targets t35.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t35.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t35.mli.expected} %{dep:t35.mli.output})))

(rule
 (targets t36.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t36.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t36.mli.expected} %{dep:t36.mli.output})))

(rule
 (targets t37.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t37.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t37.mli.expected} %{dep:t37.mli.output})))

(rule
 (targets t38.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t38.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t38.mli.expected} %{dep:t38.mli.output})))

(rule
 (targets t39.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t39.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t39.mli.expected} %{dep:t39.mli.output})))

(rule
 (targets t4.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t4.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t4.mli.expected} %{dep:t4.mli.output})))

(rule
 (targets t5.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t5.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t5.mli.expected} %{dep:t5.mli.output})))

(rule
 (targets t6.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t6.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t6.mli.expected} %{dep:t6.mli.output})))

(rule
 (targets t7.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t7.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t7.mli.expected} %{dep:t7.mli.output})))

(rule
 (targets t8.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t8.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t8.mli.expected} %{dep:t8.mli.output})))

(rule
 (targets t9.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} check --verbose %{dep:t9.mli}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t9.mli.expected} %{dep:t9.mli.output})))

