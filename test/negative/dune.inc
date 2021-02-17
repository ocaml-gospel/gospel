(rule
 (target char1.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:char1.mli}))))

(rule
 (targets char1.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:char1.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:char1.mli.expected} %{dep:char1.mli.output})))

(rule
 (target t1.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t1.mli}))))

(rule
 (targets t1.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t1.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t1.mli.expected} %{dep:t1.mli.output})))

(rule
 (target t10.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t10.mli}))))

(rule
 (targets t10.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t10.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t10.mli.expected} %{dep:t10.mli.output})))

(rule
 (target t11.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t11.mli}))))

(rule
 (targets t11.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t11.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t11.mli.expected} %{dep:t11.mli.output})))

(rule
 (target t12.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t12.mli}))))

(rule
 (targets t12.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t12.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t12.mli.expected} %{dep:t12.mli.output})))

(rule
 (target t13.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t13.mli}))))

(rule
 (targets t13.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t13.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t13.mli.expected} %{dep:t13.mli.output})))

(rule
 (target t14.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t14.mli}))))

(rule
 (targets t14.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t14.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t14.mli.expected} %{dep:t14.mli.output})))

(rule
 (target t15.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t15.mli}))))

(rule
 (targets t15.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t15.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t15.mli.expected} %{dep:t15.mli.output})))

(rule
 (target t16.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t16.mli}))))

(rule
 (targets t16.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t16.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t16.mli.expected} %{dep:t16.mli.output})))

(rule
 (target t17.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t17.mli}))))

(rule
 (targets t17.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t17.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t17.mli.expected} %{dep:t17.mli.output})))

(rule
 (target t18.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t18.mli}))))

(rule
 (targets t18.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t18.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t18.mli.expected} %{dep:t18.mli.output})))

(rule
 (target t19.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t19.mli}))))

(rule
 (targets t19.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t19.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t19.mli.expected} %{dep:t19.mli.output})))

(rule
 (target t2.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t2.mli}))))

(rule
 (targets t2.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t2.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t2.mli.expected} %{dep:t2.mli.output})))

(rule
 (target t20.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t20.mli}))))

(rule
 (targets t20.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t20.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t20.mli.expected} %{dep:t20.mli.output})))

(rule
 (target t21.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t21.mli}))))

(rule
 (targets t21.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t21.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t21.mli.expected} %{dep:t21.mli.output})))

(rule
 (target t22.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t22.mli}))))

(rule
 (targets t22.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t22.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t22.mli.expected} %{dep:t22.mli.output})))

(rule
 (target t23.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t23.mli}))))

(rule
 (targets t23.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t23.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t23.mli.expected} %{dep:t23.mli.output})))

(rule
 (target t24.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t24.mli}))))

(rule
 (targets t24.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t24.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t24.mli.expected} %{dep:t24.mli.output})))

(rule
 (target t25.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t25.mli}))))

(rule
 (targets t25.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t25.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t25.mli.expected} %{dep:t25.mli.output})))

(rule
 (target t26.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t26.mli}))))

(rule
 (targets t26.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t26.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t26.mli.expected} %{dep:t26.mli.output})))

(rule
 (target t27.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t27.mli}))))

(rule
 (targets t27.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t27.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t27.mli.expected} %{dep:t27.mli.output})))

(rule
 (target t28.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t28.mli}))))

(rule
 (targets t28.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t28.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t28.mli.expected} %{dep:t28.mli.output})))

(rule
 (target t29.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t29.mli}))))

(rule
 (targets t29.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t29.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t29.mli.expected} %{dep:t29.mli.output})))

(rule
 (target t3.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t3.mli}))))

(rule
 (targets t3.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t3.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t3.mli.expected} %{dep:t3.mli.output})))

(rule
 (target t30.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t30.mli}))))

(rule
 (targets t30.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t30.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t30.mli.expected} %{dep:t30.mli.output})))

(rule
 (target t31.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t31.mli}))))

(rule
 (targets t31.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t31.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t31.mli.expected} %{dep:t31.mli.output})))

(rule
 (target t32.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t32.mli}))))

(rule
 (targets t32.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t32.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t32.mli.expected} %{dep:t32.mli.output})))

(rule
 (target t33.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t33.mli}))))

(rule
 (targets t33.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t33.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t33.mli.expected} %{dep:t33.mli.output})))

(rule
 (target t34.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t34.mli}))))

(rule
 (targets t34.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t34.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t34.mli.expected} %{dep:t34.mli.output})))

(rule
 (target t35.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t35.mli}))))

(rule
 (targets t35.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t35.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t35.mli.expected} %{dep:t35.mli.output})))

(rule
 (target t36.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t36.mli}))))

(rule
 (targets t36.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t36.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t36.mli.expected} %{dep:t36.mli.output})))

(rule
 (target t37.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t37.mli}))))

(rule
 (targets t37.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t37.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t37.mli.expected} %{dep:t37.mli.output})))

(rule
 (target t38.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t38.mli}))))

(rule
 (targets t38.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t38.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t38.mli.expected} %{dep:t38.mli.output})))

(rule
 (target t39.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t39.mli}))))

(rule
 (targets t39.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t39.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t39.mli.expected} %{dep:t39.mli.output})))

(rule
 (target t4.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t4.mli}))))

(rule
 (targets t4.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t4.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t4.mli.expected} %{dep:t4.mli.output})))

(rule
 (target t5.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t5.mli}))))

(rule
 (targets t5.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t5.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t5.mli.expected} %{dep:t5.mli.output})))

(rule
 (target t6.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t6.mli}))))

(rule
 (targets t6.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t6.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t6.mli.expected} %{dep:t6.mli.output})))

(rule
 (target t7.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t7.mli}))))

(rule
 (targets t7.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t7.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t7.mli.expected} %{dep:t7.mli.output})))

(rule
 (target t8.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t8.mli}))))

(rule
 (targets t8.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t8.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t8.mli.expected} %{dep:t8.mli.output})))

(rule
 (target t9.mli.pp)
 (action
  (with-outputs-to %{target}
     (run gospel_pps %{dep:t9.mli}))))

(rule
 (targets t9.mli.output)
 (action
   (with-outputs-to %{targets}
      (with-accepted-exit-codes
       (or :standard 125)
       (system "%{bin:gospel} tc --print-intermediate %{dep:t9.mli.pp}")))))

(rule
 (alias runtest)
 (action (diff %{dep:t9.mli.expected} %{dep:t9.mli.output})))

