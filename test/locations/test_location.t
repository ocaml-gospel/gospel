Testing the location displayed by `gospel dumpast`, that is the location
available in the typed ast, *e.g* available to tools using `gospel`.


First, create a test artifact:

  $ cat > foo.mli << EOF
  > (* beginning of the module *)
  > 
  > type 'a t
  > (** documentation *)
  > (*@ mutable model contents : 'a sequence
  >     model size : integer *)
  > 
  > val create : int -> 'a t
  > (*@ t = create i
  >     checks i.v >= 0
  >     ensures t.contents = Sequence.empty
  >     ensures t.size = i.v *)
  > 
  > (* comments *)
  > 
  > (*@ axiom a : true *)
  > 
  > (*@ function is_full (xs : 'a sequence) (x : integer) : bool = Sequence.length xs = x *)
  > 
  > (*@ function with_spec (x : integer) (xs : integer sequence) : integer sequence *)
  > (*@ requires not (Sequence.mem x xs)
  >     ensures true *)
  > 
  > (*@ predicate rec is_sorted_sequence (l: integer sequence) =
  >        Sequence.length l < 2 \/ (l[0] <= l[1] /\ is_sorted_sequence l[1 ..]) *)
  > 
  > val add : 'a -> 'a t -> unit
  > (*@ add a t
  >     modifies t.contents
  >     (* comments *)
  >     ensures t.contents = if is_full t.contents t.size
  >                          then old t.contents
  >                          else Sequence.cons a (old t.contents) *)
  > EOF
  $ gospel dumpast foo.mli
  [{ Tast.sig_desc =
     (Tast.Sig_open (
        { Tast.opn_id = ["Stdlib"]; opn_override = <Asttypes.override_flag>;
          opn_loc = none; opn_attrs = <attributes> },
        Tast.Ghost));
     sig_loc = none };
    { Tast.sig_desc =
      (Tast.Sig_open (
         { Tast.opn_id = ["Gospelstdlib"];
           opn_override = <Asttypes.override_flag>; opn_loc = none;
           opn_attrs = <attributes> },
         Tast.Ghost));
      sig_loc = none };
    { Tast.sig_desc =
      (Tast.Sig_open (
         { Tast.opn_id = ["Annotated_stdlib"];
           opn_override = <Asttypes.override_flag>; opn_loc = none;
           opn_attrs = <attributes> },
         Tast.Ghost));
      sig_loc = none };
    { Tast.sig_desc =
      (Tast.Sig_type (Tast.Recursive,
         [{ Tast.td_ts =
            { Ttypes.ts_ident = t; ts_args = [{ Ttypes.tv_name = a }];
              ts_alias = None };
            td_params = ({ Ttypes.tv_name = a }, (<variance>, <injectivity>));
            td_cstrs = ; td_kind = Tast.Pty_abstract; td_private = Tast.Public;
            td_manifest = None; td_attrs = <attributes>;
            td_spec =
            (Some { Tast.ty_ephemeral = true;
                    ty_fields =
                    [(Symbols.Field_symbol {ls_name = contents;
                        ls_args =
                        [{ Ttypes.ty_node =
                           (Ttypes.Tyapp (
                              { Ttypes.ts_ident = t;
                                ts_args = [{ Ttypes.tv_name = a }];
                                ts_alias = None },
                              [{ Ttypes.ty_node =
                                 (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                ]
                              ))
                           }
                          ];
                        ls_value =
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = sequence;
                               ts_args = [{ Ttypes.tv_name = a }];
                               ts_alias = None },
                             [{ Ttypes.ty_node =
                                (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                               ]
                             ))
                          }},
                      true);
                      (Symbols.Field_symbol {ls_name = size;
                         ls_args =
                         [{ Ttypes.ty_node =
                            (Ttypes.Tyapp (
                               { Ttypes.ts_ident = t;
                                 ts_args = [{ Ttypes.tv_name = a }];
                                 ts_alias = None },
                               [{ Ttypes.ty_node =
                                  (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                 ]
                               ))
                            }
                           ];
                         ls_value =
                         { Ttypes.ty_node =
                           (Ttypes.Tyapp (
                              { Ttypes.ts_ident = integer; ts_args = [];
                                ts_alias = None },
                              []))
                           }},
                       false)
                      ];
                    ty_invariants = None;
                    ty_text =
                    " mutable model contents : 'a sequence\n    model size : integer ";
                    ty_loc = foo.mli:5:3 });
            td_loc = foo.mli:3:0 }
           ],
         Tast.Nonghost));
      sig_loc = foo.mli:3:0 };
    { Tast.sig_desc =
      (Tast.Sig_val (
         { Tast.vd_name = Foo.create; vd_type = int -> 'a t; vd_prim = [];
           vd_attrs = <attributes>;
           vd_args =
           [(Tast.Lnone
               { Symbols.vs_name = i;
                 vs_ty =
                 { Ttypes.ty_node =
                   (Ttypes.Tyapp (
                      { Ttypes.ts_ident = int; ts_args = []; ts_alias = None },
                      []))
                   }
                 })
             ];
           vd_ret =
           [(Tast.Lnone
               { Symbols.vs_name = t_1;
                 vs_ty =
                 { Ttypes.ty_node =
                   (Ttypes.Tyapp (
                      { Ttypes.ts_ident = t;
                        ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                      [{ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a })
                         }
                        ]
                      ))
                   }
                 })
             ];
           vd_spec =
           (Some { Tast.sp_args =
                   [(Tast.Lnone
                       { Symbols.vs_name = i;
                         vs_ty =
                         { Ttypes.ty_node =
                           (Ttypes.Tyapp (
                              { Ttypes.ts_ident = int; ts_args = [];
                                ts_alias = None },
                              []))
                           }
                         })
                     ];
                   sp_ret =
                   [(Tast.Lnone
                       { Symbols.vs_name = t_1;
                         vs_ty =
                         { Ttypes.ty_node =
                           (Ttypes.Tyapp (
                              { Ttypes.ts_ident = t;
                                ts_args = [{ Ttypes.tv_name = a }];
                                ts_alias = None },
                              [{ Ttypes.ty_node =
                                 (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                ]
                              ))
                           }
                         })
                     ];
                   sp_pre = [];
                   sp_checks =
                   [{ Tterm.t_node =
                      (Tterm.Tapp (
                         Symbols.Function_symbol {
                           ls_name = Gospelstdlib.infix >=;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = integer; ts_args = [];
                                   ts_alias = None },
                                 []))
                              };
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = integer; ts_args = [];
                                    ts_alias = None },
                                  []))
                               }
                             ];
                           ls_value =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = bool; ts_args = [];
                                  ts_alias = None },
                                []))
                             }},
                         [{ Tterm.t_node =
                            (Tterm.Tfield (
                               { Tterm.t_node =
                                 (Tterm.Tvar
                                    { Symbols.vs_name = i;
                                      vs_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = int;
                                             ts_args = []; ts_alias = None },
                                           []))
                                        }
                                      });
                                 t_ty =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = int; ts_args = [];
                                        ts_alias = None },
                                      []))
                                   };
                                 t_attrs = []; t_loc = foo.mli:10:11 },
                               Symbols.Field_symbol {ls_name = v;
                                 ls_args =
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = int; ts_args = [];
                                         ts_alias = None },
                                       []))
                                    }
                                   ];
                                 ls_value =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = integer;
                                        ts_args = []; ts_alias = None },
                                      []))
                                   }}
                               ));
                            t_ty =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = integer; ts_args = [];
                                   ts_alias = None },
                                 []))
                              };
                            t_attrs = []; t_loc = foo.mli:10:11 };
                           { Tterm.t_node = <constant>;
                             t_ty =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = integer; ts_args = [];
                                    ts_alias = None },
                                  []))
                               };
                             t_attrs = []; t_loc = foo.mli:10:18 }
                           ]
                         ));
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = bool; ts_args = [];
                             ts_alias = None },
                           []))
                        };
                      t_attrs = []; t_loc = foo.mli:10:11 }
                     ];
                   sp_post =
                   [{ Tterm.t_node =
                      (Tterm.Tapp (
                         Symbols.Function_symbol {ls_name = infix =;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) };
                             { Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) }
                             ];
                           ls_value =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = bool; ts_args = [];
                                  ts_alias = None },
                                []))
                             }},
                         [{ Tterm.t_node =
                            (Tterm.Tfield (
                               { Tterm.t_node =
                                 (Tterm.Tvar
                                    { Symbols.vs_name = t_1;
                                      vs_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = t;
                                             ts_args = [{ Ttypes.tv_name = a }];
                                             ts_alias = None },
                                           [{ Ttypes.ty_node =
                                              (Ttypes.Tyvar
                                                 { Ttypes.tv_name = a })
                                              }
                                             ]
                                           ))
                                        }
                                      });
                                 t_ty =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = t;
                                        ts_args = [{ Ttypes.tv_name = a }];
                                        ts_alias = None },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
                                         }
                                        ]
                                      ))
                                   };
                                 t_attrs = []; t_loc = foo.mli:11:12 },
                               Symbols.Field_symbol {ls_name = contents;
                                 ls_args =
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = t;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    }
                                   ];
                                 ls_value =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = sequence;
                                        ts_args = [{ Ttypes.tv_name = a }];
                                        ts_alias = None },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
                                         }
                                        ]
                                      ))
                                   }}
                               ));
                            t_ty =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = sequence;
                                   ts_args = [{ Ttypes.tv_name = a }];
                                   ts_alias = None },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              };
                            t_attrs = []; t_loc = foo.mli:11:12 };
                           { Tterm.t_node =
                             (Tterm.Tapp (
                                Symbols.Function_symbol {
                                  ls_name = Gospelstdlib.Sequence.empty;
                                  ls_args = [];
                                  ls_value =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = sequence;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    }},
                                []));
                             t_ty =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = sequence;
                                    ts_args = [{ Ttypes.tv_name = a }];
                                    ts_alias = None },
                                  [{ Ttypes.ty_node =
                                     (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                    ]
                                  ))
                               };
                             t_attrs = []; t_loc = foo.mli:11:25 }
                           ]
                         ));
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = bool; ts_args = [];
                             ts_alias = None },
                           []))
                        };
                      t_attrs = []; t_loc = foo.mli:11:12 };
                     { Tterm.t_node =
                       (Tterm.Tapp (
                          Symbols.Function_symbol {ls_name = infix =;
                            ls_args =
                            [{ Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) };
                              { Ttypes.ty_node =
                                (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) }
                              ];
                            ls_value =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = bool; ts_args = [];
                                   ts_alias = None },
                                 []))
                              }},
                          [{ Tterm.t_node =
                             (Tterm.Tfield (
                                { Tterm.t_node =
                                  (Tterm.Tvar
                                     { Symbols.vs_name = t_1;
                                       vs_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = t;
                                              ts_args =
                                              [{ Ttypes.tv_name = a }];
                                              ts_alias = None },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                              ]
                                            ))
                                         }
                                       });
                                  t_ty =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = t;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    };
                                  t_attrs = []; t_loc = foo.mli:12:12 },
                                Symbols.Field_symbol {ls_name = size;
                                  ls_args =
                                  [{ Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = t;
                                          ts_args = [{ Ttypes.tv_name = a }];
                                          ts_alias = None },
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyvar { Ttypes.tv_name = a })
                                           }
                                          ]
                                        ))
                                     }
                                    ];
                                  ls_value =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = integer;
                                         ts_args = []; ts_alias = None },
                                       []))
                                    }}
                                ));
                             t_ty =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = integer; ts_args = [];
                                    ts_alias = None },
                                  []))
                               };
                             t_attrs = []; t_loc = foo.mli:12:12 };
                            { Tterm.t_node =
                              (Tterm.Tfield (
                                 { Tterm.t_node =
                                   (Tterm.Tvar
                                      { Symbols.vs_name = i;
                                        vs_ty =
                                        { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = int;
                                               ts_args = []; ts_alias = None },
                                             []))
                                          }
                                        });
                                   t_ty =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = int; ts_args = [];
                                          ts_alias = None },
                                        []))
                                     };
                                   t_attrs = []; t_loc = foo.mli:12:21 },
                                 Symbols.Field_symbol {ls_name = v;
                                   ls_args =
                                   [{ Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = int; ts_args = [];
                                           ts_alias = None },
                                         []))
                                      }
                                     ];
                                   ls_value =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = integer;
                                          ts_args = []; ts_alias = None },
                                        []))
                                     }}
                                 ));
                              t_ty =
                              { Ttypes.ty_node =
                                (Ttypes.Tyapp (
                                   { Ttypes.ts_ident = integer; ts_args = [];
                                     ts_alias = None },
                                   []))
                                };
                              t_attrs = []; t_loc = foo.mli:12:21 }
                            ]
                          ));
                       t_ty =
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = bool; ts_args = [];
                              ts_alias = None },
                            []))
                         };
                       t_attrs = []; t_loc = foo.mli:12:12 }
                     ];
                   sp_xpost = []; sp_wr = []; sp_cs = []; sp_diverge = false;
                   sp_pure = false; sp_equiv = [];
                   sp_text =
                   " t = create i\n    checks i.v >= 0\n    ensures t.contents = Sequence.empty\n    ensures t.size = i.v ";
                   sp_loc = foo.mli:9:3 });
           vd_loc = foo.mli:8:0 },
         Tast.Nonghost));
      sig_loc = foo.mli:8:0 };
    { Tast.sig_desc =
      (Tast.Sig_axiom
         { Tast.ax_name = Foo.a_2;
           ax_term =
           { Tterm.t_node = Tterm.Ttrue;
             t_ty =
             { Ttypes.ty_node =
               (Ttypes.Tyapp (
                  { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None }, 
                  []))
               };
             t_attrs = []; t_loc = foo.mli:16:14 };
           ax_loc = foo.mli:16:3; ax_text = " axiom a : true " });
      sig_loc = foo.mli:16:0 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           Symbols.Function_symbol {ls_name = Foo.is_full;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = sequence;
                     ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                   [{ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) }]
                   ))
                };
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None
                      },
                    []))
                 }
               ];
             ls_value =
             { Ttypes.ty_node =
               (Ttypes.Tyapp (
                  { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None }, 
                  []))
               }};
           fun_rec = false;
           fun_params =
           [{ Symbols.vs_name = xs;
              vs_ty =
              { Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = sequence;
                     ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                   [{ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) }]
                   ))
                }
              };
             { Symbols.vs_name = x;
               vs_ty =
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None
                      },
                    []))
                 }
               }
             ];
           fun_def =
           (Some { Tterm.t_node =
                   (Tterm.Tapp (
                      Symbols.Function_symbol {ls_name = infix =;
                        ls_args =
                        [{ Ttypes.ty_node =
                           (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) };
                          { Ttypes.ty_node =
                            (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) }
                          ];
                        ls_value =
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = bool; ts_args = [];
                               ts_alias = None },
                             []))
                          }},
                      [{ Tterm.t_node =
                         (Tterm.Tapp (
                            Symbols.Function_symbol {
                              ls_name = Gospelstdlib.Sequence.length;
                              ls_args =
                              [{ Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = sequence;
                                      ts_args = [{ Ttypes.tv_name = a }];
                                      ts_alias = None },
                                    [{ Ttypes.ty_node =
                                       (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                      ]
                                    ))
                                 }
                                ];
                              ls_value =
                              { Ttypes.ty_node =
                                (Ttypes.Tyapp (
                                   { Ttypes.ts_ident = integer; ts_args = [];
                                     ts_alias = None },
                                   []))
                                }},
                            [{ Tterm.t_node =
                               (Tterm.Tvar
                                  { Symbols.vs_name = xs;
                                    vs_ty =
                                    { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = sequence;
                                           ts_args = [{ Ttypes.tv_name = a }];
                                           ts_alias = None },
                                         [{ Ttypes.ty_node =
                                            (Ttypes.Tyvar
                                               { Ttypes.tv_name = a })
                                            }
                                           ]
                                         ))
                                      }
                                    });
                               t_ty =
                               { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = sequence;
                                      ts_args = [{ Ttypes.tv_name = a }];
                                      ts_alias = None },
                                    [{ Ttypes.ty_node =
                                       (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                      ]
                                    ))
                                 };
                               t_attrs = []; t_loc = foo.mli:18:79 }
                              ]
                            ));
                         t_ty =
                         { Ttypes.ty_node =
                           (Ttypes.Tyapp (
                              { Ttypes.ts_ident = integer; ts_args = [];
                                ts_alias = None },
                              []))
                           };
                         t_attrs = []; t_loc = foo.mli:18:63 };
                        { Tterm.t_node =
                          (Tterm.Tvar
                             { Symbols.vs_name = x;
                               vs_ty =
                               { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = integer; ts_args = [];
                                      ts_alias = None },
                                    []))
                                 }
                               });
                          t_ty =
                          { Ttypes.ty_node =
                            (Ttypes.Tyapp (
                               { Ttypes.ts_ident = integer; ts_args = [];
                                 ts_alias = None },
                               []))
                            };
                          t_attrs = []; t_loc = foo.mli:18:84 }
                        ]
                      ));
                   t_ty =
                   { Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None
                          },
                        []))
                     };
                   t_attrs = []; t_loc = foo.mli:18:63 });
           fun_spec = None;
           fun_text =
           " function is_full (xs : 'a sequence) (x : integer) : bool = Sequence.length xs = x ";
           fun_loc = foo.mli:18:4 });
      sig_loc = foo.mli:18:4 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           Symbols.Function_symbol {ls_name = Foo.with_spec;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None },
                   []))
                };
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = sequence;
                      ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                    [{ Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = integer; ts_args = [];
                            ts_alias = None },
                          []))
                       }
                      ]
                    ))
                 }
               ];
             ls_value =
             { Ttypes.ty_node =
               (Ttypes.Tyapp (
                  { Ttypes.ts_ident = sequence;
                    ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                  [{ Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = integer; ts_args = [];
                          ts_alias = None },
                        []))
                     }
                    ]
                  ))
               }};
           fun_rec = false;
           fun_params =
           [{ Symbols.vs_name = x_1;
              vs_ty =
              { Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None },
                   []))
                }
              };
             { Symbols.vs_name = xs_1;
               vs_ty =
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = sequence;
                      ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                    [{ Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = integer; ts_args = [];
                            ts_alias = None },
                          []))
                       }
                      ]
                    ))
                 }
               }
             ];
           fun_def = None;
           fun_spec =
           (Some { Tast.fun_req =
                   [{ Tterm.t_node =
                      (Tterm.Tnot
                         { Tterm.t_node =
                           (Tterm.Tapp (
                              Symbols.Function_symbol {
                                ls_name = Gospelstdlib.Sequence.mem;
                                ls_args =
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyvar { Ttypes.tv_name = a }) };
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = sequence;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    }
                                  ];
                                ls_value =
                                { Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = bool; ts_args = [];
                                       ts_alias = None },
                                     []))
                                  }},
                              [{ Tterm.t_node =
                                 (Tterm.Tvar
                                    { Symbols.vs_name = x_1;
                                      vs_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = integer;
                                             ts_args = []; ts_alias = None },
                                           []))
                                        }
                                      });
                                 t_ty =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = integer;
                                        ts_args = []; ts_alias = None },
                                      []))
                                   };
                                 t_attrs = []; t_loc = foo.mli:21:31 };
                                { Tterm.t_node =
                                  (Tterm.Tvar
                                     { Symbols.vs_name = xs_1;
                                       vs_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = sequence;
                                              ts_args =
                                              [{ Ttypes.tv_name = a }];
                                              ts_alias = None },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = integer;
                                                    ts_args = [];
                                                    ts_alias = None },
                                                  []))
                                               }
                                              ]
                                            ))
                                         }
                                       });
                                  t_ty =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = sequence;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = integer;
                                               ts_args = []; ts_alias = None },
                                             []))
                                          }
                                         ]
                                       ))
                                    };
                                  t_attrs = []; t_loc = foo.mli:21:33 }
                                ]
                              ));
                           t_ty =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = bool; ts_args = [];
                                  ts_alias = None },
                                []))
                             };
                           t_attrs = []; t_loc = foo.mli:21:17 });
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = bool; ts_args = [];
                             ts_alias = None },
                           []))
                        };
                      t_attrs = []; t_loc = foo.mli:21:13 }
                     ];
                   fun_ens =
                   [{ Tterm.t_node = Tterm.Ttrue;
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = bool; ts_args = [];
                             ts_alias = None },
                           []))
                        };
                      t_attrs = []; t_loc = foo.mli:22:12 }
                     ];
                   fun_variant = []; fun_coer = false;
                   fun_text =
                   " requires not (Sequence.mem x xs)\n    ensures true ";
                   fun_loc = foo.mli:20:3 });
           fun_text =
           " function with_spec (x : integer) (xs : integer sequence) : integer sequence ";
           fun_loc = foo.mli:20:4 });
      sig_loc = foo.mli:20:4 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           Symbols.Function_symbol {ls_name = Foo.is_sorted_sequence;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = sequence;
                     ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                   [{ Ttypes.ty_node =
                      (Ttypes.Tyapp (
                         { Ttypes.ts_ident = integer; ts_args = [];
                           ts_alias = None },
                         []))
                      }
                     ]
                   ))
                }
               ];
             ls_value =
             { Ttypes.ty_node =
               (Ttypes.Tyapp (
                  { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None }, 
                  []))
               }};
           fun_rec = true;
           fun_params =
           [{ Symbols.vs_name = l;
              vs_ty =
              { Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = sequence;
                     ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                   [{ Ttypes.ty_node =
                      (Ttypes.Tyapp (
                         { Ttypes.ts_ident = integer; ts_args = [];
                           ts_alias = None },
                         []))
                      }
                     ]
                   ))
                }
              }
             ];
           fun_def =
           (Some { Tterm.t_node =
                   (Tterm.Tbinop (Tterm.Tor,
                      { Tterm.t_node =
                        (Tterm.Tapp (
                           Symbols.Function_symbol {
                             ls_name = Gospelstdlib.infix <;
                             ls_args =
                             [{ Ttypes.ty_node =
                                (Ttypes.Tyapp (
                                   { Ttypes.ts_ident = integer; ts_args = [];
                                     ts_alias = None },
                                   []))
                                };
                               { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = integer; ts_args = [];
                                      ts_alias = None },
                                    []))
                                 }
                               ];
                             ls_value =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = bool; ts_args = [];
                                    ts_alias = None },
                                  []))
                               }},
                           [{ Tterm.t_node =
                              (Tterm.Tapp (
                                 Symbols.Function_symbol {
                                   ls_name = Gospelstdlib.Sequence.length;
                                   ls_args =
                                   [{ Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = sequence;
                                           ts_args = [{ Ttypes.tv_name = a }];
                                           ts_alias = None },
                                         [{ Ttypes.ty_node =
                                            (Ttypes.Tyvar
                                               { Ttypes.tv_name = a })
                                            }
                                           ]
                                         ))
                                      }
                                     ];
                                   ls_value =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = integer;
                                          ts_args = []; ts_alias = None },
                                        []))
                                     }},
                                 [{ Tterm.t_node =
                                    (Tterm.Tvar
                                       { Symbols.vs_name = l;
                                         vs_ty =
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = sequence;
                                                ts_args =
                                                [{ Ttypes.tv_name = a }];
                                                ts_alias = None },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyapp (
                                                    { Ttypes.ts_ident = integer;
                                                      ts_args = [];
                                                      ts_alias = None },
                                                    []))
                                                 }
                                                ]
                                              ))
                                           }
                                         });
                                    t_ty =
                                    { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = sequence;
                                           ts_args = [{ Ttypes.tv_name = a }];
                                           ts_alias = None },
                                         [{ Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = integer;
                                                 ts_args = []; ts_alias = None
                                                 },
                                               []))
                                            }
                                           ]
                                         ))
                                      };
                                    t_attrs = []; t_loc = foo.mli:25:23 }
                                   ]
                                 ));
                              t_ty =
                              { Ttypes.ty_node =
                                (Ttypes.Tyapp (
                                   { Ttypes.ts_ident = integer; ts_args = [];
                                     ts_alias = None },
                                   []))
                                };
                              t_attrs = []; t_loc = foo.mli:25:7 };
                             { Tterm.t_node = <constant>;
                               t_ty =
                               { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = integer; ts_args = [];
                                      ts_alias = None },
                                    []))
                                 };
                               t_attrs = []; t_loc = foo.mli:25:27 }
                             ]
                           ));
                        t_ty =
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = bool; ts_args = [];
                               ts_alias = None },
                             []))
                          };
                        t_attrs = []; t_loc = foo.mli:25:7 },
                      { Tterm.t_node =
                        (Tterm.Tbinop (Tterm.Tand,
                           { Tterm.t_node =
                             (Tterm.Tapp (
                                Symbols.Function_symbol {
                                  ls_name = Gospelstdlib.infix <=;
                                  ls_args =
                                  [{ Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = integer;
                                          ts_args = []; ts_alias = None },
                                        []))
                                     };
                                    { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = integer;
                                           ts_args = []; ts_alias = None },
                                         []))
                                      }
                                    ];
                                  ls_value =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = bool; ts_args = [];
                                         ts_alias = None },
                                       []))
                                    }},
                                [{ Tterm.t_node =
                                   (Tterm.Tapp (
                                      Symbols.Function_symbol {
                                        ls_name = Gospelstdlib.mixfix [_];
                                        ls_args =
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = sequence;
                                                ts_args =
                                                [{ Ttypes.tv_name = a }];
                                                ts_alias = None },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyvar
                                                    { Ttypes.tv_name = a })
                                                 }
                                                ]
                                              ))
                                           };
                                          { Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = integer;
                                                 ts_args = []; ts_alias = None
                                                 },
                                               []))
                                            }
                                          ];
                                        ls_value =
                                        { Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }},
                                      [{ Tterm.t_node =
                                         (Tterm.Tvar
                                            { Symbols.vs_name = l;
                                              vs_ty =
                                              { Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = sequence;
                                                     ts_args =
                                                     [{ Ttypes.tv_name = a }];
                                                     ts_alias = None },
                                                   [{ Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident =
                                                           integer;
                                                           ts_args = [];
                                                           ts_alias = None },
                                                         []))
                                                      }
                                                     ]
                                                   ))
                                                }
                                              });
                                         t_ty =
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = sequence;
                                                ts_args =
                                                [{ Ttypes.tv_name = a }];
                                                ts_alias = None },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyapp (
                                                    { Ttypes.ts_ident = integer;
                                                      ts_args = [];
                                                      ts_alias = None },
                                                    []))
                                                 }
                                                ]
                                              ))
                                           };
                                         t_attrs = []; t_loc = foo.mli:25:33 };
                                        { Tterm.t_node = <constant>;
                                          t_ty =
                                          { Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = integer;
                                                 ts_args = []; ts_alias = None
                                                 },
                                               []))
                                            };
                                          t_attrs = []; t_loc = foo.mli:25:35 }
                                        ]
                                      ));
                                   t_ty =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = integer;
                                          ts_args = []; ts_alias = None },
                                        []))
                                     };
                                   t_attrs = []; t_loc = foo.mli:25:34 };
                                  { Tterm.t_node =
                                    (Tterm.Tapp (
                                       Symbols.Function_symbol {
                                         ls_name = Gospelstdlib.mixfix [_];
                                         ls_args =
                                         [{ Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = sequence;
                                                 ts_args =
                                                 [{ Ttypes.tv_name = a }];
                                                 ts_alias = None },
                                               [{ Ttypes.ty_node =
                                                  (Ttypes.Tyvar
                                                     { Ttypes.tv_name = a })
                                                  }
                                                 ]
                                               ))
                                            };
                                           { Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = integer;
                                                  ts_args = []; ts_alias = None
                                                  },
                                                []))
                                             }
                                           ];
                                         ls_value =
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyvar { Ttypes.tv_name = a })
                                           }},
                                       [{ Tterm.t_node =
                                          (Tterm.Tvar
                                             { Symbols.vs_name = l;
                                               vs_ty =
                                               { Ttypes.ty_node =
                                                 (Ttypes.Tyapp (
                                                    { Ttypes.ts_ident =
                                                      sequence;
                                                      ts_args =
                                                      [{ Ttypes.tv_name = a }];
                                                      ts_alias = None },
                                                    [{ Ttypes.ty_node =
                                                       (Ttypes.Tyapp (
                                                          { Ttypes.ts_ident =
                                                            integer;
                                                            ts_args = [];
                                                            ts_alias = None },
                                                          []))
                                                       }
                                                      ]
                                                    ))
                                                 }
                                               });
                                          t_ty =
                                          { Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = sequence;
                                                 ts_args =
                                                 [{ Ttypes.tv_name = a }];
                                                 ts_alias = None },
                                               [{ Ttypes.ty_node =
                                                  (Ttypes.Tyapp (
                                                     { Ttypes.ts_ident =
                                                       integer; ts_args = [];
                                                       ts_alias = None },
                                                     []))
                                                  }
                                                 ]
                                               ))
                                            };
                                          t_attrs = []; t_loc = foo.mli:25:41 };
                                         { Tterm.t_node = <constant>;
                                           t_ty =
                                           { Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = integer;
                                                  ts_args = []; ts_alias = None
                                                  },
                                                []))
                                             };
                                           t_attrs = []; t_loc = foo.mli:25:43
                                           }
                                         ]
                                       ));
                                    t_ty =
                                    { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = integer;
                                           ts_args = []; ts_alias = None },
                                         []))
                                      };
                                    t_attrs = []; t_loc = foo.mli:25:42 }
                                  ]
                                ));
                             t_ty =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = bool; ts_args = [];
                                    ts_alias = None },
                                  []))
                               };
                             t_attrs = []; t_loc = foo.mli:25:33 },
                           { Tterm.t_node =
                             (Tterm.Tapp (
                                Symbols.Function_symbol {
                                  ls_name = Foo.is_sorted_sequence;
                                  ls_args =
                                  [{ Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = sequence;
                                          ts_args = [{ Ttypes.tv_name = a }];
                                          ts_alias = None },
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = integer;
                                                ts_args = []; ts_alias = None },
                                              []))
                                           }
                                          ]
                                        ))
                                     }
                                    ];
                                  ls_value =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = bool; ts_args = [];
                                         ts_alias = None },
                                       []))
                                    }},
                                [{ Tterm.t_node =
                                   (Tterm.Tapp (
                                      Symbols.Function_symbol {
                                        ls_name = Gospelstdlib.mixfix [_..];
                                        ls_args =
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = sequence;
                                                ts_args =
                                                [{ Ttypes.tv_name = a }];
                                                ts_alias = None },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyvar
                                                    { Ttypes.tv_name = a })
                                                 }
                                                ]
                                              ))
                                           };
                                          { Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = integer;
                                                 ts_args = []; ts_alias = None
                                                 },
                                               []))
                                            }
                                          ];
                                        ls_value =
                                        { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = sequence;
                                               ts_args =
                                               [{ Ttypes.tv_name = a }];
                                               ts_alias = None },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a })
                                                }
                                               ]
                                             ))
                                          }},
                                      [{ Tterm.t_node =
                                         (Tterm.Tvar
                                            { Symbols.vs_name = l;
                                              vs_ty =
                                              { Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = sequence;
                                                     ts_args =
                                                     [{ Ttypes.tv_name = a }];
                                                     ts_alias = None },
                                                   [{ Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident =
                                                           integer;
                                                           ts_args = [];
                                                           ts_alias = None },
                                                         []))
                                                      }
                                                     ]
                                                   ))
                                                }
                                              });
                                         t_ty =
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = sequence;
                                                ts_args =
                                                [{ Ttypes.tv_name = a }];
                                                ts_alias = None },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyapp (
                                                    { Ttypes.ts_ident = integer;
                                                      ts_args = [];
                                                      ts_alias = None },
                                                    []))
                                                 }
                                                ]
                                              ))
                                           };
                                         t_attrs = []; t_loc = foo.mli:25:68 };
                                        { Tterm.t_node = <constant>;
                                          t_ty =
                                          { Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = integer;
                                                 ts_args = []; ts_alias = None
                                                 },
                                               []))
                                            };
                                          t_attrs = []; t_loc = foo.mli:25:70 }
                                        ]
                                      ));
                                   t_ty =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = sequence;
                                          ts_args = [{ Ttypes.tv_name = a }];
                                          ts_alias = None },
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = integer;
                                                ts_args = []; ts_alias = None },
                                              []))
                                           }
                                          ]
                                        ))
                                     };
                                   t_attrs = []; t_loc = foo.mli:25:69 }
                                  ]
                                ));
                             t_ty =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = bool; ts_args = [];
                                    ts_alias = None },
                                  []))
                               };
                             t_attrs = []; t_loc = foo.mli:25:49 }
                           ));
                        t_ty =
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = bool; ts_args = [];
                               ts_alias = None },
                             []))
                          };
                        t_attrs = []; t_loc = foo.mli:25:32 }
                      ));
                   t_ty =
                   { Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None
                          },
                        []))
                     };
                   t_attrs = []; t_loc = foo.mli:25:7 });
           fun_spec = None;
           fun_text =
           " predicate rec is_sorted_sequence (l: integer sequence) =\n       Sequence.length l < 2 \\/ (l[0] <= l[1] /\\ is_sorted_sequence l[1 ..]) ";
           fun_loc = foo.mli:24:4 });
      sig_loc = foo.mli:24:4 };
    { Tast.sig_desc =
      (Tast.Sig_val (
         { Tast.vd_name = Foo.add; vd_type = 'a -> 'a t -> unit; vd_prim = [];
           vd_attrs = <attributes>;
           vd_args =
           [(Tast.Lnone
               { Symbols.vs_name = a_3;
                 vs_ty =
                 { Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) } });
             (Tast.Lnone
                { Symbols.vs_name = t_2;
                  vs_ty =
                  { Ttypes.ty_node =
                    (Ttypes.Tyapp (
                       { Ttypes.ts_ident = t;
                         ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None },
                       [{ Ttypes.ty_node =
                          (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                         ]
                       ))
                    }
                  })
             ];
           vd_ret = [];
           vd_spec =
           (Some { Tast.sp_args =
                   [(Tast.Lnone
                       { Symbols.vs_name = a_3;
                         vs_ty =
                         { Ttypes.ty_node =
                           (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                         });
                     (Tast.Lnone
                        { Symbols.vs_name = t_2;
                          vs_ty =
                          { Ttypes.ty_node =
                            (Ttypes.Tyapp (
                               { Ttypes.ts_ident = t;
                                 ts_args = [{ Ttypes.tv_name = a }];
                                 ts_alias = None },
                               [{ Ttypes.ty_node =
                                  (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                 ]
                               ))
                            }
                          })
                     ];
                   sp_ret = []; sp_pre = []; sp_checks = [];
                   sp_post =
                   [{ Tterm.t_node =
                      (Tterm.Tapp (
                         Symbols.Function_symbol {ls_name = infix =;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) };
                             { Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) }
                             ];
                           ls_value =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = bool; ts_args = [];
                                  ts_alias = None },
                                []))
                             }},
                         [{ Tterm.t_node =
                            (Tterm.Tfield (
                               { Tterm.t_node =
                                 (Tterm.Tvar
                                    { Symbols.vs_name = t_2;
                                      vs_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = t;
                                             ts_args = [{ Ttypes.tv_name = a }];
                                             ts_alias = None },
                                           [{ Ttypes.ty_node =
                                              (Ttypes.Tyvar
                                                 { Ttypes.tv_name = a })
                                              }
                                             ]
                                           ))
                                        }
                                      });
                                 t_ty =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = t;
                                        ts_args = [{ Ttypes.tv_name = a }];
                                        ts_alias = None },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
                                         }
                                        ]
                                      ))
                                   };
                                 t_attrs = []; t_loc = foo.mli:31:12 },
                               Symbols.Field_symbol {ls_name = contents;
                                 ls_args =
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = t;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    }
                                   ];
                                 ls_value =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = sequence;
                                        ts_args = [{ Ttypes.tv_name = a }];
                                        ts_alias = None },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
                                         }
                                        ]
                                      ))
                                   }}
                               ));
                            t_ty =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = sequence;
                                   ts_args = [{ Ttypes.tv_name = a }];
                                   ts_alias = None },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              };
                            t_attrs = []; t_loc = foo.mli:31:12 };
                           { Tterm.t_node =
                             (Tterm.Tif (
                                { Tterm.t_node =
                                  (Tterm.Tapp (
                                     Symbols.Function_symbol {
                                       ls_name = Foo.is_full;
                                       ls_args =
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = sequence;
                                               ts_args =
                                               [{ Ttypes.tv_name = a }];
                                               ts_alias = None },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a })
                                                }
                                               ]
                                             ))
                                          };
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = integer;
                                                ts_args = []; ts_alias = None },
                                              []))
                                           }
                                         ];
                                       ls_value =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = bool;
                                              ts_args = []; ts_alias = None },
                                            []))
                                         }},
                                     [{ Tterm.t_node =
                                        (Tterm.Tfield (
                                           { Tterm.t_node =
                                             (Tterm.Tvar
                                                { Symbols.vs_name = t_2;
                                                  vs_ty =
                                                  { Ttypes.ty_node =
                                                    (Ttypes.Tyapp (
                                                       { Ttypes.ts_ident = t;
                                                         ts_args =
                                                         [{ Ttypes.tv_name = a
                                                            }
                                                           ];
                                                         ts_alias = None },
                                                       [{ Ttypes.ty_node =
                                                          (Ttypes.Tyvar
                                                             { Ttypes.tv_name =
                                                               a })
                                                          }
                                                         ]
                                                       ))
                                                    }
                                                  });
                                             t_ty =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = t;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a }];
                                                    ts_alias = None },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyvar
                                                        { Ttypes.tv_name = a })
                                                     }
                                                    ]
                                                  ))
                                               };
                                             t_attrs = [];
                                             t_loc = foo.mli:31:36 },
                                           Symbols.Field_symbol {
                                             ls_name = contents;
                                             ls_args =
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = t;
                                                     ts_args =
                                                     [{ Ttypes.tv_name = a }];
                                                     ts_alias = None },
                                                   [{ Ttypes.ty_node =
                                                      (Ttypes.Tyvar
                                                         { Ttypes.tv_name = a })
                                                      }
                                                     ]
                                                   ))
                                                }
                                               ];
                                             ls_value =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = sequence;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a }];
                                                    ts_alias = None },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyvar
                                                        { Ttypes.tv_name = a })
                                                     }
                                                    ]
                                                  ))
                                               }}
                                           ));
                                        t_ty =
                                        { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = sequence;
                                               ts_args =
                                               [{ Ttypes.tv_name = a }];
                                               ts_alias = None },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a })
                                                }
                                               ]
                                             ))
                                          };
                                        t_attrs = []; t_loc = foo.mli:31:36 };
                                       { Tterm.t_node =
                                         (Tterm.Tfield (
                                            { Tterm.t_node =
                                              (Tterm.Tvar
                                                 { Symbols.vs_name = t_2;
                                                   vs_ty =
                                                   { Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident = t;
                                                          ts_args =
                                                          [{ Ttypes.tv_name = a
                                                             }
                                                            ];
                                                          ts_alias = None },
                                                        [{ Ttypes.ty_node =
                                                           (Ttypes.Tyvar
                                                              { Ttypes.tv_name =
                                                                a })
                                                           }
                                                          ]
                                                        ))
                                                     }
                                                   });
                                              t_ty =
                                              { Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = t;
                                                     ts_args =
                                                     [{ Ttypes.tv_name = a }];
                                                     ts_alias = None },
                                                   [{ Ttypes.ty_node =
                                                      (Ttypes.Tyvar
                                                         { Ttypes.tv_name = a })
                                                      }
                                                     ]
                                                   ))
                                                };
                                              t_attrs = [];
                                              t_loc = foo.mli:31:47 },
                                            Symbols.Field_symbol {
                                              ls_name = size;
                                              ls_args =
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyapp (
                                                    { Ttypes.ts_ident = t;
                                                      ts_args =
                                                      [{ Ttypes.tv_name = a }];
                                                      ts_alias = None },
                                                    [{ Ttypes.ty_node =
                                                       (Ttypes.Tyvar
                                                          { Ttypes.tv_name = a
                                                            })
                                                       }
                                                      ]
                                                    ))
                                                 }
                                                ];
                                              ls_value =
                                              { Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = integer;
                                                     ts_args = [];
                                                     ts_alias = None },
                                                   []))
                                                }}
                                            ));
                                         t_ty =
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = integer;
                                                ts_args = []; ts_alias = None },
                                              []))
                                           };
                                         t_attrs = []; t_loc = foo.mli:31:47 }
                                       ]
                                     ));
                                  t_ty =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = bool; ts_args = [];
                                         ts_alias = None },
                                       []))
                                    };
                                  t_attrs = []; t_loc = foo.mli:31:28 },
                                { Tterm.t_node =
                                  (Tterm.Told
                                     { Tterm.t_node =
                                       (Tterm.Tfield (
                                          { Tterm.t_node =
                                            (Tterm.Tvar
                                               { Symbols.vs_name = t_2;
                                                 vs_ty =
                                                 { Ttypes.ty_node =
                                                   (Ttypes.Tyapp (
                                                      { Ttypes.ts_ident = t;
                                                        ts_args =
                                                        [{ Ttypes.tv_name = a }
                                                          ];
                                                        ts_alias = None },
                                                      [{ Ttypes.ty_node =
                                                         (Ttypes.Tyvar
                                                            { Ttypes.tv_name =
                                                              a })
                                                         }
                                                        ]
                                                      ))
                                                   }
                                                 });
                                            t_ty =
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = t;
                                                   ts_args =
                                                   [{ Ttypes.tv_name = a }];
                                                   ts_alias = None },
                                                 [{ Ttypes.ty_node =
                                                    (Ttypes.Tyvar
                                                       { Ttypes.tv_name = a })
                                                    }
                                                   ]
                                                 ))
                                              };
                                            t_attrs = []; t_loc = foo.mli:32:34
                                            },
                                          Symbols.Field_symbol {
                                            ls_name = contents;
                                            ls_args =
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = t;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a }];
                                                    ts_alias = None },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyvar
                                                        { Ttypes.tv_name = a })
                                                     }
                                                    ]
                                                  ))
                                               }
                                              ];
                                            ls_value =
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = sequence;
                                                   ts_args =
                                                   [{ Ttypes.tv_name = a }];
                                                   ts_alias = None },
                                                 [{ Ttypes.ty_node =
                                                    (Ttypes.Tyvar
                                                       { Ttypes.tv_name = a })
                                                    }
                                                   ]
                                                 ))
                                              }}
                                          ));
                                       t_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = sequence;
                                              ts_args =
                                              [{ Ttypes.tv_name = a }];
                                              ts_alias = None },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                              ]
                                            ))
                                         };
                                       t_attrs = []; t_loc = foo.mli:32:34 });
                                  t_ty =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = sequence;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    };
                                  t_attrs = []; t_loc = foo.mli:32:30 },
                                { Tterm.t_node =
                                  (Tterm.Tapp (
                                     Symbols.Function_symbol {
                                       ls_name = Gospelstdlib.Sequence.cons;
                                       ls_args =
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          };
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = sequence;
                                                ts_args =
                                                [{ Ttypes.tv_name = a }];
                                                ts_alias = None },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyvar
                                                    { Ttypes.tv_name = a })
                                                 }
                                                ]
                                              ))
                                           }
                                         ];
                                       ls_value =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = sequence;
                                              ts_args =
                                              [{ Ttypes.tv_name = a }];
                                              ts_alias = None },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                              ]
                                            ))
                                         }},
                                     [{ Tterm.t_node =
                                        (Tterm.Tvar
                                           { Symbols.vs_name = a_3;
                                             vs_ty =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                             });
                                        t_ty =
                                        { Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          };
                                        t_attrs = []; t_loc = foo.mli:33:44 };
                                       { Tterm.t_node =
                                         (Tterm.Told
                                            { Tterm.t_node =
                                              (Tterm.Tfield (
                                                 { Tterm.t_node =
                                                   (Tterm.Tvar
                                                      { Symbols.vs_name = t_2;
                                                        vs_ty =
                                                        { Ttypes.ty_node =
                                                          (Ttypes.Tyapp (
                                                             { Ttypes.ts_ident =
                                                               t;
                                                               ts_args =
                                                               [{ Ttypes.tv_name =
                                                                  a }
                                                                 ];
                                                               ts_alias = None
                                                               },
                                                             [{ Ttypes.ty_node =
                                                                (Ttypes.Tyvar
                                                                   { Ttypes.tv_name =
                                                                     a })
                                                                }
                                                               ]
                                                             ))
                                                          }
                                                        });
                                                   t_ty =
                                                   { Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident = t;
                                                          ts_args =
                                                          [{ Ttypes.tv_name = a
                                                             }
                                                            ];
                                                          ts_alias = None },
                                                        [{ Ttypes.ty_node =
                                                           (Ttypes.Tyvar
                                                              { Ttypes.tv_name =
                                                                a })
                                                           }
                                                          ]
                                                        ))
                                                     };
                                                   t_attrs = [];
                                                   t_loc = foo.mli:33:51 },
                                                 Symbols.Field_symbol {
                                                   ls_name = contents;
                                                   ls_args =
                                                   [{ Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident = t;
                                                           ts_args =
                                                           [{ Ttypes.tv_name =
                                                              a }
                                                             ];
                                                           ts_alias = None },
                                                         [{ Ttypes.ty_node =
                                                            (Ttypes.Tyvar
                                                               { Ttypes.tv_name =
                                                                 a })
                                                            }
                                                           ]
                                                         ))
                                                      }
                                                     ];
                                                   ls_value =
                                                   { Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident =
                                                          sequence;
                                                          ts_args =
                                                          [{ Ttypes.tv_name = a
                                                             }
                                                            ];
                                                          ts_alias = None },
                                                        [{ Ttypes.ty_node =
                                                           (Ttypes.Tyvar
                                                              { Ttypes.tv_name =
                                                                a })
                                                           }
                                                          ]
                                                        ))
                                                     }}
                                                 ));
                                              t_ty =
                                              { Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = sequence;
                                                     ts_args =
                                                     [{ Ttypes.tv_name = a }];
                                                     ts_alias = None },
                                                   [{ Ttypes.ty_node =
                                                      (Ttypes.Tyvar
                                                         { Ttypes.tv_name = a })
                                                      }
                                                     ]
                                                   ))
                                                };
                                              t_attrs = [];
                                              t_loc = foo.mli:33:51 });
                                         t_ty =
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = sequence;
                                                ts_args =
                                                [{ Ttypes.tv_name = a }];
                                                ts_alias = None },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyvar
                                                    { Ttypes.tv_name = a })
                                                 }
                                                ]
                                              ))
                                           };
                                         t_attrs = []; t_loc = foo.mli:33:46 }
                                       ]
                                     ));
                                  t_ty =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = sequence;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    };
                                  t_attrs = []; t_loc = foo.mli:33:30 }
                                ));
                             t_ty =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = sequence;
                                    ts_args = [{ Ttypes.tv_name = a }];
                                    ts_alias = None },
                                  [{ Ttypes.ty_node =
                                     (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                    ]
                                  ))
                               };
                             t_attrs = []; t_loc = foo.mli:31:25 }
                           ]
                         ));
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = bool; ts_args = [];
                             ts_alias = None },
                           []))
                        };
                      t_attrs = []; t_loc = foo.mli:31:12 }
                     ];
                   sp_xpost = [];
                   sp_wr =
                   [{ Tterm.t_node =
                      (Tterm.Tfield (
                         { Tterm.t_node =
                           (Tterm.Tvar
                              { Symbols.vs_name = t_2;
                                vs_ty =
                                { Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = t;
                                       ts_args = [{ Ttypes.tv_name = a }];
                                       ts_alias = None },
                                     [{ Ttypes.ty_node =
                                        (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                       ]
                                     ))
                                  }
                                });
                           t_ty =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = t;
                                  ts_args = [{ Ttypes.tv_name = a }];
                                  ts_alias = None },
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                  ]
                                ))
                             };
                           t_attrs = []; t_loc = foo.mli:29:13 },
                         Symbols.Field_symbol {ls_name = contents;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = t;
                                   ts_args = [{ Ttypes.tv_name = a }];
                                   ts_alias = None },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              }
                             ];
                           ls_value =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = sequence;
                                  ts_args = [{ Ttypes.tv_name = a }];
                                  ts_alias = None },
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                  ]
                                ))
                             }}
                         ));
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = sequence;
                             ts_args = [{ Ttypes.tv_name = a }];
                             ts_alias = None },
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                             ]
                           ))
                        };
                      t_attrs = []; t_loc = foo.mli:29:13 }
                     ];
                   sp_cs = []; sp_diverge = false; sp_pure = false;
                   sp_equiv = [];
                   sp_text =
                   " add a t\n    modifies t.contents\n    (* comments *)\n    ensures t.contents = if is_full t.contents t.size\n                         then old t.contents\n                         else Sequence.cons a (old t.contents) ";
                   sp_loc = foo.mli:28:3 });
           vd_loc = foo.mli:27:0 },
         Tast.Nonghost));
      sig_loc = foo.mli:27:0 }
    ]

A smaller example, focused on locations after a directive:

  $ cat > foo.mli << EOF
  > # 5 "bar.mli"
  > type 'a t
  > (** documentation *)
  > (*@ mutable model contents : 'a list
  >     model size : int *)
  > EOF
  $ gospel dumpast foo.mli | grep '_loc.*:'
                    ty_loc = bar.mli:7:3 });
            td_loc = bar.mli:5:0 }
      sig_loc = bar.mli:5:0 }

Clean up:

  $ rm foo.mli
