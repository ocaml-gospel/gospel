Testing the location displayed by `gospel dumpast`, that is the location
available in the typed ast, *e.g* available to tools using `gospel`.


First, create a test artifact:

  $ cat > foo.mli << EOF
  > (* beginning of the module *)
  > 
  > type 'a t
  > (** documentation *)
  > (*@ ephemeral
  >     model contents : 'a list
  >     model size : int *)
  > 
  > val create : int -> 'a t
  > (*@ t = create i
  >     checks i >= 0
  >     ensures t.contents = []
  >     ensures t.size = i *)
  > 
  > (* comments *)
  > 
  > (*@ axiom a : true *)
  > 
  > (*@ function is_full (xs : 'a list) (x : integer) : bool = Sequence.length xs = x *)
  > 
  > (*@ function with_spec (x : integer) (xs : integer list) : integer list *)
  > (*@ requires not (Sequence.mem x xs)
  >     ensures true *)
  > 
  > (*@ predicate rec is_sorted_list (l: int list) = match l with
  >       | [] | _ :: [] -> true
  >       | h :: (y :: _ as t) -> h <= y /\ is_sorted_list t *)
  > 
  > val add : 'a -> 'a t -> unit
  > (*@ add a t
  >     modifies t.contents
  >     (* comments *)
  >     ensures t.contents = if is_full t.contents t.size
  >                          then old t.contents
  >                          else a :: (old t.contents) *)
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
      (Tast.Sig_type (Tast.Recursive,
         [{ Tast.td_ts =
            { Ttypes.ts_ident = t; ts_args = [{ Ttypes.tv_name = a }];
              ts_alias = None; ts_model = (true, Ttypes.Fields) };
            td_params = ({ Ttypes.tv_name = a }, (<variance>, <injectivity>));
            td_cstrs = ; td_kind = Tast.Pty_abstract; td_private = Tast.Public;
            td_manifest = None; td_attrs = <attributes>;
            td_spec =
            { Tast.ty_ephemeral = true;
              ty_model =
              (Tast.Fields
                 [Symbols.Field_symbol {ls_name = size;
                    ls_args =
                    [{ Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = t;
                            ts_args = [{ Ttypes.tv_name = a }];
                            ts_alias = None; ts_model = (true, Ttypes.Fields) },
                          [{ Ttypes.ty_node =
                             (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                            ]
                          ))
                       }
                      ];
                    ls_value =
                    { Ttypes.ty_node =
                      (Ttypes.Tyapp (
                         { Ttypes.ts_ident = int; ts_args = [];
                           ts_alias = None; ts_model = (false, Ttypes.Self) },
                         []))
                      }};
                   Symbols.Field_symbol {ls_name = contents;
                     ls_args =
                     [{ Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = t;
                             ts_args = [{ Ttypes.tv_name = a }];
                             ts_alias = None; ts_model = (true, Ttypes.Fields)
                             },
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                             ]
                           ))
                        }
                       ];
                     ls_value =
                     { Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = list;
                            ts_args = [{ Ttypes.tv_name = a_1 }];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
                          [{ Ttypes.ty_node =
                             (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                            ]
                          ))
                       }}
                   ]);
              ty_invariants = None;
              ty_text =
              " ephemeral\n    model contents : 'a list\n    model size : int ";
              ty_loc = foo.mli:5:3 };
            td_loc = foo.mli:3:0 }
           ],
         Tast.Nonghost));
      sig_loc = foo.mli:3:0 };
    { Tast.sig_desc =
      (Tast.Sig_val (
         { Tast.vd_name = Foo.create; vd_type = int -> 'a t; vd_prim = [];
           vd_attrs = <attributes>;
           vd_args =
           [{ Tast.lb_vs =
              { Symbols.vs_name = i;
                vs_ty =
                { Ttypes.ty_node =
                  (Ttypes.Tyapp (
                     { Ttypes.ts_ident = int; ts_args = []; ts_alias = None;
                       ts_model = (false, Ttypes.Self) },
                     []))
                  }
                };
              lb_label = Tast.Lnone;
              lb_consumes =
              (Some ({ Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = int; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
                          []))
                       },
                     { Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = int; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
                          []))
                       }));
              lb_produces =
              (Some ({ Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = int; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
                          []))
                       },
                     { Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = int; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
                          []))
                       }));
              lb_modified = false }
             ];
           vd_ret =
           [{ Tast.lb_vs =
              { Symbols.vs_name = t_1;
                vs_ty =
                { Ttypes.ty_node =
                  (Ttypes.Tyapp (
                     { Ttypes.ts_ident = t; ts_args = [{ Ttypes.tv_name = a }];
                       ts_alias = None; ts_model = (true, Ttypes.Fields) },
                     [{ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a })
                        }
                       ]
                     ))
                  }
                };
              lb_label = Tast.Lnone; lb_consumes = None;
              lb_produces =
              (Some ({ Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = t;
                            ts_args = [{ Ttypes.tv_name = a }];
                            ts_alias = None; ts_model = (true, Ttypes.Fields) },
                          [{ Ttypes.ty_node =
                             (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                            ]
                          ))
                       },
                     { Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = t;
                            ts_args = [{ Ttypes.tv_name = a }];
                            ts_alias = None; ts_model = (true, Ttypes.Fields) },
                          [{ Ttypes.ty_node =
                             (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                            ]
                          ))
                       }));
              lb_modified = true }
             ];
           vd_spec =
           { Tast.sp_args =
             [{ Tast.lb_vs =
                { Symbols.vs_name = i;
                  vs_ty =
                  { Ttypes.ty_node =
                    (Ttypes.Tyapp (
                       { Ttypes.ts_ident = int; ts_args = []; ts_alias = None;
                         ts_model = (false, Ttypes.Self) },
                       []))
                    }
                  };
                lb_label = Tast.Lnone;
                lb_consumes =
                (Some ({ Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = int; ts_args = [];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            []))
                         },
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = int; ts_args = [];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            []))
                         }));
                lb_produces =
                (Some ({ Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = int; ts_args = [];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            []))
                         },
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = int; ts_args = [];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            []))
                         }));
                lb_modified = false }
               ];
             sp_ret =
             [{ Tast.lb_vs =
                { Symbols.vs_name = t_1;
                  vs_ty =
                  { Ttypes.ty_node =
                    (Ttypes.Tyapp (
                       { Ttypes.ts_ident = t;
                         ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None;
                         ts_model = (true, Ttypes.Fields) },
                       [{ Ttypes.ty_node =
                          (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                         ]
                       ))
                    }
                  };
                lb_label = Tast.Lnone; lb_consumes = None;
                lb_produces =
                (Some ({ Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = t;
                              ts_args = [{ Ttypes.tv_name = a }];
                              ts_alias = None; ts_model = (true, Ttypes.Fields)
                              },
                            [{ Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                              ]
                            ))
                         },
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = t;
                              ts_args = [{ Ttypes.tv_name = a }];
                              ts_alias = None; ts_model = (true, Ttypes.Fields)
                              },
                            [{ Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                              ]
                            ))
                         }));
                lb_modified = true }
               ];
             sp_pre = [];
             sp_checks =
             [{ Tterm.t_node =
                (Tterm.Tapp (
                   Symbols.Function_symbol {ls_name = Gospelstdlib.infix >=;
                     ls_args =
                     [{ Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = integer; ts_args = [];
                             ts_alias = None; ts_model = (false, Ttypes.Self) },
                           []))
                        };
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = integer; ts_args = [];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            []))
                         }
                       ];
                     ls_value =
                     { Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = bool; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
                          []))
                       }},
                   [{ Tterm.t_node =
                      (Tterm.Tapp (
                         Symbols.Function_symbol {
                           ls_name = Gospelstdlib.integer_of_int;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = int; ts_args = [];
                                   ts_alias = None;
                                   ts_model = (false, Ttypes.Self) },
                                 []))
                              }
                             ];
                           ls_value =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = integer; ts_args = [];
                                  ts_alias = None;
                                  ts_model = (false, Ttypes.Self) },
                                []))
                             }},
                         [{ Tterm.t_node =
                            (Tterm.Tvar
                               { Symbols.vs_name = i;
                                 vs_ty =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = int; ts_args = [];
                                        ts_alias = None;
                                        ts_model = (false, Ttypes.Self) },
                                      []))
                                   }
                                 });
                            t_ty =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = int; ts_args = [];
                                   ts_alias = None;
                                   ts_model = (false, Ttypes.Self) },
                                 []))
                              };
                            t_attrs = []; t_loc = foo.mli:11:11 }
                           ]
                         ));
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = integer; ts_args = [];
                             ts_alias = None; ts_model = (false, Ttypes.Self) },
                           []))
                        };
                      t_attrs = []; t_loc = foo.mli:11:11 };
                     { Tterm.t_node = <constant>;
                       t_ty =
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = integer; ts_args = [];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            []))
                         };
                       t_attrs = []; t_loc = foo.mli:11:16 }
                     ]
                   ));
                t_ty =
                { Ttypes.ty_node =
                  (Ttypes.Tyapp (
                     { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None;
                       ts_model = (false, Ttypes.Self) },
                     []))
                  };
                t_attrs = []; t_loc = foo.mli:11:11 }
               ];
             sp_post =
             [{ Tterm.t_node =
                (Tterm.Tapp (
                   Symbols.Function_symbol {ls_name = infix =;
                     ls_args =
                     [{ Ttypes.ty_node =
                        (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                       { Ttypes.ty_node =
                         (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                       ];
                     ls_value =
                     { Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = bool; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
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
                                       ts_alias = None;
                                       ts_model = (true, Ttypes.Fields) },
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
                                  ts_alias = None;
                                  ts_model = (true, Ttypes.Fields) },
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                  ]
                                ))
                             };
                           t_attrs = []; t_loc = foo.mli:12:12 },
                         Symbols.Field_symbol {ls_name = contents;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = t;
                                   ts_args = [{ Ttypes.tv_name = a }];
                                   ts_alias = None;
                                   ts_model = (true, Ttypes.Fields) },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              }
                             ];
                           ls_value =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = list;
                                  ts_args = [{ Ttypes.tv_name = a_1 }];
                                  ts_alias = None;
                                  ts_model = (false, Ttypes.Self) },
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                  ]
                                ))
                             }}
                         ));
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = list;
                             ts_args = [{ Ttypes.tv_name = a_1 }];
                             ts_alias = None; ts_model = (false, Ttypes.Self) },
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                             ]
                           ))
                        };
                      t_attrs = []; t_loc = foo.mli:12:12 };
                     { Tterm.t_node =
                       (Tterm.Tapp (
                          Symbols.Constructor_symbol {ls_name = [];
                            ls_args = (Symbols.Cstr_tuple []);
                            ls_value =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = list;
                                   ts_args = [{ Ttypes.tv_name = a_1 }];
                                   ts_alias = None;
                                   ts_model = (false, Ttypes.Self) },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) }
                                   ]
                                 ))
                              }},
                          []));
                       t_ty =
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = list;
                              ts_args = [{ Ttypes.tv_name = a_1 }];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            [{ Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                              ]
                            ))
                         };
                       t_attrs = []; t_loc = foo.mli:12:25 }
                     ]
                   ));
                t_ty =
                { Ttypes.ty_node =
                  (Ttypes.Tyapp (
                     { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None;
                       ts_model = (false, Ttypes.Self) },
                     []))
                  };
                t_attrs = []; t_loc = foo.mli:12:12 };
               { Tterm.t_node =
                 (Tterm.Tapp (
                    Symbols.Function_symbol {ls_name = infix =;
                      ls_args =
                      [{ Ttypes.ty_node =
                         (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                        { Ttypes.ty_node =
                          (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                        ];
                      ls_value =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = bool; ts_args = [];
                             ts_alias = None; ts_model = (false, Ttypes.Self) },
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
                                        ts_alias = None;
                                        ts_model = (true, Ttypes.Fields) },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
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
                                   ts_alias = None;
                                   ts_model = (true, Ttypes.Fields) },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              };
                            t_attrs = []; t_loc = foo.mli:13:12 },
                          Symbols.Field_symbol {ls_name = size;
                            ls_args =
                            [{ Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = t;
                                    ts_args = [{ Ttypes.tv_name = a }];
                                    ts_alias = None;
                                    ts_model = (true, Ttypes.Fields) },
                                  [{ Ttypes.ty_node =
                                     (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                    ]
                                  ))
                               }
                              ];
                            ls_value =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = int; ts_args = [];
                                   ts_alias = None;
                                   ts_model = (false, Ttypes.Self) },
                                 []))
                              }}
                          ));
                       t_ty =
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = int; ts_args = [];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            []))
                         };
                       t_attrs = []; t_loc = foo.mli:13:12 };
                      { Tterm.t_node =
                        (Tterm.Tvar
                           { Symbols.vs_name = i;
                             vs_ty =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = int; ts_args = [];
                                    ts_alias = None;
                                    ts_model = (false, Ttypes.Self) },
                                  []))
                               }
                             });
                        t_ty =
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = int; ts_args = [];
                               ts_alias = None; ts_model = (false, Ttypes.Self)
                               },
                             []))
                          };
                        t_attrs = []; t_loc = foo.mli:13:21 }
                      ]
                    ));
                 t_ty =
                 { Ttypes.ty_node =
                   (Ttypes.Tyapp (
                      { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None;
                        ts_model = (false, Ttypes.Self) },
                      []))
                   };
                 t_attrs = []; t_loc = foo.mli:13:12 }
               ];
             sp_xpost = []; sp_diverge = false; sp_pure = false; sp_equiv = [];
             sp_text =
             " t = create i\n    checks i >= 0\n    ensures t.contents = []\n    ensures t.size = i ";
             sp_loc = foo.mli:10:3 };
           vd_loc = foo.mli:9:0 },
         Tast.Nonghost));
      sig_loc = foo.mli:9:0 };
    { Tast.sig_desc =
      (Tast.Sig_axiom
         { Tast.ax_name = Foo.a_3;
           ax_term =
           { Tterm.t_node = Tterm.Ttrue;
             t_ty =
             { Ttypes.ty_node =
               (Ttypes.Tyapp (
                  { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None;
                    ts_model = (false, Ttypes.Self) },
                  []))
               };
             t_attrs = []; t_loc = foo.mli:17:14 };
           ax_loc = foo.mli:17:3; ax_text = " axiom a : true " });
      sig_loc = foo.mli:17:0 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           Symbols.Function_symbol {ls_name = Foo.is_full;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = list;
                     ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None;
                     ts_model = (false, Ttypes.Self) },
                   [{ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) }]
                   ))
                };
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None;
                      ts_model = (false, Ttypes.Self) },
                    []))
                 }
               ];
             ls_value =
             { Ttypes.ty_node =
               (Ttypes.Tyapp (
                  { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None;
                    ts_model = (false, Ttypes.Self) },
                  []))
               }};
           fun_rec = false;
           fun_params =
           [{ Symbols.vs_name = xs;
              vs_ty =
              { Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = list;
                     ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None;
                     ts_model = (false, Ttypes.Self) },
                   [{ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) }]
                   ))
                }
              };
             { Symbols.vs_name = x;
               vs_ty =
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None;
                      ts_model = (false, Ttypes.Self) },
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
                           (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                          { Ttypes.ty_node =
                            (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                          ];
                        ls_value =
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = bool; ts_args = [];
                               ts_alias = None; ts_model = (false, Ttypes.Self)
                               },
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
                                      ts_alias = None;
                                      ts_model = (false, Ttypes.Self) },
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
                                     ts_alias = None;
                                     ts_model = (false, Ttypes.Self) },
                                   []))
                                }},
                            [{ Tterm.t_node =
                               (Tterm.Tapp (
                                  Symbols.Function_symbol {
                                    ls_name = Gospelstdlib.of_list;
                                    ls_args =
                                    [{ Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = list;
                                            ts_args =
                                            [{ Ttypes.tv_name = a_1 }];
                                            ts_alias = None;
                                            ts_model = (false, Ttypes.Self) },
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
                                           ts_args = [{ Ttypes.tv_name = a }];
                                           ts_alias = None;
                                           ts_model = (false, Ttypes.Self) },
                                         [{ Ttypes.ty_node =
                                            (Ttypes.Tyvar
                                               { Ttypes.tv_name = a })
                                            }
                                           ]
                                         ))
                                      }},
                                  [{ Tterm.t_node =
                                     (Tterm.Tvar
                                        { Symbols.vs_name = xs;
                                          vs_ty =
                                          { Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = list;
                                                 ts_args =
                                                 [{ Ttypes.tv_name = a_1 }];
                                                 ts_alias = None;
                                                 ts_model =
                                                 (false, Ttypes.Self) },
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
                                          { Ttypes.ts_ident = list;
                                            ts_args =
                                            [{ Ttypes.tv_name = a_1 }];
                                            ts_alias = None;
                                            ts_model = (false, Ttypes.Self) },
                                          [{ Ttypes.ty_node =
                                             (Ttypes.Tyvar
                                                { Ttypes.tv_name = a })
                                             }
                                            ]
                                          ))
                                       };
                                     t_attrs = []; t_loc = foo.mli:19:75 }
                                    ]
                                  ));
                               t_ty =
                               { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = sequence;
                                      ts_args = [{ Ttypes.tv_name = a }];
                                      ts_alias = None;
                                      ts_model = (false, Ttypes.Self) },
                                    [{ Ttypes.ty_node =
                                       (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                      ]
                                    ))
                                 };
                               t_attrs = []; t_loc = foo.mli:19:75 }
                              ]
                            ));
                         t_ty =
                         { Ttypes.ty_node =
                           (Ttypes.Tyapp (
                              { Ttypes.ts_ident = integer; ts_args = [];
                                ts_alias = None;
                                ts_model = (false, Ttypes.Self) },
                              []))
                           };
                         t_attrs = []; t_loc = foo.mli:19:59 };
                        { Tterm.t_node =
                          (Tterm.Tvar
                             { Symbols.vs_name = x;
                               vs_ty =
                               { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = integer; ts_args = [];
                                      ts_alias = None;
                                      ts_model = (false, Ttypes.Self) },
                                    []))
                                 }
                               });
                          t_ty =
                          { Ttypes.ty_node =
                            (Ttypes.Tyapp (
                               { Ttypes.ts_ident = integer; ts_args = [];
                                 ts_alias = None;
                                 ts_model = (false, Ttypes.Self) },
                               []))
                            };
                          t_attrs = []; t_loc = foo.mli:19:80 }
                        ]
                      ));
                   t_ty =
                   { Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = bool; ts_args = [];
                          ts_alias = None; ts_model = (false, Ttypes.Self) },
                        []))
                     };
                   t_attrs = []; t_loc = foo.mli:19:59 });
           fun_spec = None;
           fun_text =
           " function is_full (xs : 'a list) (x : integer) : bool = Sequence.length xs = x ";
           fun_loc = foo.mli:19:4 });
      sig_loc = foo.mli:19:4 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           Symbols.Function_symbol {ls_name = Foo.with_spec;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None;
                     ts_model = (false, Ttypes.Self) },
                   []))
                };
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = list;
                      ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None;
                      ts_model = (false, Ttypes.Self) },
                    [{ Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = integer; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
                          []))
                       }
                      ]
                    ))
                 }
               ];
             ls_value =
             { Ttypes.ty_node =
               (Ttypes.Tyapp (
                  { Ttypes.ts_ident = list;
                    ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None;
                    ts_model = (false, Ttypes.Self) },
                  [{ Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = integer; ts_args = [];
                          ts_alias = None; ts_model = (false, Ttypes.Self) },
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
                   { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None;
                     ts_model = (false, Ttypes.Self) },
                   []))
                }
              };
             { Symbols.vs_name = xs_1;
               vs_ty =
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = list;
                      ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None;
                      ts_model = (false, Ttypes.Self) },
                    [{ Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = integer; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
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
                                         ts_alias = None;
                                         ts_model = (false, Ttypes.Self) },
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
                                       ts_alias = None;
                                       ts_model = (false, Ttypes.Self) },
                                     []))
                                  }},
                              [{ Tterm.t_node =
                                 (Tterm.Tvar
                                    { Symbols.vs_name = x_1;
                                      vs_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = integer;
                                             ts_args = []; ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
                                           []))
                                        }
                                      });
                                 t_ty =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = integer;
                                        ts_args = []; ts_alias = None;
                                        ts_model = (false, Ttypes.Self) },
                                      []))
                                   };
                                 t_attrs = []; t_loc = foo.mli:22:31 };
                                { Tterm.t_node =
                                  (Tterm.Tapp (
                                     Symbols.Function_symbol {
                                       ls_name = Gospelstdlib.of_list;
                                       ls_args =
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = list;
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
                                               ts_alias = None;
                                               ts_model = (false, Ttypes.Self)
                                               },
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
                                              ts_alias = None;
                                              ts_model = (false, Ttypes.Self) },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                              ]
                                            ))
                                         }},
                                     [{ Tterm.t_node =
                                        (Tterm.Tvar
                                           { Symbols.vs_name = xs_1;
                                             vs_ty =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = list;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a_1 }];
                                                    ts_alias = None;
                                                    ts_model =
                                                    (false, Ttypes.Self) },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident =
                                                          integer;
                                                          ts_args = [];
                                                          ts_alias = None;
                                                          ts_model =
                                                          (false, Ttypes.Self)
                                                          },
                                                        []))
                                                     }
                                                    ]
                                                  ))
                                               }
                                             });
                                        t_ty =
                                        { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = list;
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
                                               ts_alias = None;
                                               ts_model = (false, Ttypes.Self)
                                               },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = integer;
                                                     ts_args = [];
                                                     ts_alias = None;
                                                     ts_model =
                                                     (false, Ttypes.Self) },
                                                   []))
                                                }
                                               ]
                                             ))
                                          };
                                        t_attrs = []; t_loc = foo.mli:22:33 }
                                       ]
                                     ));
                                  t_ty =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = sequence;
                                         ts_args = [{ Ttypes.tv_name = a }];
                                         ts_alias = None;
                                         ts_model = (false, Ttypes.Self) },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = integer;
                                               ts_args = []; ts_alias = None;
                                               ts_model = (false, Ttypes.Self)
                                               },
                                             []))
                                          }
                                         ]
                                       ))
                                    };
                                  t_attrs = []; t_loc = foo.mli:22:33 }
                                ]
                              ));
                           t_ty =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = bool; ts_args = [];
                                  ts_alias = None;
                                  ts_model = (false, Ttypes.Self) },
                                []))
                             };
                           t_attrs = []; t_loc = foo.mli:22:17 });
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = bool; ts_args = [];
                             ts_alias = None; ts_model = (false, Ttypes.Self) },
                           []))
                        };
                      t_attrs = []; t_loc = foo.mli:22:13 }
                     ];
                   fun_ens =
                   [{ Tterm.t_node = Tterm.Ttrue;
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = bool; ts_args = [];
                             ts_alias = None; ts_model = (false, Ttypes.Self) },
                           []))
                        };
                      t_attrs = []; t_loc = foo.mli:23:12 }
                     ];
                   fun_variant = []; fun_coer = false;
                   fun_text =
                   " requires not (Sequence.mem x xs)\n    ensures true ";
                   fun_loc = foo.mli:21:3 });
           fun_text =
           " function with_spec (x : integer) (xs : integer list) : integer list ";
           fun_loc = foo.mli:21:4 });
      sig_loc = foo.mli:21:4 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           Symbols.Function_symbol {ls_name = Foo.is_sorted_list;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = list;
                     ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None;
                     ts_model = (false, Ttypes.Self) },
                   [{ Ttypes.ty_node =
                      (Ttypes.Tyapp (
                         { Ttypes.ts_ident = int; ts_args = [];
                           ts_alias = None; ts_model = (false, Ttypes.Self) },
                         []))
                      }
                     ]
                   ))
                }
               ];
             ls_value =
             { Ttypes.ty_node =
               (Ttypes.Tyapp (
                  { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None;
                    ts_model = (false, Ttypes.Self) },
                  []))
               }};
           fun_rec = true;
           fun_params =
           [{ Symbols.vs_name = l;
              vs_ty =
              { Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = list;
                     ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None;
                     ts_model = (false, Ttypes.Self) },
                   [{ Ttypes.ty_node =
                      (Ttypes.Tyapp (
                         { Ttypes.ts_ident = int; ts_args = [];
                           ts_alias = None; ts_model = (false, Ttypes.Self) },
                         []))
                      }
                     ]
                   ))
                }
              }
             ];
           fun_def =
           (Some { Tterm.t_node =
                   (Tterm.Tcase (
                      { Tterm.t_node =
                        (Tterm.Tvar
                           { Symbols.vs_name = l;
                             vs_ty =
                             { Ttypes.ty_node =
                               (Ttypes.Tyapp (
                                  { Ttypes.ts_ident = list;
                                    ts_args = [{ Ttypes.tv_name = a_1 }];
                                    ts_alias = None;
                                    ts_model = (false, Ttypes.Self) },
                                  [{ Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = int; ts_args = [];
                                          ts_alias = None;
                                          ts_model = (false, Ttypes.Self) },
                                        []))
                                     }
                                    ]
                                  ))
                               }
                             });
                        t_ty =
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = list;
                               ts_args = [{ Ttypes.tv_name = a_1 }];
                               ts_alias = None; ts_model = (false, Ttypes.Self)
                               },
                             [{ Ttypes.ty_node =
                                (Ttypes.Tyapp (
                                   { Ttypes.ts_ident = int; ts_args = [];
                                     ts_alias = None;
                                     ts_model = (false, Ttypes.Self) },
                                   []))
                                }
                               ]
                             ))
                          };
                        t_attrs = []; t_loc = foo.mli:25:55 },
                      [({ Tterm.p_node =
                          (Tterm.Por (
                             { Tterm.p_node =
                               (Tterm.Papp (
                                  Symbols.Constructor_symbol {ls_name = [];
                                    ls_args = (Symbols.Cstr_tuple []);
                                    ls_value =
                                    { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = list;
                                           ts_args = [{ Ttypes.tv_name = a_1 }];
                                           ts_alias = None;
                                           ts_model = (false, Ttypes.Self) },
                                         [{ Ttypes.ty_node =
                                            (Ttypes.Tyvar
                                               { Ttypes.tv_name = a_1 })
                                            }
                                           ]
                                         ))
                                      }},
                                  []));
                               p_ty =
                               { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = list;
                                      ts_args = [{ Ttypes.tv_name = a_1 }];
                                      ts_alias = None;
                                      ts_model = (false, Ttypes.Self) },
                                    [{ Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = int;
                                            ts_args = []; ts_alias = None;
                                            ts_model = (false, Ttypes.Self) },
                                          []))
                                       }
                                      ]
                                    ))
                                 };
                               p_loc = foo.mli:26:8 },
                             { Tterm.p_node =
                               (Tterm.Papp (
                                  Symbols.Constructor_symbol {
                                    ls_name = infix ::;
                                    ls_args =
                                    (Symbols.Cstr_tuple
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar
                                             { Ttypes.tv_name = a_1 })
                                          };
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = list;
                                                ts_args =
                                                [{ Ttypes.tv_name = a_1 }];
                                                ts_alias = None;
                                                ts_model = (false, Ttypes.Self)
                                                },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyvar
                                                    { Ttypes.tv_name = a_1 })
                                                 }
                                                ]
                                              ))
                                           }
                                         ]);
                                    ls_value =
                                    { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = list;
                                           ts_args = [{ Ttypes.tv_name = a_1 }];
                                           ts_alias = None;
                                           ts_model = (false, Ttypes.Self) },
                                         [{ Ttypes.ty_node =
                                            (Ttypes.Tyvar
                                               { Ttypes.tv_name = a_1 })
                                            }
                                           ]
                                         ))
                                      }},
                                  [{ Tterm.p_node = Tterm.Pwild;
                                     p_ty =
                                     { Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = int;
                                            ts_args = []; ts_alias = None;
                                            ts_model = (false, Ttypes.Self) },
                                          []))
                                       };
                                     p_loc = foo.mli:26:13 };
                                    { Tterm.p_node =
                                      (Tterm.Papp (
                                         Symbols.Constructor_symbol {
                                           ls_name = [];
                                           ls_args = (Symbols.Cstr_tuple []);
                                           ls_value =
                                           { Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = list;
                                                  ts_args =
                                                  [{ Ttypes.tv_name = a_1 }];
                                                  ts_alias = None;
                                                  ts_model =
                                                  (false, Ttypes.Self) },
                                                [{ Ttypes.ty_node =
                                                   (Ttypes.Tyvar
                                                      { Ttypes.tv_name = a_1 })
                                                   }
                                                  ]
                                                ))
                                             }},
                                         []));
                                      p_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = list;
                                             ts_args =
                                             [{ Ttypes.tv_name = a_1 }];
                                             ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
                                           [{ Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = int;
                                                   ts_args = [];
                                                   ts_alias = None;
                                                   ts_model =
                                                   (false, Ttypes.Self) },
                                                 []))
                                              }
                                             ]
                                           ))
                                        };
                                      p_loc = foo.mli:26:18 }
                                    ]
                                  ));
                               p_ty =
                               { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = list;
                                      ts_args = [{ Ttypes.tv_name = a_1 }];
                                      ts_alias = None;
                                      ts_model = (false, Ttypes.Self) },
                                    [{ Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = int;
                                            ts_args = []; ts_alias = None;
                                            ts_model = (false, Ttypes.Self) },
                                          []))
                                       }
                                      ]
                                    ))
                                 };
                               p_loc = foo.mli:26:13 }
                             ));
                          p_ty =
                          { Ttypes.ty_node =
                            (Ttypes.Tyapp (
                               { Ttypes.ts_ident = list;
                                 ts_args = [{ Ttypes.tv_name = a_1 }];
                                 ts_alias = None;
                                 ts_model = (false, Ttypes.Self) },
                               [{ Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = int; ts_args = [];
                                       ts_alias = None;
                                       ts_model = (false, Ttypes.Self) },
                                     []))
                                  }
                                 ]
                               ))
                            };
                          p_loc = foo.mli:26:8 },
                        None,
                        { Tterm.t_node = Tterm.Ttrue;
                          t_ty =
                          { Ttypes.ty_node =
                            (Ttypes.Tyapp (
                               { Ttypes.ts_ident = bool; ts_args = [];
                                 ts_alias = None;
                                 ts_model = (false, Ttypes.Self) },
                               []))
                            };
                          t_attrs = []; t_loc = foo.mli:26:24 });
                        ({ Tterm.p_node =
                           (Tterm.Papp (
                              Symbols.Constructor_symbol {ls_name = infix ::;
                                ls_args =
                                (Symbols.Cstr_tuple
                                   [{ Ttypes.ty_node =
                                      (Ttypes.Tyvar { Ttypes.tv_name = a_1 }) };
                                     { Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = list;
                                            ts_args =
                                            [{ Ttypes.tv_name = a_1 }];
                                            ts_alias = None;
                                            ts_model = (false, Ttypes.Self) },
                                          [{ Ttypes.ty_node =
                                             (Ttypes.Tyvar
                                                { Ttypes.tv_name = a_1 })
                                             }
                                            ]
                                          ))
                                       }
                                     ]);
                                ls_value =
                                { Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = list;
                                       ts_args = [{ Ttypes.tv_name = a_1 }];
                                       ts_alias = None;
                                       ts_model = (false, Ttypes.Self) },
                                     [{ Ttypes.ty_node =
                                        (Ttypes.Tyvar { Ttypes.tv_name = a_1 })
                                        }
                                       ]
                                     ))
                                  }},
                              [{ Tterm.p_node =
                                 (Tterm.Pvar
                                    { Symbols.vs_name = h;
                                      vs_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = int;
                                             ts_args = []; ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
                                           []))
                                        }
                                      });
                                 p_ty =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = int; ts_args = [];
                                        ts_alias = None;
                                        ts_model = (false, Ttypes.Self) },
                                      []))
                                   };
                                 p_loc = foo.mli:27:8 };
                                { Tterm.p_node =
                                  (Tterm.Pas (
                                     { Tterm.p_node =
                                       (Tterm.Papp (
                                          Symbols.Constructor_symbol {
                                            ls_name = infix ::;
                                            ls_args =
                                            (Symbols.Cstr_tuple
                                               [{ Ttypes.ty_node =
                                                  (Ttypes.Tyvar
                                                     { Ttypes.tv_name = a_1 })
                                                  };
                                                 { Ttypes.ty_node =
                                                   (Ttypes.Tyapp (
                                                      { Ttypes.ts_ident = list;
                                                        ts_args =
                                                        [{ Ttypes.tv_name = a_1
                                                           }
                                                          ];
                                                        ts_alias = None;
                                                        ts_model =
                                                        (false, Ttypes.Self) },
                                                      [{ Ttypes.ty_node =
                                                         (Ttypes.Tyvar
                                                            { Ttypes.tv_name =
                                                              a_1 })
                                                         }
                                                        ]
                                                      ))
                                                   }
                                                 ]);
                                            ls_value =
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = list;
                                                   ts_args =
                                                   [{ Ttypes.tv_name = a_1 }];
                                                   ts_alias = None;
                                                   ts_model =
                                                   (false, Ttypes.Self) },
                                                 [{ Ttypes.ty_node =
                                                    (Ttypes.Tyvar
                                                       { Ttypes.tv_name = a_1 })
                                                    }
                                                   ]
                                                 ))
                                              }},
                                          [{ Tterm.p_node =
                                             (Tterm.Pvar
                                                { Symbols.vs_name = y;
                                                  vs_ty =
                                                  { Ttypes.ty_node =
                                                    (Ttypes.Tyapp (
                                                       { Ttypes.ts_ident = int;
                                                         ts_args = [];
                                                         ts_alias = None;
                                                         ts_model =
                                                         (false, Ttypes.Self) },
                                                       []))
                                                    }
                                                  });
                                             p_ty =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = int;
                                                    ts_args = [];
                                                    ts_alias = None;
                                                    ts_model =
                                                    (false, Ttypes.Self) },
                                                  []))
                                               };
                                             p_loc = foo.mli:27:14 };
                                            { Tterm.p_node = Tterm.Pwild;
                                              p_ty =
                                              { Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = list;
                                                     ts_args =
                                                     [{ Ttypes.tv_name = a_1 }];
                                                     ts_alias = None;
                                                     ts_model =
                                                     (false, Ttypes.Self) },
                                                   [{ Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident =
                                                           int; ts_args = [];
                                                           ts_alias = None;
                                                           ts_model =
                                                           (false, Ttypes.Self)
                                                           },
                                                         []))
                                                      }
                                                     ]
                                                   ))
                                                };
                                              p_loc = foo.mli:27:19 }
                                            ]
                                          ));
                                       p_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = list;
                                              ts_args =
                                              [{ Ttypes.tv_name = a_1 }];
                                              ts_alias = None;
                                              ts_model = (false, Ttypes.Self) },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = int;
                                                    ts_args = [];
                                                    ts_alias = None;
                                                    ts_model =
                                                    (false, Ttypes.Self) },
                                                  []))
                                               }
                                              ]
                                            ))
                                         };
                                       p_loc = foo.mli:27:14 },
                                     { Symbols.vs_name = t_2;
                                       vs_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = list;
                                              ts_args =
                                              [{ Ttypes.tv_name = a_1 }];
                                              ts_alias = None;
                                              ts_model = (false, Ttypes.Self) },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = int;
                                                    ts_args = [];
                                                    ts_alias = None;
                                                    ts_model =
                                                    (false, Ttypes.Self) },
                                                  []))
                                               }
                                              ]
                                            ))
                                         }
                                       }
                                     ));
                                  p_ty =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = list;
                                         ts_args = [{ Ttypes.tv_name = a_1 }];
                                         ts_alias = None;
                                         ts_model = (false, Ttypes.Self) },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = int;
                                               ts_args = []; ts_alias = None;
                                               ts_model = (false, Ttypes.Self)
                                               },
                                             []))
                                          }
                                         ]
                                       ))
                                    };
                                  p_loc = foo.mli:27:13 }
                                ]
                              ));
                           p_ty =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = list;
                                  ts_args = [{ Ttypes.tv_name = a_1 }];
                                  ts_alias = None;
                                  ts_model = (false, Ttypes.Self) },
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = int; ts_args = [];
                                        ts_alias = None;
                                        ts_model = (false, Ttypes.Self) },
                                      []))
                                   }
                                  ]
                                ))
                             };
                           p_loc = foo.mli:27:8 },
                         None,
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
                                             ts_args = []; ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
                                           []))
                                        };
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = integer;
                                              ts_args = []; ts_alias = None;
                                              ts_model = (false, Ttypes.Self) },
                                            []))
                                         }
                                       ];
                                     ls_value =
                                     { Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = bool;
                                            ts_args = []; ts_alias = None;
                                            ts_model = (false, Ttypes.Self) },
                                          []))
                                       }},
                                   [{ Tterm.t_node =
                                      (Tterm.Tapp (
                                         Symbols.Function_symbol {
                                           ls_name =
                                           Gospelstdlib.integer_of_int;
                                           ls_args =
                                           [{ Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = int;
                                                   ts_args = [];
                                                   ts_alias = None;
                                                   ts_model =
                                                   (false, Ttypes.Self) },
                                                 []))
                                              }
                                             ];
                                           ls_value =
                                           { Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = integer;
                                                  ts_args = [];
                                                  ts_alias = None;
                                                  ts_model =
                                                  (false, Ttypes.Self) },
                                                []))
                                             }},
                                         [{ Tterm.t_node =
                                            (Tterm.Tvar
                                               { Symbols.vs_name = h;
                                                 vs_ty =
                                                 { Ttypes.ty_node =
                                                   (Ttypes.Tyapp (
                                                      { Ttypes.ts_ident = int;
                                                        ts_args = [];
                                                        ts_alias = None;
                                                        ts_model =
                                                        (false, Ttypes.Self) },
                                                      []))
                                                   }
                                                 });
                                            t_ty =
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = int;
                                                   ts_args = [];
                                                   ts_alias = None;
                                                   ts_model =
                                                   (false, Ttypes.Self) },
                                                 []))
                                              };
                                            t_attrs = []; t_loc = foo.mli:27:30
                                            }
                                           ]
                                         ));
                                      t_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = integer;
                                             ts_args = []; ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
                                           []))
                                        };
                                      t_attrs = []; t_loc = foo.mli:27:30 };
                                     { Tterm.t_node =
                                       (Tterm.Tapp (
                                          Symbols.Function_symbol {
                                            ls_name =
                                            Gospelstdlib.integer_of_int;
                                            ls_args =
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = int;
                                                    ts_args = [];
                                                    ts_alias = None;
                                                    ts_model =
                                                    (false, Ttypes.Self) },
                                                  []))
                                               }
                                              ];
                                            ls_value =
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = integer;
                                                   ts_args = [];
                                                   ts_alias = None;
                                                   ts_model =
                                                   (false, Ttypes.Self) },
                                                 []))
                                              }},
                                          [{ Tterm.t_node =
                                             (Tterm.Tvar
                                                { Symbols.vs_name = y;
                                                  vs_ty =
                                                  { Ttypes.ty_node =
                                                    (Ttypes.Tyapp (
                                                       { Ttypes.ts_ident = int;
                                                         ts_args = [];
                                                         ts_alias = None;
                                                         ts_model =
                                                         (false, Ttypes.Self) },
                                                       []))
                                                    }
                                                  });
                                             t_ty =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = int;
                                                    ts_args = [];
                                                    ts_alias = None;
                                                    ts_model =
                                                    (false, Ttypes.Self) },
                                                  []))
                                               };
                                             t_attrs = [];
                                             t_loc = foo.mli:27:35 }
                                            ]
                                          ));
                                       t_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = integer;
                                              ts_args = []; ts_alias = None;
                                              ts_model = (false, Ttypes.Self) },
                                            []))
                                         };
                                       t_attrs = []; t_loc = foo.mli:27:35 }
                                     ]
                                   ));
                                t_ty =
                                { Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = bool; ts_args = [];
                                       ts_alias = None;
                                       ts_model = (false, Ttypes.Self) },
                                     []))
                                  };
                                t_attrs = []; t_loc = foo.mli:27:30 },
                              { Tterm.t_node =
                                (Tterm.Tapp (
                                   Symbols.Function_symbol {
                                     ls_name = Foo.is_sorted_list;
                                     ls_args =
                                     [{ Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = list;
                                             ts_args =
                                             [{ Ttypes.tv_name = a_1 }];
                                             ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
                                           [{ Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = int;
                                                   ts_args = [];
                                                   ts_alias = None;
                                                   ts_model =
                                                   (false, Ttypes.Self) },
                                                 []))
                                              }
                                             ]
                                           ))
                                        }
                                       ];
                                     ls_value =
                                     { Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = bool;
                                            ts_args = []; ts_alias = None;
                                            ts_model = (false, Ttypes.Self) },
                                          []))
                                       }},
                                   [{ Tterm.t_node =
                                      (Tterm.Tvar
                                         { Symbols.vs_name = t_2;
                                           vs_ty =
                                           { Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = list;
                                                  ts_args =
                                                  [{ Ttypes.tv_name = a_1 }];
                                                  ts_alias = None;
                                                  ts_model =
                                                  (false, Ttypes.Self) },
                                                [{ Ttypes.ty_node =
                                                   (Ttypes.Tyapp (
                                                      { Ttypes.ts_ident = int;
                                                        ts_args = [];
                                                        ts_alias = None;
                                                        ts_model =
                                                        (false, Ttypes.Self) },
                                                      []))
                                                   }
                                                  ]
                                                ))
                                             }
                                           });
                                      t_ty =
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = list;
                                             ts_args =
                                             [{ Ttypes.tv_name = a_1 }];
                                             ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
                                           [{ Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = int;
                                                   ts_args = [];
                                                   ts_alias = None;
                                                   ts_model =
                                                   (false, Ttypes.Self) },
                                                 []))
                                              }
                                             ]
                                           ))
                                        };
                                      t_attrs = []; t_loc = foo.mli:27:55 }
                                     ]
                                   ));
                                t_ty =
                                { Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = bool; ts_args = [];
                                       ts_alias = None;
                                       ts_model = (false, Ttypes.Self) },
                                     []))
                                  };
                                t_attrs = []; t_loc = foo.mli:27:40 }
                              ));
                           t_ty =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = bool; ts_args = [];
                                  ts_alias = None;
                                  ts_model = (false, Ttypes.Self) },
                                []))
                             };
                           t_attrs = []; t_loc = foo.mli:27:30 })
                        ]
                      ));
                   t_ty =
                   { Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = bool; ts_args = [];
                          ts_alias = None; ts_model = (false, Ttypes.Self) },
                        []))
                     };
                   t_attrs = []; t_loc = foo.mli:25:49 });
           fun_spec = None;
           fun_text =
           " predicate rec is_sorted_list (l: int list) = match l with\n      | [] | _ :: [] -> true\n      | h :: (y :: _ as t) -> h <= y /\\ is_sorted_list t ";
           fun_loc = foo.mli:25:4 });
      sig_loc = foo.mli:25:4 };
    { Tast.sig_desc =
      (Tast.Sig_val (
         { Tast.vd_name = Foo.add; vd_type = 'a -> 'a t -> unit; vd_prim = [];
           vd_attrs = <attributes>;
           vd_args =
           [{ Tast.lb_vs =
              { Symbols.vs_name = a_4;
                vs_ty =
                { Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) } };
              lb_label = Tast.Lnone;
              lb_consumes =
              (Some ({ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) },
                     { Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) }));
              lb_produces =
              (Some ({ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) },
                     { Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) }));
              lb_modified = false };
             { Tast.lb_vs =
               { Symbols.vs_name = t_3;
                 vs_ty =
                 { Ttypes.ty_node =
                   (Ttypes.Tyapp (
                      { Ttypes.ts_ident = t;
                        ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None;
                        ts_model = (true, Ttypes.Fields) },
                      [{ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a })
                         }
                        ]
                      ))
                   }
                 };
               lb_label = Tast.Lnone;
               lb_consumes =
               (Some ({ Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = t;
                             ts_args = [{ Ttypes.tv_name = a }];
                             ts_alias = None; ts_model = (true, Ttypes.Fields)
                             },
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                             ]
                           ))
                        },
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = t;
                             ts_args = [{ Ttypes.tv_name = a }];
                             ts_alias = None; ts_model = (true, Ttypes.Fields)
                             },
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                             ]
                           ))
                        }));
               lb_produces =
               (Some ({ Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = t;
                             ts_args = [{ Ttypes.tv_name = a }];
                             ts_alias = None; ts_model = (true, Ttypes.Fields)
                             },
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                             ]
                           ))
                        },
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = t;
                             ts_args = [{ Ttypes.tv_name = a }];
                             ts_alias = None; ts_model = (true, Ttypes.Fields)
                             },
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                             ]
                           ))
                        }));
               lb_modified = true }
             ];
           vd_ret = [];
           vd_spec =
           { Tast.sp_args =
             [{ Tast.lb_vs =
                { Symbols.vs_name = a_4;
                  vs_ty =
                  { Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) } };
                lb_label = Tast.Lnone;
                lb_consumes =
                (Some ({ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a })
                         },
                       { Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a })
                         }));
                lb_produces =
                (Some ({ Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a })
                         },
                       { Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a })
                         }));
                lb_modified = false };
               { Tast.lb_vs =
                 { Symbols.vs_name = t_3;
                   vs_ty =
                   { Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = t;
                          ts_args = [{ Ttypes.tv_name = a }]; ts_alias = None;
                          ts_model = (true, Ttypes.Fields) },
                        [{ Ttypes.ty_node =
                           (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                          ]
                        ))
                     }
                   };
                 lb_label = Tast.Lnone;
                 lb_consumes =
                 (Some ({ Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = t;
                               ts_args = [{ Ttypes.tv_name = a }];
                               ts_alias = None;
                               ts_model = (true, Ttypes.Fields) },
                             [{ Ttypes.ty_node =
                                (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                               ]
                             ))
                          },
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = t;
                               ts_args = [{ Ttypes.tv_name = a }];
                               ts_alias = None;
                               ts_model = (true, Ttypes.Fields) },
                             [{ Ttypes.ty_node =
                                (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                               ]
                             ))
                          }));
                 lb_produces =
                 (Some ({ Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = t;
                               ts_args = [{ Ttypes.tv_name = a }];
                               ts_alias = None;
                               ts_model = (true, Ttypes.Fields) },
                             [{ Ttypes.ty_node =
                                (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                               ]
                             ))
                          },
                        { Ttypes.ty_node =
                          (Ttypes.Tyapp (
                             { Ttypes.ts_ident = t;
                               ts_args = [{ Ttypes.tv_name = a }];
                               ts_alias = None;
                               ts_model = (true, Ttypes.Fields) },
                             [{ Ttypes.ty_node =
                                (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                               ]
                             ))
                          }));
                 lb_modified = true }
               ];
             sp_ret = []; sp_pre = []; sp_checks = [];
             sp_post =
             [{ Tterm.t_node =
                (Tterm.Tapp (
                   Symbols.Function_symbol {ls_name = infix =;
                     ls_args =
                     [{ Ttypes.ty_node =
                        (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                       { Ttypes.ty_node =
                         (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                       ];
                     ls_value =
                     { Ttypes.ty_node =
                       (Ttypes.Tyapp (
                          { Ttypes.ts_ident = bool; ts_args = [];
                            ts_alias = None; ts_model = (false, Ttypes.Self) },
                          []))
                       }},
                   [{ Tterm.t_node =
                      (Tterm.Tfield (
                         { Tterm.t_node =
                           (Tterm.Tvar
                              { Symbols.vs_name = t_3;
                                vs_ty =
                                { Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = t;
                                       ts_args = [{ Ttypes.tv_name = a }];
                                       ts_alias = None;
                                       ts_model = (true, Ttypes.Fields) },
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
                                  ts_alias = None;
                                  ts_model = (true, Ttypes.Fields) },
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                  ]
                                ))
                             };
                           t_attrs = []; t_loc = foo.mli:33:12 },
                         Symbols.Field_symbol {ls_name = contents;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = t;
                                   ts_args = [{ Ttypes.tv_name = a }];
                                   ts_alias = None;
                                   ts_model = (true, Ttypes.Fields) },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              }
                             ];
                           ls_value =
                           { Ttypes.ty_node =
                             (Ttypes.Tyapp (
                                { Ttypes.ts_ident = list;
                                  ts_args = [{ Ttypes.tv_name = a_1 }];
                                  ts_alias = None;
                                  ts_model = (false, Ttypes.Self) },
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                  ]
                                ))
                             }}
                         ));
                      t_ty =
                      { Ttypes.ty_node =
                        (Ttypes.Tyapp (
                           { Ttypes.ts_ident = list;
                             ts_args = [{ Ttypes.tv_name = a_1 }];
                             ts_alias = None; ts_model = (false, Ttypes.Self) },
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                             ]
                           ))
                        };
                      t_attrs = []; t_loc = foo.mli:33:12 };
                     { Tterm.t_node =
                       (Tterm.Tif (
                          { Tterm.t_node =
                            (Tterm.Tapp (
                               Symbols.Function_symbol {ls_name = Foo.is_full;
                                 ls_args =
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = list;
                                         ts_args = [{ Ttypes.tv_name = a_1 }];
                                         ts_alias = None;
                                         ts_model = (false, Ttypes.Self) },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    };
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = integer;
                                          ts_args = []; ts_alias = None;
                                          ts_model = (false, Ttypes.Self) },
                                        []))
                                     }
                                   ];
                                 ls_value =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = bool; ts_args = [];
                                        ts_alias = None;
                                        ts_model = (false, Ttypes.Self) },
                                      []))
                                   }},
                               [{ Tterm.t_node =
                                  (Tterm.Tfield (
                                     { Tterm.t_node =
                                       (Tterm.Tvar
                                          { Symbols.vs_name = t_3;
                                            vs_ty =
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = t;
                                                   ts_args =
                                                   [{ Ttypes.tv_name = a }];
                                                   ts_alias = None;
                                                   ts_model =
                                                   (true, Ttypes.Fields) },
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
                                              ts_args =
                                              [{ Ttypes.tv_name = a }];
                                              ts_alias = None;
                                              ts_model = (true, Ttypes.Fields)
                                              },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                              ]
                                            ))
                                         };
                                       t_attrs = []; t_loc = foo.mli:33:36 },
                                     Symbols.Field_symbol {ls_name = contents;
                                       ls_args =
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = t;
                                               ts_args =
                                               [{ Ttypes.tv_name = a }];
                                               ts_alias = None;
                                               ts_model = (true, Ttypes.Fields)
                                               },
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
                                            { Ttypes.ts_ident = list;
                                              ts_args =
                                              [{ Ttypes.tv_name = a_1 }];
                                              ts_alias = None;
                                              ts_model = (false, Ttypes.Self) },
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
                                       { Ttypes.ts_ident = list;
                                         ts_args = [{ Ttypes.tv_name = a_1 }];
                                         ts_alias = None;
                                         ts_model = (false, Ttypes.Self) },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    };
                                  t_attrs = []; t_loc = foo.mli:33:36 };
                                 { Tterm.t_node =
                                   (Tterm.Tapp (
                                      Symbols.Function_symbol {
                                        ls_name = Gospelstdlib.integer_of_int;
                                        ls_args =
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = int;
                                                ts_args = []; ts_alias = None;
                                                ts_model = (false, Ttypes.Self)
                                                },
                                              []))
                                           }
                                          ];
                                        ls_value =
                                        { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = integer;
                                               ts_args = []; ts_alias = None;
                                               ts_model = (false, Ttypes.Self)
                                               },
                                             []))
                                          }},
                                      [{ Tterm.t_node =
                                         (Tterm.Tfield (
                                            { Tterm.t_node =
                                              (Tterm.Tvar
                                                 { Symbols.vs_name = t_3;
                                                   vs_ty =
                                                   { Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident = t;
                                                          ts_args =
                                                          [{ Ttypes.tv_name = a
                                                             }
                                                            ];
                                                          ts_alias = None;
                                                          ts_model =
                                                          (true, Ttypes.Fields)
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
                                                     [{ Ttypes.tv_name = a }];
                                                     ts_alias = None;
                                                     ts_model =
                                                     (true, Ttypes.Fields) },
                                                   [{ Ttypes.ty_node =
                                                      (Ttypes.Tyvar
                                                         { Ttypes.tv_name = a })
                                                      }
                                                     ]
                                                   ))
                                                };
                                              t_attrs = [];
                                              t_loc = foo.mli:33:47 },
                                            Symbols.Field_symbol {
                                              ls_name = size;
                                              ls_args =
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyapp (
                                                    { Ttypes.ts_ident = t;
                                                      ts_args =
                                                      [{ Ttypes.tv_name = a }];
                                                      ts_alias = None;
                                                      ts_model =
                                                      (true, Ttypes.Fields) },
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
                                                   { Ttypes.ts_ident = int;
                                                     ts_args = [];
                                                     ts_alias = None;
                                                     ts_model =
                                                     (false, Ttypes.Self) },
                                                   []))
                                                }}
                                            ));
                                         t_ty =
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = int;
                                                ts_args = []; ts_alias = None;
                                                ts_model = (false, Ttypes.Self)
                                                },
                                              []))
                                           };
                                         t_attrs = []; t_loc = foo.mli:33:47 }
                                        ]
                                      ));
                                   t_ty =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = integer;
                                          ts_args = []; ts_alias = None;
                                          ts_model = (false, Ttypes.Self) },
                                        []))
                                     };
                                   t_attrs = []; t_loc = foo.mli:33:47 }
                                 ]
                               ));
                            t_ty =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = bool; ts_args = [];
                                   ts_alias = None;
                                   ts_model = (false, Ttypes.Self) },
                                 []))
                              };
                            t_attrs = []; t_loc = foo.mli:33:28 },
                          { Tterm.t_node =
                            (Tterm.Told
                               { Tterm.t_node =
                                 (Tterm.Tfield (
                                    { Tterm.t_node =
                                      (Tterm.Tvar
                                         { Symbols.vs_name = t_3;
                                           vs_ty =
                                           { Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = t;
                                                  ts_args =
                                                  [{ Ttypes.tv_name = a }];
                                                  ts_alias = None;
                                                  ts_model =
                                                  (true, Ttypes.Fields) },
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
                                             ts_alias = None;
                                             ts_model = (true, Ttypes.Fields) },
                                           [{ Ttypes.ty_node =
                                              (Ttypes.Tyvar
                                                 { Ttypes.tv_name = a })
                                              }
                                             ]
                                           ))
                                        };
                                      t_attrs = []; t_loc = foo.mli:34:34 },
                                    Symbols.Field_symbol {ls_name = contents;
                                      ls_args =
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = t;
                                              ts_args =
                                              [{ Ttypes.tv_name = a }];
                                              ts_alias = None;
                                              ts_model = (true, Ttypes.Fields)
                                              },
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
                                           { Ttypes.ts_ident = list;
                                             ts_args =
                                             [{ Ttypes.tv_name = a_1 }];
                                             ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
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
                                      { Ttypes.ts_ident = list;
                                        ts_args = [{ Ttypes.tv_name = a_1 }];
                                        ts_alias = None;
                                        ts_model = (false, Ttypes.Self) },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
                                         }
                                        ]
                                      ))
                                   };
                                 t_attrs = []; t_loc = foo.mli:34:34 });
                            t_ty =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = list;
                                   ts_args = [{ Ttypes.tv_name = a_1 }];
                                   ts_alias = None;
                                   ts_model = (false, Ttypes.Self) },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              };
                            t_attrs = []; t_loc = foo.mli:34:30 },
                          { Tterm.t_node =
                            (Tterm.Tapp (
                               Symbols.Constructor_symbol {ls_name = infix ::;
                                 ls_args =
                                 (Symbols.Cstr_tuple
                                    [{ Ttypes.ty_node =
                                       (Ttypes.Tyvar { Ttypes.tv_name = a_1 })
                                       };
                                      { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = list;
                                             ts_args =
                                             [{ Ttypes.tv_name = a_1 }];
                                             ts_alias = None;
                                             ts_model = (false, Ttypes.Self) },
                                           [{ Ttypes.ty_node =
                                              (Ttypes.Tyvar
                                                 { Ttypes.tv_name = a_1 })
                                              }
                                             ]
                                           ))
                                        }
                                      ]);
                                 ls_value =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = list;
                                        ts_args = [{ Ttypes.tv_name = a_1 }];
                                        ts_alias = None;
                                        ts_model = (false, Ttypes.Self) },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a_1 })
                                         }
                                        ]
                                      ))
                                   }},
                               [{ Tterm.t_node =
                                  (Tterm.Tvar
                                     { Symbols.vs_name = a_4;
                                       vs_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
                                         }
                                       });
                                  t_ty =
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) };
                                  t_attrs = []; t_loc = foo.mli:35:30 };
                                 { Tterm.t_node =
                                   (Tterm.Told
                                      { Tterm.t_node =
                                        (Tterm.Tfield (
                                           { Tterm.t_node =
                                             (Tterm.Tvar
                                                { Symbols.vs_name = t_3;
                                                  vs_ty =
                                                  { Ttypes.ty_node =
                                                    (Ttypes.Tyapp (
                                                       { Ttypes.ts_ident = t;
                                                         ts_args =
                                                         [{ Ttypes.tv_name = a
                                                            }
                                                           ];
                                                         ts_alias = None;
                                                         ts_model =
                                                         (true, Ttypes.Fields)
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
                                                    [{ Ttypes.tv_name = a }];
                                                    ts_alias = None;
                                                    ts_model =
                                                    (true, Ttypes.Fields) },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyvar
                                                        { Ttypes.tv_name = a })
                                                     }
                                                    ]
                                                  ))
                                               };
                                             t_attrs = [];
                                             t_loc = foo.mli:35:40 },
                                           Symbols.Field_symbol {
                                             ls_name = contents;
                                             ls_args =
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = t;
                                                     ts_args =
                                                     [{ Ttypes.tv_name = a }];
                                                     ts_alias = None;
                                                     ts_model =
                                                     (true, Ttypes.Fields) },
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
                                                  { Ttypes.ts_ident = list;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a_1 }];
                                                    ts_alias = None;
                                                    ts_model =
                                                    (false, Ttypes.Self) },
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
                                             { Ttypes.ts_ident = list;
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
                                               ts_alias = None;
                                               ts_model = (false, Ttypes.Self)
                                               },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a })
                                                }
                                               ]
                                             ))
                                          };
                                        t_attrs = []; t_loc = foo.mli:35:40 });
                                   t_ty =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = list;
                                          ts_args = [{ Ttypes.tv_name = a_1 }];
                                          ts_alias = None;
                                          ts_model = (false, Ttypes.Self) },
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyvar { Ttypes.tv_name = a })
                                           }
                                          ]
                                        ))
                                     };
                                   t_attrs = []; t_loc = foo.mli:35:35 }
                                 ]
                               ));
                            t_ty =
                            { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = list;
                                   ts_args = [{ Ttypes.tv_name = a_1 }];
                                   ts_alias = None;
                                   ts_model = (false, Ttypes.Self) },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              };
                            t_attrs = []; t_loc = foo.mli:35:30 }
                          ));
                       t_ty =
                       { Ttypes.ty_node =
                         (Ttypes.Tyapp (
                            { Ttypes.ts_ident = list;
                              ts_args = [{ Ttypes.tv_name = a_1 }];
                              ts_alias = None; ts_model = (false, Ttypes.Self)
                              },
                            [{ Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                              ]
                            ))
                         };
                       t_attrs = []; t_loc = foo.mli:33:25 }
                     ]
                   ));
                t_ty =
                { Ttypes.ty_node =
                  (Ttypes.Tyapp (
                     { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None;
                       ts_model = (false, Ttypes.Self) },
                     []))
                  };
                t_attrs = []; t_loc = foo.mli:33:12 }
               ];
             sp_xpost = []; sp_diverge = false; sp_pure = false; sp_equiv = [];
             sp_text =
             " add a t\n    modifies t.contents\n    (* comments *)\n    ensures t.contents = if is_full t.contents t.size\n                         then old t.contents\n                         else a :: (old t.contents) ";
             sp_loc = foo.mli:30:3 };
           vd_loc = foo.mli:29:0 },
         Tast.Nonghost));
      sig_loc = foo.mli:29:0 }
    ]

A smaller example, focused on locations after a directive:

  $ cat > foo.mli << EOF
  > # 5 "bar.mli"
  > type 'a t
  > (** documentation *)
  > (*@ model contents : 'a list
  >     model size : int *)
  > EOF
  $ gospel dumpast foo.mli | grep '_loc.*:'
              ty_loc = bar.mli:7:3 };
            td_loc = bar.mli:5:0 }
      sig_loc = bar.mli:5:0 }

Clean up:

  $ rm foo.mli
