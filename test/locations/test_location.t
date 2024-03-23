Testing the location displayed by `gospel dumpast`, that is the location
available in the typed ast, *e.g* available to tools using `gospel`.


First, create a test artifact:

  $ cat > foo.mli << EOF
  > (* beginning of the module *)
  > 
  > type 'a t
  > (** documentation *)
  > (*@ mutable model contents : 'a list
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
  > (*@ function is_full (xs : 'a list) (x : integer) : bool = List.length xs = x *)
  > 
  > (*@ function with_spec (x : integer) (xs : integer list) : integer list *)
  > (*@ requires not (List.mem x xs)
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
              ts_alias = None };
            td_params = ({ Ttypes.tv_name = a }, (<variance>, <injectivity>));
            td_cstrs = ; td_kind = Tast.Pty_abstract; td_private = Tast.Public;
            td_manifest = None; td_attrs = <attributes>;
            td_spec =
            (Some { Tast.ty_ephemeral = true;
                    ty_fields =
                    [({ Symbols.ls_name = contents;
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
                        (Some { Ttypes.ty_node =
                                (Ttypes.Tyapp (
                                   { Ttypes.ts_ident = list;
                                     ts_args = [{ Ttypes.tv_name = a_1 }];
                                     ts_alias = None },
                                   [{ Ttypes.ty_node =
                                      (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                     ]
                                   ))
                                });
                        ls_constr = false; ls_field = true },
                      true);
                      ({ Symbols.ls_name = size;
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
                         (Some { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = int; ts_args = [];
                                      ts_alias = None },
                                    []))
                                 });
                         ls_constr = false; ls_field = true },
                       false)
                      ];
                    ty_invariants = None;
                    ty_text =
                    " mutable model contents : 'a list\n    model size : int ";
                    ty_loc = foo.mli:5:3 });
            td_loc = foo.mli:3:0 }
           ],
         Tast.Nonghost));
      sig_loc = foo.mli:3:0 };
    { Tast.sig_desc =
      (Tast.Sig_val (
         { Tast.vd_name = create; vd_type = int -> 'a t; vd_prim = [];
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
                         { Symbols.ls_name = infix >=;
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
                           ls_value = None; ls_constr = false; ls_field = false
                           },
                         [{ Tterm.t_node =
                            (Tterm.Tapp (
                               { Symbols.ls_name = integer_of_int;
                                 ls_args =
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = int; ts_args = [];
                                         ts_alias = None },
                                       []))
                                    }
                                   ];
                                 ls_value =
                                 (Some { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = integer;
                                              ts_args = []; ts_alias = None },
                                            []))
                                         });
                                 ls_constr = false; ls_field = false },
                               [{ Tterm.t_node =
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
                                  (Some { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = int;
                                               ts_args = []; ts_alias = None },
                                             []))
                                          });
                                  t_attrs = []; t_loc = foo.mli:10:11 }
                                 ]
                               ));
                            t_ty =
                            (Some { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = integer;
                                         ts_args = []; ts_alias = None },
                                       []))
                                    });
                            t_attrs = []; t_loc = foo.mli:10:11 };
                           { Tterm.t_node = <constant>;
                             t_ty =
                             (Some { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = integer;
                                          ts_args = []; ts_alias = None },
                                        []))
                                     });
                             t_attrs = []; t_loc = foo.mli:10:16 }
                           ]
                         ));
                      t_ty = None; t_attrs = []; t_loc = foo.mli:10:11 }
                     ];
                   sp_post =
                   [{ Tterm.t_node =
                      (Tterm.Tapp (
                         { Symbols.ls_name = infix =;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                             { Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                             ];
                           ls_value = None; ls_constr = false; ls_field = false
                           },
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
                                 (Some { Ttypes.ty_node =
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
                                         });
                                 t_attrs = []; t_loc = foo.mli:11:12 },
                               { Symbols.ls_name = contents;
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
                                 (Some { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = list;
                                              ts_args =
                                              [{ Ttypes.tv_name = a_1 }];
                                              ts_alias = None },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                              ]
                                            ))
                                         });
                                 ls_constr = false; ls_field = true }
                               ));
                            t_ty =
                            (Some { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = list;
                                         ts_args = [{ Ttypes.tv_name = a_1 }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    });
                            t_attrs = []; t_loc = foo.mli:11:12 };
                           { Tterm.t_node =
                             (Tterm.Tapp (
                                { Symbols.ls_name = []; ls_args = [];
                                  ls_value =
                                  (Some { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = list;
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
                                               ts_alias = None },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a_1 })
                                                }
                                               ]
                                             ))
                                          });
                                  ls_constr = true; ls_field = false },
                                []));
                             t_ty =
                             (Some { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = list;
                                          ts_args = [{ Ttypes.tv_name = a_1 }];
                                          ts_alias = None },
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyvar { Ttypes.tv_name = a })
                                           }
                                          ]
                                        ))
                                     });
                             t_attrs = []; t_loc = foo.mli:11:25 }
                           ]
                         ));
                      t_ty = None; t_attrs = []; t_loc = foo.mli:11:12 };
                     { Tterm.t_node =
                       (Tterm.Tapp (
                          { Symbols.ls_name = infix =;
                            ls_args =
                            [{ Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                              { Ttypes.ty_node =
                                (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                              ];
                            ls_value = None; ls_constr = false;
                            ls_field = false },
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
                                  (Some { Ttypes.ty_node =
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
                                          });
                                  t_attrs = []; t_loc = foo.mli:12:12 },
                                { Symbols.ls_name = size;
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
                                  (Some { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = int;
                                               ts_args = []; ts_alias = None },
                                             []))
                                          });
                                  ls_constr = false; ls_field = true }
                                ));
                             t_ty =
                             (Some { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = int; ts_args = [];
                                          ts_alias = None },
                                        []))
                                     });
                             t_attrs = []; t_loc = foo.mli:12:12 };
                            { Tterm.t_node =
                              (Tterm.Tvar
                                 { Symbols.vs_name = i;
                                   vs_ty =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = int; ts_args = [];
                                          ts_alias = None },
                                        []))
                                     }
                                   });
                              t_ty =
                              (Some { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = int; ts_args = [];
                                           ts_alias = None },
                                         []))
                                      });
                              t_attrs = []; t_loc = foo.mli:12:21 }
                            ]
                          ));
                       t_ty = None; t_attrs = []; t_loc = foo.mli:12:12 }
                     ];
                   sp_xpost = []; sp_wr = []; sp_cs = []; sp_diverge = false;
                   sp_pure = false; sp_equiv = [];
                   sp_text =
                   " t = create i\n    checks i >= 0\n    ensures t.contents = []\n    ensures t.size = i ";
                   sp_loc = foo.mli:9:3 });
           vd_loc = foo.mli:8:0 },
         Tast.Nonghost));
      sig_loc = foo.mli:8:0 };
    { Tast.sig_desc =
      (Tast.Sig_axiom
         { Tast.ax_name = a_3;
           ax_term =
           { Tterm.t_node = Tterm.Ttrue; t_ty = None; t_attrs = [];
             t_loc = foo.mli:16:14 };
           ax_loc = foo.mli:16:3; ax_text = " axiom a : true " });
      sig_loc = foo.mli:16:0 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           { Symbols.ls_name = is_full;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = list;
                     ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None },
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
             (Some { Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = bool; ts_args = []; ts_alias = None
                          },
                        []))
                     });
             ls_constr = false; ls_field = false };
           fun_rec = false;
           fun_params =
           [{ Symbols.vs_name = xs;
              vs_ty =
              { Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = list;
                     ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None },
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
                   (Tterm.Tif (
                      { Tterm.t_node =
                        (Tterm.Tapp (
                           { Symbols.ls_name = infix =;
                             ls_args =
                             [{ Ttypes.ty_node =
                                (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                               { Ttypes.ty_node =
                                 (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                               ];
                             ls_value = None; ls_constr = false;
                             ls_field = false },
                           [{ Tterm.t_node =
                              (Tterm.Tapp (
                                 { Symbols.ls_name = length;
                                   ls_args =
                                   [{ Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = list;
                                           ts_args = [{ Ttypes.tv_name = a_1 }];
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
                                   (Some { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = integer;
                                                ts_args = []; ts_alias = None },
                                              []))
                                           });
                                   ls_constr = false; ls_field = false },
                                 [{ Tterm.t_node =
                                    (Tterm.Tvar
                                       { Symbols.vs_name = xs;
                                         vs_ty =
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = list;
                                                ts_args =
                                                [{ Ttypes.tv_name = a_1 }];
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
                                    (Some { Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = list;
                                                 ts_args =
                                                 [{ Ttypes.tv_name = a_1 }];
                                                 ts_alias = None },
                                               [{ Ttypes.ty_node =
                                                  (Ttypes.Tyvar
                                                     { Ttypes.tv_name = a })
                                                  }
                                                 ]
                                               ))
                                            });
                                    t_attrs = []; t_loc = foo.mli:18:71 }
                                   ]
                                 ));
                              t_ty =
                              (Some { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = integer;
                                           ts_args = []; ts_alias = None },
                                         []))
                                      });
                              t_attrs = []; t_loc = foo.mli:18:59 };
                             { Tterm.t_node =
                               (Tterm.Tvar
                                  { Symbols.vs_name = x;
                                    vs_ty =
                                    { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = integer;
                                           ts_args = []; ts_alias = None },
                                         []))
                                      }
                                    });
                               t_ty =
                               (Some { Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = integer;
                                            ts_args = []; ts_alias = None },
                                          []))
                                       });
                               t_attrs = []; t_loc = foo.mli:18:76 }
                             ]
                           ));
                        t_ty = None; t_attrs = []; t_loc = foo.mli:18:59 },
                      { Tterm.t_node =
                        (Tterm.Tapp (
                           { Symbols.ls_name = true; ls_args = [];
                             ls_value =
                             (Some { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = bool; ts_args = [];
                                          ts_alias = None },
                                        []))
                                     });
                             ls_constr = true; ls_field = false },
                           []));
                        t_ty =
                        (Some { Ttypes.ty_node =
                                (Ttypes.Tyapp (
                                   { Ttypes.ts_ident = bool; ts_args = [];
                                     ts_alias = None },
                                   []))
                                });
                        t_attrs = []; t_loc = foo.mli:18:59 },
                      { Tterm.t_node =
                        (Tterm.Tapp (
                           { Symbols.ls_name = false; ls_args = [];
                             ls_value =
                             (Some { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = bool; ts_args = [];
                                          ts_alias = None },
                                        []))
                                     });
                             ls_constr = true; ls_field = false },
                           []));
                        t_ty =
                        (Some { Ttypes.ty_node =
                                (Ttypes.Tyapp (
                                   { Ttypes.ts_ident = bool; ts_args = [];
                                     ts_alias = None },
                                   []))
                                });
                        t_attrs = []; t_loc = foo.mli:18:59 }
                      ));
                   t_ty =
                   (Some { Ttypes.ty_node =
                           (Ttypes.Tyapp (
                              { Ttypes.ts_ident = bool; ts_args = [];
                                ts_alias = None },
                              []))
                           });
                   t_attrs = []; t_loc = foo.mli:18:59 });
           fun_spec = None;
           fun_text =
           " function is_full (xs : 'a list) (x : integer) : bool = List.length xs = x ";
           fun_loc = foo.mli:18:4 });
      sig_loc = foo.mli:18:4 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           { Symbols.ls_name = with_spec;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = integer; ts_args = []; ts_alias = None },
                   []))
                };
               { Ttypes.ty_node =
                 (Ttypes.Tyapp (
                    { Ttypes.ts_ident = list;
                      ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None },
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
             (Some { Ttypes.ty_node =
                     (Ttypes.Tyapp (
                        { Ttypes.ts_ident = list;
                          ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None
                          },
                        [{ Ttypes.ty_node =
                           (Ttypes.Tyapp (
                              { Ttypes.ts_ident = integer; ts_args = [];
                                ts_alias = None },
                              []))
                           }
                          ]
                        ))
                     });
             ls_constr = false; ls_field = false };
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
                    { Ttypes.ts_ident = list;
                      ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None },
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
                              { Symbols.ls_name = mem;
                                ls_args =
                                [{ Ttypes.ty_node =
                                   (Ttypes.Tyvar { Ttypes.tv_name = a }) };
                                  { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = list;
                                         ts_args = [{ Ttypes.tv_name = a_1 }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    }
                                  ];
                                ls_value = None; ls_constr = false;
                                ls_field = false },
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
                                 (Some { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = integer;
                                              ts_args = []; ts_alias = None },
                                            []))
                                         });
                                 t_attrs = []; t_loc = foo.mli:21:27 };
                                { Tterm.t_node =
                                  (Tterm.Tvar
                                     { Symbols.vs_name = xs_1;
                                       vs_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = list;
                                              ts_args =
                                              [{ Ttypes.tv_name = a_1 }];
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
                                  (Some { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = list;
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
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
                                          });
                                  t_attrs = []; t_loc = foo.mli:21:29 }
                                ]
                              ));
                           t_ty = None; t_attrs = []; t_loc = foo.mli:21:17 });
                      t_ty = None; t_attrs = []; t_loc = foo.mli:21:13 }
                     ];
                   fun_ens =
                   [{ Tterm.t_node = Tterm.Ttrue; t_ty = None; t_attrs = [];
                      t_loc = foo.mli:22:12 }
                     ];
                   fun_variant = []; fun_coer = false;
                   fun_text =
                   " requires not (List.mem x xs)\n    ensures true ";
                   fun_loc = foo.mli:20:3 });
           fun_text =
           " function with_spec (x : integer) (xs : integer list) : integer list ";
           fun_loc = foo.mli:20:4 });
      sig_loc = foo.mli:20:4 };
    { Tast.sig_desc =
      (Tast.Sig_function
         { Tast.fun_ls =
           { Symbols.ls_name = is_sorted_list;
             ls_args =
             [{ Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = list;
                     ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None },
                   [{ Ttypes.ty_node =
                      (Ttypes.Tyapp (
                         { Ttypes.ts_ident = int; ts_args = []; ts_alias = None
                           },
                         []))
                      }
                     ]
                   ))
                }
               ];
             ls_value = None; ls_constr = false; ls_field = false };
           fun_rec = true;
           fun_params =
           [{ Symbols.vs_name = l;
              vs_ty =
              { Ttypes.ty_node =
                (Ttypes.Tyapp (
                   { Ttypes.ts_ident = list;
                     ts_args = [{ Ttypes.tv_name = a_1 }]; ts_alias = None },
                   [{ Ttypes.ty_node =
                      (Ttypes.Tyapp (
                         { Ttypes.ts_ident = int; ts_args = []; ts_alias = None
                           },
                         []))
                      }
                     ]
                   ))
                }
              }
             ];
           fun_def =
           (Some { Tterm.t_node =
                   (Tterm.Tapp (
                      { Symbols.ls_name = infix =;
                        ls_args =
                        [{ Ttypes.ty_node =
                           (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                          { Ttypes.ty_node =
                            (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                          ];
                        ls_value = None; ls_constr = false; ls_field = false },
                      [{ Tterm.t_node =
                         (Tterm.Tcase (
                            { Tterm.t_node =
                              (Tterm.Tvar
                                 { Symbols.vs_name = l;
                                   vs_ty =
                                   { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = list;
                                          ts_args = [{ Ttypes.tv_name = a_1 }];
                                          ts_alias = None },
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = int;
                                                ts_args = []; ts_alias = None },
                                              []))
                                           }
                                          ]
                                        ))
                                     }
                                   });
                              t_ty =
                              (Some { Ttypes.ty_node =
                                      (Ttypes.Tyapp (
                                         { Ttypes.ts_ident = list;
                                           ts_args = [{ Ttypes.tv_name = a_1 }];
                                           ts_alias = None },
                                         [{ Ttypes.ty_node =
                                            (Ttypes.Tyapp (
                                               { Ttypes.ts_ident = int;
                                                 ts_args = []; ts_alias = None
                                                 },
                                               []))
                                            }
                                           ]
                                         ))
                                      });
                              t_attrs = []; t_loc = foo.mli:24:55 },
                            [({ Tterm.p_node =
                                (Tterm.Por (
                                   { Tterm.p_node =
                                     (Tterm.Papp (
                                        { Symbols.ls_name = []; ls_args = [];
                                          ls_value =
                                          (Some { Ttypes.ty_node =
                                                  (Ttypes.Tyapp (
                                                     { Ttypes.ts_ident = list;
                                                       ts_args =
                                                       [{ Ttypes.tv_name = a_1
                                                          }
                                                         ];
                                                       ts_alias = None },
                                                     [{ Ttypes.ty_node =
                                                        (Ttypes.Tyvar
                                                           { Ttypes.tv_name =
                                                             a_1 })
                                                        }
                                                       ]
                                                     ))
                                                  });
                                          ls_constr = true; ls_field = false },
                                        []));
                                     p_ty =
                                     { Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = list;
                                            ts_args =
                                            [{ Ttypes.tv_name = a_1 }];
                                            ts_alias = None },
                                          [{ Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = int;
                                                  ts_args = []; ts_alias = None
                                                  },
                                                []))
                                             }
                                            ]
                                          ))
                                       };
                                     p_loc = foo.mli:25:8 },
                                   { Tterm.p_node =
                                     (Tterm.Papp (
                                        { Symbols.ls_name = infix ::;
                                          ls_args =
                                          [{ Ttypes.ty_node =
                                             (Ttypes.Tyvar
                                                { Ttypes.tv_name = a_1 })
                                             };
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = list;
                                                   ts_args =
                                                   [{ Ttypes.tv_name = a_1 }];
                                                   ts_alias = None },
                                                 [{ Ttypes.ty_node =
                                                    (Ttypes.Tyvar
                                                       { Ttypes.tv_name = a_1 })
                                                    }
                                                   ]
                                                 ))
                                              }
                                            ];
                                          ls_value =
                                          (Some { Ttypes.ty_node =
                                                  (Ttypes.Tyapp (
                                                     { Ttypes.ts_ident = list;
                                                       ts_args =
                                                       [{ Ttypes.tv_name = a_1
                                                          }
                                                         ];
                                                       ts_alias = None },
                                                     [{ Ttypes.ty_node =
                                                        (Ttypes.Tyvar
                                                           { Ttypes.tv_name =
                                                             a_1 })
                                                        }
                                                       ]
                                                     ))
                                                  });
                                          ls_constr = true; ls_field = false },
                                        [{ Tterm.p_node = Tterm.Pwild;
                                           p_ty =
                                           { Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = int;
                                                  ts_args = []; ts_alias = None
                                                  },
                                                []))
                                             };
                                           p_loc = foo.mli:25:13 };
                                          { Tterm.p_node =
                                            (Tterm.Papp (
                                               { Symbols.ls_name = [];
                                                 ls_args = [];
                                                 ls_value =
                                                 (Some { Ttypes.ty_node =
                                                         (Ttypes.Tyapp (
                                                            { Ttypes.ts_ident =
                                                              list;
                                                              ts_args =
                                                              [{ Ttypes.tv_name =
                                                                 a_1 }
                                                                ];
                                                              ts_alias = None },
                                                            [{ Ttypes.ty_node =
                                                               (Ttypes.Tyvar
                                                                  { Ttypes.tv_name =
                                                                    a_1 })
                                                               }
                                                              ]
                                                            ))
                                                         });
                                                 ls_constr = true;
                                                 ls_field = false },
                                               []));
                                            p_ty =
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = list;
                                                   ts_args =
                                                   [{ Ttypes.tv_name = a_1 }];
                                                   ts_alias = None },
                                                 [{ Ttypes.ty_node =
                                                    (Ttypes.Tyapp (
                                                       { Ttypes.ts_ident = int;
                                                         ts_args = [];
                                                         ts_alias = None },
                                                       []))
                                                    }
                                                   ]
                                                 ))
                                              };
                                            p_loc = foo.mli:25:18 }
                                          ]
                                        ));
                                     p_ty =
                                     { Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = list;
                                            ts_args =
                                            [{ Ttypes.tv_name = a_1 }];
                                            ts_alias = None },
                                          [{ Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = int;
                                                  ts_args = []; ts_alias = None
                                                  },
                                                []))
                                             }
                                            ]
                                          ))
                                       };
                                     p_loc = foo.mli:25:13 }
                                   ));
                                p_ty =
                                { Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = list;
                                       ts_args = [{ Ttypes.tv_name = a_1 }];
                                       ts_alias = None },
                                     [{ Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = int;
                                             ts_args = []; ts_alias = None },
                                           []))
                                        }
                                       ]
                                     ))
                                  };
                                p_loc = foo.mli:25:8 },
                              None,
                              { Tterm.t_node =
                                (Tterm.Tapp (
                                   { Symbols.ls_name = true; ls_args = [];
                                     ls_value =
                                     (Some { Ttypes.ty_node =
                                             (Ttypes.Tyapp (
                                                { Ttypes.ts_ident = bool;
                                                  ts_args = []; ts_alias = None
                                                  },
                                                []))
                                             });
                                     ls_constr = true; ls_field = false },
                                   []));
                                t_ty =
                                (Some { Ttypes.ty_node =
                                        (Ttypes.Tyapp (
                                           { Ttypes.ts_ident = bool;
                                             ts_args = []; ts_alias = None },
                                           []))
                                        });
                                t_attrs = []; t_loc = foo.mli:25:24 });
                              ({ Tterm.p_node =
                                 (Tterm.Papp (
                                    { Symbols.ls_name = infix ::;
                                      ls_args =
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a_1 })
                                         };
                                        { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = list;
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
                                               ts_alias = None },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a_1 })
                                                }
                                               ]
                                             ))
                                          }
                                        ];
                                      ls_value =
                                      (Some { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = list;
                                                   ts_args =
                                                   [{ Ttypes.tv_name = a_1 }];
                                                   ts_alias = None },
                                                 [{ Ttypes.ty_node =
                                                    (Ttypes.Tyvar
                                                       { Ttypes.tv_name = a_1 })
                                                    }
                                                   ]
                                                 ))
                                              });
                                      ls_constr = true; ls_field = false },
                                    [{ Tterm.p_node =
                                       (Tterm.Pvar
                                          { Symbols.vs_name = h;
                                            vs_ty =
                                            { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = int;
                                                   ts_args = [];
                                                   ts_alias = None },
                                                 []))
                                              }
                                            });
                                       p_ty =
                                       { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = int;
                                              ts_args = []; ts_alias = None },
                                            []))
                                         };
                                       p_loc = foo.mli:26:8 };
                                      { Tterm.p_node =
                                        (Tterm.Pas (
                                           { Tterm.p_node =
                                             (Tterm.Papp (
                                                { Symbols.ls_name = infix ::;
                                                  ls_args =
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyvar
                                                        { Ttypes.tv_name = a_1
                                                          })
                                                     };
                                                    { Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident =
                                                           list;
                                                           ts_args =
                                                           [{ Ttypes.tv_name =
                                                              a_1 }
                                                             ];
                                                           ts_alias = None },
                                                         [{ Ttypes.ty_node =
                                                            (Ttypes.Tyvar
                                                               { Ttypes.tv_name =
                                                                 a_1 })
                                                            }
                                                           ]
                                                         ))
                                                      }
                                                    ];
                                                  ls_value =
                                                  (Some { Ttypes.ty_node =
                                                          (Ttypes.Tyapp (
                                                             { Ttypes.ts_ident =
                                                               list;
                                                               ts_args =
                                                               [{ Ttypes.tv_name =
                                                                  a_1 }
                                                                 ];
                                                               ts_alias = None
                                                               },
                                                             [{ Ttypes.ty_node =
                                                                (Ttypes.Tyvar
                                                                   { Ttypes.tv_name =
                                                                     a_1 })
                                                                }
                                                               ]
                                                             ))
                                                          });
                                                  ls_constr = true;
                                                  ls_field = false },
                                                [{ Tterm.p_node =
                                                   (Tterm.Pvar
                                                      { Symbols.vs_name = y;
                                                        vs_ty =
                                                        { Ttypes.ty_node =
                                                          (Ttypes.Tyapp (
                                                             { Ttypes.ts_ident =
                                                               int;
                                                               ts_args = [];
                                                               ts_alias = None
                                                               },
                                                             []))
                                                          }
                                                        });
                                                   p_ty =
                                                   { Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident = int;
                                                          ts_args = [];
                                                          ts_alias = None },
                                                        []))
                                                     };
                                                   p_loc = foo.mli:26:14 };
                                                  { Tterm.p_node = Tterm.Pwild;
                                                    p_ty =
                                                    { Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident =
                                                           list;
                                                           ts_args =
                                                           [{ Ttypes.tv_name =
                                                              a_1 }
                                                             ];
                                                           ts_alias = None },
                                                         [{ Ttypes.ty_node =
                                                            (Ttypes.Tyapp (
                                                               { Ttypes.ts_ident =
                                                                 int;
                                                                 ts_args = [];
                                                                 ts_alias =
                                                                 None },
                                                               []))
                                                            }
                                                           ]
                                                         ))
                                                      };
                                                    p_loc = foo.mli:26:19 }
                                                  ]
                                                ));
                                             p_ty =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = list;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a_1 }];
                                                    ts_alias = None },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident = int;
                                                          ts_args = [];
                                                          ts_alias = None },
                                                        []))
                                                     }
                                                    ]
                                                  ))
                                               };
                                             p_loc = foo.mli:26:14 },
                                           { Symbols.vs_name = t_2;
                                             vs_ty =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = list;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a_1 }];
                                                    ts_alias = None },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident = int;
                                                          ts_args = [];
                                                          ts_alias = None },
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
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
                                               ts_alias = None },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = int;
                                                     ts_args = [];
                                                     ts_alias = None },
                                                   []))
                                                }
                                               ]
                                             ))
                                          };
                                        p_loc = foo.mli:26:13 }
                                      ]
                                    ));
                                 p_ty =
                                 { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = list;
                                        ts_args = [{ Ttypes.tv_name = a_1 }];
                                        ts_alias = None },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = int;
                                              ts_args = []; ts_alias = None },
                                            []))
                                         }
                                        ]
                                      ))
                                   };
                                 p_loc = foo.mli:26:8 },
                               None,
                               { Tterm.t_node =
                                 (Tterm.Tif (
                                    { Tterm.t_node =
                                      (Tterm.Tbinop (Tterm.Tand,
                                         { Tterm.t_node =
                                           (Tterm.Tapp (
                                              { Symbols.ls_name = infix <=;
                                                ls_args =
                                                [{ Ttypes.ty_node =
                                                   (Ttypes.Tyapp (
                                                      { Ttypes.ts_ident =
                                                        integer; ts_args = [];
                                                        ts_alias = None },
                                                      []))
                                                   };
                                                  { Ttypes.ty_node =
                                                    (Ttypes.Tyapp (
                                                       { Ttypes.ts_ident =
                                                         integer; ts_args = [];
                                                         ts_alias = None },
                                                       []))
                                                    }
                                                  ];
                                                ls_value = None;
                                                ls_constr = false;
                                                ls_field = false },
                                              [{ Tterm.t_node =
                                                 (Tterm.Tapp (
                                                    { Symbols.ls_name =
                                                      integer_of_int;
                                                      ls_args =
                                                      [{ Ttypes.ty_node =
                                                         (Ttypes.Tyapp (
                                                            { Ttypes.ts_ident =
                                                              int;
                                                              ts_args = [];
                                                              ts_alias = None },
                                                            []))
                                                         }
                                                        ];
                                                      ls_value =
                                                      (Some { Ttypes.ty_node =
                                                              (Ttypes.Tyapp (
                                                                 { Ttypes.ts_ident =
                                                                   integer;
                                                                   ts_args = [];
                                                                   ts_alias =
                                                                   None },
                                                                 []))
                                                              });
                                                      ls_constr = false;
                                                      ls_field = false },
                                                    [{ Tterm.t_node =
                                                       (Tterm.Tvar
                                                          { Symbols.vs_name = h;
                                                            vs_ty =
                                                            { Ttypes.ty_node =
                                                              (Ttypes.Tyapp (
                                                                 { Ttypes.ts_ident =
                                                                   int;
                                                                   ts_args = [];
                                                                   ts_alias =
                                                                   None },
                                                                 []))
                                                              }
                                                            });
                                                       t_ty =
                                                       (Some { Ttypes.ty_node =
                                                               (Ttypes.Tyapp (
                                                                  { Ttypes.ts_ident =
                                                                    int;
                                                                    ts_args =
                                                                    [];
                                                                    ts_alias =
                                                                    None },
                                                                  []))
                                                               });
                                                       t_attrs = [];
                                                       t_loc = foo.mli:26:30 }
                                                      ]
                                                    ));
                                                 t_ty =
                                                 (Some { Ttypes.ty_node =
                                                         (Ttypes.Tyapp (
                                                            { Ttypes.ts_ident =
                                                              integer;
                                                              ts_args = [];
                                                              ts_alias = None },
                                                            []))
                                                         });
                                                 t_attrs = [];
                                                 t_loc = foo.mli:26:30 };
                                                { Tterm.t_node =
                                                  (Tterm.Tapp (
                                                     { Symbols.ls_name =
                                                       integer_of_int;
                                                       ls_args =
                                                       [{ Ttypes.ty_node =
                                                          (Ttypes.Tyapp (
                                                             { Ttypes.ts_ident =
                                                               int;
                                                               ts_args = [];
                                                               ts_alias = None
                                                               },
                                                             []))
                                                          }
                                                         ];
                                                       ls_value =
                                                       (Some { Ttypes.ty_node =
                                                               (Ttypes.Tyapp (
                                                                  { Ttypes.ts_ident =
                                                                    integer;
                                                                    ts_args =
                                                                    [];
                                                                    ts_alias =
                                                                    None },
                                                                  []))
                                                               });
                                                       ls_constr = false;
                                                       ls_field = false },
                                                     [{ Tterm.t_node =
                                                        (Tterm.Tvar
                                                           { Symbols.vs_name =
                                                             y;
                                                             vs_ty =
                                                             { Ttypes.ty_node =
                                                               (Ttypes.Tyapp (
                                                                  { Ttypes.ts_ident =
                                                                    int;
                                                                    ts_args =
                                                                    [];
                                                                    ts_alias =
                                                                    None },
                                                                  []))
                                                               }
                                                             });
                                                        t_ty =
                                                        (Some { Ttypes.ty_node =
                                                                (Ttypes.Tyapp (
                                                                   { Ttypes.ts_ident =
                                                                     int;
                                                                     ts_args =
                                                                     [];
                                                                     ts_alias =
                                                                     None },
                                                                   []))
                                                                });
                                                        t_attrs = [];
                                                        t_loc = foo.mli:26:35 }
                                                       ]
                                                     ));
                                                  t_ty =
                                                  (Some { Ttypes.ty_node =
                                                          (Ttypes.Tyapp (
                                                             { Ttypes.ts_ident =
                                                               integer;
                                                               ts_args = [];
                                                               ts_alias = None
                                                               },
                                                             []))
                                                          });
                                                  t_attrs = [];
                                                  t_loc = foo.mli:26:35 }
                                                ]
                                              ));
                                           t_ty = None; t_attrs = [];
                                           t_loc = foo.mli:26:30 },
                                         { Tterm.t_node =
                                           (Tterm.Tapp (
                                              { Symbols.ls_name =
                                                is_sorted_list;
                                                ls_args =
                                                [{ Ttypes.ty_node =
                                                   (Ttypes.Tyapp (
                                                      { Ttypes.ts_ident = list;
                                                        ts_args =
                                                        [{ Ttypes.tv_name = a_1
                                                           }
                                                          ];
                                                        ts_alias = None },
                                                      [{ Ttypes.ty_node =
                                                         (Ttypes.Tyapp (
                                                            { Ttypes.ts_ident =
                                                              int;
                                                              ts_args = [];
                                                              ts_alias = None },
                                                            []))
                                                         }
                                                        ]
                                                      ))
                                                   }
                                                  ];
                                                ls_value = None;
                                                ls_constr = false;
                                                ls_field = false },
                                              [{ Tterm.t_node =
                                                 (Tterm.Tvar
                                                    { Symbols.vs_name = t_2;
                                                      vs_ty =
                                                      { Ttypes.ty_node =
                                                        (Ttypes.Tyapp (
                                                           { Ttypes.ts_ident =
                                                             list;
                                                             ts_args =
                                                             [{ Ttypes.tv_name =
                                                                a_1 }
                                                               ];
                                                             ts_alias = None },
                                                           [{ Ttypes.ty_node =
                                                              (Ttypes.Tyapp (
                                                                 { Ttypes.ts_ident =
                                                                   int;
                                                                   ts_args = [];
                                                                   ts_alias =
                                                                   None },
                                                                 []))
                                                              }
                                                             ]
                                                           ))
                                                        }
                                                      });
                                                 t_ty =
                                                 (Some { Ttypes.ty_node =
                                                         (Ttypes.Tyapp (
                                                            { Ttypes.ts_ident =
                                                              list;
                                                              ts_args =
                                                              [{ Ttypes.tv_name =
                                                                 a_1 }
                                                                ];
                                                              ts_alias = None },
                                                            [{ Ttypes.ty_node =
                                                               (Ttypes.Tyapp (
                                                                  { Ttypes.ts_ident =
                                                                    int;
                                                                    ts_args =
                                                                    [];
                                                                    ts_alias =
                                                                    None },
                                                                  []))
                                                               }
                                                              ]
                                                            ))
                                                         });
                                                 t_attrs = [];
                                                 t_loc = foo.mli:26:55 }
                                                ]
                                              ));
                                           t_ty = None; t_attrs = [];
                                           t_loc = foo.mli:26:40 }
                                         ));
                                      t_ty = None; t_attrs = [];
                                      t_loc = foo.mli:26:30 },
                                    { Tterm.t_node =
                                      (Tterm.Tapp (
                                         { Symbols.ls_name = true;
                                           ls_args = [];
                                           ls_value =
                                           (Some { Ttypes.ty_node =
                                                   (Ttypes.Tyapp (
                                                      { Ttypes.ts_ident = bool;
                                                        ts_args = [];
                                                        ts_alias = None },
                                                      []))
                                                   });
                                           ls_constr = true; ls_field = false },
                                         []));
                                      t_ty =
                                      (Some { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = bool;
                                                   ts_args = [];
                                                   ts_alias = None },
                                                 []))
                                              });
                                      t_attrs = []; t_loc = foo.mli:26:30 },
                                    { Tterm.t_node =
                                      (Tterm.Tapp (
                                         { Symbols.ls_name = false;
                                           ls_args = [];
                                           ls_value =
                                           (Some { Ttypes.ty_node =
                                                   (Ttypes.Tyapp (
                                                      { Ttypes.ts_ident = bool;
                                                        ts_args = [];
                                                        ts_alias = None },
                                                      []))
                                                   });
                                           ls_constr = true; ls_field = false },
                                         []));
                                      t_ty =
                                      (Some { Ttypes.ty_node =
                                              (Ttypes.Tyapp (
                                                 { Ttypes.ts_ident = bool;
                                                   ts_args = [];
                                                   ts_alias = None },
                                                 []))
                                              });
                                      t_attrs = []; t_loc = foo.mli:26:30 }
                                    ));
                                 t_ty =
                                 (Some { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = bool;
                                              ts_args = []; ts_alias = None },
                                            []))
                                         });
                                 t_attrs = []; t_loc = foo.mli:26:30 })
                              ]
                            ));
                         t_ty =
                         (Some { Ttypes.ty_node =
                                 (Ttypes.Tyapp (
                                    { Ttypes.ts_ident = bool; ts_args = [];
                                      ts_alias = None },
                                    []))
                                 });
                         t_attrs = []; t_loc = foo.mli:24:49 };
                        { Tterm.t_node =
                          (Tterm.Tapp (
                             { Symbols.ls_name = true; ls_args = [];
                               ls_value =
                               (Some { Ttypes.ty_node =
                                       (Ttypes.Tyapp (
                                          { Ttypes.ts_ident = bool;
                                            ts_args = []; ts_alias = None },
                                          []))
                                       });
                               ls_constr = true; ls_field = false },
                             []));
                          t_ty =
                          (Some { Ttypes.ty_node =
                                  (Ttypes.Tyapp (
                                     { Ttypes.ts_ident = bool; ts_args = [];
                                       ts_alias = None },
                                     []))
                                  });
                          t_attrs = []; t_loc = foo.mli:24:49 }
                        ]
                      ));
                   t_ty = None; t_attrs = []; t_loc = foo.mli:24:49 });
           fun_spec = None;
           fun_text =
           " predicate rec is_sorted_list (l: int list) = match l with\n      | [] | _ :: [] -> true\n      | h :: (y :: _ as t) -> h <= y /\\ is_sorted_list t ";
           fun_loc = foo.mli:24:4 });
      sig_loc = foo.mli:24:4 };
    { Tast.sig_desc =
      (Tast.Sig_val (
         { Tast.vd_name = add; vd_type = 'a -> 'a t -> unit; vd_prim = [];
           vd_attrs = <attributes>;
           vd_args =
           [(Tast.Lnone
               { Symbols.vs_name = a_4;
                 vs_ty =
                 { Ttypes.ty_node = (Ttypes.Tyvar { Ttypes.tv_name = a }) } });
             (Tast.Lnone
                { Symbols.vs_name = t_3;
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
                       { Symbols.vs_name = a_4;
                         vs_ty =
                         { Ttypes.ty_node =
                           (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                         });
                     (Tast.Lnone
                        { Symbols.vs_name = t_3;
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
                         { Symbols.ls_name = infix =;
                           ls_args =
                           [{ Ttypes.ty_node =
                              (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) };
                             { Ttypes.ty_node =
                               (Ttypes.Tyvar { Ttypes.tv_name = a_2 }) }
                             ];
                           ls_value = None; ls_constr = false; ls_field = false
                           },
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
                                 (Some { Ttypes.ty_node =
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
                                         });
                                 t_attrs = []; t_loc = foo.mli:32:12 },
                               { Symbols.ls_name = contents;
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
                                 (Some { Ttypes.ty_node =
                                         (Ttypes.Tyapp (
                                            { Ttypes.ts_ident = list;
                                              ts_args =
                                              [{ Ttypes.tv_name = a_1 }];
                                              ts_alias = None },
                                            [{ Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                              ]
                                            ))
                                         });
                                 ls_constr = false; ls_field = true }
                               ));
                            t_ty =
                            (Some { Ttypes.ty_node =
                                    (Ttypes.Tyapp (
                                       { Ttypes.ts_ident = list;
                                         ts_args = [{ Ttypes.tv_name = a_1 }];
                                         ts_alias = None },
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar { Ttypes.tv_name = a })
                                          }
                                         ]
                                       ))
                                    });
                            t_attrs = []; t_loc = foo.mli:32:12 };
                           { Tterm.t_node =
                             (Tterm.Tif (
                                { Tterm.t_node =
                                  (Tterm.Tapp (
                                     { Symbols.ls_name = infix =;
                                       ls_args =
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar
                                             { Ttypes.tv_name = a_2 })
                                          };
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyvar
                                              { Ttypes.tv_name = a_2 })
                                           }
                                         ];
                                       ls_value = None; ls_constr = false;
                                       ls_field = false },
                                     [{ Tterm.t_node =
                                        (Tterm.Tapp (
                                           { Symbols.ls_name = is_full;
                                             ls_args =
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = list;
                                                     ts_args =
                                                     [{ Ttypes.tv_name = a_1 }];
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
                                                      ts_args = [];
                                                      ts_alias = None },
                                                    []))
                                                 }
                                               ];
                                             ls_value =
                                             (Some { Ttypes.ty_node =
                                                     (Ttypes.Tyapp (
                                                        { Ttypes.ts_ident =
                                                          bool; ts_args = [];
                                                          ts_alias = None },
                                                        []))
                                                     });
                                             ls_constr = false;
                                             ls_field = false },
                                           [{ Tterm.t_node =
                                              (Tterm.Tfield (
                                                 { Tterm.t_node =
                                                   (Tterm.Tvar
                                                      { Symbols.vs_name = t_3;
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
                                                   (Some { Ttypes.ty_node =
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
                                                           });
                                                   t_attrs = [];
                                                   t_loc = foo.mli:32:36 },
                                                 { Symbols.ls_name = contents;
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
                                                   (Some { Ttypes.ty_node =
                                                           (Ttypes.Tyapp (
                                                              { Ttypes.ts_ident =
                                                                list;
                                                                ts_args =
                                                                [{ Ttypes.tv_name =
                                                                   a_1 }
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
                                                           });
                                                   ls_constr = false;
                                                   ls_field = true }
                                                 ));
                                              t_ty =
                                              (Some { Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident =
                                                           list;
                                                           ts_args =
                                                           [{ Ttypes.tv_name =
                                                              a_1 }
                                                             ];
                                                           ts_alias = None },
                                                         [{ Ttypes.ty_node =
                                                            (Ttypes.Tyvar
                                                               { Ttypes.tv_name =
                                                                 a })
                                                            }
                                                           ]
                                                         ))
                                                      });
                                              t_attrs = [];
                                              t_loc = foo.mli:32:36 };
                                             { Tterm.t_node =
                                               (Tterm.Tapp (
                                                  { Symbols.ls_name =
                                                    integer_of_int;
                                                    ls_args =
                                                    [{ Ttypes.ty_node =
                                                       (Ttypes.Tyapp (
                                                          { Ttypes.ts_ident =
                                                            int; ts_args = [];
                                                            ts_alias = None },
                                                          []))
                                                       }
                                                      ];
                                                    ls_value =
                                                    (Some { Ttypes.ty_node =
                                                            (Ttypes.Tyapp (
                                                               { Ttypes.ts_ident =
                                                                 integer;
                                                                 ts_args = [];
                                                                 ts_alias =
                                                                 None },
                                                               []))
                                                            });
                                                    ls_constr = false;
                                                    ls_field = false },
                                                  [{ Tterm.t_node =
                                                     (Tterm.Tfield (
                                                        { Tterm.t_node =
                                                          (Tterm.Tvar
                                                             { Symbols.vs_name =
                                                               t_3;
                                                               vs_ty =
                                                               { Ttypes.ty_node =
                                                                 (Ttypes.Tyapp (
                                                                    { Ttypes.ts_ident =
                                                                      t;
                                                                      ts_args =
                                                                      [{ Ttypes.tv_name =
                                                                      a }];
                                                                      ts_alias =
                                                                      None },
                                                                    [{ Ttypes.ty_node =
                                                                      (Ttypes.Tyvar
                                                                      { Ttypes.tv_name =
                                                                      a }) }]
                                                                    ))
                                                                 }
                                                               });
                                                          t_ty =
                                                          (Some { Ttypes.ty_node =
                                                                  (Ttypes.Tyapp (
                                                                     { Ttypes.ts_ident =
                                                                      t;
                                                                      ts_args =
                                                                      [{ Ttypes.tv_name =
                                                                      a }];
                                                                      ts_alias =
                                                                      None },
                                                                     [{ Ttypes.ty_node =
                                                                      (Ttypes.Tyvar
                                                                      { Ttypes.tv_name =
                                                                      a }) }]
                                                                     ))
                                                                  });
                                                          t_attrs = [];
                                                          t_loc = foo.mli:32:47
                                                          },
                                                        { Symbols.ls_name =
                                                          size;
                                                          ls_args =
                                                          [{ Ttypes.ty_node =
                                                             (Ttypes.Tyapp (
                                                                { Ttypes.ts_ident =
                                                                  t;
                                                                  ts_args =
                                                                  [{ Ttypes.tv_name =
                                                                     a }
                                                                    ];
                                                                  ts_alias =
                                                                  None },
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
                                                          (Some { Ttypes.ty_node =
                                                                  (Ttypes.Tyapp (
                                                                     { Ttypes.ts_ident =
                                                                      int;
                                                                      ts_args =
                                                                      [];
                                                                      ts_alias =
                                                                      None },
                                                                     []))
                                                                  });
                                                          ls_constr = false;
                                                          ls_field = true }
                                                        ));
                                                     t_ty =
                                                     (Some { Ttypes.ty_node =
                                                             (Ttypes.Tyapp (
                                                                { Ttypes.ts_ident =
                                                                  int;
                                                                  ts_args = [];
                                                                  ts_alias =
                                                                  None },
                                                                []))
                                                             });
                                                     t_attrs = [];
                                                     t_loc = foo.mli:32:47 }
                                                    ]
                                                  ));
                                               t_ty =
                                               (Some { Ttypes.ty_node =
                                                       (Ttypes.Tyapp (
                                                          { Ttypes.ts_ident =
                                                            integer;
                                                            ts_args = [];
                                                            ts_alias = None },
                                                          []))
                                                       });
                                               t_attrs = [];
                                               t_loc = foo.mli:32:47 }
                                             ]
                                           ));
                                        t_ty =
                                        (Some { Ttypes.ty_node =
                                                (Ttypes.Tyapp (
                                                   { Ttypes.ts_ident = bool;
                                                     ts_args = [];
                                                     ts_alias = None },
                                                   []))
                                                });
                                        t_attrs = []; t_loc = foo.mli:32:28 };
                                       { Tterm.t_node =
                                         (Tterm.Tapp (
                                            { Symbols.ls_name = true;
                                              ls_args = [];
                                              ls_value =
                                              (Some { Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident =
                                                           bool; ts_args = [];
                                                           ts_alias = None },
                                                         []))
                                                      });
                                              ls_constr = true;
                                              ls_field = false },
                                            []));
                                         t_ty =
                                         (Some { Ttypes.ty_node =
                                                 (Ttypes.Tyapp (
                                                    { Ttypes.ts_ident = bool;
                                                      ts_args = [];
                                                      ts_alias = None },
                                                    []))
                                                 });
                                         t_attrs = []; t_loc = foo.mli:32:28 }
                                       ]
                                     ));
                                  t_ty = None; t_attrs = [];
                                  t_loc = foo.mli:32:28 },
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
                                            (Some { Ttypes.ty_node =
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
                                                    });
                                            t_attrs = []; t_loc = foo.mli:33:34
                                            },
                                          { Symbols.ls_name = contents;
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
                                            (Some { Ttypes.ty_node =
                                                    (Ttypes.Tyapp (
                                                       { Ttypes.ts_ident = list;
                                                         ts_args =
                                                         [{ Ttypes.tv_name =
                                                            a_1 }
                                                           ];
                                                         ts_alias = None },
                                                       [{ Ttypes.ty_node =
                                                          (Ttypes.Tyvar
                                                             { Ttypes.tv_name =
                                                               a })
                                                          }
                                                         ]
                                                       ))
                                                    });
                                            ls_constr = false; ls_field = true
                                            }
                                          ));
                                       t_ty =
                                       (Some { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = list;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a_1 }];
                                                    ts_alias = None },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyvar
                                                        { Ttypes.tv_name = a })
                                                     }
                                                    ]
                                                  ))
                                               });
                                       t_attrs = []; t_loc = foo.mli:33:34 });
                                  t_ty =
                                  (Some { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = list;
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
                                               ts_alias = None },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a })
                                                }
                                               ]
                                             ))
                                          });
                                  t_attrs = []; t_loc = foo.mli:33:30 },
                                { Tterm.t_node =
                                  (Tterm.Tapp (
                                     { Symbols.ls_name = infix ::;
                                       ls_args =
                                       [{ Ttypes.ty_node =
                                          (Ttypes.Tyvar
                                             { Ttypes.tv_name = a_1 })
                                          };
                                         { Ttypes.ty_node =
                                           (Ttypes.Tyapp (
                                              { Ttypes.ts_ident = list;
                                                ts_args =
                                                [{ Ttypes.tv_name = a_1 }];
                                                ts_alias = None },
                                              [{ Ttypes.ty_node =
                                                 (Ttypes.Tyvar
                                                    { Ttypes.tv_name = a_1 })
                                                 }
                                                ]
                                              ))
                                           }
                                         ];
                                       ls_value =
                                       (Some { Ttypes.ty_node =
                                               (Ttypes.Tyapp (
                                                  { Ttypes.ts_ident = list;
                                                    ts_args =
                                                    [{ Ttypes.tv_name = a_1 }];
                                                    ts_alias = None },
                                                  [{ Ttypes.ty_node =
                                                     (Ttypes.Tyvar
                                                        { Ttypes.tv_name = a_1
                                                          })
                                                     }
                                                    ]
                                                  ))
                                               });
                                       ls_constr = true; ls_field = false },
                                     [{ Tterm.t_node =
                                        (Tterm.Tvar
                                           { Symbols.vs_name = a_4;
                                             vs_ty =
                                             { Ttypes.ty_node =
                                               (Ttypes.Tyvar
                                                  { Ttypes.tv_name = a })
                                               }
                                             });
                                        t_ty =
                                        (Some { Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a })
                                                });
                                        t_attrs = []; t_loc = foo.mli:34:30 };
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
                                                   (Some { Ttypes.ty_node =
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
                                                           });
                                                   t_attrs = [];
                                                   t_loc = foo.mli:34:40 },
                                                 { Symbols.ls_name = contents;
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
                                                   (Some { Ttypes.ty_node =
                                                           (Ttypes.Tyapp (
                                                              { Ttypes.ts_ident =
                                                                list;
                                                                ts_args =
                                                                [{ Ttypes.tv_name =
                                                                   a_1 }
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
                                                           });
                                                   ls_constr = false;
                                                   ls_field = true }
                                                 ));
                                              t_ty =
                                              (Some { Ttypes.ty_node =
                                                      (Ttypes.Tyapp (
                                                         { Ttypes.ts_ident =
                                                           list;
                                                           ts_args =
                                                           [{ Ttypes.tv_name =
                                                              a_1 }
                                                             ];
                                                           ts_alias = None },
                                                         [{ Ttypes.ty_node =
                                                            (Ttypes.Tyvar
                                                               { Ttypes.tv_name =
                                                                 a })
                                                            }
                                                           ]
                                                         ))
                                                      });
                                              t_attrs = [];
                                              t_loc = foo.mli:34:40 });
                                         t_ty =
                                         (Some { Ttypes.ty_node =
                                                 (Ttypes.Tyapp (
                                                    { Ttypes.ts_ident = list;
                                                      ts_args =
                                                      [{ Ttypes.tv_name = a_1 }
                                                        ];
                                                      ts_alias = None },
                                                    [{ Ttypes.ty_node =
                                                       (Ttypes.Tyvar
                                                          { Ttypes.tv_name = a
                                                            })
                                                       }
                                                      ]
                                                    ))
                                                 });
                                         t_attrs = []; t_loc = foo.mli:34:35 }
                                       ]
                                     ));
                                  t_ty =
                                  (Some { Ttypes.ty_node =
                                          (Ttypes.Tyapp (
                                             { Ttypes.ts_ident = list;
                                               ts_args =
                                               [{ Ttypes.tv_name = a_1 }];
                                               ts_alias = None },
                                             [{ Ttypes.ty_node =
                                                (Ttypes.Tyvar
                                                   { Ttypes.tv_name = a })
                                                }
                                               ]
                                             ))
                                          });
                                  t_attrs = []; t_loc = foo.mli:34:30 }
                                ));
                             t_ty =
                             (Some { Ttypes.ty_node =
                                     (Ttypes.Tyapp (
                                        { Ttypes.ts_ident = list;
                                          ts_args = [{ Ttypes.tv_name = a_1 }];
                                          ts_alias = None },
                                        [{ Ttypes.ty_node =
                                           (Ttypes.Tyvar { Ttypes.tv_name = a })
                                           }
                                          ]
                                        ))
                                     });
                             t_attrs = []; t_loc = foo.mli:32:25 }
                           ]
                         ));
                      t_ty = None; t_attrs = []; t_loc = foo.mli:32:12 }
                     ];
                   sp_xpost = [];
                   sp_wr =
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
                                       ts_alias = None },
                                     [{ Ttypes.ty_node =
                                        (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                       ]
                                     ))
                                  }
                                });
                           t_ty =
                           (Some { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = t;
                                        ts_args = [{ Ttypes.tv_name = a }];
                                        ts_alias = None },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
                                         }
                                        ]
                                      ))
                                   });
                           t_attrs = []; t_loc = foo.mli:30:13 },
                         { Symbols.ls_name = contents;
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
                           (Some { Ttypes.ty_node =
                                   (Ttypes.Tyapp (
                                      { Ttypes.ts_ident = list;
                                        ts_args = [{ Ttypes.tv_name = a_1 }];
                                        ts_alias = None },
                                      [{ Ttypes.ty_node =
                                         (Ttypes.Tyvar { Ttypes.tv_name = a })
                                         }
                                        ]
                                      ))
                                   });
                           ls_constr = false; ls_field = true }
                         ));
                      t_ty =
                      (Some { Ttypes.ty_node =
                              (Ttypes.Tyapp (
                                 { Ttypes.ts_ident = list;
                                   ts_args = [{ Ttypes.tv_name = a_1 }];
                                   ts_alias = None },
                                 [{ Ttypes.ty_node =
                                    (Ttypes.Tyvar { Ttypes.tv_name = a }) }
                                   ]
                                 ))
                              });
                      t_attrs = []; t_loc = foo.mli:30:13 }
                     ];
                   sp_cs = []; sp_diverge = false; sp_pure = false;
                   sp_equiv = [];
                   sp_text =
                   " add a t\n    modifies t.contents\n    (* comments *)\n    ensures t.contents = if is_full t.contents t.size\n                         then old t.contents\n                         else a :: (old t.contents) ";
                   sp_loc = foo.mli:29:3 });
           vd_loc = foo.mli:28:0 },
         Tast.Nonghost));
      sig_loc = foo.mli:28:0 }
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
