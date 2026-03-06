concrete VerbFao of Verb = CatFao ** open ResFao in {
  flags
    coding = "UTF-8" ;
  lin PredVP np vp = {Converb = case np.isPre of {
                                  True => \\s,n => np.s ! s ! n
                                                     ! case s of {
                                                         Def => case n of {
                                                                  Pl => Nom;
                                                                  Sg => Nom
                                                                };
                                                         Indef => case n of {
                                                                    Pl => Nom;
                                                                    Sg => Nom
                                                                  }
                                                       }
                                                     ++ vp.Converb;
                                  False => \\s,n => vp.Converb
                                                      ++ np.s ! s ! n
                                                           ! case n of {
                                                               Pl => Nom;
                                                               Sg => case s of {
                                                                       Def => Nom;
                                                                       Indef => Nom
                                                                     }
                                                             }
                                };
                      Imperative_Jussive = case np.isPre of {
                                             True => \\s,n,c,n => np.s ! s ! n ! c
                                                                    ++ vp.Imperative_Jussive ! n;
                                             False => \\s,n,c,n => vp.Imperative_Jussive ! n
                                                                     ++ np.s ! s ! n ! c
                                           };
                      Indicative = case np.isPre of {
                                     True => \\s,p,n,t => np.s ! s ! n
                                                            ! case s of {
                                                                Def => case t of {
                                                                         Past => case n of {
                                                                                   Pl => Nom;
                                                                                   Sg => Nom
                                                                                 };
                                                                         Pres => case n of {
                                                                                   Pl => Nom;
                                                                                   Sg => case p of {
                                                                                           PSg P3 => Nom;
                                                                                           PPl => Nom
                                                                                         }
                                                                                 }
                                                                       };
                                                                Indef => case t of {
                                                                           Past => case p of {
                                                                                     PSg P3 => Nom;
                                                                                     PPl => case n of {
                                                                                              Pl => Nom;
                                                                                              Sg => Nom
                                                                                            }
                                                                                   };
                                                                           Pres => case p of {
                                                                                     PSg P3 => case n of {
                                                                                                 Pl => Nom;
                                                                                                 Sg => Nom
                                                                                               };
                                                                                     PPl => case n of {
                                                                                              Pl => Nom;
                                                                                              Sg => Nom
                                                                                            }
                                                                                   }
                                                                         }
                                                              }
                                                            ++ vp.Indicative ! t ! p;
                                     False => \\s,n,t,p => vp.Indicative ! t ! p
                                                             ++ np.s ! s ! n
                                                                  ! case n of {
                                                                      Pl => Nom;
                                                                      Sg => case s of {
                                                                              Def => case p of {
                                                                                       PSg P3 => Nom;
                                                                                       PPl => case t of {
                                                                                                Past => Nom;
                                                                                                Pres => Nom
                                                                                              }
                                                                                     };
                                                                              Indef => case p of {
                                                                                         PSg P3 => Nom;
                                                                                         PPl => case t of {
                                                                                                  Past => Nom;
                                                                                                  Pres => Nom
                                                                                                }
                                                                                       }
                                                                            }
                                                                    }
                                   };
                      Nonfinite = case np.isPre of {
                                    True => \\s,n => np.s ! s ! n
                                                       ! case s of {
                                                           Def => case n of {
                                                                    Pl => Nom;
                                                                    Sg => Nom
                                                                  };
                                                           Indef => case n of {
                                                                      Pl => Nom;
                                                                      Sg => Nom
                                                                    }
                                                         }
                                                       ++ vp.Nonfinite;
                                    False => \\s,n => vp.Nonfinite
                                                        ++ np.s ! s ! n
                                                             ! case n of {
                                                                 Pl => Nom;
                                                                 Sg => case s of {
                                                                         Def => Nom;
                                                                         Indef => Nom
                                                                       }
                                                               }
                                  };
                      Participle = case np.isPre of {
                                     True => \\s,n,t => np.s ! s ! n
                                                          ! case t of {
                                                              Past => case s of {
                                                                        Def => case n of {
                                                                                 Pl => Nom;
                                                                                 Sg => Nom
                                                                               };
                                                                        Indef => case n of {
                                                                                   Pl => Nom;
                                                                                   Sg => Nom
                                                                                 }
                                                                      };
                                                              Pres => case s of {
                                                                        Def => case n of {
                                                                                 Pl => Nom;
                                                                                 Sg => Nom
                                                                               };
                                                                        Indef => case n of {
                                                                                   Pl => Nom;
                                                                                   Sg => Nom
                                                                                 }
                                                                      }
                                                            }
                                                          ++ vp.Participle ! t;
                                     False => \\s,n,t => vp.Participle ! t
                                                           ++ np.s ! s ! n
                                                                ! case n of {
                                                                    Pl => Nom;
                                                                    Sg => case s of {
                                                                            Def => case t of {
                                                                                     Past => Nom;
                                                                                     Pres => Nom
                                                                                   };
                                                                            Indef => case t of {
                                                                                       Past => Nom;
                                                                                       Pres => Nom
                                                                                     }
                                                                          }
                                                                  }
                                   }} ;
  lin UseV2 np vp = {Converb = case np.isPre of {
                                 True => \\c,n,s => np.s ! s ! n ! c ++ vp.Converb;
                                 False => \\c,n,s => vp.Converb ++ np.s ! s ! n ! c
                               };
                     Imperative_Jussive = \\c,n,s,n => vp.Imperative_Jussive ! n
                                                         ++ np.s ! s ! n ! c;
                     Indicative = case np.isPre of {
                                    True => \\c,n,t,s => np.s ! s ! n ! c
                                                           ++ vp.Indicative ! t ! PPl;
                                    False => \\c,n,t,s => vp.Indicative ! t
                                                            ! case t of {
                                                                Past => case n of {
                                                                          Pl => case c of {
                                                                                  Acc => case s of {
                                                                                           Def => PPl;
                                                                                           Indef => PPl
                                                                                         };
                                                                                  Dat => PPl;
                                                                                  Nom => PPl
                                                                                };
                                                                          Sg => case s of {
                                                                                  Def => case c of {
                                                                                           Acc => PPl;
                                                                                           Dat => PPl
                                                                                         };
                                                                                  Indef => case c of {
                                                                                             Acc => PPl;
                                                                                             Dat => PPl;
                                                                                             Nom => PPl
                                                                                           }
                                                                                }
                                                                        };
                                                                Pres => case c of {
                                                                          Acc => case s of {
                                                                                   Def => case n of {
                                                                                            Pl => PPl;
                                                                                            Sg => PPl
                                                                                          };
                                                                                   Indef => case n of {
                                                                                              Pl => PPl;
                                                                                              Sg => PPl
                                                                                            }
                                                                                 };
                                                                          Dat => case n of {
                                                                                   Pl => case s of {
                                                                                           Def => PPl;
                                                                                           Indef => PPl
                                                                                         };
                                                                                   Sg => case s of {
                                                                                           Def => PPl;
                                                                                           Indef => PPl
                                                                                         }
                                                                                 };
                                                                          Nom => case n of {
                                                                                   Pl => PPl;
                                                                                   Sg => case s of {
                                                                                           Def => PPl;
                                                                                           Indef => PPl
                                                                                         }
                                                                                 }
                                                                        }
                                                              }
                                                            ++ np.s ! s ! n ! c
                                  };
                     Nonfinite = case np.isPre of {
                                   True => \\c,n,s => np.s ! s ! n ! c ++ vp.Nonfinite;
                                   False => \\c,n,s => vp.Nonfinite ++ np.s ! s ! n ! c
                                 };
                     Participle = case np.isPre of {
                                    True => \\c,n,t,s => np.s ! s ! n ! c ++ vp.Participle ! t;
                                    False => \\c,n,t,s => vp.Participle ! t ++ np.s ! s ! n ! c
                                  }} ;
}