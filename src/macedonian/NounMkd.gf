concrete NounMkd of Noun = CatMkd ** open Prelude,ResMkd in {
  flags
    coding = "UTF-8" ;
  lin AdjCN ap cn = {s = case ap.isPre of {
                           False => \\_,n => cn.s ! Def Unspecified ! n
                                               ++ ap.s ! Indef
                                                    ! case n of {
                                                        Pl => GPl;
                                                        Sg => GSg cn.g
                                                      };
                           True => \\s,n => ap.s ! s
                                              ! case cn.g of {
                                                  Masc => case n of {
                                                            Pl => GPl;
                                                            Sg => GSg Masc
                                                          };
                                                  Neuter => case n of {
                                                              Pl => GPl;
                                                              Sg => GSg Neuter
                                                            };
                                                  x => GSg x
                                                }
                                              ++ cn.s ! Indef ! n
                         };
                     count_form = ap.s ! Indef ! GPl ++ cn.count_form;
                     vocative = case ap.isPre of {
                                  True => \\n => ap.s ! Indef
                                                     ! case cn.g of {
                                                         Masc => case n of {
                                                                   Pl => GPl;
                                                                   Sg => GSg Masc
                                                                 };
                                                         Neuter => case n of {
                                                                     Pl => GPl;
                                                                     Sg => GSg Neuter
                                                                   };
                                                         x => GSg x
                                                       }
                                                     ++ cn.vocative ! n;
                                  False => \\n => cn.vocative ! n
                                                      ++ ap.s ! Indef
                                                           ! case n of {
                                                               Pl => GPl;
                                                               Sg => GSg cn.g
                                                             }
                                };
                     g = cn.g} ;
  lin AdvCN cn adv = {s = \\s,n => cn.s ! s ! n ++ adv.s;
                      count_form = cn.count_form;
                      vocative = \\n => cn.vocative ! n ++ adv.s; g = cn.g} ;
  lin DefArt = {s = []; sp = Def Unspecified} ;
  lin DetCN det cn = {s = det.s ++ cn.s ! det.sp ! det.n;
                      vocative = det.s ++ cn.vocative ! det.n} ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n;
                          sp = det.sp} ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin UseN n = n ;
}
