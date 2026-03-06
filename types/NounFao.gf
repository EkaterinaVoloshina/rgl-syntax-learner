concrete NounFao of Noun = CatFao ** open ResFao in {
  flags
    coding = "UTF-8" ;
  lin AdjCN ap cn = {s = case ap.isPre of {
                           True => \\g => cn.s ! Def ! Sg ! Nom ++ ap.s ! g ! Sg ! Nom;
                           False => \\g,c,s,c,n,n => ap.s ! g ! n ! c ++ cn.s ! s ! n ! c
                         }} ;
  lin AdvCN adv cn = {s = \\c,n,s => cn.s ! s ! n ! c ++ adv.s} ;
  lin DefArt = {s = []; sp = Def} ;
  lin DetCN det cn = {s = \\c,s,n => det.s
                                       ++ cn.s ! det.sp ! det.n ! c;
                      n = det.n; sp = det.sp} ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n;
                          sp = det.sp} ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumNP num np = {s = case det.isPre of {
                            True => \\c => cn.s ! Indef
                                             ! case c of {
                                                 Acc => Sg;
                                                 Dat => Pl
                                               }
                                             ! c
                                             ++ num.s;
                            False => \\n,c => num.s
                                                ++ cn.s
                                                     ! case c of {
                                                         Acc => case n of {
                                                                  Pl => Indef;
                                                                  Sg => Indef
                                                                };
                                                         Dat => case n of {
                                                                  Pl => Indef;
                                                                  Sg => Indef
                                                                };
                                                         Gen => Indef;
                                                         Nom => case n of {
                                                                  Pl => Indef;
                                                                  Sg => Indef
                                                                }
                                                       }
                                                     ! n
                                                     ! c
                          }} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin PrepNP prep np = {sp = np.sp; n = np.n;
                        s = case prep.isPre of {
                              True => \\c,n,s => n.s ! s ! n ! c ++ adp.s;
                              False => \\c,s,n => adp.s ++ n.s ! s ! n ! c
                            }} ;
  lin UseN n = {s = n.s} ;
}