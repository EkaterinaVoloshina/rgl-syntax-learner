concrete NounSqi of Noun = CatSqi ** open Prelude,ResSqi in {
  flags
    coding = "UTF-8" ;
  lin AdjCN ap cn = {s = case ap.isPre of {
                           False => \\s,c,n => cn.s ! s ! c ! n ++ ap.s ! c ! cn.g ! n;
                           True => \\s,_,n => ap.s
                                                ! case s of {
                                                    Def => Acc;
                                                    Indef => Nom
                                                  }
                                                ! cn.g
                                                ! n
                                                ++ cn.s ! s
                                                     ! case cn.g of {
                                                         Fem => Acc;
                                                         Masc => Nom
                                                       }
                                                     ! n
                         };
                     g = cn.g} ;
  lin AdvCN cn adv = {s = \\s,c,n => cn.s ! s ! c ! n ++ adv.s;
                      g = cn.g} ;
  lin DefArt = {s = []; sp = Def} ;
  lin DetCN det cn = {s = \\c => det.s ++ cn.s ! det.sp ! c ! det.n;
                      g = cn.g; n = det.n} ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n;
                          sp = det.sp} ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin UseN n = {s = \\s,c,n2 => n.s ! s ! c ! n2; g = n.g} ;
}