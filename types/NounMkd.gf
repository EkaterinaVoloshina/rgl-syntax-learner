concrete NounMkd of Noun = CatMkd ** open ResMkd in {
  flags
    coding = "UTF-8" ;
  lin AdjCN ap cn = {s = case ap.isPre of {
                           True => \\g => cn.s ! Def Unspecified ! Sg ++ ap.s ! Indef ! g;
                           False => \\s,g => ap.s ! s ! g ++ cn.s ! Indef ! Sg
                         }} ;
  lin AdvCN adv cn = {s = \\n => cn.s
                                   ! case cn.g of {
                                       Fem => Def Unspecified;
                                       Masc => Indef;
                                       Neuter => Def Unspecified
                                     }
                                   ! n
                                   ++ adv.s;
                      g = cn.g} ;
  lin DefArt = {s = []; sp = Def Unspecified} ;
  lin DetCN det cn = {s = det.s ++ cn.s ! det.sp ! det.n; n = det.n;
                      sp = det.sp} ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n;
                          sp = det.sp} ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumNP num np = {s = \\n => num.s ++ cn.s ! Indef ! n} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin PrepNP adp np = <> ;
  lin UseN n = {s = n.s} ;
}