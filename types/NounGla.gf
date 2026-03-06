concrete NounGla of Noun = CatGla ** open ResGla in {
  flags
    coding = "UTF-8" ;
  lin AdjCN ap cn = {voc = \\g,n => cn.voc ! n ++ ap.voc ! g;
                     g = cn.g} ;
  lin AdvCN adv cn = {voc = case adv.isPre of {
                              True => \\n => cn.voc ! n ++ adv.voc;
                              False => \\n => cn.voc ! n ++ adv.s
                            }} ;
  lin DefArt = {s = []; sp = Def} ;
  lin DetCN det cn = {s = cn.s; g = cn.g; s = det.s; n = det.n;
                      sp = det.sp} ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n;
                          sp = det.sp} ;
  lin IndefArt = {s = []; sp = Indef} ;
  lin NumNP num np = {s = cn.s; g = cn.g} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin PrepNP adp np = {voc = \\p => adp.s ! p ++ np.voc;
                       s = \\p => adp.s ! p ++ np.s} ;
  lin UseN n = {voc = n.voc; g = n.g} ;
}