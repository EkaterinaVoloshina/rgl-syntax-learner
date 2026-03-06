concrete NounKaz of Noun = CatKaz ** open ResKaz in {
  flags
    coding = "UTF-8" ;
  lin AdjCN ap cn = {s = \\c => ap.s ++ cn.s ! c ! Pl} ;
  lin DefArt = {s = []} ;
  lin DetCN det cn = {s = \\c => det.s ++ cn.s ! c ! Pl; n = det.n} ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n} ;
  lin IndefArt = {s = []} ;
  lin NumNP num np = {s = \\c => num.s ++ cn.s ! c ! Pl} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin PrepNP adp np = {s = case adp.isPre of {
                             True => \\c => np.s ! c ++ adp.s;
                             False => \\c => adp.s ++ np.s ! c
                           }} ;
  lin UseN n = {s = n.s} ;
}