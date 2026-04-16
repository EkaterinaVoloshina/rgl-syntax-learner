concrete NounHye of Noun = CatHye ** open Prelude,ResHye in {
  flags
    coding = "UTF-8" ;
  lin AdvCN cn adv = {s = \\c,n => adv.s ++ cn.s ! c ! n;
                      def_dat = \\n => adv.s ++ cn.def_dat ! n;
                      def_nom = \\n => adv.s ++ cn.def_nom ! n;
                      poss1 = \\c,n => adv.s ++ cn.poss1 ! c ! n;
                      poss2 = \\c,n => adv.s ++ cn.poss2 ! c ! n} ;
  lin DefArt = {s = []} ;
  lin DetCN det cn = {s = \\c => det.s ++ cn.s ! c ! det.n;
                      def_dat = det.s ++ cn.def_dat ! det.n;
                      def_nom = det.s ++ cn.def_nom ! det.n;
                      poss1 = \\c => det.s ++ cn.poss1 ! c ! det.n;
                      poss2 = \\c => det.s ++ cn.poss2 ! c ! det.n} ;
  lin DetQuant det num = {s = det.s ++ num.s; n = num.n} ;
  lin IndefArt = {s = []} ;
  lin NumPl = {s = []; n = Pl} ;
  lin NumSg = {s = []; n = Sg} ;
  lin UseN n = n ;
}