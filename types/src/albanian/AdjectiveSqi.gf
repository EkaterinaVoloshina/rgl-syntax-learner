concrete AdjectiveSqi of Adjective = CatSqi ** open Prelude,ResSqi in {
  flags
    coding = "UTF-8" ;
  lin AdAP ada ap = {s = \\c,g,n => ada.s ++ ap.s ! c ! g ! n;
                     isPre = ap.isPre} ;
  lin PositA a = {s = \\c,g,n => a.s ! c ! g ! n; isPre = False} ;
}