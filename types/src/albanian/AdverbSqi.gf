concrete AdverbSqi of Adverb = CatSqi ** open Prelude,ResSqi in {
  flags
    coding = "UTF-8" ;
  lin PrepNP p np = {s = \\c => p.s ++ np.s ! c} ;
}