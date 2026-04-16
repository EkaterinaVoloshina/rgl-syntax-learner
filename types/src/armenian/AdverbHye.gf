concrete AdverbHye of Adverb = CatHye ** open Prelude,ResHye in {
  flags
    coding = "UTF-8" ;
  lin PrepNP p np = {s = case p.isPre of {
                           False => \\c => np.s ! c ++ p.s;
                           True => \\c => p.s ++ np.s ! c
                         }} ;
}