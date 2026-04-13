concrete AdverbHye of Adverb = CatHye ** open Prelude,ResHye in {
  flags
    coding = "UTF-8" ;
  lin PrepNP p np = {s = case p.isPre of {
                           False => \\_ => np.s ! Dat ++ p.s;
                           False => \\c => np.s ! c ++ p.s
                         }} ;
}