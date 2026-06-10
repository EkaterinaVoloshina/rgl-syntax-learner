concrete VerbMkd of Verb = CatMkd ** open Prelude,ResMkd in {
  flags
    coding = "UTF-8" ;
  lin ComplSlash vps np = {present = \\a,n,p => vps.present ! a ! n
                                                  ! p
                                                  ++ np.vocative;
                           aorist = \\n,p => vps.aorist ! n ! p ++ np.vocative;
                           imperfect = \\_,_ => vps.imperfect ! Imperfective ! Pl ! P1
                                                  ++ np.vocative;
                           imperative = vps.imperative ! Perfective ! Sg ++ np.vocative;
                           participle = {aorist = \\a => vps.participle.aorist ! a ! GSg Masc
                                                           ++ np.vocative;
                                         perfect = \\a => vps.participle.perfect ! a
                                                            ++ np.vocative}} ;
  lin SlashV2a v = v ;
}
