concrete SentenceMkd of Sentence = CatMkd ** open Prelude,ResMkd in {
  flags
    coding = "UTF-8" ;
  lin PredVP np vp = {present = \\_,n => np.vocative
                                           ++ vp.present ! Imperfective ! n ! P3;
                      aorist = \\n => np.vocative ++ vp.aorist ! n ! P3;
                      imperfect = \\n => np.vocative ++ vp.imperfect ! n ! P3;
                      participle = {aorist = \\a => np.vocative
                                                      ++ vp.participle.aorist ! a;
                                    perfect = \\a => np.vocative ++ vp.participle.perfect ! a}} ;
}