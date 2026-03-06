concrete VerbGla of Verb = CatGla ** open ResGla in {
  flags
    coding = "UTF-8" ;
  lin PredVP np vp = {s = case np.isPre of {
                            True => vp.s ++ np.voc;
                            False => vp.s ++ np.s
                          };
                      past = case np.isPre of {
                               True => \\v => vp.past ! v ++ np.voc;
                               False => \\v => vp.past ! v ++ np.voc
                             };
                      noun = case np.isPre of {
                               True => vp.noun ++ np.voc;
                               False => vp.noun ++ np.s
                             };
                      participle = case np.isPre of {
                                     True => vp.participle ++ np.voc;
                                     False => vp.participle ++ np.s
                                   }} ;
  lin UseV2 np vp = {s = vp.s ++ np.voc;
                     past = case np.isPre of {
                              True => \\v => vp.past ! v ++ np.voc;
                              False => \\v => vp.past ! v ++ np.voc
                            };
                     noun = vp.noun ++ np.voc; participle = vp.participle ++ np.voc} ;
}