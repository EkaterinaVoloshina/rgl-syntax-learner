concrete VerbMkd of Verb = CatMkd ** open ResMkd in {
  flags
    coding = "UTF-8" ;
  lin PredVP np vp = {present = case np.isPre of {
                                  True => \\n,a => vp.present ! a ! n ! P3 ++ np.s;
                                  False => \\a,n,p => np.s ++ vp.present ! a ! n ! p
                                };
                      aorist = case np.isPre of {
                                 True => vp.aorist ! Sg ! P3 ++ np.s;
                                 False => \\n,p => np.s ++ vp.aorist ! n ! p
                               };
                      imperfect = \\a,p => np.s
                                             ++ vp.imperfect ! a
                                                  ! case p of {
                                                      P1 => Pl;
                                                      P3 => Sg
                                                    }
                                                  ! p} ;
}