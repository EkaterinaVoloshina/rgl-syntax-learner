concrete VerbKaz of Verb = CatKaz ** open ResKaz in {
  flags
    coding = "UTF-8" ;
  lin PredVP np vp = {Infinitive = case np.isPre of {
                                     True => \\c => np.s ! c ++ vp.Infinitive;
                                     False => \\c => vp.Infinitive ++ np.s ! c
                                   };
                      Imperative_Jussive = \\f,p,c,n => np.s ! c
                                                          ++ vp.Imperative_Jussive ! p ! f ! n;
                      Subjunctive = case np.isPre of {
                                      True => \\c => vp.Subjunctive ! P3 ! Sg ++ np.s ! c;
                                      False => \\c,n,p => np.s ! c ++ vp.Subjunctive ! p ! n
                                    }} ;
  lin UseV2 np vp = {Infinitive = \\c => np.s ! c ++ vp.Infinitive;
                     Imperative_Jussive = \\c,n,f,p => np.s ! c
                                                         ++ vp.Imperative_Jussive ! p ! f ! n;
                     Subjunctive = \\c,p,n => np.s ! c ++ vp.Subjunctive ! p ! n} ;
}