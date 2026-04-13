concrete VerbHye of Verb = CatHye ** open Prelude,ResHye in {
  flags
    coding = "UTF-8" ;
  lin AdvVP vps adv = {s = \\_ => adv.s ++ vps.s;
                       conditional = \\a,p,n,_ => adv.s ++ vps.conditional ! a ! p ! n;
                       converb = {imperfective = \\_ => adv.s ++ vps.converb.imperfective;
                                  futCon1 = \\_ => adv.s ++ vps.converb.futCon1;
                                  futCon2 = \\_ => adv.s ++ vps.converb.futCon2;
                                  negative = \\_ => adv.s ++ vps.converb.negative;
                                  perfective = \\_ => adv.s ++ vps.converb.perfective;
                                  simultaneous = \\_ => adv.s ++ vps.converb.simultaneous};
                       imperative = \\n => vps.imperative ! n ++ adv.s;
                       passive = \\_ => adv.s ++ vps.passive;
                       past = \\p,n,_ => adv.s ++ vps.past ! p ! n;
                       participle = \\p,_ => adv.s ++ vps.participle ! p;
                       subjunctive = \\a,p,n,_ => adv.s ++ vps.subjunctive ! a ! p ! n} ;
  lin ComplSlash vps np = {s = \\c => np.s ! c ++ vps.s;
                           conditional = \\a,p,n,c => vps.conditional ! a ! p ! n ++ np.s ! c;
                           converb = {imperfective = \\c => np.s ! c
                                                              ++ vps.converb.imperfective;
                                      futCon1 = \\c => np.s ! c ++ vps.converb.futCon1;
                                      futCon2 = \\c => np.s ! c ++ vps.converb.futCon2;
                                      negative = \\c => np.s ! c ++ vps.converb.negative;
                                      perfective = \\c => np.s ! c ++ vps.converb.perfective;
                                      simultaneous = \\c => np.s ! c ++ vps.converb.simultaneous};
                           imperative = \\n => np.s ! Nom ++ vps.imperative ! n;
                           passive = \\c => np.s ! c ++ vps.passive;
                           past = \\p,n,c => vps.past ! p ! n ++ np.s ! c;
                           participle = \\p,c => np.s ! c ++ vps.participle ! p;
                           subjunctive = \\a,p,n,c => vps.subjunctive ! a ! p ! n
                                                        ++ np.s ! c} ;
  lin SlashV2a v = v ;
}