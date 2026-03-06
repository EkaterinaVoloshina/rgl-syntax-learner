concrete CatMkd of Cat = CommonX ** open ResMkd,Prelude in {
  flags
    coding = "UTF-8" ;
  lincat A = {s : Species => GenNum => Str; adverb : Str} ;
  linref A = \a -> a.s ! Indef ! GSg Masc ;
  lincat A2 = {s : Species => GenNum => Str; adverb : Str;
               c2 : {s : Str}} ;
  linref A2 = \a -> a.s ! Indef ! GSg Masc ;
  lincat ACard = {s : Str} ;
  lincat AP = {s : Str} ;
  lincat CN = {s : Species => Number => Str; count_form : Str;
               vocative : Number => Str; g : Gender} ;
  lincat Card = {s : Str} ;
  lincat Cl = {s : Str} ;
  lincat ClSlash = {s : Str} ;
  lincat Comp = {s : Str} ;
  lincat Conj = {s : Str} ;
  lincat DAP = {s : Str} ;
  lincat Decimal = {s : Str; n : Number; hasDot : Bool} ;
  lincat Det = {s : Str} ;
  lincat Digits = {s : Str; n : Number; tail : DTail} ;
  lincat GN = {s : Str} ;
  lincat IComp = {s : Str} ;
  lincat IDet = {s : Str} ;
  lincat IP = {s : Str} ;
  lincat IQuant = {s : Str} ;
  lincat Imp = {s : Str} ;
  lincat LN = {s : Str} ;
  lincat N = {s : Species => Number => Str; count_form : Str;
              vocative : Number => Str; rel : Species => GenNum => Str;
              relType : NRelType; g : Gender} ;
  linref N = \n -> n.s ! Indef ! Sg ;
  lincat N2 = {s : Species => Number => Str; count_form : Str;
               vocative : Number => Str; rel : Species => GenNum => Str;
               relType : NRelType; g : Gender; c2 : {s : Str}} ;
  linref N2 = \n -> n.s ! Indef ! Sg ;
  lincat N3 = {s : Species => Number => Str; count_form : Str;
               vocative : Number => Str; rel : Species => GenNum => Str;
               relType : NRelType; g : Gender; c2 : {s : Str}; c3 : {s : Str}} ;
  linref N3 = \n -> n.s ! Indef ! Sg ;
  lincat NP = {s : Str} ;
  lincat Num = {s : Str} ;
  lincat Numeral = {s : Str} ;
  lincat Ord = {s : Str} ;
  lincat PN = {s : Str} ;
  lincat Predet = {s : Str} ;
  lincat Prep = {s : Str} ;
  lincat Pron = {s : Role => Str; clitic : Case => Str} ;
  lincat QCl = {s : Str} ;
  lincat QS = {s : Str} ;
  lincat Quant = {s : Str} ;
  lincat RCl = {s : Str} ;
  lincat RP = {s : Str} ;
  lincat RS = {s : Str} ;
  lincat S = {s : Str} ;
  lincat SN = {s : Str} ;
  lincat SSlash = {s : Str} ;
  lincat Subj = {s : Str} ;
  lincat V = {present : Aspect => Number => Person => Str;
              aorist : Number => Person => Str;
              imperfect : Aspect => Number => Person => Str;
              imperative : Aspect => Number => Str;
              participle : {aorist : Aspect => GenNum => Str;
                            imperfect : GenNum => Str; perfect : Aspect => Str;
                            adjectival : Aspect => Str; adverbial : Str};
              noun_from_verb : Str; isRefl : Bool} ;
  linref V = \v -> v.present ! Imperfective ! Sg ! P3
                     ++ case <v.isRefl : Bool> of {
                          True => "се";
                          False => []
                        } ;
  lincat V2 = {present : Aspect => Number => Person => Str;
               aorist : Number => Person => Str;
               imperfect : Aspect => Number => Person => Str;
               imperative : Aspect => Number => Str;
               participle : {aorist : Aspect => GenNum => Str;
                             imperfect : GenNum => Str; perfect : Aspect => Str;
                             adjectival : Aspect => Str; adverbial : Str};
               noun_from_verb : Str; isRefl : Bool; c2 : {s : Str}} ;
  linref V2 = \v -> v.present ! Imperfective ! Sg ! P3
                      ++ case <v.isRefl : Bool> of {
                           True => "се";
                           False => []
                         } ;
  lincat V2A = {present : Aspect => Number => Person => Str;
                aorist : Number => Person => Str;
                imperfect : Aspect => Number => Person => Str;
                imperative : Aspect => Number => Str;
                participle : {aorist : Aspect => GenNum => Str;
                              imperfect : GenNum => Str; perfect : Aspect => Str;
                              adjectival : Aspect => Str; adverbial : Str};
                noun_from_verb : Str; isRefl : Bool; c2 : {s : Str};
                c3 : {s : Str}} ;
  linref V2A = \v -> v.present ! Imperfective ! Sg ! P3
                       ++ case <v.isRefl : Bool> of {
                            True => "се";
                            False => []
                          } ;
  lincat V2Q = {present : Aspect => Number => Person => Str;
                aorist : Number => Person => Str;
                imperfect : Aspect => Number => Person => Str;
                imperative : Aspect => Number => Str;
                participle : {aorist : Aspect => GenNum => Str;
                              imperfect : GenNum => Str; perfect : Aspect => Str;
                              adjectival : Aspect => Str; adverbial : Str};
                noun_from_verb : Str; isRefl : Bool; c2 : {s : Str}} ;
  linref V2Q = \v -> v.present ! Imperfective ! Sg ! P3
                       ++ case <v.isRefl : Bool> of {
                            True => "се";
                            False => []
                          } ;
  lincat V2S = {present : Aspect => Number => Person => Str;
                aorist : Number => Person => Str;
                imperfect : Aspect => Number => Person => Str;
                imperative : Aspect => Number => Str;
                participle : {aorist : Aspect => GenNum => Str;
                              imperfect : GenNum => Str; perfect : Aspect => Str;
                              adjectival : Aspect => Str; adverbial : Str};
                noun_from_verb : Str; isRefl : Bool; c2 : {s : Str}} ;
  linref V2S = \v -> v.present ! Imperfective ! Sg ! P3
                       ++ case <v.isRefl : Bool> of {
                            True => "се";
                            False => []
                          } ;
  lincat V2V = {present : Aspect => Number => Person => Str;
                aorist : Number => Person => Str;
                imperfect : Aspect => Number => Person => Str;
                imperative : Aspect => Number => Str;
                participle : {aorist : Aspect => GenNum => Str;
                              imperfect : GenNum => Str; perfect : Aspect => Str;
                              adjectival : Aspect => Str; adverbial : Str};
                noun_from_verb : Str; isRefl : Bool; c2 : {s : Str};
                c3 : {s : Str}} ;
  linref V2V = \v -> v.present ! Imperfective ! Sg ! P3
                       ++ case <v.isRefl : Bool> of {
                            True => "се";
                            False => []
                          } ;
  lincat V3 = {present : Aspect => Number => Person => Str;
               aorist : Number => Person => Str;
               imperfect : Aspect => Number => Person => Str;
               imperative : Aspect => Number => Str;
               participle : {aorist : Aspect => GenNum => Str;
                             imperfect : GenNum => Str; perfect : Aspect => Str;
                             adjectival : Aspect => Str; adverbial : Str};
               noun_from_verb : Str; isRefl : Bool; c2 : {s : Str};
               c3 : {s : Str}} ;
  linref V3 = \v -> v.present ! Imperfective ! Sg ! P3
                      ++ case <v.isRefl : Bool> of {
                           True => "се";
                           False => []
                         } ;
  lincat VA = {present : Aspect => Number => Person => Str;
               aorist : Number => Person => Str;
               imperfect : Aspect => Number => Person => Str;
               imperative : Aspect => Number => Str;
               participle : {aorist : Aspect => GenNum => Str;
                             imperfect : GenNum => Str; perfect : Aspect => Str;
                             adjectival : Aspect => Str; adverbial : Str};
               noun_from_verb : Str; isRefl : Bool} ;
  linref VA = \v -> v.present ! Imperfective ! Sg ! P3
                      ++ case <v.isRefl : Bool> of {
                           True => "се";
                           False => []
                         } ;
  lincat VP = {s : Str} ;
  lincat VPSlash = {s : Str} ;
  lincat VQ = {present : Aspect => Number => Person => Str;
               aorist : Number => Person => Str;
               imperfect : Aspect => Number => Person => Str;
               imperative : Aspect => Number => Str;
               participle : {aorist : Aspect => GenNum => Str;
                             imperfect : GenNum => Str; perfect : Aspect => Str;
                             adjectival : Aspect => Str; adverbial : Str};
               noun_from_verb : Str; isRefl : Bool} ;
  linref VQ = \v -> v.present ! Imperfective ! Sg ! P3
                      ++ case <v.isRefl : Bool> of {
                           True => "се";
                           False => []
                         } ;
  lincat VS = {present : Aspect => Number => Person => Str;
               aorist : Number => Person => Str;
               imperfect : Aspect => Number => Person => Str;
               imperative : Aspect => Number => Str;
               participle : {aorist : Aspect => GenNum => Str;
                             imperfect : GenNum => Str; perfect : Aspect => Str;
                             adjectival : Aspect => Str; adverbial : Str};
               noun_from_verb : Str; isRefl : Bool} ;
  linref VS = \v -> v.present ! Imperfective ! Sg ! P3
                      ++ case <v.isRefl : Bool> of {
                           True => "се";
                           False => []
                         } ;
  lincat VV = {present : Aspect => Number => Person => Str;
               aorist : Number => Person => Str;
               imperfect : Aspect => Number => Person => Str;
               imperative : Aspect => Number => Str;
               participle : {aorist : Aspect => GenNum => Str;
                             imperfect : GenNum => Str; perfect : Aspect => Str;
                             adjectival : Aspect => Str; adverbial : Str};
               noun_from_verb : Str; isRefl : Bool} ;
  linref VV = \v -> v.present ! Imperfective ! Sg ! P3
                      ++ case <v.isRefl : Bool> of {
                           True => "се";
                           False => []
                         } ;
}