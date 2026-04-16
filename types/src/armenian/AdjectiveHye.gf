concrete AdjectiveHye of Adjective = CatHye ** open Prelude,ResHye in {
  lin AdAP ada ap = {s = \\c,_ => ada.s ++ ap.s ! c ! Sg;
                     poss1 = \\c,_ => ada.s ++ ap.poss1 ! c ! Sg;
                     poss2 = \\c,_ => ada.s ++ ap.poss2 ! c ! Sg; isPre = ap.isPre} ;
  lin PositA a = a ** {isPre = True} ;
}
