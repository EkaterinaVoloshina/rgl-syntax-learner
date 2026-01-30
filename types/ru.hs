import CodeGen
 
 --     g = forms.g ;
 --     mayben=forms.mayben ;
 --     anim = forms.anim ;
 --     rel = forms.rel ;
 --     rt = forms.rt ;
mapping =
  [ 
    (Right (identS "Number",identS "Sg"),         ("Number","Sing"))
  , (Right (identS "Number",identS "Pl"),         ("Number","Plur"))
  , (Right (identS "GenNum",identS "GSg"),        ("Number","Sing"))
  , (Right (identS "GenNum",identS "GPl"),        ("Number","Plur"))
  , (Right (identS "Gender",identS "Masc"),       ("Gender","Masc"))
  , (Right (identS "Gender",identS "Fem"),        ("Gender","Fem"))
  , (Right (identS "Gender",identS "Neuter"),     ("Gender","Neut"))
  , (Right (identS "Str",identS "snom"),          ("Case","Nom"))
  , (Right (identS "Str",identS "sgen"),           ("Case","Gen"))
  , (Right (identS "Str",identS "sdat"),           ("Case","Dat"))
  , (Right (identS "Str",identS "sacc"),          ("Case","Acc"))
  , (Right (identS "Str",identS "sins"),          ("Case","Ins"))
  , (Right (identS "Str",identS "sprep"),          ("Case","Loc"))
  , (Right (identS "Str",identS "sptv"),          ("Case","Par"))
  --, (Right (identS "Case",identS "Loc"),          ("Case","Loc"))
  , (Right (identS "Case",identS "VocRus"),       ("Case","Voc"))
  , (Right (identS "Animacy",identS "Animate"),   ("Animacy","Anim"))
  , (Right (identS "Animacy",identS "Inanimate"), ("Animacy","Inam"))
  ]

  C (S (S (P (Vr (IC (Id {rawId2utf8 = "ap"}))) 
  (LIdent (Id {rawId2utf8 = "s"}))) (QC (IC (Id {rawId2utf8 = "ResMkd"}),IC (Id {rawId2utf8 = "Indef"})))) (QC (IC (Id {rawId2utf8 = "ResMkd"}),IC (Id {rawId2utf8 = "GPl"}))))
   (P (Vr (IC (Id {rawId2utf8 = "cn"}))) (LIdent (Id {rawId2utf8 = "count_form"})))

main = do
  (cnc,gr) <- loadGrammar "../../gf-rgl/src/russian/LangRus.gf"
  trees <- readCONLL "../SUD_Russian-SynTagRus"
  learn cnc gr mapping noSmarts trees
