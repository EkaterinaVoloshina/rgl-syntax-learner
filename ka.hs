import CodeGen

mapping =
  [ (Right (identS "Case",identS "Nom"),         ("Case","Nom"))
  , (Right (identS "Case",identS "Acc"),         ("Case","Acc"))
  , (Right (identS "Case",identS "Dat"),         ("Case","Dat"))
  , (Right (identS "Case",identS "Gen"),         ("Case","Gen"))
  , (Right (identS "Case",identS "Ablat"),       ("Case","Abl")) 
  , (Right (identS "Case",identS "Instr"),       ("Case","Ins"))
  , (Right (identS "Case",identS "Loc"),         ("Case","Loc"))
  , (Right (identS "Number",identS "Pl"),        ("Number","Plur"))
  , (Right (identS "Number",identS "Sg"),        ("Number","Sing"))
  , (Right (identS "Number",identS "Pl"),        ("Number[psor]","Plur"))
  , (Right (identS "Number",identS "Sg"),        ("Number[psor]","Sing"))
  , (Right (identS "Person",identS "P1"),        ("Person[psor]","1"))
  , (Right (identS "Person",identS "P3"),        ("Person[psor]","3"))
  , (Right (identS "Person",identS "P2"),        ("Person[psor]","2"))
  , (Right (identS "Formality",identS "Formal"), ("Polite","Form"))
  -- , (Right (identS "Formality",identS "Informal")) -- default? 

  ]

main = do
  (cnc,gr) <- loadGrammar "../../gf-rgl/src/kazakh/LangKaz.gf"
  trees <- readCONLL "../SUD_Kazakh-KTB"
  learn cnc gr mapping noSmarts trees
