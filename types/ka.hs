import CodeGen

mapping =
  [ (Right (identS "Number",identS "Sg"),         ("Number","Sing"))
  , (Right (identS "Number",identS "Pl"),         ("Number","Plur"))
  , (Right (identS "GenNum",identS "GSg"),        ("Number","Sing"))
  , (Right (identS "GenNum",identS "GPl"),        ("Number","Plur"))
  , (Right (identS "Gender",identS "Masc"),       ("Gender","Masc"))
  , (Right (identS "Gender",identS "Fem"),        ("Gender","Fem"))
  , (Right (identS "Gender",identS "Neuter"),     ("Gender","Neut"))
  , (Right (identS "Species",identS "Indef"),     ("Definite","Ind"))
  , (Right (identS "Species",identS "Def"),       ("Definite","Def"))
  , (Right (identS "Distance",identS "Proximal"), ("Distance","Proximal"))
  , (Right (identS "Distance",identS "Distal"),   ("Distance","Distal"))
  , (Right (identS "NRelType",identS "AdvMod"),   ("RelType","Adv"))
  , (Right (identS "NRelType",identS "AdjMod"),   ("RelType","Adj"))
  , (Right (identS "NRelType",identS "Pref"),     ("RelType","Pref"))
  ]

main = do
  (cnc,gr) <- loadGrammar "../../gf-rgl/src/kazakh/LangKaz.gf"
  trees <- readCONLL "../SUD_Kazakh-KTB"
  learn cnc gr mapping noSmarts trees
