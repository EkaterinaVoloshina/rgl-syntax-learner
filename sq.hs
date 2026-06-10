import CodeGen

mapping =
  [ (Left (rawIdentS "count_form"),               ("Number","Count"))
  , (Left (rawIdentS "vocative"),                 ("Case","Voc"))
  , (Left (rawIdentS "rel"),                      ("Relative","Yes"))
  , (Left (rawIdentS "adverb"),                   ("Adverb","Yes"))
  , (Right (identS "Number",identS "Sg"),         ("Number","Sing"))
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
  (cnc,gr) <- loadGrammar "../../gf-rgl/src/albanian/LangSqi.gf"
  trees <- readCONLL "../SUD_Albanian-STAF/"
  learn cnc gr mapping noSmarts trees
