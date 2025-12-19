import CodeGen

mapping =
  [ (Left  (rawIdentS "compar"),                  ("Degree","Cmp")) 
  , (Left  (rawIdentS "voc"),                     ("Case","Voc"))
  , (Right (identS "Number",identS "Sg"),         ("Number","Sing"))
  , (Right (identS "Number",identS "Pl"),         ("Number","Plur"))
  , (Right (identS "AForm",identS "ASg"),         ("Number","Sing"))
  , (Right (identS "AForm",identS "APl"),         ("Number","Plur"))
  , (Right (identS "Gender",identS "Masc"),       ("Gender","Masc"))
  , (Right (identS "Gender",identS "Fem"),        ("Gender","Fem"))
--  , (Right (identS "Species",identS "Indef"),     ("Definite","Ind")) -- default
--  , (Right (identS "Species",identS "Def"),       ("Definite","Def"))
  , (Right (identS "Case",identS "Nom"),          ("Case","Nom"))
  , (Right (identS "Case",identS "Dat"),          ("Case","Dat"))
  , (Right (identS "Case",identS "Gen"),          ("Case","Gen"))
  ]

main = do
  (cnc,gr) <- loadGrammar "../../gf-rgl/src/gaelic/LangGla.gf"
  trees <- readCONLL "../SUD_Scottish_Gaelic-ARCOSG/"
  learn cnc gr mapping noSmarts trees
