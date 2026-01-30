import CodeGen
import SyntaxLearner

mapping =
  [ 
    (Left (rawIdentS "compar"),                   ("Degree","Compar"))
  , (Right (identS "Number",identS "Sg"),         ("Number","Sing"))
  , (Right (identS "Number",identS "Pl"),         ("Number","Plur"))
  , (Right (identS "Case",identS "Nom"),          ("Case","Nom"))
  , (Right (identS "Case",identS "Acc"),          ("Case","Acc"))
  , (Right (identS "Case",identS "Dat"),          ("Case","Dat"))
  , (Right (identS "Case",identS "Gen"),          ("Case","Gen"))
  , (Right (identS "Gender",identS "Masc"),       ("Gender","Masc"))
  , (Right (identS "Gender",identS "Fem"),        ("Gender","Fem"))
  , (Right (identS "Gender",identS "Neutr"),      ("Gender","Neut"))
  , (Right (identS "Species",identS "Indef"),     ("Definite","Ind"))
  , (Right (identS "Species",identS "Def"),       ("Definite","Def"))
  ]

main = do
  (cnc,gr) <- loadGrammar "../../gf-rgl/src/faroese/LangFao.gf"
  trees <- readCONLL "../SUD_Faroese-FarPaHC"
  --print trees
  learnGrammar cnc gr mapping noSmarts trees
