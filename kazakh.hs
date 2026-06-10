import Learner.Shell

insertDefaults cfg (idx, lemma, pos, patts, deprel) = (idx, lemma, pos, patts' pos, deprel)
    where param = map fst patts
          defs = [(["NOUN"],("Number", "Sing")), (["NOUN"],("Case", "Nom")), (["VERB"],("Polarity", "Pos"))]
          patts' pos = correct patts ++ map snd (filter (\(ps, (k,v)) -> pos `elem` ps && k `notElem` param) defs)
          correct = map (\(x, y) -> if y == "Plur,Sing" then (x, "Sing") else (x,y))

main = learnerMain ((defaultConfig "kk" "kaz" "kazakh")
                       { cfgTreebanks = ["SUD_Kazakh-KTB"],
                         cfgDefaults = insertDefaults
                       })
