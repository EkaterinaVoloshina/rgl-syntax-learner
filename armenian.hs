import Learner.Shell

insertDefaults cfg (idx, lemma, pos, patts, deprel) = (idx, lemma, pos, patts' pos, deprel)
    where param = map fst patts
          defs = [(["ADJ"],("Number", "Sing")), (["ADJ"],("Case", "Nom"))]
          patts' pos = patts ++ map snd (filter (\(ps, (k,v)) -> pos `elem` ps && k `notElem` param) defs)
          

main = learnerMain ((defaultConfig "hy" "hye" "armenian")
                       { cfgTreebanks = ["SUD_Armenian-ArmTDP","SUD_Armenian-BSUT"]
                       , cfgDefaults =  insertDefaults
                       })
