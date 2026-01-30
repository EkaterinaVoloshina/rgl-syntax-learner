module SyntaxLearner where 


import qualified Data.Map as Map
import GF.Text.Pretty hiding (empty)
import GF.Grammar.Grammar hiding (Rule(..))
import Data.Maybe
import GF.Infra.Ident
import GF.Grammar.Lookup
import Control.Monad
import GF.Infra.Ident
import CodeGen
import Data.List

modmap = Map.fromList [("ap", "Adjective"), ("cn", "Noun"), ("adv", "Adverb"), ("ada", "Adverb"), 
    ("a", "Adjective"), ("n", "Noun"), ("adp", "Res")]

learnGrammar cnc gr mapping noSmarts trees = do 
    let lang = getLang cnc

    (adjCN, oneArgs) <- learnAdjCN lang cnc gr mapping noSmarts trees
    --(adAP, twoArgs) <- learnAdAP lang cnc gr mapping noSmarts trees
    --(advAP, threeArgs) <- learnAdvAP lang cnc gr mapping noSmarts trees

    let allArgs = Map.fromListWith (++) (concat [oneArgs])

    (detCN, np) <- learnDetCN lang cnc gr mapping noSmarts trees (fromJust (Map.lookup "cn" allArgs))
    (advCN, fourArgs) <- learnAdv lang cnc gr mapping noSmarts trees np
    let positA = learnPositA lang allArgs
    let useN = learnUseN lang allArgs 


    let jments = Map.fromListWith (++) (catMaybes ([positA,useN,adjCN, detCN] ++ advCN))

    forM_ (Map.toList jments) $ \(m, funs) -> do 
        let mod = getModule lang m funs
        writeFile (m ++ lang ++ ".gf") (show (pp mod))


learnAdjCN lang cnc gr mapping noSmarts trees = do 
    let name = "AdjCN"
    let pattern = (QueryPattern {pos="NOUN", rel=Nothing, morpho=Nothing, idx="cn"}, 
                    QueryPattern {pos="ADJ", rel = Just "mod", morpho=Nothing, idx="ap"})
    let (_, patts) = unzip $ query trees pattern
    (fun, args) <- learn cnc gr mapping noSmarts patts name pattern
    let argMap = Map.fromList args
    
    let (fields, addArgs) = combineTrees name "cn" "ap" modmap lang fun argMap ["ap", "cn"]
    return (fields, addArgs ++ args)

{-learnAdAP lang cnc gr mapping noSmarts trees = do 
    let name = "AdAP"
    (fun, args) <- learn cnc gr mapping noSmarts trees name (("ADJ", Nothing),("ADV", Just "mod")) ("ada", "adj")
    let (fields, addArgs) = combineTrees name "adj" "ada" modmap lang fun args ["ada", "adj"]
    return (fields, addArgs ++ args) 

--learnAdvAP lang cnc gr mapping noSmarts trees = do 
--    let name = "AdvAP"
--    (fun, args) <- learn cnc gr mapping noSmarts trees name (("ADJ", Nothing),("NOUN", Just "mod")) ("ap", "adv")
--    let (fields, addArgs) = combineTrees name "ap" "adv" modmap lang fun args ["adv", "ap"]
--    return (fields, addArgs ++ args)-}

learnAdv lang cnc gr mapping noSmarts trees np = do
    -- first find everything that will output Adv
    let name1 = "PrepNP"
    let pattern = (QueryPattern {pos="ADP", rel=Nothing, morpho=Nothing, idx="adp"}, 
                   QueryPattern {pos="NOUN", rel = Just "comp:obj", morpho=Nothing, idx="n"})
    let (trees', patts) = unzip $ query trees pattern
    (fun, args) <- learn cnc gr mapping noSmarts patts name1 pattern
    let argMap = Map.fromList args
    
    let f = Map.fromListWith (++) (getNewType np fun "n")
    let (f', _) = unzip $ [matchFields "np" "prep" "NounMkd" "s" (Map.lookup "s" f)]
    let fields = Just ("Noun", [getFun "PrepNP" ["prep", "np"] (R f')])
    
    -- find AdvCN structures
    let name2 = "AdvCN" 
    let pattern2 = (QueryPattern {pos="NOUN", rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos="ADP", rel = Just "udep", morpho=Nothing, idx="adv"})
    let (_, patts2) = unzip $ query trees' pattern2
    let patts3 = unionPatts (patts, 0) (patts2, 1)

    (fun, args) <- learn cnc gr mapping noSmarts patts3 name2 pattern2
    let argMap = Map.fromList args
    let (fields2, addArgs2) = combineTrees name2 "cn" "adv" modmap lang fun argMap ["adv", "cn"]

    return ([fields, fields2], addArgs2 ++ args)

unionPatts (patts1, p1) (patts2, p2) = filter (\p -> (getPosition p p2) `elem` patts1') patts2 
    where 
        getPosition patt 0 = fst patt
        getPosition patt 1 = snd patt

        patts1' = getPosition (unzip patts1) p1
        

learnPositA lang ffs = getPosFun lang modmap "PositA" "a" "ap" ffs

learnUseN lang ffs = getPosFun lang modmap "UseN" "n" "cn" ffs


learnDetCN lang cnc gr mapping noSmarts trees cn = do
   {-let ind_ty = case lookupResDef gr (cnc,identS "IndefArt") of
        ok r = 
    let def_ty = lookupResDef gr (cnc,identS "IndefArt") -}
    --print ind_ty
    let fields = ["g"]
    let name = "DetCN" 
    let pattern1 = (QueryPattern {pos="NOUN", rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos="NUM", rel = Just "mod", morpho=Nothing, idx="num"})
    let (trees', patts) = unzip $ query trees pattern1
    (fun, args) <- learn cnc gr mapping noSmarts patts name pattern1
    let f = Map.fromListWith (++) (getNewType fields fun "cn")
    let argMap = Map.fromList args

    let fMap = addArgs f (nub cn) fields
    
    let (f', _) = unzip $ [matchFields "cn" "det" "NounMkd" "s" (Map.lookup "s" f)]
    let numNP = getFun "NumNP" ["num", "np"] (R (f' ++ fMap))

    let pattern2 = (QueryPattern {pos="NOUN", rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos="DET", rel = Just "det", morpho=Nothing, idx="det"})
    (fun2, args2) <- learn cnc gr mapping noSmarts patts name pattern2
    let f2 = getNewType fields fun2 "cn"
    print f2
    let pattern3 = (QueryPattern {pos="NOUN", rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos="DET", rel = Just "mod", morpho=Nothing, idx="det"})

    return $ (Just ("Noun", [numSg, numPl, indefArt, defArt, defQuant, numNP, detCN cn]), (Map.keys f ++ fields))
    where 
        addArgs np cn fields = map (\x -> getOneField x "cn") (filter (\x -> (x `Map.notMember` np) && (x `elem` fields)) cn)
        numSg = getFun "NumSg" [] (R [(LIdent (rawIdentS "s"), (Nothing, Empty)), (LIdent (rawIdentS "n"), (Nothing, Cn (identS "Sg")))])
        numPl = getFun "NumPl" [] (R [(LIdent (rawIdentS "s"), (Nothing, Empty)), (LIdent (rawIdentS "n"), (Nothing, Cn (identS "Pl")))])
        -- specify defineteness 
        -- check if it exists in the morphology
        artFields = [(LIdent (rawIdentS "s"), (Nothing, Empty))]
        
        indefArt = getFun "IndefArt" [] (R artFields)
        defArt = getFun "DefArt" [] (R artFields)
        -- concat in s; copy def
        fs1 = ["s", "num"]
        fs2 = map (\(LIdent x, _) -> showRawIdent x) artFields
        defQuant = getFun "DetQuant" ["det", "num"] (R (map (\x -> concatFields x ("det", fs1) ("num", fs2)) (nub $ fs1 ++ fs2)))
        detCN cn = getFun "DetCN" ["det", "cn"] (R (map (\x -> concatFields x ("det", fs1 ++ fs2) ("cn", cn)) (nub $ fs1 ++ fs2 ++ cn)))

        concatFields f (id1, fs1) (id2, fs2) | f `elem` fs1 && f `elem` fs2 = (LIdent (rawIdentS f), (Nothing, C (P (Vr (identS id1)) (LIdent (rawIdentS f))) (P (Vr (identS id2)) (LIdent (rawIdentS f)))))
        concatFields f (id1, fs1) (id2, fs2) | f `elem` fs1 = getOneField f id1
        concatFields f (id1, fs1) (id2, fs2) | f `elem` fs2 = getOneField f id2

        getOneField f arg = (LIdent (rawIdentS f), (Nothing, P (Vr (identS arg)) (LIdent (rawIdentS f))))

