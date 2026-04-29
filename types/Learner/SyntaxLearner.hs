module Learner.SyntaxLearner(options,learn) where

import System.FilePath
import System.Console.GetOpt
import Learner.Config
import Learner.Evaluation
import Learner.CodeGen
import Learner.RGL
import qualified Data.Map as Map
import GF.Text.Pretty hiding (empty)
import GF.Grammar.Grammar hiding (Rule(..))
import Data.Char (toUpper,toLower)
import Data.Maybe
import GF.Infra.Ident
import GF.Grammar.Lookup
import GF.Grammar.Lockfield
import GF.Grammar.Macros
import Control.Monad
import Data.List
import GF.Data.Operations
import GF.Infra.Option
import GF.Grammar.Printer


-- add output folder
options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
  , Option [] ["output"] (ReqArg (\s cfg->cfg{cfgOutputFolder=s}) "DIR") "output folder"
  , Option [] ["results"] (ReqArg (\s cfg->cfg{cfgResultsFolder=s}) "DIR") "results folder"
  , Option [] ["size"] (ReqArg (\s cfg->cfg{cfgTrainSize=Just (read s)}) "number") "train size"
  , Option [] ["stopping"] (ReqArg (\s cfg->cfg{cfgSyntaxStopping=read s}) "number") "minimal accuracy"
  , Option [] ["split"] (ReqArg (\s cfg->cfg{cfgSplit=read s}) "number") "train/test split ratio"
  ]

-- | main function to learn different parts of a grammar
learn cfg = do
    (cnc,gr) <- loadGrammar ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Lang" ++".gf")

    (train, test) <- mapAndUnzipM (readCONLL cfg) (cfgTreebanks cfg)
    let (train', test') = (concat train, concat test)
    let trees = maybe (train', test') (\n -> (take n train', test')) (cfgTrainSize cfg)

    
    let res = []
    -- block of functions that handle CN type --
    (gr, adjCN, advCN, positA, useN, res) <- learnCN cfg cnc gr noSmarts trees res

    (gr, adAP, res) <- learnAdAP cfg cnc gr noSmarts trees res
    --(advAP, threeArgs) <- learnAdvAP lang cnc gr mapping noSmarts trees

    -- block of functions that handle NP type -- 
    (gr, detCN, np) <- learnDetCN cfg cnc gr noSmarts trees res
    (gr, prepNP, res) <- learnPrepNP cfg cnc gr noSmarts trees res

    -- block of functions that handle VP type -- 
    (gr, complSlash, advVP, slashV2a, res) <- learnComplSlash cfg cnc gr noSmarts trees res
    (gr, predVP, res) <- learnPredVP cfg cnc gr noSmarts trees res

    appendFile (cfgResultsFolder cfg </> cfgLangName cfg ++ "_results.txt") (
        "Train Ratio: " ++ show (fromIntegral (length train') / fromIntegral (length train' + length test')) ++ "\n"
        ++ "Train Size: " ++ show (cfgTrainSize cfg) ++ "\n"
        ++ "Stopping Threshold: " ++ show (cfgSyntaxStopping cfg) ++ "\n"
        ++ concatMap (\(id, (acc, contr,s,total)) -> showIdent id ++ "\t" ++ show acc ++ " " ++ show total ++ "\nPatterns: \n" ++ concatMap (\x -> show x ++ "\n") s ++ "\nCounterexamples: \n" ++ concatMap (\(x, y) -> show y ++ " " ++ show x ++ "\n") (count (map (\[(_,_,_,m1,_), (_,_,_,m2,_)] -> [m1, m2]) contr)) ++ "\n") res
        ++ "\n")

    -- block of functions to create modules -- 
    let cat_mn       = cfgLangModuleName cfg "Cat"
        paradigms_mn = cfgLangModuleName cfg "Paradigms"
        grammar_mn   = cfgLangModuleName cfg "Grammar"
    cat_mo     <- lookupModule gr cat_mn
    paradigms_mo <- lookupModule gr paradigms_mn
    grammar_mo <- lookupModule gr grammar_mn

    cat_mo' <- (if OSimple (moduleNameS "Prelude") `notElem` mopens cat_mo then return (cat_mo {mopens=OSimple (moduleNameS "Prelude"):mopens cat_mo}) else return cat_mo)

    grammar_mo <-
      foldM (\lang_mo (m, funs) ->
                 do let mn = cfgLangModuleName cfg m
                        mo = getModule cfg (moduleNameS m) funs
                    writeFile (cfgOutputFolder cfg </> cfgLangName cfg </> cfgLangModuleFileName cfg m ++ ".gf")
                              (show (ppModule Unqualified (mn,mo)))
                    case lookup mn (mextend lang_mo) of
                      Nothing -> return grammar_mo{mextend=(mn,MIAll):mextend lang_mo}
                      Just _  -> return grammar_mo)
            grammar_mo
            ((Map.toList . Map.fromListWith (++))
                  (catMaybes [positA, useN, adAP, adjCN, detCN, predVP, complSlash, slashV2a, prepNP, advCN, advVP]))

    writeFile (cfgOutputFolder cfg </> cfgLangName cfg </> cfgLangModuleFileName cfg "Grammar" ++ ".gf") (show (ppModule Unqualified (grammar_mn,grammar_mo)))
    writeFile (cfgOutputFolder cfg </> cfgLangName cfg </> cfgLangModuleFileName cfg "Cat" ++ ".gf") (show (ppModule Unqualified (cat_mn,cat_mo')))
    writeFile (cfgOutputFolder cfg </> cfgLangName cfg </> cfgLangModuleFileName cfg "Paradigms" ++ ".gf") (show (ppModule Unqualified (paradigms_mn,paradigms_mo)))


learnCN cfg cnc gr noSmarts trees res = do
    let (train, test) = trees

    n_ty <- lookupResDef gr (cnc,identS "N")
    let n_p = QueryPattern {pos=Just ["NOUN"], rel=Nothing, morpho=Nothing, idx=identS "cn", var_type=n_ty, lin_order=NA}

    -- find AdjCN structures
    let name1 = identS "AdjCN"
    a_ty <- lookupResDef gr (cnc,identS "A")
    let a_p = QueryPattern {pos=Just ["ADJ"], rel=Just "mod", morpho=Nothing, idx=identS "ap", var_type=a_ty, lin_order=NA}
        pattern1 = [n_p,a_p]
    let patts = query cfg train pattern1

    terms1 <- learnPattern cfg cnc gr name1 (pattern2context cfg pattern1 patts noSmarts)

    -- find AdvCN structures
    let name2 = identS "AdvCN"
    adv_ty <- lookupResDef gr (cnc,identS "Adv")
    let adv_p = QueryPattern {pos=Just ["ADP"], rel=Just "udep", morpho=Nothing, idx=identS "adv", var_type=adv_ty, lin_order=NA}
        pattern2 = [n_p, adv_p]
    let patts2 = query cfg train pattern2
    terms2 <- learnPattern cfg cnc gr name2 (pattern2context cfg pattern2 patts2 noSmarts)

    let ([ap_ty0],cn_ty) = combineTypes (terms1++terms2) [a_p] n_p
        (used_isPre, adjCN) = combineTerms gr name1 terms1 (Just (idx a_p)) n_p cn_ty [idx a_p, idx n_p]
        (_, advCN) = combineTerms gr name2 terms2 Nothing n_p cn_ty [idx n_p,idx adv_p]
        ap_ty | used_isPre = extendTypeWithIsPre ap_ty0
              | otherwise  = ap_ty0
  
    let positA = Just ("Adjective", [getFun (identS "PositA") [a] (generateTerm gr [(Vr a,a_ty)] [] ap_ty)])
                 where
                   a = identS "a"

        useN = Just ("Noun", [getFun (identS "UseN") [n] (generateTerm gr [(Vr n,n_ty)] [] cn_ty)])
               where
                 n = identS "n"

    gr <- modifyCat cfg gr [("AP",ap_ty),("CN",cn_ty)]

    let results = [(name1, eval gr cfg test pattern1 cn_ty adjCN),
                   (name2, eval gr cfg test pattern2 cn_ty advCN)] ++ res
    

    return (gr, adjCN, advCN, positA, useN, results)


learnComplSlash cfg cnc gr noSmarts trees res = do
    let (train, test) = trees
    let name1 = identS "ComplSlash"
    np_ty <- lookupResDef gr (cnc,identS "NP")
    v2_ty <- lookupResDef gr (cnc,identS "V2")
    let v2_p = QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx=identS "vp", var_type=v2_ty, lin_order=NA}
        np_p = QueryPattern {pos=Just ["NOUN"], rel = Just "comp:obj", morpho=Nothing, idx=identS "np", var_type=np_ty, lin_order=NA}
        pattern1 = [v2_p,np_p]
        patts = query cfg train pattern1
        mb_compl_ty =
          case v2_ty of
            RecType ltys -> case [ty | (l,_,ty) <- ltys, l == ident2label (identS "c2")] of
                              (ty:_) -> Just ty
                              _      -> Nothing
            _            -> Nothing
        ctxt =
          case mb_compl_ty of
            Nothing       -> pattern2context cfg pattern1 patts noSmarts
            Just compl_ty -> let c2_n (id,lemma,pos,morph,rel) = ((id,"","",[m | m@("Case",c) <- morph],""),P (Vr (idx v2_p)) c2,compl_ty)
                                 patts' = map (\[v2_n,np_n] -> [(v2_n,Vr (idx v2_p),var_type v2_p),c2_n np_n,(np_n,Vr (idx np_p),var_type np_p)]) patts
                             in CodeContext [(Vr (idx v2_p),var_type v2_p),(P (Vr (idx v2_p)) c2,compl_ty),(Vr (idx np_p),var_type np_p)]
                                            patts'
                                            noSmarts
    
    terms1 <- learnPattern cfg cnc gr name1 ctxt
    
    let name2 = identS "AdvVP"
    adv_ty <- lookupResDef gr (cnc,identS "Adv")

    v_ty <- lookupResDef gr (cnc,identS "V")
    let v_p  = QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx=identS "vp", var_type=v_ty, lin_order=NA}
        adv_p = QueryPattern {pos=Just ["ADP"], rel = Just "udep", morpho=Nothing, idx=identS "adv", var_type=adv_ty, lin_order=NA}
        pattern2 = [v_p,adv_p]
    let patts = query cfg train pattern2

    terms2 <- learnPattern cfg cnc gr name2 (pattern2context cfg pattern2 patts noSmarts)

    let (_, vp_ty) = combineTypes (terms1++terms2) [np_p] v_p
        (_, complSlash) = combineTerms gr name1 terms1 Nothing v_p vp_ty [idx v_p,idx np_p]
        (_, advVP) = combineTerms gr name2 terms2 Nothing v_p vp_ty [idx v_p,idx adv_p]

        slashV2a = Just ("Verb", [getFun (identS "SlashV2a") [v] (Vr v)])
                   where
                     v = identS "v"

    gr <- modifyCat cfg gr [("VPSlash",unLock v2_ty),("VP",vp_ty)]
    let results = [(name1, eval gr cfg test pattern1 vp_ty complSlash),
                   (name2, eval gr cfg test pattern2 vp_ty advVP)] ++ res

    return (gr, complSlash, advVP, slashV2a, results)
    where

      unLock (RecType ltys) = RecType [x | x@(l,xs,ty) <- ltys, isNothing (isLockLabel l)]
      c2 = ident2label (identS "c2")
      
      
      

learnPredVP cfg cnc gr noSmarts trees res = do
    let (train, test) = trees
    let name = identS "PredVP"
    np_ty <- lookupResDef gr (cnc,identS "NP")
    vp_ty <- lookupResDef gr (cnc,identS "VP")
    let vp_p = QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx=identS "vp", var_type=vp_ty, lin_order=NA}
        np_p = QueryPattern {pos=Just ["NOUN"], rel=Just "subj", morpho=Nothing, idx=identS "np", var_type=np_ty, lin_order=NA}
        pattern = [vp_p,np_p]
    let patts = query cfg train pattern
    fun <- learnPattern cfg cnc gr name (pattern2context cfg pattern patts noSmarts)
    
    let (_, cl_ty, _, fs) = combineOneTerms gr name fun Nothing np_p vp_p [idx np_p, idx vp_p]
    gr <- modifyCat cfg gr [("Cl", cl_ty)]



    let results = ((name, eval gr cfg test pattern cl_ty fs):res)
    return (gr, fs, results)

learnAdAP cfg cnc gr snoSmarts trees res = do
    let (train, test) = trees
    let name = identS "AdAP"
    adv_ty <- lookupResDef gr (cnc,identS "Adv")
    ap_ty  <- lookupResDef gr (cnc,identS "AP")
    let ap_p  = QueryPattern {pos=Just ["ADJ"], rel=Nothing, morpho=Nothing, idx=identS "ap", var_type=ap_ty, lin_order=NA}
        ada_p = QueryPattern {pos=Just ["ADV"], rel=Just "mod", morpho=Nothing, idx=identS "ada", var_type=adv_ty, lin_order=NA}
        pattern = [ap_p,ada_p]
    let patts = query cfg train pattern
    terms <- learnPattern cfg cnc gr name (pattern2context cfg pattern patts noSmarts)
    let (_, fun) = combineTermsWTypes gr name terms Nothing ada_p ap_p [idx ada_p, idx ap_p]
    let results = ((name, eval gr cfg test pattern ap_ty fun): res)
    return (gr, fun, results)

--learnAdvAP lang cnc gr mapping noSmarts trees = do 
--    let name = identS "AdvAP"
--    (fun, args) <- learn cnc gr mapping noSmarts trees name (("ADJ", Nothing),("NOUN", Just "mod")) ("ap", "adv")
--    let (fields, addArgs) = combineTrees name "ap" "adv" modmap lang fun args ["adv", "ap"]
--    return (fields, addArgs ++ args)

learnPrepNP cfg cnc gr noSmarts trees res = do
    -- first find everything that will output Adv
    let (train, test) = trees
    let name = identS "PrepNP"
    prep_ty <- lookupResDef gr (cnc,identS "Prep")
    np_ty <- lookupResDef gr (cnc,identS "NP")
    let prep_p = QueryPattern {pos= Just ["ADP"], rel=Nothing, morpho=Nothing, idx=identS "p", var_type=prep_ty, lin_order=NA}
        np_p   = QueryPattern {pos= Just ["NOUN"], rel=Just "comp:obj", morpho=Nothing, idx=identS "np", var_type=np_ty, lin_order=NA}
        pattern = [prep_p, np_p]
    let patts = query cfg train pattern
    let patts' = map fixCase patts
   
    terms <- learnPattern cfg cnc gr name (pattern2context cfg pattern patts' noSmarts)

    let (_, _, used_isPre, fun) = combineOneTerms gr name terms (Just (idx prep_p)) prep_p np_p{var_type=defLinType} [idx prep_p, idx np_p]
    gr <- if used_isPre
            then do gr <- modifyCat cfg gr [("Prep", extendTypeWithIsPre prep_ty)]
                    modifyDefaultIsPre cfg gr (identS "mkPrep") True
            else return gr


    let results = (name, eval gr cfg test pattern np_ty fun):res
    return (gr, fun, results)

    where
        fixCase ((idx1, lemma1, pos1, patts1, deprel1):noun@(_, _, _, patts2, _):rest) =
            ((idx1, lemma1, pos1, patts', deprel1): noun:rest)
            where patts' = patts1 ++ filter (\(x, y) -> x == "Case") patts2

lookupParam gr cfg param = case allParamValues gr (QC (MN  (identS (cfgLangModuleFileName cfg "Res")), identS param)) of
    Ok m -> Just m
    Bad msg -> Nothing

learnDetCN cfg cnc gr noSmarts trees res = do
    let (train, test) = trees
    let pattern = [QueryPattern {pos= Nothing, rel=Nothing, morpho=Nothing, idx=identW, var_type=R [], lin_order=NA},
                   QueryPattern {pos= Just ["DET"], rel = Nothing, morpho=Just [("PronType", Match ["Art"])], idx=identS "det", var_type=R [], lin_order=NA}]
        patts = query cfg train pattern
        artPatt = nub $ map (\[x@(_,lemma1,pos,m1,_), y@(_,lemma2,_,m2,_)] -> if pos == "DET" then (lemma1, m1) else (lemma2, m2)) patts

        allFeats = nub $ map fst (concatMap (\(Tag _ f _ _ _ _) -> f) all_tags)

        feats = filter (`elem` allFeats) (nub $ concatMap (\(x, y) -> map fst y) artPatt)

    let (numSg, numPl, num_ty) = createNum cfg gr
        (indef, def, quant_ty) = createArt cfg gr
        (dQuant, det_ty) = detQuant num_ty quant_ty

    cn_ty <- lookupResDef gr (cnc,identS "CN")
    let np_ty = addNum (filterNumSp cn_ty) det_ty

    let name = identS "DetCN"

    let cn_p  = QueryPattern {pos= Just ["NOUN"], rel=Nothing, morpho=Nothing, idx=identS "cn", var_type=cn_ty, lin_order=NA}
        det_p = QueryPattern {pos= Just ["DET"], rel=Just "det", morpho=Nothing, idx=identS "det", var_type=det_ty, lin_order=NA}
        pattern = [cn_p, det_p]
        patts0  = query cfg train pattern
        preorder = length [() | [(id1,lemma1,pos1,morph1,rel1),(id2,lemma2,pos2,morph2,rel2)] <- patts0, id2 < id1] > length patts0 `div` 2
        args = [(Vr (idx det_p), det_ty), (Vr (idx cn_p), cn_ty)]
        term = Abs Explicit (idx det_p) (Abs Explicit (idx cn_p) (generateTerm gr (if preorder then reverse args else args) [] np_ty))
        detCN = (name,CncFun Nothing (Just (L NoLoc term)) Nothing Nothing)

    let lincats = [("NP", np_ty), ("Quant", quant_ty), ("Num", num_ty), ("Det", det_ty)]
    gr <- modifyCat cfg gr lincats

    return (gr, Just ("Noun", [detCN, dQuant, def, indef, numSg, numPl]), [])
    where
      filterNumSp (Table (QC (_,c)) ty)
        | c == identS "Number"  = filterNumSp ty
        | c == identS "Species" = filterNumSp ty
      filterNumSp (RecType ltys) =
        RecType [(l,xs,filterNumSp ty) | (l,xs,ty) <- ltys, isNothing (isLockLabel l)]
      filterNumSp ty = composSafeOp filterNumSp ty

      addNum (RecType ltys1) (RecType ltys2) =
        RecType (ltys1 ++ [x | x@(l,_,QC (_,id)) <- ltys2, id == identS "Number"])
      addNum ty _ = ty



modifyCat cfg gr lincats = do
  mo <- lookupModule gr cat_mn
  let lincats' = Map.toList (jments mo) ++ map (\(x, y) -> (identS x, CncCat (Just (L NoLoc y)) Nothing Nothing Nothing Nothing)) lincats
  return gr{moduleMap=Map.insert cat_mn mo{jments = Map.fromList lincats'} (moduleMap gr)}
  where
    cat_mn = cfgLangModuleName cfg "Cat"


count xs = Map.toList (Map.fromListWith (+) [(x,1) | x <- xs])
