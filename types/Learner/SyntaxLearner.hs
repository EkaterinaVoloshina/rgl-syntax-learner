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
    (gr, adjCN, advCN, positA, res) <- learnCN cfg cnc gr noSmarts trees res

    (gr, adAP, res) <- learnAdAP cfg cnc gr noSmarts trees res
    --(advAP, threeArgs) <- learnAdvAP lang cnc gr mapping noSmarts trees

    let useN = learnUseN cfg

    -- block of functions that handle NP type -- 


    (gr, detCN, np) <- learnDetCN cfg cnc gr noSmarts trees res
    (gr, prepNP, res) <- learnPrepNP cfg cnc gr noSmarts trees res

    -- block of functions that handle VP type -- 
    (gr, complSlash, advVP, res) <- learnComplSlash cfg cnc gr noSmarts trees res
    (gr, predVP, res) <- learnPredVP cfg cnc gr noSmarts trees res
    let slashV2a = learnSlashV2a cfg


    appendFile (cfgResultsFolder cfg </> cfgLangName cfg ++ "_results.txt") (
        "Train Ratio: " ++ show (fromIntegral (length train') / fromIntegral (length train' + length test')) ++ "\n"
        ++ "Train Size: " ++ show (cfgTrainSize cfg) ++ "\n"
        ++ "Stopping Threshold: " ++ show (cfgSyntaxStopping cfg) ++ "\n"
        ++ concatMap (\(id, r) -> showIdent id ++ "\t" ++ show r ++ "\n") res
        ++ "\n")

    -- block of functions to create modules -- 
    let cat_mn     = cfgLangModuleName cfg "Cat"
        grammar_mn = cfgLangModuleName cfg "Grammar"
    cat_mo     <- lookupModule gr cat_mn
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


learnCN cfg cnc gr noSmarts trees res = do
    let (train, test) = trees

    n_ty <- lookupResDef gr (cnc,identS "N")
    let n_p = QueryPattern {pos=Just ["NOUN"], rel=Nothing, morpho=Nothing, idx=identS "cn", var_type=n_ty, lin_order=NA}

    -- find AdjCN structures
    let name1 = identS "AdjCN"
    a_ty <- lookupResDef gr (cnc,identS "A")
    let a_p = QueryPattern {pos=Just ["ADJ"], rel=Just "mod", morpho=Nothing, idx=identS "ap", var_type=a_ty, lin_order=NA}
        pattern1 = [n_p,a_p]
    let (_, patts) = unzip $ query train pattern1
    terms1 <- learnPattern cfg cnc gr noSmarts patts name1 pattern1

    -- find AdvCN structures
    let name2 = identS "AdvCN"
    adv_ty <- lookupResDef gr (cnc,identS "Adv")
    let adv_p = QueryPattern {pos=Just ["ADP"], rel=Just "udep", morpho=Nothing, idx=identS "adv", var_type=adv_ty, lin_order=NA}
        pattern2 = [n_p, adv_p]
    let (_, patts2) = unzip $ query train pattern2
    terms2 <- learnPattern cfg cnc gr noSmarts patts2 name2 pattern2

    let ([ap_ty0],cn_ty) = combineTypes (terms1++terms2) [a_p] n_p
        (used_isPre, adjCN) = combineTerms gr name1 terms1 (Just (idx a_p)) n_p cn_ty [idx a_p, idx n_p]
        (_, advCN) = combineTerms gr name2 terms2 Nothing n_p cn_ty [idx n_p,idx adv_p]
        ap_ty | used_isPre = extendTypeWithIsPre ap_ty0
              | otherwise  = ap_ty0

    let positA = Just ("Adjective", [getFun (identS "PositA") [a] def])
                 where
                   a = identS "a"
                   def | used_isPre = extendTermWithIsPre (Vr a)
                       | otherwise  = Vr a

    gr <- modifyCat cfg gr [("AP",ap_ty),("CN",cn_ty)]

    let results = [(name1, eval gr cfg test pattern1 adjCN),
                   (name2, eval gr cfg test pattern2 advCN)] ++ res

    return (gr, adjCN, advCN, positA, results)


learnComplSlash cfg cnc gr noSmarts trees res = do
    let (train, test) = trees
    let name1 = identS "ComplSlash"
    np_ty <- lookupResDef gr (cnc,identS "NP")
    v_ty <- lookupResDef gr (cnc,identS "V")
    let v_p  = QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx=identS "vps", var_type=v_ty, lin_order=NA}
        np_p = QueryPattern {pos=Just ["NOUN"], rel = Just "comp:obj", morpho=Nothing, idx=identS "np", var_type=np_ty, lin_order=NA}
        pattern1 = [v_p,np_p]
    let (_, patts) = unzip $ query train pattern1
    terms1 <- learnPattern cfg cnc gr noSmarts patts name1 pattern1

    let name2 = identS "AdvVP"
    adv_ty <- lookupResDef gr (cnc,identS "Adv")

    let
        --v_p  = QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx=identS "vps", var_type=v_ty, lin_order=NA}
        adv_p = QueryPattern {pos=Just ["ADP"], rel = Just "udep", morpho=Nothing, idx=identS "adv", var_type=adv_ty, lin_order=NA}
        pattern2 = [v_p,adv_p]
    let (_, patts) = unzip $ query train pattern2
    terms2 <- learnPattern cfg cnc gr noSmarts patts name2 pattern2


    let (_, vp_ty) = combineTypes (terms1++terms2) [np_p] v_p
        (_, сomplSlash) = combineTerms gr name1 terms1 Nothing v_p vp_ty [idx v_p,idx np_p]
        (_, advVP) = combineTerms gr name2 terms2 Nothing v_p vp_ty [idx v_p,idx adv_p]

    gr <- modifyCat cfg gr [("VPSlash",unLock v_ty),("VP",vp_ty)]
    let results = [(name1, eval gr cfg test pattern1 сomplSlash),
                    (name2, eval gr cfg test pattern2 advVP)] ++ res
    return (gr, сomplSlash, advVP, results)
    where
      unLock (RecType ltys) = RecType [x | x@(l,xs,ty) <- ltys, isNothing (isLockLabel l)]

learnPredVP cfg cnc gr noSmarts trees res = do
    let (train, test) = trees
    let name = identS "PredVP"
    np_ty <- lookupResDef gr (cnc,identS "NP")
    vp_ty <- lookupResDef gr (cnc,identS "VP")
    let vp_p = QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx=identS "vp", var_type=vp_ty, lin_order=NA}
        np_p = QueryPattern {pos=Just ["NOUN"], rel=Just "subj", morpho=Nothing, idx=identS "np", var_type=np_ty, lin_order=NA}
        pattern = [vp_p,np_p]
    let (_, patts) = unzip $ query train pattern
    fun <- learnPattern cfg cnc gr noSmarts patts name pattern
    let (_, cl_ty, _, fs) = combineOneTerms gr name fun Nothing np_p vp_p [idx np_p, idx vp_p]
    gr <- modifyCat cfg gr [("Cl", cl_ty)]

    let results = ((name, eval gr cfg test pattern fs):res)
    return (gr, fs, results)

learnSlashV2a cfg = Just ("Verb", [getFun (identS "SlashV2a") [v] (Vr v)])
  where
    v = identS "v"

learnAdAP cfg cnc gr snoSmarts trees res = do
    let (train, test) = trees
    let name = identS "AdAP"
    adv_ty <- lookupResDef gr (cnc,identS "Adv")
    ap_ty  <- lookupResDef gr (cnc,identS "AP")
    let ap_p  = QueryPattern {pos=Just ["ADJ"], rel=Nothing, morpho=Nothing, idx=identS "ap", var_type=ap_ty, lin_order=NA}
        ada_p = QueryPattern {pos=Just ["ADV"], rel=Just "mod", morpho=Nothing, idx=identS "ada", var_type=adv_ty, lin_order=NA}
        pattern = [ap_p,ada_p]
    let (_, patts) = unzip $ query train pattern
    terms <- learnPattern cfg cnc gr noSmarts patts name pattern
    let (_, _, _, fun) = combineOneTerms gr name terms Nothing ada_p ap_p [idx ada_p, idx ap_p]
    let results = ((name, eval gr cfg test pattern fun): res)
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
    let (trees', patts1) = unzip $ query train pattern

    terms <- learnPattern cfg cnc gr noSmarts patts1 name pattern

    print terms
    let (_, _, used_isPre, fun) = combineOneTerms gr name terms (Just (idx prep_p)) prep_p np_p{var_type=defLinType} [idx prep_p, idx np_p]
    gr <- if used_isPre
            then modifyCat cfg gr [("Prep", extendTypeWithIsPre prep_ty)]
            else return gr


    let results = (name, eval gr cfg test pattern fun):res
    return (gr, fun, results)

learnUseN cfg = Just ("Noun", [getFun (identS "UseN") [n] (Vr n)])
  where
    n = identS "n"

learnDetCN cfg cnc gr noSmarts trees res = do
    let (train, test) = trees
    let pattern = [QueryPattern {pos= Nothing, rel=Nothing, morpho=Nothing, idx=identW, var_type=R [], lin_order=NA},
                   QueryPattern {pos= Just ["DET"], rel = Nothing, morpho=Just [("PronType", Match ["Art"])], idx=identS "det", var_type=R [], lin_order=NA}]
        (trees', patts) = unzip $ query train pattern
        artPatt = nub $ map (\(x@(_,lemma1,pos,m1,_), y@(_,lemma2,_,m2,_)) -> if pos == "DET" then (lemma1, m1) else (lemma2, m2)) patts

        allFeats = nub $ map fst (concatMap (\(Tag _ f _ _ _ _) -> f) all_tags)

        feats = filter (`elem` allFeats) (nub $ concatMap (\(x, y) -> map fst y) artPatt)

    let (numSg, numPl, numType) = createNum cfg gr
        (indef, def, quant_ty) = createArt cfg gr
        (dQuant, det_ty) = detQuant numType quant_ty

    let name = identS "DetCN"

    cn_ty <- lookupResDef gr (cnc,identS "CN")
    let cn_p  = QueryPattern {pos= Just ["NOUN"], rel=Nothing, morpho=Nothing, idx=identS "cn", var_type=cn_ty, lin_order=NA}
        det_p = QueryPattern {pos= Just ["DET"], rel=Just "det", morpho=Nothing, idx=identS "det", var_type=det_ty, lin_order=NA}
        pattern = [cn_p, det_p]
    let (trees', patts0) = unzip $ query train pattern
        patts = [(n_patt,(id2,lemma2,pos2,patch morph1 morph2,rel2)) | (n_patt@(id1,lemma1,pos1,morph1,rel1),(id2,lemma2,pos2,morph2,rel2)) <- patts0]
        patch morph1 morph2 = filter (\x -> fst x /= "Definite") morph2 ++ filter (\x -> fst x == "Definite") morph1
    terms <- learnPattern cfg cnc gr noSmarts patts name pattern

    let (_,np_ty,_,Just (_,[detCN])) = combineOneTerms gr name terms Nothing det_p cn_p [idx det_p, idx cn_p]

    let lincats = [("NP", np_ty), ("Quant", quant_ty), ("Num", numType), ("Det", det_ty)]
    gr <- modifyCat cfg gr lincats

    return (gr, Just ("Noun", [detCN, dQuant, def, indef, numSg, numPl]), [])


modifyCat cfg gr lincats = do
  mo <- lookupModule gr cat_mn
  let lincats' = Map.toList (jments mo) ++ map (\(x, y) -> (identS x, CncCat (Just (L NoLoc y)) Nothing Nothing Nothing Nothing)) lincats
  return gr{moduleMap=Map.insert cat_mn mo{jments = Map.fromList lincats'} (moduleMap gr)}
  where
    cat_mn = cfgLangModuleName cfg "Cat"
