module Learner.SyntaxLearner(options,learn) where

import System.FilePath
import System.Console.GetOpt
import Learner.Config
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

options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
  , Option [] ["stopping"] (ReqArg (\s cfg->cfg{cfgSyntaxStopping=read s}) "number") "minimal accuracy"
  ]

-- | main function to learn different parts of a grammar
learn cfg = do
    (cnc,gr) <- loadGrammar ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Lang" ++".gf")
    
    trees <- fmap concat $ mapM (readCONLL cfg) (cfgTreebanks cfg)

    -- block of functions that handle CN type --
    (gr, adjCN, oneArgs) <- learnAdjCN cfg cnc gr noSmarts trees

    (adAP, twoArgs) <- learnAdAP cfg cnc gr noSmarts trees
    --(advAP, threeArgs) <- learnAdvAP lang cnc gr mapping noSmarts trees

    let allArgs = Map.empty -- TODO: Map.fromListWith (++) (concat [oneArgs, twoArgs])
    let positA = learnPositA cfg allArgs
    let useN = learnUseN cfg allArgs 

    -- block of functions that handle NP type -- 

    -- TODO: add lincats directly to cat 
    (gr, detCN, np) <- learnDetCN cfg cnc gr noSmarts trees
    (prepNP, advCN, fourArgs) <- learnAdv cfg cnc gr noSmarts trees
    
    -- block of functions that handle VP type -- 
    (gr, complSlash, verbArgs) <- learnComplSlash cfg cnc gr noSmarts trees
    (gr, predVP, verb2Args) <- learnPredVP cfg cnc gr noSmarts trees

    -- block of functions to create modules -- 
    let cat_mn     = cfgLangModuleName cfg "Cat"
        grammar_mn = cfgLangModuleName cfg "Grammar"
    cat_mo     <- lookupModule gr cat_mn
    grammar_mo <- lookupModule gr grammar_mn

    grammar_mo <-
      foldM (\lang_mo (m, funs) ->
                 do let mn = cfgLangModuleName cfg m
                        mo = getModule cfg mn (moduleNameS m) funs
                    writeFile ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg m ++ ".gf") (show mo)
                    case lookup mn (mextend lang_mo) of
                      Nothing -> return grammar_mo{mextend=(mn,MIAll):mextend lang_mo}
                      Just _  -> return grammar_mo)
            grammar_mo
            ((Map.toList . Map.fromListWith (++))
                  (catMaybes ([positA, useN, adAP, adjCN,  detCN, predVP, complSlash, prepNP, advCN])))

    writeFile ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Grammar" ++ ".gf") (show (ppModule Unqualified (grammar_mn,grammar_mo)))
    writeFile ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Cat" ++ ".gf") (show (ppModule Unqualified (cat_mn,cat_mo)))

 
addLincats cat lincats = Map.union (Map.fromList (map (\(x, y) -> (identS x, y)) lincats)) (jments cat)

lookupRes gr cfg = do
  case lookupModule gr (cfgLangModuleName cfg "Res") of 
    Ok m -> m 
    Bad f -> ModInfo {jments = Map.empty, msrc="", mstatus = MSComplete, mextend = [], mwith=Nothing, mopens=[], mexdeps=[], mflags = noOptions,  mtype = MTResource}


learnAdjCN cfg cnc gr noSmarts trees = do
    let name = identS "AdjCN"
    a_ty <- lookupResDef gr (cnc,identS "A")
    n_ty <- lookupResDef gr (cnc,identS "N")
    let n_p = QueryPattern {pos=Just ["NOUN"], rel=Nothing, morpho=Nothing, idx=identS "cn", var_type=n_ty}
        a_p = QueryPattern {pos=Just ["ADJ"], rel=Just "mod", morpho=Nothing, idx=identS "ap", var_type=a_ty}
        pattern = (n_p,a_p)
    let (_, patts) = unzip $ query trees pattern
    fun <- learnPattern cfg cnc gr noSmarts patts name pattern
    let (ap_ty, cn_ty, fields, addArgs) = combineTrees cfg gr name fun a_p n_p [idx a_p, idx n_p]
    gr <- modifyCat cfg gr [("AP",ap_ty),("CN",cn_ty)]
    return (gr, fields, addArgs)

learnComplSlash cfg cnc gr noSmarts trees = do
    let name = identS "ComplSlash"
    np_ty <- lookupResDef gr (cnc,identS "NP")
    v_ty <- lookupResDef gr (cnc,identS "V2")
    let v_p  = QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx=identS "vps", var_type=v_ty}
        np_p = QueryPattern {pos=Just ["NOUN"], rel = Just "comp:obj", morpho=Nothing, idx=identS "np", var_type=np_ty}
        pattern = (v_p,np_p)
    let (_, patts) = unzip $ query trees pattern
    fun <- learnPattern cfg cnc gr noSmarts patts name pattern
    let (_, vp_ty, fields, addArgs) = combineTrees cfg gr name fun np_p{idx=identW} v_p [idx v_p,idx np_p]
    gr <- modifyCat cfg gr [("VPSlash",v_ty),("VP",vp_ty)]
    return (gr, fields, addArgs)

learnPredVP cfg cnc gr noSmarts trees = do 
    let name = identS "PredVP"
    np_ty <- lookupResDef gr (cnc,identS "NP")
    vp_ty <- lookupResDef gr (cnc,identS "VP")
    let vp_p = QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx=identS "vp", var_type=vp_ty}
        np_p = QueryPattern {pos=Just ["NOUN"], rel=Just "subj", morpho=Nothing, idx=identS "np", var_type=np_ty}
        pattern = (vp_p,np_p)
    let (_, patts) = unzip $ query trees pattern
    fun <- learnPattern cfg cnc gr noSmarts patts name pattern
    let (_, cl_ty, fs, addArgs) = combineTrees cfg gr name fun np_p{idx=identW} vp_p [idx np_p, idx vp_p]
    gr <- modifyCat cfg gr [("Cl", cl_ty)]
    return (gr, fs, addArgs)

learnAdAP cfg cnc gr snoSmarts trees = do 
    let name = identS "AdAP"
    adv_ty <- lookupResDef gr (cnc,identS "Adv")
    ap_ty  <- lookupResDef gr (cnc,identS "AP")
    let ap_p  = QueryPattern {pos=Just ["ADJ"], rel=Nothing, morpho=Nothing, idx=identS "ap", var_type=ap_ty}
        ada_p = QueryPattern {pos=Just ["ADV"], rel=Just "mod", morpho=Nothing, idx=identS "ada", var_type=adv_ty}
        pattern = (ap_p,ada_p)
    let (_, patts) = unzip $ query trees pattern
    fun <- learnPattern cfg cnc gr noSmarts patts name pattern
    let (_, _, fields, addArgs) = combineTrees cfg gr name fun ada_p ap_p [idx ada_p, idx ap_p]
    return (fields, addArgs)

--learnAdvAP lang cnc gr mapping noSmarts trees = do 
--    let name = identS "AdvAP"
--    (fun, args) <- learn cnc gr mapping noSmarts trees name (("ADJ", Nothing),("NOUN", Just "mod")) ("ap", "adv")
--    let (fields, addArgs) = combineTrees name "ap" "adv" modmap lang fun args ["adv", "ap"]
--    return (fields, addArgs ++ args)

learnAdv cfg cnc gr noSmarts trees = do
    -- first find everything that will output Adv
    let name1 = identS "PrepNP"
    prep_ty <- lookupResDef gr (cnc,identS "Prep")
    np_ty <- lookupResDef gr (cnc,identS "NP")
    let prep_p = QueryPattern {pos= Just ["ADP"], rel=Nothing, morpho=Nothing, idx=identS "p", var_type=prep_ty}
        np_p   = QueryPattern {pos= Just ["NOUN"], rel=Just "comp:obj", morpho=Nothing, idx=identS "np", var_type=np_ty}
        pattern = (prep_p, np_p)
    let (trees', patts1) = unzip $ query trees pattern
    fun <- learnPattern cfg cnc gr noSmarts patts1 name1 pattern
    let (_, _, fields1, addArgs) = combineTrees cfg gr name1 fun prep_p np_p{var_type=defLinType} [idx prep_p, idx np_p]

    -- find AdvCN structures
    let name2 = identS "AdvCN"
    adv_ty <- lookupResDef gr (cnc,identS "Adv")
    cn_ty <- lookupResDef gr (cnc,identS "CN")
    let cn_p  = QueryPattern {pos=Just ["NOUN"], rel=Nothing, morpho=Nothing, idx=identS "cn", var_type=cn_ty}
        adv_p = QueryPattern {pos=Just ["ADP"], rel=Just "udep", morpho=Nothing, idx=identS "adv", var_type=adv_ty}
        pattern2 = (cn_p, adv_p)
    let (_, patts2) = unzip $ query trees' pattern2
    let patts3 = unionPatts (patts1, 0) (patts2, 1)
    fun <- learnPattern cfg cnc gr noSmarts patts3 name2 pattern2
    let (_, _, fields2, addArgs2) = combineTrees cfg gr name2 fun cn_p adv_p [idx cn_p,idx adv_p]

    return (fields1, fields2, addArgs2)

learnPositA lang ffs = Just ("Adjective", [getFun (identS "PositA") [a] (Vr a)])
  where
    a = identS "a"

learnUseN cfg ffs = Just ("Noun", [getFun (identS "UseN") [n] (Vr n)])
  where
    n = identS "s"

learnDetCN cfg cnc gr noSmarts trees = do
    let pattern = (QueryPattern {pos= Nothing, rel=Nothing, morpho=Nothing, idx=identW, var_type=R []},
                   QueryPattern {pos= Just ["DET"], rel = Nothing, morpho=Just [("PronType", Match "Art")], idx=identS "det", var_type=R []})
        (trees', patts) = unzip $ query trees pattern
        artPatt = nub $ map (\(x@(_,lemma1,pos,m1,_), y@(_,lemma2,_,m2,_)) -> if pos == "DET" then (lemma1, m1) else (lemma2, m2)) patts

        allFeats = nub $ (map fst (concat (map (\(Tag _ f _ _ _ _) -> f) all_tags)))

        feats = filter (\x -> x `elem` allFeats) (nub $ concatMap (\(x, y) -> map fst y) artPatt)

    let (numSg, numPl, numType) = createNum cfg gr
        (indef, def, quant_ty) = createArt cfg gr
        (dQuant, det_ty) = detQuant numType quant_ty

    let name = identS "DetCN"
    cn_ty <- lookupResDef gr (cnc,identS "CN")
    let cn_p  = QueryPattern {pos= Just ["NOUN"], rel=Nothing, morpho=Nothing, idx=identS "cn", var_type=cn_ty}
        det_p = QueryPattern {pos= Just ["DET"], rel=Just "det", morpho=Nothing, idx=identS "det", var_type=det_ty}
        pattern = (cn_p, det_p)
    let (trees', patts0) = unzip $ query trees pattern
        patts = [(n_patt,(id2,lemma2,pos2,patch morph1 morph2,rel2)) | (n_patt@(id1,lemma1,pos1,morph1,rel1),(id2,lemma2,pos2,morph2,rel2)) <- patts0]
        patch morph1 morph2 = filter (\x -> fst x /= "Definite") morph2 ++ filter (\x -> fst x == "Definite") morph1
    fun <- learnPattern cfg cnc gr noSmarts patts name pattern

    let (_,np_ty,Just (_,[detCN]),_) = combineTrees cfg gr name fun det_p{idx=identW} cn_p [idx det_p, idx cn_p]

    let lincats = [("NP", np_ty), ("Quant", quant_ty), ("Num", numType), ("Det", det_ty)]
    gr <- modifyCat cfg gr lincats

    return $ (gr, Just ("Noun", [detCN, dQuant, def, indef, numSg, numPl]), [])

modifyCat cfg gr lincats = do
  mo <- lookupModule gr cat_mn
  let lincats' = Map.toList (jments mo) ++ (map (\(x, y) -> (identS x, CncCat (Just (L NoLoc y)) Nothing Nothing Nothing Nothing)) lincats)
  return gr{moduleMap=Map.insert cat_mn mo{jments = Map.fromList lincats'} (moduleMap gr)}
  where
    cat_mn = cfgLangModuleName cfg "Cat"
