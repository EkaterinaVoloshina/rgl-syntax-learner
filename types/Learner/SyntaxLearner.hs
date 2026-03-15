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
import Control.Monad
import Data.List
import GF.Data.Operations
import GF.Infra.Option
import GF.Grammar.Printer

options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
  , Option [] ["stopping"] (ReqArg (\s cfg->cfg{cfgSyntaxStopping=read s}) "number") "minimal accuracy"
  ]

-- | Map of modules where each type and its related functions are defined
modmap = Map.fromList [("ap", "Adjective"), 
                       ("cn", "Noun"), 
                       ("adv", "Adverb"), 
                       ("ada", "Adverb"), 
                       ("a", "Adjective"), 
                       ("n", "Noun"), 
                       ("adp", "Res"),
                       ("vp", "Verb"),
                       ("np", "Noun")]

-- | main function to learn different parts of a grammar
learn cfg = do
    (cnc,gr) <- loadGrammar ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Lang" ++".gf")
    
    trees <- fmap concat $ mapM (readCONLL cfg) (cfgTreebanks cfg)

    -- block of functions that handle CN type --
    (gr, adjCN, oneArgs) <- learnAdjCN cfg cnc gr noSmarts trees

    (adAP, twoArgs) <- learnAdAP cfg cnc gr noSmarts trees
    --(advAP, threeArgs) <- learnAdvAP lang cnc gr mapping noSmarts trees

    let allArgs = Map.fromListWith (++) (concat [oneArgs, twoArgs])
    let positA = learnPositA cfg allArgs
    let useN = learnUseN cfg allArgs 

    -- block of functions that handle NP type -- 

    -- TODO: add lincats directly to cat 
    (gr, detCN, np) <- learnDetCN cfg cnc gr noSmarts trees
    (advCN, fourArgs) <- learnAdv cfg cnc gr noSmarts trees
    
    -- block of functions that handle VP type -- 
    (v2, verbArgs) <- learnV2 cfg cnc gr noSmarts trees
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
                  (catMaybes ([positA, useN, adAP, adjCN,  detCN, predVP, v2] ++ advCN)))

    writeFile ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Grammar" ++ ".gf") (show (ppModule Unqualified (grammar_mn,grammar_mo)))
    writeFile ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Cat" ++ ".gf") (show (ppModule Unqualified (cat_mn,cat_mo)))

 
addLincats cat lincats = Map.union (Map.fromList (map (\(x, y) -> (identS x, y)) lincats)) (jments cat)

lookupRes gr cfg = do
  case lookupModule gr (cfgLangModuleName cfg "Res") of 
    Ok m -> m 
    Bad f -> ModInfo {jments = Map.empty, msrc="", mstatus = MSComplete, mextend = [], mwith=Nothing, mopens=[], mexdeps=[], mflags = noOptions,  mtype = MTResource}


learnAdjCN cfg cnc gr noSmarts trees = do
    let name = "AdjCN"
    let pattern = (QueryPattern {pos=Just ["NOUN"], rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos=Just ["ADJ"], rel=Just "mod", morpho=Nothing, idx="ap"})
    let (_, patts) = unzip $ query trees pattern
    let (pos1, pos2) = mapPOS pattern
    a_ty <- lookupResDef gr (cnc,identS pos2)
    n_ty <- lookupResDef gr (cnc,identS pos1)
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern 0 (a_ty, n_ty)
    gr <- modifyCat cfg gr [("AP",a_ty),("CN", lincat)]
    let (fields, addArgs) = combineTrees cfg name "cn" "ap" modmap fun (Map.fromList args) ["ap", "cn"]
    return (gr, fields, addArgs ++ args)

learnV2 cfg cnc gr noSmarts trees = do 
    let name = "UseV2"
    let pattern = ( 
                    QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx="vp"},
                    QueryPattern {pos=Just ["NOUN", "PRON"], rel = Just "comp:obj", morpho=Nothing, idx="np"})
    let (_, patts) = unzip $ query trees pattern
    let (pos1, pos2) = mapPOS pattern
    np <- lookupResDef gr (cnc,identS "NP")
    v_ty <- lookupResDef gr (cnc,identS pos1)
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern 0 (np, v_ty)
    let fun' = Map.fromListWith (++) (map (\(f, t) -> (snd (head (filter (\(x, y) -> x == "vp") f)), [(map fst f, t)])) fun)
    
    let argMap = Map.fromList args
    let (fields, addArgs) = combineTrees cfg name "vp" "" modmap fun argMap ["np", "vp"]
    return (fields, addArgs ++ args)
    where
        values y = lookupValues y gr (cfgLangName cfg)

learnPredVP cfg cnc gr noSmarts trees = do 
    let name = "PredVP"
    let pattern = ( 
                    QueryPattern {pos=Just ["VERB", "AUX"], rel=Nothing, morpho=Nothing, idx="vp"},
                    QueryPattern {pos=Just ["NOUN", "PRON"], rel = Just "subj", morpho=Nothing, idx="np"})
    let (verb,_) = mapPOS pattern
    np  <- lookupResDef gr (cnc, identS "NP")
    v_ty <- lookupResDef gr (cnc,identS verb)
    let (_, patts) = unzip $ query trees pattern
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern 0 (np, v_ty)
    let argMap = Map.fromList args
    let (fs, addArgs) = combineTrees cfg name "vp" "" modmap fun argMap ["np", "vp"]
    gr <- modifyCat cfg gr [("VP", lincat)]
    return (gr, fs, addArgs ++ args)


learnAdAP cfg cnc gr snoSmarts trees = do 
    let name = "AdAP"
    let pattern = ( 
                    QueryPattern {pos=Just ["ADJ"], rel=Nothing, morpho=Nothing, idx="ap"},
                    QueryPattern {pos=Just ["ADV"], rel = Just "mod", morpho=Nothing, idx="ada"})
    let (_, patts) = unzip $ query trees pattern
    let (adj, adv) = mapPOS pattern
    adv_ty <- lookupResDef gr (cnc,identS adv)
    adj_ty <- lookupResDef gr (cnc,identS adj)
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern 0 (adj_ty, adv_ty)
    let argMap = Map.fromList args
    let (fields, addArgs) = combineTrees cfg name "ap" "ada" modmap fun argMap ["ada", "ap"]
    return (fields, addArgs ++ args) 

--learnAdvAP lang cnc gr mapping noSmarts trees = do 
--    let name = "AdvAP"
--    (fun, args) <- learn cnc gr mapping noSmarts trees name (("ADJ", Nothing),("NOUN", Just "mod")) ("ap", "adv")
--    let (fields, addArgs) = combineTrees name "ap" "adv" modmap lang fun args ["adv", "ap"]
--    return (fields, addArgs ++ args)

learnAdv cfg cnc gr noSmarts trees = do
    -- first find everything that will output Adv
    let name1 = "PrepNP"
    let pattern = (QueryPattern {pos= Just ["ADP"], rel=Nothing, morpho=Nothing, idx="adp"}, 
                   QueryPattern {pos= Just ["NOUN"], rel = Just "comp:obj", morpho=Nothing, idx="np"})
    let (trees', patts) = unzip $ query trees pattern
    let (pos1, pos2) = mapPOS pattern
    adp_ty <- lookupResDef gr (cnc,identS pos1)
    np <- lookupResDef gr (cnc,identS "NP")
    
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name1 pattern 1 (np,adp_ty)
    let argMap = Map.fromList args
    let (fields, addArgs) = combineTrees cfg name1 "np" "adp" modmap fun argMap ["adp", "np"]


    -- find AdvCN structures
    let name2 = "AdvCN" 
    let pattern2 = (QueryPattern {pos= Just ["NOUN"], rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos=Just ["ADP"], rel = Just "udep", morpho=Nothing, idx="adv"})
    let (_, patts2) = unzip $ query trees' pattern2
    let patts3 = unionPatts (patts, 0) (patts2, 1)
    let (pos1, pos2) = mapPOS pattern2
    a_ty <- lookupResDef gr (cnc,identS pos2)
    n_ty <- lookupResDef gr (cnc,identS pos1)
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts3 name2 pattern2 0 (np, n_ty)
    let argMap = Map.fromList args
    let (fields2, addArgs2) = combineTrees cfg name2 "cn" "adv" modmap fun argMap ["cn","adv"]

    return ([fields, fields2], addArgs2 ++ args)

learnPositA lang ffs = Just ("Adjective", [getFun "PositA" ["a"] (Vr (identS "a"))])
learnUseN cfg ffs = Just ("Noun", [getFun "UseN" ["n"] (Vr (identS "n"))])

learnDetCN cfg cnc gr noSmarts trees = do
    -- fields to include and exclude for NP type
    let fields = 
          case lookupResDef gr (cfgLangModuleName cfg "Res",identS "Species") of
            Ok (QC (_,m))   -> ((showIdent m):fs)
            Bad msg -> fs
          where
            fs = ["Number"]

    let sp = getDefParam "Species" gr cfg
    
    let catMap = Map.fromList [("s", Just ("det", "sp")), ("n", Just ("det", "n"))]
    let fields' = [show (label t) | t <- all_tags, all (\x -> fst x `elem` fields) (ud_tag t)]

    

    let (numSg, _) = createNum "NumSg" "Sg" 
    let (numPl, numType) = createNum "NumPl" "Pl"

    let name1 = "Art"
    let pattern = (QueryPattern {pos= Nothing, rel=Nothing, morpho=Nothing, idx=""},
                   QueryPattern {pos= Just ["DET"], rel = Nothing, morpho=Just [("PronType", Match "Art")], idx="det"})
    let (trees', patts) = unzip $ query trees pattern 

    let artPatt = nub $ map (\(x@(_,lemma1,pos,m1,_), y@(_,lemma2,_,m2,_)) -> if pos == "DET" then (lemma1, m1) else (lemma2, m2)) patts

  
    -- gather information about all features 
    
    let allFeats = nub $ (map fst (concat (map (\(Tag _ f _ _ _ _) -> f) all_tags)))

    let feats = filter (\x -> x `elem` allFeats) (nub $ concat $ map (\(x, y) -> map fst y) artPatt)
    
    -- recursively for each feature sepatate (FEAT, div)
    let feats = delete "Definite" feats
    
    {-if "Number" in feats then do 
        let feats = delete "Number" feats -}
    
   {-} let patt' = Map.fromListWith (++) (mapMaybe (\(lemma,m) -> sepList "Definite" m (Just lemma)) artPatt)
    print (sortFeats feats patt') -}

    let (indef, _) = indefArt fields sp
    let (def, quantType) = defArt fields sp

    let (dQuant, detType) = detQuant fields numType quantType
   
    -- count_form?
    let name = "NumNP" 
    let pattern1 = (QueryPattern {pos=Just ["NOUN"], rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos=Just ["NUM"], rel = Just "mod", morpho=Nothing, idx="num"})
    let (trees', patts) = unzip $ query trees pattern1
    let (pos1, pos2) = mapPOS pattern1
    a_ty <- lookupResDef gr (cnc,identS pos2)
    (RecType cn) <- lookupResDef gr (cnc,identS "CN")
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern1 0 (a_ty, RecType cn)

    let np = Map.fromListWith (++) (getNewType fields' fun "cn")
    let argMap = Map.fromList args

    
    let fMap = addArgs np (nub (getIds cn)) fields'
    
    let (f', _) = unzip $ [matchFields "cn" "det" "NounMkd" "s" (Map.lookup "s" np)]
    let numNP = getFun "NumNP" ["num", "np"] (R (f' ++ fMap))
    
    let name2 = "DetCN"
    let pattern2 = (QueryPattern {pos= Just ["NOUN"], rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos= Just ["DET"], rel = Just "det", morpho=Nothing, idx="det"})
    let (trees', patts) = unzip $ query trees pattern2
    let (pos1, pos2) = mapPOS pattern2
    a_ty <- lookupResDef gr (cnc,identS pos2)
    n_ty <- lookupResDef gr (cnc,identS pos1)
    (fun2, args2, lincat2) <- learnPattern cfg cnc gr noSmarts patts name2 pattern2 0 (a_ty, n_ty)

    
    let fun3 = map (\(a, (x, i)) -> (a, (replaceIdent x catMap, i))) fun2 
    let f2 = Map.fromListWith (++) (getNewType fields' fun3 "cn")
    let argMap2 = Map.fromList args2
    
    let fMap = addArgs f2 (nub [showRawIdent idx | (lbl@(LIdent idx),_,_) <- cn, isNothing (isLockLabel lbl)]) fields'
    let quantFs = [getOneField (showRawIdent x) "det" | (LIdent x, _,_) <- quantType, (showRawIdent x) `Map.notMember` f2]
    let (f', _) = unzip $ [matchFields "cn" "det" "NounMkd" "s" (Map.lookup "s" f2)]
    
    let detCN = getFun "DetCN" ["det", "cn"] (R (f' ++ fMap ++ quantFs))

    let RecType lc = lincat2
    
    let lincat' = RecType [(LIdent l, r, filterTable t fields)| (lbl@(LIdent l), r, t) <- (nubBy (\x y -> getLbl x == getLbl y) (lc ++ cn ++ quantType))
                                                              , (showRawIdent l) `notElem` fields'
                                                              , isNothing (isLockLabel lbl)]

    
    let lincats = [("NP", lincat'), ("Quant", RecType quantType), ("Det", RecType quantType), ("Num", RecType numType), ("Det", RecType detType)]
    gr <- modifyCat cfg gr lincats

    --let gr' = prependModule gr (MN (identS $ "Cat" ++ lang), cat)
    return $ (gr, Just ("Noun", [detCN, dQuant, def, indef, numSg, numPl]), (Map.keys np ++ fields'))
    where 
        getLbl (lbl, _, _)= lbl 
        
{-sepList param patt _ | isNothing (lookup param patt) = Nothing
sepList param patt (Just lemma) | otherwise      = Just ((param,val), [(("lemma",lemma) : (delete (param, val) patt))])
    where
        val = fromJust (lookup param patt)
sepList param patt Nothing | otherwise = Just ((param,val), [delete (param, val) patt])
    where
        val = fromJust (lookup param patt)

sortFeats [feat] mapp = Map.map (\mm -> Map.fromListWith (++) (mapMaybe (\m -> sepList feat m) mm)) mapp
sortFeats (feat:feats) mapp = Map.map (\mm -> Map.map (sortFeats feats) (Map.fromListWith (++) (mapMaybe (\m -> sepList feat m) mm))) mapp -}

modifyCat cfg gr lincats = do
  mo <- lookupModule gr cat_mn
  let lincats' = Map.toList (jments mo) ++ (map (\(x, y) -> (identS x, CncCat (Just (L NoLoc y)) Nothing Nothing Nothing Nothing)) lincats)
  return gr{moduleMap=Map.insert cat_mn mo{jments = Map.fromList lincats'} (moduleMap gr)}
  where
    cat_mn = cfgLangModuleName cfg "Cat"

filterTable t@(Table (QC (_, idx)) t2@(Table _ _)) fields | (showIdent idx) `elem` fields = filterTable t2 fields
filterTable t@(Table q t2@(Table _ _)) fields | otherwise = Table q (filterTable t2 fields)
filterTable t@(Table (QC (_, idx)) t2@(Sort _)) fields | (showIdent idx) `elem` fields = t2
filterTable t fields | otherwise = t


replaceIdent (T TRaw [(PV x, T TRaw ts)]) m | (showIdent x `Map.member` m) = replaceIdent (T TRaw ts) m
replaceIdent (T TRaw [(PV x, C s1 s2)]) m | (showIdent x `Map.member` m) = C (repId s1 m) (repId s2 m)
replaceIdent (T TRaw ts) m  = T TRaw (map (\x -> replId x m) ts)
replaceIdent x m = x

replId (PV x, T TRaw ts) m                                = (PV x, T TRaw (map (\x -> replId x m) ts))
replId (PV x, C s1 s2) m  = (PV x, C (repId s1 m) (repId s2 m))

repId (P (Vr idx) (LIdent field)) m = (P (Vr idx) (LIdent field))
repId (S s@(S _ _) (Vr f)) m | showIdent f `Map.member` m && lookupMap (showIdent f) m == Nothing = repId s m
repId (S s@(S _ _) (Vr f)) m | showIdent f `Map.member` m = (S (repId s m) (P (Vr (identS d)) (LIdent (rawIdentS val))))
    where (d, val) = fromJust (lookupMap (showIdent f) m)

repId (S s@(S _ _) (Vr f)) m | otherwise = (S (repId s m) (Vr f))
repId (S s@(P _ _) q@(QC (_, idx))) m | showIdent idx `Map.member` m && lookupMap (showIdent idx) m == Nothing = repId s m
repId (S s@(P _ _) q@(QC (_, idx))) m | showIdent idx `Map.member` m = (S (repId s m) (P (Vr (identS d)) (LIdent (rawIdentS val))))
    where (d, val) = fromJust (lookupMap (showIdent idx) m)
repId (S s@(P _ _) q@(QC (_, idx))) m | otherwise = (S s q)

repId (S p@(P _ _) (Vr f)) m | showIdent f `Map.member` m && lookupMap (showIdent f) m == Nothing = p
repId (S p@(P _ _) (Vr f)) m | showIdent f `Map.member` m = (S p (P (Vr (identS d)) (LIdent (rawIdentS val))))
    where (d, val) = fromJust (lookupMap (showIdent f) m)
repId (S p@(P _ _) (Vr f)) m | otherwise = (S p (Vr f))
repId x m  = x

lookupMap i m = fromJust (Map.lookup i m)

addArgs np cn fields = map (\x -> getOneField x "cn") (filter (\x -> (x `Map.notMember` np) && (x `notElem` fields)) cn)
        
lookupValues param gr lang = do 
    x <- case allParamValues gr (QC (MN  (identS ("Res" ++ lang)), identS param)) of 
        Ok m -> return m
        Bad  msg -> return []
 
    
    map getParamValues x
    --x

getParamValues (QC (_, idx)) = showIdent idx
getParamValues (App (QC (_, idx)) (QC (_, idx2))) = showIdent idx ++ " " ++ showIdent idx2

getDefParam param gr cfg =
  case Map.lookup (identS param) (jments (lookupRes gr cfg)) of 
    Just (ResParam _ x) -> map getParamValues (fst (fromJust x))
    Nothing -> []
