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
import Control.Monad
import Data.List
import GF.Data.Operations
import GF.Infra.Option
import GF.Grammar.Printer

options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
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
    let lang = toTitle $ cfgIso3 cfg
    (cnc,gr) <- loadGrammar ("src" </> cfgLangName cfg </> "Lang"++toTitle lang ++".gf")

    
    trees <- fmap concat $ mapM (readCONLL cfg) (cfgTreebanks cfg)


    -- block of functions that handle CN type --
    (adjCN, lincatCN, oneArgs) <- learnAdjCN cfg cnc gr noSmarts trees
    --(adAP, twoArgs) <- learnAdAP lang cnc gr mapping noSmarts trees
    --(advAP, threeArgs) <- learnAdvAP lang cnc gr mapping noSmarts trees

    let allArgs = Map.fromListWith (++) (concat [oneArgs])
    let positA = learnPositA lang allArgs
    let useN = learnUseN lang allArgs 

    -- block of functions that handle NP type -- 
    (detCN, lincats, np) <- learnDetCN cfg cnc gr noSmarts trees lincatCN
    (advCN, fourArgs) <- learnAdv cfg cnc gr noSmarts trees (snd (head lincats))
    
    -- block of functions that handle VP type -- 
    (v2, verbArgs) <- learnV2 cfg cnc gr noSmarts trees
    (predVP, verb2Args) <- learnPredVP cfg cnc gr noSmarts trees


    -- block of functions to create modules -- 

    let cat = lookupCat gr lang
    let jments cat = Map.union (Map.fromList (map (\(x, y) -> (identS x, y)) lincats)) (Map.insert (identS "CN") lincatCN (jments cat))

    -- lincats to Res and Cat
    let jments = Map.fromListWith (++) (catMaybes ([positA, useN, adjCN, detCN, predVP, v2] ++ advCN))
    forM_ (Map.toList jments) $ \(m, funs) -> do 
        let mod = getModule lang m funs
        writeFile (m ++ lang ++ ".gf") (show mod)
    writeFile ("Cat" ++ lang ++ ".gf") (show (ppModule Unqualified (MN (identS $ "Cat" ++ lang), cat)))

lookupCat gr lang = do
    case lookupModule gr (MN (identS ("Cat" ++ lang))) of 
        Ok m -> m 
        Bad f -> ModInfo {jments = Map.empty, msrc="", mstatus = MSComplete, mextend = [], mwith=Nothing, mopens=[], mexdeps=[], mflags = noOptions,  mtype = MTConcrete (MN  (identS $ "Cat" ++ lang))}

lookupRes gr lang = do
    case lookupModule gr (MN (identS ("Res" ++ lang))) of 
        Ok m -> m 
        Bad f -> ModInfo {jments = Map.empty, msrc="", mstatus = MSComplete, mextend = [], mwith=Nothing, mopens=[], mexdeps=[], mflags = noOptions,  mtype = MTConcrete (MN  (identS $ "Cat" ++ lang))}


learnAdjCN cfg cnc gr noSmarts trees = do 
    let name = "AdjCN"
    let pattern = (QueryPattern {pos=Just ["NOUN"], rel=Nothing, morpho=Nothing, idx="cn"}, 
                    QueryPattern {pos=Just ["ADJ"], rel=Just "mod", morpho=Nothing, idx="ap"})
    let (_, patts) = unzip $ query trees pattern
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern 0
    let (fields, addArgs) = combineTrees cfg name "cn" "ap" modmap fun (Map.fromList args) ["ap", "cn"]
    return (fields, lincat, addArgs ++ args)

learnV2 cfg cnc gr noSmarts trees = do 
    let fields = ["Tense", "Polarity", "Aspect"]
    let m = Map.fromList (map (\y-> (y, Nothing)) (concat (map (\y -> (map toLower (take 1 y)):(values y)) fields)))
    let name = "UseV2"
    let pattern = ( 
                    QueryPattern {pos=Just ["VERB"], rel=Nothing, morpho=Nothing, idx="vp"},
                    QueryPattern {pos=Just ["NOUN", "PRON"], rel = Just "comp:obj", morpho=Nothing, idx="np"})
    let (_, patts) = unzip $ query trees pattern
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern 0
    let fun' = Map.fromListWith (++) (map (\(f, t) -> (snd (head (filter (\(x, y) -> x == "vp") f)), [(map fst f, t)])) fun)
    
    let argMap = Map.fromList args
    let (fields, addArgs) = combineTrees cfg name "vp" "np" modmap fun argMap ["np", "vp"]


    {-let (f', _) = unzip (map (\x -> matchFields "vp" "np" ("Verb" ++ toTitle (cfgIso3 cfg)) x (Map.lookup x fun')) (fromJust (Map.lookup "vp" argMap)))
    print (R f')
    print (pp $ R (map (\(idx, (rest, val)) -> (idx, (Nothing, replaceIdent val m))) f'))
    -}
    return (fields, addArgs ++ args)
    where
        values y = lookupValues y gr (cfgIso3 cfg)

learnPredVP cfg cnc gr noSmarts trees = do 
    let fields = ["Tense", "Polarity", "Aspect"]
    let m = Map.fromList (map (\y-> (y, Nothing)) (concat (map (\y -> (map toLower (take 1 y)):(values y)) fields)))
    let name = "PredVP"
    let pattern = ( 
                    QueryPattern {pos=Just ["VERB", "AUX"], rel=Nothing, morpho=Nothing, idx="vp"},
                    QueryPattern {pos=Just ["NOUN", "PRON"], rel = Just "subj", morpho=Nothing, idx="np"})
    let (_, patts) = unzip $ query trees pattern
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern 0
    -- print fun
    let fun' = Map.fromListWith (++) (map (\(f, t) -> (snd (head (filter (\(x, y) -> x == "vp") f)), [(map fst f, t)])) fun)
    
    let argMap = Map.fromList args
    let (fields, addArgs) = combineTrees cfg name "vp" "np" modmap fun argMap ["np", "vp"]


    let (f', _) = unzip (map (\x -> matchFields "vp" "np" ("Verb" ++ toTitle (cfgIso3 cfg)) x (Map.lookup x fun')) (fromJust (Map.lookup "vp" argMap)))
    --print (R f')
    --print (pp $ R (map (\(idx, (rest, val)) -> (idx, (Nothing, replaceIdent val m))) f'))

    return (fields, addArgs ++ args)
    where
        values y = lookupValues y gr (cfgIso3 cfg)

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

learnAdv cfg cnc gr noSmarts trees (RecType np) = do
    -- first find everything that will output Adv
    let name1 = "PrepNP"
    let pattern = (QueryPattern {pos= Just ["ADP"], rel=Nothing, morpho=Nothing, idx="adp"}, 
                   QueryPattern {pos= Just ["NOUN"], rel = Just "comp:obj", morpho=Nothing, idx="n"})
    let (trees', patts) = unzip $ query trees pattern
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name1 pattern 1
    let argMap = Map.fromList args
    
    let f = Map.fromListWith (++) (getNewType (getIds np) fun "n")
    
    let (f', _) = unzip $ [matchFields "np" "prep" ("Noun" ++ toTitle (cfgIso3 cfg)) "s" (Map.lookup "s" f)]
    let fs = fillType np lincat [] f' (gr, toTitle (cfgIso3 cfg))
    let fields = Just ("Noun", [getFun "PrepNP" ["prep", "np"] (R fs)])

    -- find AdvCN structures
    let name2 = "AdvCN" 
    let pattern2 = (QueryPattern {pos= Just ["NOUN"], rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos=Just ["ADP"], rel = Just "udep", morpho=Nothing, idx="adv"})
    let (_, patts2) = unzip $ query trees' pattern2
    let patts3 = unionPatts (patts, 0) (patts2, 1)

    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts3 name2 pattern2 0
    let argMap = Map.fromList args
    let (fields2, addArgs2) = combineTrees cfg name2 "cn" "adv" modmap fun argMap ["adv", "cn"]

    return ([fields, fields2], addArgs2 ++ args)

    
fillType [] typ2 new f g = new
fillType ((lid, r, t):typ1) typ2@(RecType t2) new f g | (lid, r, t) `elem` t2 = fillType typ1 typ2 ((lid, getVal f lid):new) f g
    where 
        getVal ((idx, (_,val)):[]) lid = (Nothing, val)
        getVal ((idx, (_,val)):f) lid | idx == lid = (Nothing, val)
        getVal ((idx, _):f) lid | otherwise = getVal f lid

fillType ((lid, r, t):typ1) typ2@(RecType t2) new f (gr, lang) | found /= Empty = fillType typ1 typ2 ((lid, getUpdatedVal f lid t found):new) f (gr, lang)
    where 
        found = checkPres (lid, r, t) typ2

        checkPres (lid, r, t) (RecType []) = Empty
        checkPres (lid, r, t) typ2@(RecType ((idx, _, t1):t2)) | idx == lid = t1
        checkPres (lid, r, t) typ2@(RecType ((idx, r1, t1):t2)) | otherwise = checkPres (lid,r,t) (RecType t2) 

        getUpdatedVal ((idx, (_,val)):[]) lid t typ = (Nothing, unpackVal val t typ [])
        getUpdatedVal ((idx, (_,val)):f) lid t typ | idx == lid = (Nothing, unpackVal val t typ [])
        getUpdatedVal ((idx, (_,val)):f) lid t typ | otherwise = getUpdatedVal f lid t typ

        unpackVal val t (Table (QC (_, idx)) typ) fields | t == typ  = replaceIdent val m
            where
                values y = lookupValues y gr lang
                m = Map.fromList (map (\y-> (y, Nothing)) (concat (map (\y -> (map toLower (take 1 y)):(values y)) (fields ++ [showIdent idx]))))
        unpackVal val t (Table (QC (_, idx)) typ) fields | otherwise = unpackVal val t typ (fields ++ [showIdent idx])

        filterVal t@(Table (QC (_, idx)) t2@(Table _ _)) fields | (showIdent idx) `elem` fields = filterTable t2 fields
        filter t@(Table (QC (_, idx)) t2@(Sort _)) fields | (showIdent idx) `elem` fields = t2
        filterTable t fields | otherwise = t




fillType ((lid, r, t):typ1) typ2@(RecType t2) new f g | otherwise = fillType typ1 typ2 ((lid,(Nothing,(P (Vr (identS "np")) (lid)))):new) f g
             
        
        

learnPositA lang ffs = getPosFun lang modmap "PositA" "a" "ap" ffs

learnUseN lang ffs = getPosFun lang modmap "UseN" "n" "cn" ffs

learnDetCN cfg cnc gr noSmarts trees (RecType cn) = do
    -- fields to include and exclude for NP type

    let fields = lookupTerm ("Res" ++  toTitle (cfgIso3 cfg)) gr "Species" ["Number"]
    let sp = getDefParam "Species" gr (toTitle (cfgIso3 cfg))
    
    let catMap = Map.fromList [("s", Just ("det", "sp")), ("n", Just ("det", "n"))]
    let fields' = [show (label t) | t <- all_tags, fst (ud_tag t) `elem` fields]
    

    let (numSg, _) = createNum "NumSg" "Sg" 
    let (numPl, numType) = createNum "NumPl" "Pl"
    
    let name1 = "Art"
    let pattern = (QueryPattern {pos= Nothing, rel=Nothing, morpho=Nothing, idx=""}, 
                   QueryPattern {pos= Just ["DET"], rel = Nothing, morpho=Just [("PronType", Match "Art")], idx="det"})
    let (trees', patts) = unzip $ query trees pattern 

    let (indef, _) = indefArt fields sp
    let (def, artType) = defArt fields sp

    let (dQuant, quantType) = defQuant fields numType artType
   

    let name = "NumNP" 
    let pattern1 = (QueryPattern {pos=Just ["NOUN"], rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos=Just ["NUM"], rel = Just "mod", morpho=Nothing, idx="num"})
    let (trees', patts) = unzip $ query trees pattern1
    (fun, args, lincat) <- learnPattern cfg cnc gr noSmarts patts name pattern1 0

    let np = Map.fromListWith (++) (getNewType fields' fun "cn")
    let argMap = Map.fromList args

    
    let fMap = addArgs np (nub (getIds cn)) fields'
    
    let (f', _) = unzip $ [matchFields "cn" "det" "NounMkd" "s" (Map.lookup "s" np)]
    let numNP = getFun "NumNP" ["num", "np"] (R (f' ++ fMap))
    
    let name2 = "DetCN"
    let pattern2 = (QueryPattern {pos= Just ["NOUN"], rel=Nothing, morpho=Nothing, idx="cn"}, 
                   QueryPattern {pos= Just ["DET"], rel = Just "det", morpho=Nothing, idx="det"})
    let (trees', patts) = unzip $ query trees pattern2
    (fun2, args2, lincat2) <- learnPattern cfg cnc gr noSmarts patts name2 pattern2 0

    
    let fun3 = map (\(a, (x, i)) -> (a, (replaceIdent x catMap, i))) fun2 
    let f2 = Map.fromListWith (++) (getNewType fields' fun3 "cn")
    let argMap2 = Map.fromList args2
    
    let fMap = addArgs f2 (nub (map (\(LIdent idx,_,_) -> showRawIdent idx) cn)) fields'
    let quantFs = [getOneField (showRawIdent x) "det" | (LIdent x, _,_) <- quantType, (showRawIdent x) `Map.notMember` f2]
    let (f', _) = unzip $ [matchFields "cn" "det" "NounMkd" "s" (Map.lookup "s" f2)]
    let detCN = getFun "DetCN" ["det", "cn"] (R (f' ++ fMap ++ quantFs))

    let RecType lc = lincat2
    let lincat' = RecType ([(LIdent l, r, filterTable t fields)| (LIdent l, r, t) <- (nubBy (\x y -> getLbl x == getLbl y) (lc ++ cn ++ quantType)), (showRawIdent l) `notElem` fields'])


    return $ (Just ("Noun", [detCN, numNP, dQuant, def, indef, numSg, numPl]), [("NP", lincat'), ("Quant", RecType quantType), ("Det", RecType artType), ("Num", RecType numType)], (Map.keys np ++ fields'))
    where getLbl (lbl, _, _)= lbl

filterTable t@(Table (QC (_, idx)) t2@(Table _ _)) fields | (showIdent idx) `elem` fields = filterTable t2 fields
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
    x <- case allParamValues gr (Con (identS param)) of 
        Ok m -> return m
        Bad  msg -> return []
 
    
    map getParamValues x
    --x

getParamValues (QC (_, idx)) = showIdent idx
getParamValues (App (QC (_, idx)) (QC (_, idx2))) = showIdent idx ++ " " ++ showIdent idx2

getDefParam param gr lang = case Map.lookup (identS param) (jments (lookupRes gr lang)) of 
                    Just (ResParam _ x) -> map getParamValues  (fst (fromJust x))
                    Nothing -> []

toTitle []     = []
toTitle (c:cs) = toUpper c : cs
