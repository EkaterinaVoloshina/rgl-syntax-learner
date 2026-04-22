module Learner.Evaluation(eval, evalToUD, evalTerm, compareTrees, compareTree, equalTrees,
interpretFun, interpret,constructTable, filterFields, lookupUDTag, lookupRecord, lookupValues, getParamValues, findType,
contrast) where

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
import Data.Map.Strict (fromListWith, toList)
import Data.List (sort)
import Data.Function(on)
import Learner.Config
import Learner.CodeGen
import Data.Char (toUpper, isLower)
import Debug.Trace(trace)


type UDTag = [(String, String)]
type GFTag = String

data TypeTable = Tab GFTag UDTag [TypeTable] | Node GFTag UDTag | EmptyNode
    deriving (Show, Eq)

type Condition = [GFTag]
data AnnotatedTypeTable = ATab GFTag UDTag [AnnotatedTypeTable] | ANode GFTag UDTag Bool Condition | AEmptyNode
    deriving (Show, Eq)


exceptions = [P (P (Vr (identS "vp")) (LIdent (rawIdentS "c2"))) (LIdent (rawIdentS "s"))]

eval gr cfg trees patts oType Nothing = (-1, [], [])
eval gr cfg trees patts oType (Just fun) = (fromIntegral (length (filter (`elem` res') res)) / fromIntegral (length res), filter (`notElem` res') res, result)
    where
        (CncFun _ f _ _) = snd $ head (snd fun)
        (L _ abs) = fromJust f
        (R rs) = unpack abs
        headType = showIdent (idx (head patts))
        getMapTypes (RecType ts) = Map.fromList (map (\(LIdent l, _, t) -> (showRawIdent l, constructTable (findType l t) gr (toTitle (cfgIso3 cfg)))) ts)

        --patts1 = map (\(x, y) -> ((cfgDefaults cfg) cfg x, (cfgDefaults cfg) cfg y)) patts
        args' = Map.fromList (map (\p -> (showIdent (idx p), getMapTypes (var_type p))) patts)

        result = concatMap (\x -> map (\y -> (fst x, y)) (snd x)) ( concatMap (evalTerm gr cfg oType args') rs)
        patts' = map (getUpdatedPatterns patts headType) result

        res = query cfg trees patts

        res' = nub $ concatMap (query cfg trees) patts'

        unpack (Abs _ _ abs) = unpack abs
        unpack abs = abs

        toTitle (c:cs) = toUpper c:cs

-- TODO: non binary
getUpdatedPatterns patts head  ((b1, b2), (p1, p2)) = if head == b1 then [update b1 (Just b2) (toFeat p1) (findPattern b1 patts), update b2 Nothing (toFeat p2) (findPattern b2 patts)] else [update b2 Nothing (toFeat p2) (findPattern b2 patts), update b1 (Just b2) (toFeat p1) (findPattern b1 patts)]
    where
            update b (Just b2) p1 p2 | isJust (morpho p2) = p2 {morpho = Just (fromJust (morpho p2) ++  p1), lin_order = Before b2}
            update b (Just b2) p1 p2 | isNothing (morpho p2) = p2 {morpho = Just p1, lin_order = Before b2}
            update b Nothing p1 p2 | isJust (morpho p2)= p2 {morpho =Just (fromJust (morpho p2) ++  p1)}
            update b Nothing p1 p2 | isNothing (morpho p2) = p2 {morpho = Just p1}

            findPattern b []  = QueryPattern { pos = Nothing
                                             , rel = Nothing
                                             , morpho = Nothing
                                             , idx = identW
                                             , var_type = Meta 0
                                             , lin_order = NA
                                             }
            findPattern b (patt:patts) | idx patt == identS b = patt
            findPattern b (patt:patts)                        = findPattern b patts

            toFeat x = map (\(k, y) -> (k, Match y)) (groupTuples (filter (/=("", "")) x))
            groupTuples tuples = [(k, sort v) | (k, v) <- grouped tuples]
            grouped tuples = toList $ fromListWith (++) [(k, [v]) | (k, v) <- tuples]

            mapFst f (a, b) = (f a, b)


compareTrees [] = []
compareTrees (x:xs) = (x':compareTrees ys')
    where (x', ys') = compareTree x xs []

compareTree t [] res = (t, res)
compareTree t (y:ys) res | eq = compareTree t' ys res
    where (eq, t') = equalTrees t y
compareTree t (y:ys) res      = compareTree t ys (y:res)

equalTrees :: AnnotatedTypeTable -> AnnotatedTypeTable -> (Bool, AnnotatedTypeTable)
equalTrees (ATab gf ud ts) (ATab gf' _ ts') | gf == gf' = (and eqs, ATab gf ud ts'')
    where (eqs, ts'') = unzip $ map (\(x, y) -> equalTrees x y) (zip ts ts')
equalTrees (ANode gf ud True cond@(x:xs)) (ANode gf' _ False []) | gf == gf' = (True, ANode gf ud True cond)
equalTrees (ANode gf ud False []) (ANode gf' _ True cond@(x:xs)) | gf == gf' = (True, ANode gf ud True cond)
equalTrees (ANode gf ud b1 cond) (ANode gf' _ b2 cond') | gf == gf' = (True, ANode gf ud (b1 && b2) (cond ++ cond'))
equalTrees t t' = (False, t)


evalToUD (ATab gf ud t) = map mapT (concatMap evalToUD t)
    where mapT ([], cond) = ([], cond)
          mapT  (x, cond)  = (ud ++ x, cond)
evalToUD (ANode gf ud True cond) = [(ud, concatMap (filter (/= ("", "")) . lookupUDTag) cond)]
evalToUD (ANode gf ud False cond) = []
evalToUD AEmptyNode = []

evalTerm gr cfg t args (LIdent ident, (_, f)) = map getPatts res
    where
        toTitle (c:cs) = toUpper c:cs
        getMapTypes (RecType ts) = Map.fromList (map (\(LIdent l, _, t) -> (showRawIdent l, constructTable (findType l t) gr (toTitle (cfgIso3 cfg)))) ts)
        t' = concatMap filterFields $ fromJust (Map.lookup (showRawIdent ident) (getMapTypes t))
        res = interpretFun args t' [] f

        patt' [] [] = [([], [])]
        patt' [] ys = map (\(x,_) -> ([], x)) ys
        patt' xs [] = map (\(x,_) -> (x, [])) xs
        patt' xs ys = concat [contrast x y | x <- xs, y <- ys]
        getPatts ((base1, arg1), (base2, arg2)) = ((base1, base2), patt' (getUD arg1) (getUD arg2))
        getUD = concatMap evalToUD


contrast (tags1, cond1) (tags2, cond2) | and check1 && and check2 = [(tags1 ++ concat add2, tags2 ++ concat add1)]
    where
        (check1, add1)  = checkCond cond1 tags2
        (check2, add2)  = checkCond cond2 tags1

        checkCond cond tags = unzip $ map (\x -> checkTag x tags) cond

        checkTag (param, val) [] = (True, [(param,val)])
        checkTag (param, val) ((param', val'):tags) | param' == param = (val == val', [])
        checkTag (param, val) ((param', val'):tags)                   = checkTag (param, val) tags
contrast (tags1, cond1) (tags2, cond2) = []

interpretFun args t vars (S t1 _ ) = interpretFun args t vars t1
interpretFun args t vars (T TRaw ts) = concatMap getF ts
    where
        getF (PV y, x) = interpretFun args (concatMap stepF t) (addedVars (showIdent y)) x
        getF (_, x) = interpretFun args t vars x
        addedVars y = (y, map getValues t):vars
        stepF (Tab _ _ ts') = ts'
        stepF _ = []

interpretFun args t vars c@(C x1 x2) = getBases (stripExceptions c)
     where
        valMap = Map.fromList vars
        f xs = compareTrees (map (\(_,x,_) -> x) xs)



        stripExceptions (C c1 c2) | c1 `elem` exceptions = c2
        stripExceptions (C c1 c2) | c2 `elem` exceptions =  c1
        stripExceptions (C c1 c2) = C (stripExceptions c1) (stripExceptions c2)
        stripExceptions e = e

        getBases (C c1 c2) = [(annotate c1, annotate c2)]
        getBases _ = []

        annotate x = (findBase x, f (interpret args valMap [] [] [] x))

        findBase (P (Vr id) (LIdent id2)) = showIdent id
        findBase (P p (LIdent id2)) = findBase p
        findBase (S s1 s2) = findBase s1
        findBase x = trace ("BASE: " ++ show x) ""



interpretFun args t vars (R x) = concatMap (\(_, (_, r)) -> interpretFun args t vars r) x
interpretFun args t vars x = []

interpret args vars vv ts cond (S s1@(T _ _) s2) = interpret args vars (vv' s2) ts cond s1
    where vv' (P (Vr id) (LIdent id2)) = ((map getValues (fromJust (Map.lookup (showRawIdent id2) (fromJust (Map.lookup (showIdent id) args))))):vv)
          vv' (Vr id) = fromJust (Map.lookup (showIdent id) vars):vv
          vv' x = trace (show "I am here " ++ show x) $ []
interpret args vars vv ts cond (S s1 s2) = interpret args vars vv values cond s2
    where values = interpret args vars vv ts cond s1
interpret args vars vv [] cond p@(P (Vr id) (LIdent id2)) =  evalRecord (map (\x -> (True, toAnnotated x, x)) values)
    where
        values = fromJust (Map.lookup (showRawIdent id2) (fromJust (Map.lookup (showIdent id) args)))
        toAnnotated (Tab gf ud ts) = ATab gf ud (map toAnnotated ts)
        toAnnotated (Node gf ud) = ANode gf ud True []
        toAnnotated EmptyNode = AEmptyNode

        evalRecord ts | showRawIdent id2 == "s" = ts
        evalRecord ts =  nub $ concatMap (sortTS cond (map getValues values)) ts

interpret args vars vv ts cond (P (Vr id) (LIdent id2)) = nub $ concatMap (sortTS cond (map getValues values)) ts
    where
        values = concatMap filterVal (fromJust (Map.lookup (showRawIdent id2) (fromJust (Map.lookup (showIdent id) args))))
        filterVal (Tab _ [("", "")] children) = children
        filterVal x = [x]

interpret args vars vv ts cond (P p (LIdent id2)) =  concatMap (sortTS cond [showRawIdent id2]) ts'
    where ts' = interpret args vars vv ts cond p
interpret args vars vv ts cond (Vr id)  =  nub $ concatMap (sortTS cond values) ts
    where
          values = fromMaybe [] (Map.lookup (showIdent id) vars)

interpret args vars vv ts cond (QC (_, v)) = concatMap (sortTS cond [showIdent v]) ts
interpret args vars vv ts cond (App (QC (_, v1)) (QC (_, v2))) = concatMap (sortTS cond [(showIdent v1 ++ " " ++ showIdent v2)]) ts
interpret args vars vv ts cond (App (QC (_, v1)) ((P (Vr id) (LIdent id2)))) = concatMap (sortTS' cond v1 (map getValues  vals)) ts
    where vals = fromJust (Map.lookup (showRawIdent id2) (fromJust (Map.lookup (showIdent id) args)))
interpret args vars vv ts cond (App (QC (_, v1)) (Vr id)) = concatMap (sortTS' cond v1 vals) ts
    where (vals:vv') = vv


interpret args vars (v:vv) ts cond (T TRaw rows) = concatMap interpretWithCond rows
    where interpretWithCond (PP (_, id) _, t) = interpret args vars (v:vv) ts ((showIdent id):cond) t
          interpretWithCond (PV id, t) = interpret args vars (v:vv) ts cond t
          interpretWithCond (_, t) = interpret args vars vv ts cond t

          getV [] v = v
          getV ((PP (_, id) _, _):rest) v = getV rest (delete (showIdent id) v)
          getV x v = v

interpret args vars vv ts cond (C c1 c2) | c1 `elem` exceptions = interpret args vars vv ts cond c2
interpret args vars vv ts cond (C c1 c2) | c2 `elem` exceptions = interpret args vars vv ts cond c1
interpret args vars vv ts _ x = concatMap mapT ts
    where mapT (b, t, Tab gf ud ts') = map (\t' -> (b, t, t')) ts'
          mapT (b, t, _) = [(b, t, EmptyNode)]

findType l x | null all_labels = findType' x
    where all_labels = [ud_tag t | t <- all_tags, label t == ident2label (identS (showRawIdent l))]
findType l (Sort id) | showIdent id == "Str" = [showRawIdent l]
findType l x = showRawIdent l : findType' x
findType' (Sort id) | showIdent id /= "Str" = [showIdent id]
findType' (QC (_, id)) = [showIdent id]
findType' (Table (QC (_, id)) t) = (showIdent id ):(findType' t)
findType' (RecType ts)  = concatMap (\(LIdent id,_,t) -> findType id t) ts
findType' _ = []


sortTS cond v (True, t, t')  = matchTypeTable cond v t t'
sortTS cond v (False, t, _)  = [(False, t, EmptyNode)]


sortTS' cond v1 vs (True, t, t') | found = matchTypeTable (c ++ cond) v' t t'
    where v' = map (\v -> showIdent v1 ++ " " ++ v) vs
          (found, c) = findVal vs t'
          findVal [] t = (False, [])
          findVal (v:vs) t | getValues t == showIdent v1 ++ " " ++ v = (True, [v])
          findVal (v:vs) t = findVal vs t

sortTS' cond v1 vs (_, t, t')  = [(False, t, EmptyNode)]

matchTypeTable cond v t cur@(Node gf ud) = let found = gf `elem` v in [(found, (annotate found cond cur t), EmptyNode)]
matchTypeTable cond v t cur@(Tab gf ud ts) =
                                        let found = gf `elem` v
                                            ann = annotate found cond cur t
                                        in  map (\t' -> (found, ann, t')) ts
matchTypeTable cond v t x = [(True, t, EmptyNode)]


annotate False cond (Tab gf _ _) t@(ATab gf' ud ts) | gf == gf' = ATab gf ud (map changeToFalse ts)
    where
        changeToFalse t@(ATab gf ud ts) = ATab gf ud (map changeToFalse ts)
        changeToFalse t@(ANode gf ud _ cond') = ANode gf ud False cond'
annotate True cond (Tab gf _ _) t@(ATab gf' ud ts) | gf == gf' = ATab gf ud (map (addCond cond) ts)
    where
        addCond cond (ATab gf ud ts) = ATab gf ud (map (addCond cond) ts)
        addCond cond (ANode gf ud t cond') = ANode gf ud t (cond ++ cond')
annotate b cond cur t@(ATab gf ud ts) = ATab gf ud (map (annotate b cond cur) ts)
annotate False cond (Node gf ud) t@(ANode gf' _ _ cond') | gf == gf' = ANode gf' ud False cond'
annotate True cond (Node gf ud) t@(ANode gf' _ _ cond') | gf == gf' = ANode gf' ud True (cond' ++ cond)
annotate _ cond cur t = t




constructTable :: [GFTag] -> Grammar -> String -> [TypeTable]
constructTable []     gr lang = [EmptyNode]
-- special case for Str
--constructTable ["Str"] gf lang = 
constructTable [x]    gr lang = case lookupValues x gr lang of
    [] -> [Node x (lookupRecord x)]
    t -> map mapValuesToNode t
    where mapValuesToNode [y] = Node y (lookupUDTag y)
          mapValuesToNode (y:y2:ys) = Node (y ++ " " ++ y2) (lookupUDTag y ++ lookupUDTag y2)
-- special case for Str
constructTable (x:"Str":[]) gr lang = case lookupValues x gr lang of
        [] -> [Node x (lookupRecord x)]
        t  -> map mapValuesToNode t
    where
            mapValuesToNode [y] = Node y (lookupUDTag y)
            mapValuesToNode (y:y2:ys) = Node (y ++ " " ++ y2) (lookupUDTag y ++ lookupUDTag y2)
constructTable (x:xs) gr lang = 
    case lookupValues x gr lang of
        [] -> [Tab x (lookupRecord x) (constructTable xs gr lang)]
        t -> map mapValuesToTab t
    where mapValuesToTab [y] = Tab y (lookupUDTag y) (constructTable xs gr lang)
          mapValuesToTab (y:y2:ys) = Tab (y ++ " " ++ y2) (lookupUDTag y ++ lookupUDTag y2) (constructTable xs gr lang)
            -- Tab y (lookupUDTag y) [Tab y2 (lookupUDTag y2) (constructTable xs gr lang)]


lookupUDTag :: GFTag -> UDTag
lookupUDTag l = getTag [ud_tag t | t <- all_tags, ident t == (identS l)]
    where getTag [] = [("", "")]
          getTag (x:xs) = x

lookupRecord :: GFTag -> UDTag
lookupRecord l = getTag [ud_tag t | t <- all_tags, label t == ident2label (identS l)]
    where getTag [] = [("", "")]
          getTag (x:xs) = x

lookupValues param gr lang = do
    x <- case allParamValues gr (QC (MN  (identS ("Res" ++ lang)), identS param)) of
        Ok m -> return m
        Bad  msg -> return []

    map getParamValues x

getParamValues (QC (_, idx)) = [showIdent idx]
getParamValues (App a1 a2) = getParamValues a1 ++ getParamValues a2

getValues (Node tag _) = tag
getValues (Tab tag _ _) = tag
getValues x = ""


filterFields (Tab x _ ts) | isLower (head x) = ts
filterFields (Node x _) | isLower (head x) = []
filterFields x = [x]