module Learner.Evaluation(evalToUD, evalTerm, compareTrees, compareTree, equalTrees,
interpretFun, interpret,constructTable, lookupUDTag, lookupRecord, lookupValues, getParamValues, findType) where

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
import Learner.Config

type UDTag = [(String, String)]
type GFTag = String

data TypeTable = Tab GFTag UDTag [TypeTable] | Node GFTag UDTag | EmptyNode
    deriving (Show, Eq)

type Condition = [GFTag]
data AnnotatedTypeTable = ATab GFTag UDTag [AnnotatedTypeTable] | ANode GFTag UDTag Bool Condition | AEmptyNode
    deriving (Show, Eq)

compareTrees [] = []
compareTrees (x:xs) = (x':compareTrees ys')
    where (x', ys') = compareTree x xs []

compareTree t [] res = (t, res)
compareTree t (y:ys) res | eq = compareTree t' ys res
    where (eq, t') = equalTrees t y
compareTree t (y:ys) res | otherwise = compareTree t ys (y:res)

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
evalToUD (ANode gf ud True cond) = [(ud, concatMap lookupUDTag cond)]
evalToUD (ANode gf ud False cond) = []
evalToUD (AEmptyNode) = []

evalTerm t args (LIdent ident, (_, f)) = map getPatts res
    where
        res = interpretFun args (fromJust (Map.lookup (showRawIdent ident) (fromJust (Map.lookup t args)))) [] f
        getPatts ((base1, arg1), (base2, arg2)) = ((base1, base2), (concat [contrast x y | x <- getUD arg1, y <- getUD arg2]))
        getUD = concatMap evalToUD 

contrast (tags1, cond1) (tags2, cond2) | and check1 && and check2 = [(tags1 ++ concat add2, tags2 ++ concat add1)]
    where 
        (check1, add1)  = checkCond cond1 tags2
        (check2, add2)  = checkCond cond2 tags1
        
        checkCond cond tags = unzip $ map (\x -> checkTag x tags) cond

        checkTag (param, val) [] = (True, [(param,val)])
        checkTag (param, val) ((param', val'):tags) | param' == param = (val == val', [])
        checkTag (param, val) ((param', val'):tags) | otherwise = checkTag (param, val) tags
contrast (tags1, cond1) (tags2, cond2) | otherwise = []

interpretFun args t vars (S t1 _ ) = interpretFun args t vars t1
interpretFun args t vars (T TRaw ts) = concatMap getF ts
    where 
        getF (PV y, x) = interpretFun args (concatMap stepF t) (addedVars (showIdent y)) x
        getF (_, x) = interpretFun args t vars x
        addedVars y = (y, map getValues t):vars
        stepF (Tab _ _ ts') = ts'
        stepF _ = []

interpretFun args t vars (C x1 x2) = [((findBase x1, f (interpret args valMap [] [] [] x1)), (findBase x2, f (interpret args valMap [] [] [] x2)))]
    where 
        valMap = Map.fromList vars
        f xs = compareTrees (map (\(_,x,_) -> x) xs)

        findBase (P (Vr id) (LIdent id2)) = showIdent id
        findBase (S s1 s2) = findBase s1

interpretFun _ _ _ _ = []
   


--interpret :: Map.Map String (Map.Map String [TypeTable]) -> [(Bool, AnnotatedTypeTable, TypeTable)]
--              -> [GFTag] -> Term
--              -> [(Bool, AnnotatedTypeTable, TypeTable)]

interpret args vars vv ts cond (S s1@(T _ _) s2) = interpret args vars (vv' s2) ts cond s1 
    where vv' (P (Vr id) (LIdent id2)) = ((map getValues (fromJust (Map.lookup (showRawIdent id2) (fromJust (Map.lookup (showIdent id) args))))):vv)
          vv' (Vr id) = ((fromJust (Map.lookup (showIdent id) vars)):vv)
interpret args vars vv ts cond (S s1 s2) = interpret args vars vv (interpret args vars vv ts cond s1) cond s2 
interpret args vars vv [] cond (P (Vr id) (LIdent id2)) = map (\x -> (True, toAnnotated x, x)) (fromJust (Map.lookup (showRawIdent id2) (fromJust (Map.lookup (showIdent id) args))))
    where 
            toAnnotated (Tab gf ud ts) = ATab gf ud (map toAnnotated ts)
            toAnnotated (Node gf ud) = ANode gf ud True []
            toAnnotated EmptyNode = AEmptyNode

interpret args vars vv ts cond (P (Vr id) (LIdent id2)) = nub $ concatMap (sortTS cond (map getValues values)) ts
    where values = fromJust (Map.lookup (showRawIdent id2) (fromJust (Map.lookup (showIdent id) args)))
interpret args vars vv ts cond (Vr id)  = nub $ concatMap (sortTS cond values) ts
    where 
          values = fromJust (Map.lookup (showIdent id) vars)
        
interpret args vars vv ts cond (QC (_, v)) = concatMap (sortTS cond [(showIdent v)]) ts
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

interpret args vars vv ts _ _ = concatMap mapT ts
    where mapT (b, t, Tab gf ud ts') = map (\t' -> (b, t, t')) ts'
          mapT (b, t, _) = [(b, t, EmptyNode)]

findType l (Sort id) | (showIdent id) == "Str" = [showRawIdent l, showIdent id]
findType l x = findType' x
findType' (Sort id) = [showIdent id]
findType' (QC (_, id)) = [showIdent id]
findType' (Table (QC (_, id)) t) = (showIdent id ):(findType' t)
findType' _ = []
 
sortTS cond v (True, t, t')   = matchTypeTable cond v t t'
sortTS cond v (False, t, _)  = [(False, t, EmptyNode)]


sortTS' cond v1 vs (True, t, t') | found = matchTypeTable (c ++ cond) v' t t'
    where v' = map (\v -> showIdent v1 ++ " " ++ v) vs
          (found, c) = findVal vs t'
          findVal [] t = (False, [])
          findVal (v:vs) t | getValues t == showIdent v1 ++ " " ++ v = (True, [v])
          findVal (v:vs) t = findVal vs t

sortTS' cond v1 vs (_, t, t')  = [(False, t, EmptyNode)]

matchTypeTable cond v t cur@(Node gf ud) = let found = gf `elem` v in [(found, (annotate found cond cur t), EmptyNode)]
matchTypeTable cond v t cur@(Tab gf ud ts) = let found = gf `elem` v 
                                                 ann = annotate found cond cur t
                                        in  map (\t' -> (found, ann, t')) ts
--matchTypeTable cond v t em = [(False, t, em)]


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
constructTable [x]    gr lang = map mapValuesToNode (lookupValues x gr lang)
    where mapValuesToNode [y] = Node y (lookupUDTag y)
          mapValuesToNode (y:y2:ys) = Node (y ++ " " ++ y2) (lookupUDTag y ++ lookupUDTag y2)
-- special case for Str
constructTable (x:"Str":[]) gr lang = case lookupValues x gr lang of 
        [] -> [Node x (lookupRecord x)]
        t  -> map mapValuesToNode t
    where 
          mapValuesToNode [y] = Node y (lookupUDTag y)
          mapValuesToNode (y:y2:ys) = Node (y ++ " " ++ y2) (lookupUDTag y ++ lookupUDTag y2)
constructTable (x:xs) gr lang = map mapValuesToTab (lookupValues x gr lang)
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
getParamValues (App (QC (_, idx)) (QC (_, idx2))) = [showIdent idx, showIdent idx2]

getValues (Node tag _) = tag
getValues (Tab tag _ _) = tag