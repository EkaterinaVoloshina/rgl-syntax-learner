module Learner.CodeGen(readCONLL,Node(..),ppNode,drawTree,
               identS,Ident,
               rawIdentS,RawIdent, unionPatts,
               noSmarts,
               learnPattern, query,
               getFun, getModule,
               combineTypes, combineTermsWTypes, combineTerms, combineOneTerms,
               extendTypeWithIsPre, extendTermWithIsPre,
               detQuant, createArt, createNum,
               generateTerm,
               QueryPattern(..), Val(..), Order(..)) where

import Prelude hiding ((<>))
import GF.Infra.Ident hiding (isPrefixOf)
import GF.Infra.Option
import GF.Data.Operations
import GF.Data.Operations as Op
import GF.Text.Pretty hiding (empty)
import GF.Grammar.Lookup
import GF.Grammar.Printer
import GF.Grammar.Predef
import GF.Grammar.Grammar hiding (Rule(..))
import GF.Grammar.Lockfield
import GF.Grammar.Macros (termForm,collectOp,composSafeOp,typeFormCnc,defLinType,appForm)
import GF.Compile
import System.Exit
import System.Process
import System.FilePath
import System.Directory
import Data.Tree
import Data.Maybe hiding (fromList)
import Data.Char(toLower)
import Data.List (sortOn,mapAccumL,partition,nub,isPrefixOf,isInfixOf,isSuffixOf)
import Data.Either (partitionEithers)
import Control.Monad
import Control.Applicative hiding (Const)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Learner.Config hiding (POS)
import Learner.DecisionTree

import Debug.Trace(trace)

type Types = Map.Map String [Label]


data Val = Match [String] | Not [String] deriving (Show)
type Feat = (String, Val)

data Order = NA | Before String | After String deriving (Show)

data QueryPattern = QueryPattern {
    pos :: Maybe [String],
    rel :: Maybe String,
    morpho :: Maybe [Feat],
    idx :: Ident,
    var_type :: Type,
    lin_order :: Order
} deriving (Show)

--query :: Tree Node -> ((String, Maybe String), (String, Maybe String)) -> IO [(Node, Node)]
query trees fun = do
  t <- trees
  let p = pairs fun []
  -- fun to tuples
  res (head p) t
  where
    pairs [a,b]      res = res ++ [(a,b)]
    pairs (a:b:rest) res = pairs rest (res ++ [(a,b)])

    res patt t = filter (\(t, e) -> matchEdges patt e) (edges t)




learnPattern cfg cnc gr smarts pat name pattern = do

    putStrLn ("== " ++ showIdent name ++ " ==" )
    -- TODO: generalize
    let QueryPattern {idx=ap, var_type=a_ty} = pattern !! 1
        QueryPattern {idx=cn, var_type=n_ty} = pattern !! 0
        patts = do  -- query edges matching AdjCN
          (n1, n2) <- pat
          --let (_,_,_,p,_) = n1 
          --trace (show (filter (\(k,v) -> k `notElem` (map fst p)) (cfgDefaults cfg)) ++ " " ++ show (insertDefaults cfg n1)) $ return ()
          return [(insertDefaults cfg n1,Vr cn,n_ty),(insertDefaults cfg n2,Vr ap,a_ty)]

        cctxt = CodeContext [(Vr ap,a_ty),(Vr cn,n_ty)] -- argument types
                            patts                       -- matching patterns
                            smarts
    -- putStrLn ("Patterns: ")
    -- mapM_ (print . hsep . punctuate (pp ';') . map (\(n,_,_) -> ppNode n)) patts


    let res = fmap (Map.fromListWith (++)) $ runGenM $ do
                patt <- anyOf (patterns cctxt)

                inh <- runGenM $ do
                          ((_,_,_,morpho,_),t,ty) <- anyOf patt
                          findInh gr cctxt morpho t ty

                (vs,ts) <- mapAccumM (\vs ((id,_,_,morpho,_),t,ty) -> findStr gr cctxt vs id morpho t ty) [] patt
                let str = foldr (\(_,t1) t2 -> case t2 of {Empty -> t1; t2 -> C t1 t2}) Empty (sortOn fst ts)

                return ((str,length vs,length inh),[(reverse vs,inh)])

    m <- case res of
      Ok m    -> return m
      Bad msg -> fail msg

    terms <- forM (Map.toList m) $ \((t0,dim_dataset,dim_inh),dataset) -> do
     -- examples
      when (cfgVerbose cfg) $ do
        putStrLn "Pattern:"
        print (ppTerm Unqualified 0 t0)

        forM_ dataset $ \(vs,inh) ->
          print (hsep (map (\(_,t,_) -> ppTerm Unqualified 10 t) vs) <+> pp '|' <+>
                                        hsep (punctuate ";" (map (\(t1,t2,ty) -> pp t1 <> pp '=' <> pp t2 <+> pp ':' <+> pp ty) inh)))

      let types = map (\(_,_,ty)->ty) (fst (head dataset))
          (freq, accuracy,_,t, dt) = instantiate types dim_dataset dataset t0 []
      if (dim_dataset == 0 || dim_inh > 0) && accuracy > cfgSyntaxStopping cfg
        then do when (cfgVerbose cfg) $ do
                  putStrLn ""
                  putStrLn ("Found term with accuracy "++show accuracy++":")
                  print (pp (raw t))
                  putStrLn ""
                return (t, freq)

        else let res   = (sortOn (\(_,acc,_,_,_)-> -acc))
                            (map (\varIndex ->
                                    let var      = freshVar [] (types !! varIndex)
                                        subst0   = [(varIndex+1,Vr var)]
                                        dataset' = map (selectVars subst0) dataset
                                    in instantiate types dim_dataset dataset' t0 subst0)
                                 [0..dim_dataset-1])
            in case res of
                  ((freq,accuracy,subst0,t,dt):rest)
                    | null rest || accuracy > cfgSyntaxStopping cfg -> do
                          when (cfgVerbose cfg) $ do
                            putStrLn ""
                            putStrLn ("Found term with accuracy "++show accuracy++":")
                            print (pp (raw t))
                            putStrLn ""
                          return (t, freq)

                    | otherwise -> do
                          cross_breed types dim_dataset dataset t0 subst0 rest
                  [] -> do
                    return (Empty,0)

    return (filter (\(t,_) -> t /= Empty) terms)
  where
    -- applying raw makes the term more readable
    raw (T _ [(p,t)]) = T TRaw [(p,raw t)]
    raw t             = t

    selectVars subst0 ([],               inh) = ([], inh)
    selectVars subst0 (v_ty@(i,v,ty):vs, inh) =
      case lookup i subst0 of
        Just t  -> selectVars subst0 (vs,(t,v,ty):inh)
        Nothing -> let (vs',inh') = selectVars subst0 (vs,inh)
                   in (v_ty:vs',inh')

    decisionTree2term (Leaf t _) = t
    decisionTree2term (Decision (TermName t f) _ children) =
      let pairs = [(f k, decisionTree2term dt) | (k,dt) <- Map.toList children]
          (xs,ys) = partition (uncurry (==)) pairs
          (us,vs) = partitionEithers (map fun_pairs pairs)
          cs0 = [(p,t2) | (t1,t2) <- ys, Ok p <- [term2patt t1]]
          cs1 = [(p,t2) | (t1,t2) <- vs, Ok p <- [term2patt t1]]
      in case nub us of
           [u] -> case cs1 of
                    []  -> App u t
                    cs1 -> let x = identS "x"
                           in S (T TRaw (cs1++[(PV x,App u (Vr x))])) t
           _   -> case (xs,ys) of
                    ([],[]) -> Meta 0
                    ([],ys) -> S (T TRaw cs0) t
                    (xs,[]) -> t
                    (xs,ys) -> let x = identS "x"
                               in S (T TRaw (cs0++[(PV x,Vr x)])) t

      where
        term2patt :: Term -> Err Patt
        term2patt trm = do
          ([], QC c, aa) <- termForm trm
          aa' <- mapM term2patt aa
          return (PP c aa')

        fun_pairs (t,App t1 t2)
          | t == t2 = Left  t1
        fun_pairs p = Right p

    substitute subst (Meta i) =
      case lookup i subst of
        Just t  -> t
        Nothing -> Meta i
    substitute subst t = composSafeOp (substitute subst) t

    instantiate types dim_dataset dataset t0 subst0 =
      let t = foldr (\(i,Vr var) t -> T (TTyped (types !! (i-1))) [(PV var,t)]) (substitute subst t0) subst0
      in (length dataset', fromIntegral (length dataset')/fromIntegral (length dataset), subst0, t, dts)
      where
        attrs        = zipWith (\i (t,_,ty) -> A (TermName t (\(_,v,_) -> v)) ((!!i) . snd)) [0..] (snd (head dataset))
        (subst, dts) = unzip substAll
        (dataset',substAll) =
            mapAccumL
              (\dataset i ->
                   case lookup i subst0 of
                     Just t  -> (dataset, ((i,t),Nothing))
                     Nothing -> let (dataset',dt) = build attrs [(d,t) | d@(vs,inh) <- dataset, (i',t,_) <- vs, i'==i]
                                in (map fst dataset',((i,decisionTree2term dt), Just dt)))
              dataset
              [1..dim_dataset]

    cross_breed types dim_dataset dataset t0 subst0 ((_,_,subst0',_,_):rest) =
       let subst0'' = subst0++subst0'
           dataset' = map (selectVars subst0'') dataset
           (freq,accuracy,_, t, dt) = instantiate types dim_dataset dataset' t0 subst0''
       in if null rest || accuracy > cfgSyntaxStopping cfg
            then do when (cfgVerbose cfg) $ do
                      putStrLn ""
                      putStrLn ("Found term with accuracy "++show accuracy++":")
                      print (pp (raw t))
                      putStrLn ""
                    return (t, freq)
            else cross_breed types dim_dataset dataset t0 subst0'' rest


type POS  = String
type Node = (Int,String,POS,[(String,String)],String)


data CodeContext
   = CodeContext {
       args     :: [(Term,Type)],
       patterns :: [[(Node,Term,Type)]],
       smarts   :: Map.Map QIdent [(QIdent,Context)]
     }

noSmarts = Map.empty

data TermName a = TermName Term (a -> Term)


readCONLL :: Config -> String -> IO ([Tree Node], [Tree Node])
readCONLL cfg treebank = do
  let fdir  = "data" </> cfgLangName cfg
  let fpath = fdir </> treebank
  exist <- doesDirectoryExist fpath
  when (not exist) $ do
    createDirectoryIfMissing True fdir
    res <- system ("curl https://grew.fr/download/SUD_2.17/"++treebank++".tgz | tar -C "++fdir++" -xzf -")
    case res of
      ExitSuccess -> return ()
      _           -> exitWith res
  fs <- getDirectoryContents fpath
  let files = filter (isSuffixOf ".conllu") fs
  let test = filter (isSuffixOf "test.conllu") files
  let train = filter (\f -> f `notElem` test) files
  if null train || null test then do
    dataset <- fmap concat $ forM fs $ \f -> do
     if takeExtension f == ".conllu"
       then do
          ls <- fmap lines $ readFile (fpath </> f)
          return ((map (toTree "0" (0,"root","",[],"")) (stanzas ls)))
       else return []
    if cfgSplit cfg == 1 then do return (dataset, dataset)
    else do
      let trainRatio = round ((cfgSplit cfg) * fromIntegral (length dataset))
      return (take trainRatio dataset, drop trainRatio dataset)
  else do
    trainDataset <- fmap concat $ forM train $ \f -> do
      ls <- fmap lines $ readFile (fpath </> f)
      return ((map (toTree "0" (0,"root","",[],"")) (stanzas ls)))
    testDataset <- fmap concat $ forM test $ \f -> do
      ls <- fmap lines $ readFile (fpath </> f)
      return ((map (toTree "0" (0,"root","",[],"")) (stanzas ls)))

    return (trainDataset, testDataset)


  where
    stanzas []           = []
    stanzas (('#':_):ls) = stanzas ls
    stanzas ([]:ls)      = [] : stanzas ls
    stanzas (l:ls)       = case stanzas ls of
                             []     -> [[split '\t' l]]
                             (s:ss) -> (split '\t' l:s):ss

    toTree id lbl stanza =
      Node lbl [toTree (l!!0) (read (l!!0),l!!1,l!!3,toAttrs (l!!5),l!!7) stanza | l <- stanza, id==l!!6]

    toAttrs "_" = []
    toAttrs s   = map toAttr (split '|' s)
      where
        toAttr s =
          case break (=='=') s of
            (x,'=':y) -> (x,y)

matchEdges (p1, p2) (n1@(_,_,pos1,m1,rel1),n2@(_,_,pos2,m2,rel2)) = checkPos (pos p1) pos1 &&
                    checkPos (pos p2) pos2 && checkRel (rel p1) rel1 && checkRel (rel p2) rel2 && checkMorpho (morpho p1) m1 && checkMorpho (morpho p2) m2
  where
        checkRel p r = isNothing p || fromJust p == r
        checkMorpho q e = isNothing q || and (map (\m -> checkFeat m e) (fromJust q))

        checkFeat (feat, Match []) m2 = False
        checkFeat (feat, Match (v:val)) m2 | (feat, v) `elem` m2 = True
        checkFeat (feat, Match (v:val)) m2 | otherwise = checkFeat (feat, Match val) m2
        checkFeat (feat, Not []) m2 = True
        checkFeat (feat, Not (v:val)) m2 | (feat, v) `elem` m2 = False
        checkFeat (feat, Not (v:val)) m2  = checkFeat (feat, Not val) m2
        checkPos pos p = isNothing pos || p `elem` (fromJust pos)

insertDefaults cfg (idx, lemma, pos, patts, deprel) = (idx, lemma, pos, patts', deprel)
    where param = map fst patts
          patts' = patts ++ filter (\(k,v) -> k `notElem` param) (cfgDefaults cfg)

getFun :: Ident -> [Ident] -> Term -> (Ident, Info)
getFun name args f = (name, CncFun (Nothing) (Just (L NoLoc (getArgs args f))) Nothing Nothing)
  where
    getArgs [] f = f
    getArgs (arg:args) f = Abs Explicit arg (getArgs args f)

getModule cfg abs_mn jments =
   ModInfo
     { jments = Map.fromList jments
     , msrc=""
     , mstatus = MSComplete
     , mextend = [(cfgLangModuleName cfg "Cat", MIAll)]
     , mwith=Nothing
     , mopens=[OSimple (moduleNameS "Prelude"),OSimple (cfgLangModuleName cfg "Res")]
     , mexdeps=[]
     , mflags = noOptions
     , mtype = MTConcrete abs_mn
     }

createPlaceholder cfg gr lbl con_typ fun1 con1 fun2 con2 =
  (mkFun fun1 con1, mkFun fun2 con2, typ)
  where
    cRes = cfgLangModuleName cfg "Res"

    mkFun fun con =
      getFun fun [] (R ( (theLinLabel, (Nothing, Empty))
                       : if null values
                           then []
                           else take 1 [(lbl, (Nothing, value))
                                             | value <- values
                                             , let (QC (_,f),xs) = appForm value
                                             , f == con]))

    q_con_typ = (cRes, con_typ)
    typ = RecType ( (theLinLabel, [], Sort cStr)
                  : if null values
                      then []
                      else [(lbl, [], QC q_con_typ)])

    values =
      case lookupOrigInfo gr q_con_typ of
        Ok (_,ResParam _ (Just (ts,_))) -> ts
        _                               -> []

createNum cfg gr =
  createPlaceholder cfg gr
                    (LIdent (rawIdentS "n")) (identS "Number")
                    (identS "NumSg") (identS "Sg")
                    (identS "NumPl") (identS "Pl")

createArt cfg gr =
  createPlaceholder cfg gr
                    (LIdent (rawIdentS "sp")) (identS "Species")
                    (identS "IndefArt") (identS "Indef")
                    (identS "DefArt")   (identS "Def")

detQuant (RecType num) (RecType art) =
  (getFun name [det_id, num_id] (R (map (\x -> concatFields x (det_id, fs2) (num_id, fs1)) (nub $ fs1 ++ fs2))), RecType typ)
  where
    name = identS "DetQuant"
    det_id  = identS "det"
    num_id  = identS "num"

    fs typ = map (\(f, _, _) -> f) typ
    fs1 = fs num
    fs2 = fs art

    typ = nub $ num ++ art

    concatFields f (id1, fs1) (id2, fs2) | f `elem` fs1 && f `elem` fs2 = (f, (Nothing, C (P (Vr id1) f) (P (Vr id2) f)))
    concatFields f (id1, fs1) (id2, fs2) | f `elem` fs1 = getOneField f id1
    concatFields f (id1, fs1) (id2, fs2) | f `elem` fs2 = getOneField f id2

getOneField lbl arg = (lbl, (Nothing, P (Vr arg) lbl))

-- helper functions
unionPatts (patts1, p1) (patts2, p2) = filter (\p -> (getPosition p p2) `elem` patts1') patts2
    where
        getPosition patt 0 = fst patt
        getPosition patt 1 = snd patt

        patts1' = getPosition (unzip patts1) p1

combineTypes [] a_ps n_p = (map (const defLinType) a_ps,defLinType)
combineTypes ts a_ps n_p = (ap_tys,cn_ty)
  where
    -- group variations that come from word order
    varTrees = Map.fromListWith (++)
                                (map (\(t, freq) ->
                                          let f = getStrLabels t
                                          in (fromJust (lookup (idx n_p) f), [(map fst f,t,freq)]))
                                     ts)

    usedLabels = concatMap (getUsedLabels . fst) ts
    ap_tys = map (\a_p -> filterModFields usedLabels (idx a_p) (var_type a_p)) a_ps
    cn_ty = filterHeadFields [] usedLabels (idx n_p) (var_type n_p)

    filterHeadFields path used vr (RecType ltys) =
      RecType [(lbl,xs,filterHeadFields (lbl:path) used' vr ty)
                     | (lbl,xs,ty) <- ltys
                     , let used' = narrow used vr lbl
                     , not (null used')]
    filterHeadFields path used vr ty =
      case Map.lookup (reverse path) varTrees of
        Just ts -> let (ty_args_count0,ty') = unwrapTableTypes ty
                       ty_args_count
                          = foldl (\ty_args_count (_,t,_) ->
                                      let t_args_count   = (Map.toList . Map.fromListWith (+)) (unwrapTables t)
                                      in foldl (uncurry . increment) ty_args_count t_args_count)
                                  ty_args_count0
                                  ts
                   in reapply ty_args_count ty'
        Nothing -> ty
      where
        unwrapTableTypes (Table arg res) = ((arg,0):args,res')
          where (args,res') = unwrapTableTypes res
        unwrapTableTypes ty              = ([],ty)

        unwrapTables (T (TTyped ty) [(_,t)]) = (ty,1):unwrapTables t
        unwrapTables t                       = []

        increment xs          y 0 = xs
        increment []          y c = (y,1):increment [] y (c-1)
        increment ((x,c'):xs) y c
          | x == y    = (x,c'+1):increment xs y (c-1)
          | otherwise = (x,c')  :increment xs y c

        reapply []             ty = ty
        reapply ((arg,n):args) ty
          | n == 0    = reapply args ty
          | otherwise = Table arg (reapply args ty)

    filterModFields used vr (RecType ltys) =
      RecType [(lbl,xs,filterModFields used' vr ty)
                     | (lbl,xs,ty) <- ltys
                     , let used' = narrow used vr lbl
                     , not (null used')]
    filterModFields used vr ty = ty

    narrow used vr lbl = [(vr,lbls) | (vr',lbl':lbls) <- used
                                    , vr==vr', lbl==lbl']


combineTerms gr funName [] mb_var_isPre n_p cn_ty argNames = (False, Nothing)
combineTerms gr funName ts mb_var_isPre n_p cn_ty argNames =
  let (used_isPre, t) = mkRecord [] False cn_ty
  in (used_isPre, Just (showIdent abs_mn, [getFun funName argNames t]))
  where
    Ok (MN abs_mn,_) = lookupOrigInfo gr (moduleNameS "Lang",funName)

    -- group variations that come from word order
    varTrees = Map.fromListWith (++)
                                (map (\(t, freq) ->
                                          let f = getStrLabels t
                                          in (fromJust (lookup (idx n_p) f), [(map fst f,t,freq)]))
                                     ts)

    mkRecord lbls used_isPre (RecType ltys) =
      let (used_isPre',lts) =
               mapAccumL (\s (lbl,_,ty) ->
                               let (s',t) = mkRecord (lbl:lbls) s ty
                               in (s',(lbl,(Nothing, t))))
                         used_isPre
                         (filter (\(lbl,_,_)->isNothing (isLockLabel lbl)) ltys)
      in (used_isPre',R lts)
    mkRecord lbls used_isPre ty =
      case Map.lookup (reverse lbls) varTrees of
        Nothing -> (used_isPre, foldl P (Vr (idx n_p)) lbls)
        Just [(_,def,_)] -> (used_isPre, normalizeTbl ty def)
        Just defs ->
          case mb_var_isPre of
            Just var_isPre -> getDefs var_isPre used_isPre (unzip3 defs)
            Nothing        -> let (_,def,_):_ = sortOn (\(_,_,rank) -> -rank) defs
                              in (used_isPre, normalizeTbl ty def)
      where
        getDefs var_isPre used_isPre (order, defs', rs)
          | length (nub order) == 1 = (used_isPre, normalizeTbl ty (fst (head (sortOn snd (zip defs' rs)))))
        getDefs var_isPre used_isPre (order1:orders, def1:restDefs,_) =
          (True,
           S (T TRaw [getPreOrPost order1 (normalizeTbl ty def1), getPreOrPost order2 (normalizeTbl ty def2)])
             (P (Vr var_isPre) cIsPre))
          where
            (order2, def2) = head (filter (\(x, y) -> x /= order1) (zip orders restDefs))
            getPreOrPost o def
              | head o == var_isPre = (PP (cPrelude, cTrue)  [], def)
              | otherwise           = (PP (cPrelude, cFalse) [], def)

    normalizeTbl ty t = reorderTables [] t
      where
        reorderTables args (T (TTyped ty) [(PV v,t)]) = reorderTables ((ty,v):args) t
        reorderTables args t                          = reconstruct args ty t
          where
            reconstruct args (Table arg res) t =
              T TRaw [(PV v,reconstruct args res t)]
              where
                v = fromMaybe identW (lookup arg args)
            reconstruct args _               t = t

combineOneTerms gr funName [] mb_var_isPre a_p n_p argNames = (defLinType,defLinType,False,Nothing)
combineOneTerms gr funName ts mb_var_isPre a_p n_p argNames =
  let ([ap_ty],cn_ty) = combineTypes ts [a_p] n_p
      (used_isPre, fun) = combineTerms gr funName ts mb_var_isPre n_p cn_ty argNames
  in (ap_ty,cn_ty,used_isPre,fun)

combineTermsWTypes gr funName [] mb_var_isPre a_p n_p argNames = (False,Nothing)
combineTermsWTypes gr funName ts mb_var_isPre a_p n_p argNames =
  let cn_ty = var_type n_p
  in combineTerms gr funName ts mb_var_isPre n_p cn_ty argNames

cPrelude = moduleNameS "Prelude"
cIsPre = LIdent (rawIdentS "isPre")

extendTypeWithIsPre (RecType ltys)
  | not (any (\(l,_,_) -> l == cIsPre) ltys) =
      RecType (ltys ++ [(cIsPre,[],Q (cPrelude,cBool))])
extendTypeWithIsPre ty             = ty

extendTermWithIsPre t = ExtR t (R [(cIsPre,(Nothing,Q (cPrelude,cTrue)))])

getUsedLabels t@(P _ lbl) =
  let (var,lbls) = unpack t in [(var, reverse lbls)]
  where
    unpack (Vr var)  = (var,[])
    unpack (P t lbl) = let (var,lbls) = unpack t in (var,lbl:lbls)
getUsedLabels t = collectOp getUsedLabels t

getStrLabels t@(P _ lbl) =
  let (var,lbls) = unpack t in [(var, reverse lbls)]
  where
    unpack (Vr var)  = (var,[])
    unpack (P t lbl) = let (var,lbls) = unpack t in (var,lbl:lbls)
getStrLabels (S t _) = getStrLabels t
getStrLabels t = collectOp getStrLabels t

nodes t = collect t []
  where
    collect t@(Node n children) ns = foldr collect (n:ns) children

edges t = collect t []
  where
    collect t@(Node n1 children) es = foldr collect ([(t, (n1,n2)) | Node n2 _ <- children]++es) children

subtrees t = collect t []
  where
    collect t@(Node n children) ts = foldr collect (t:ts) children

ppNode (id,lemma,pos,morph,rel) = pp id <+> pp lemma <+> pp pos <+> pp (show morph) <+> pp rel

ppEdge (n1,n2) = ppNode n1 <+> pp "->" <+> ppNode n2

split sep []     = []
split sep (c:cs)
  | c == sep  = [] : split sep cs
  | otherwise = case split sep cs of
                  []     -> [[c]]
                  (x:xs) -> (c:x):xs

findStr gr cfg vs id morpho t (Sort s) | s == cStr  = return (vs,(id,t))
findStr gr cfg vs id morpho t (RecType fs) = do
  (l,_,ty) <- anyOf fs
  morpho <- foldM (flip pop) morpho [v | t <- all_tags, label t==l, v <- ud_tag t]
  findStr gr cfg vs id morpho (P t l) ty
findStr gr cfg vs id morpho t (Table arg res) = do
  v <- findParam gr cfg morpho arg
  let i = length vs+1
      p = Meta i
  findStr gr cfg ((i,v,arg):vs) id morpho (S t p) res
findStr gr cfg vs id morpho t ty = empty

findParam gr cfg morpho (QC q) = do
  (q,ctxt) <- case Map.lookup q (smarts cfg) of
                Just ps -> anyOf ps
                Nothing -> do (mn,info) <- lookupOrigInfo gr q
                              case info of
                                ResParam (Just (L _ ps)) _ -> do (id,ctxt) <- anyOf ps
                                                                 return ((mn,id),ctxt)
                                _                          -> raise $ render (ppQIdent Qualified q <+> "has no parameter values defined")
  morpho <- foldM (flip pop) morpho [v | t <- all_tags, ident t==snd q, v <- ud_tag t]
  foldM (\t (_,_,ty) -> fmap (App t) (findParam gr cfg morpho ty)) (QC q) ctxt

findInh gr cfg morpho t ty@(QC q) = do
  v <- findParam gr cfg morpho ty
  return (t,v,ty)
findInh gr cfg morpho t (RecType fs) = do
  (l,_,ty) <- anyOf fs
  morpho  <- foldM (flip pop) morpho [v | t <- all_tags, label t==l, v <- ud_tag t]
  findInh gr cfg morpho (P t l) ty
findInh gr cfg morpho t ty = empty

generateTerm gr env lbls (Prod bt _ arg res) =
  let x = freshVar env arg
      t = generateTerm gr ((Vr x,arg):env) lbls res
  in Abs bt x t
generateTerm gr env lbls (Table arg res) =
  let x = freshVar env arg
  in T TRaw [(PV x,generateTerm gr ((Vr x,arg):env) lbls res)]
generateTerm gr env lbls (RecType ltys) =
  R [(l,(Nothing,generateTerm gr env (l:lbls) ty)) | (l,[],ty) <- ltys, isNothing (isLockLabel l)]
generateTerm gr env lbls (Sort s)
  | s == cStr = fromMaybe Empty (concatArgs (reverse env))
  where
    concatArgs []            = return Empty
    concatArgs ((t,ty):args) =
      case firstValue gr env (reverse lbls) t ty (Sort cStr) of
        Nothing -> concatArgs args
        Just t1 -> do t2 <- concatArgs args
                      case t2 of
                        Empty -> return t1
                        _     -> return (C t1 t2)
generateTerm gr env lbls ty0@(QC c) = select env
  where
    select []           =
      case lookupOrigInfo gr c of
        Ok (mn,ResParam (Just (L _ ((fn,ctxt):_))) _)
            -> foldl (\t (_,_,ty) -> App t (generateTerm gr env lbls ty)) (QC (mn,fn)) ctxt
        _   -> FV []
    select ((t,ty):env) =
      case firstValue gr env [] t ty ty0 of
        Just t  -> t
        Nothing -> select env

firstValue gr env lbls t (Table arg res) ty0 = do
  t' <- select env
  firstValue gr env lbls (S t t') res ty0
  where
    select []           =
      case allParamValues gr arg of
        Ok (t:ts) -> return t
        _         -> return (FV [])
    select ((t,ty):env) =
      firstValue gr env lbls t ty arg <|> select env
firstValue gr env lbls t (RecType ltys) ty0 =
  let preselect =
        case lbls of
          []     -> [firstValue gr env [] (P t l') ty ty0 | (l',_,ty) <- ltys, l'==theLinLabel]
          (l:ls) -> [firstValue gr env ls (P t l') ty ty0 | (l',_,ty) <- ltys, l'==l]
  in case preselect of
      (Just t:_) -> return t
      _          -> selectAny ltys
  where
    selectAny []              = empty
    selectAny ((l,_,ty):ltys) =
      case firstValue gr env lbls (P t l) ty ty0 of
        Just t  -> return t
        Nothing -> selectAny ltys
firstValue gr env lbls t ty ty0
  | ty == ty0 = return t
  | otherwise = empty

freshVar env ty = fresh (letter ty) 1
  where
    letter (QC (_,c)) =
      convert (showIdent c)
    letter (RecType xs) =
      --case [cat | (l,_,_) <- xs, Just cat <- [isLockLabel l]] of
      case [id | (lbl,_,_) <- xs, Just id <- [isLockLabel lbl]] of
        [cat] -> convert (showRawIdent cat)
        _     -> "v"
    letter _ = "v"

    convert [c1,c2] = [toLower c1,toLower c2]
    convert (c:_)   = [toLower c]

    fresh c i =
      let v | i == 1    = identS c
            | otherwise = identS (c++show i)
      in case [x | (Vr x,_) <- env, x == v] of
           [] -> v
           _  -> fresh c (i+1)

newtype GenM a = GenM (forall r . Set.Set QIdent -> (a -> r -> Err r) -> r -> Err r)

instance Functor GenM where
  fmap f (GenM g) = GenM (\s k -> g s (k . f))

instance Applicative GenM where
  pure x  = GenM (\s k -> k x)
  (GenM h) <*> (GenM g) = GenM (\s k -> h s (\f -> g s (k . f)))

instance Alternative GenM where
  empty = GenM (\_ k -> Ok)
  (GenM h) <|> (GenM g) = GenM (\s k r -> case g s k r of
                                            Ok r    -> h s k r
                                            Bad msg -> Bad msg)

instance Monad GenM where
  (GenM h) >>= g = GenM (\s k -> h s (\x -> case g x of {GenM g -> g s k}))

instance MonadFail GenM where
  fail msg = GenM (\_ k _ -> Bad msg)

instance ErrorMonad GenM where
  raise msg = GenM (\_ k _ -> Bad msg)
  handle (GenM g) h = GenM (\s k r -> case g s k r of
                                        Bad msg -> case h msg of
                                                     GenM h -> h s k r
                                        Ok r    -> Ok r)


runGenM (GenM g) =
  case g Set.empty (\x xs -> Ok (x:xs)) [] of
    Ok xs   -> return xs
    Bad msg -> raise msg


anyOf xs = GenM (choose xs)
  where
    choose []     s k r = Ok r
    choose (x:xs) s k r = case k x r of
                            Ok r    -> choose xs s k r
                            Bad msg -> Bad msg

-- fix the pop issue
pop x  []     = empty
pop x0 (x:xs)
  | x == x0   = return xs
  | otherwise = do xs <- pop x0 xs
                   return (x:xs)

mapAccumM :: Monad m => (s -> a -> m (s, b)) -> s -> [a] -> m (s, [b])
mapAccumM f s []     = return (s,[])
mapAccumM f s (x:xs) = do
  (s,x)  <- f s x
  (s,xs) <- mapAccumM f s xs
  return (s,x:xs)
