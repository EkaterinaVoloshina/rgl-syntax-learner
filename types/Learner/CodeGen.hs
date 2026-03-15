module Learner.CodeGen(readCONLL,Node(..),ppNode,drawTree,
               identS,Ident,
               rawIdentS,RawIdent, unionPatts,
               noSmarts,
               learnPattern, query, getPosFun, getIds,
               getFun, getModule, matchFields,
               combineTrees, mapPOS, getNewType, 
               getOneField, defArt, indefArt, detQuant, createArt, createNum,
               QueryPattern(..), Val(..)) where

import Debug.Trace(trace, traceShowId, traceShow)
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
import GF.Grammar.Macros (termForm,composSafeOp,typeFormCnc)
import GF.Compile
import System.Exit
import System.Process
import System.FilePath
import System.Directory
import Data.Tree
import Data.Maybe hiding (fromList)
import Data.Char(toLower)
import Data.List (sortOn,mapAccumL,partition,nub,isPrefixOf,isInfixOf)
import Control.Monad
import Control.Applicative hiding (Const)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Learner.Config hiding (POS)
import Learner.DecisionTree

type Types = Map.Map String [Label]


data Val = Match String | Not String  deriving (Show)
type Feat = (String, Val)

data QueryPattern = QueryPattern {
    pos :: Maybe [String],
    rel :: Maybe String, 
    morpho :: Maybe [Feat],
    idx :: String
} deriving (Show)

mapPOS fun = (mapOne (fst fun), mapOne (snd fun))
  where mapOne f = showIdent (posCat (fromJust (lookupUPOS (head (fromJust (pos f))))))


--query :: Tree Node -> ((String, Maybe String), (String, Maybe String)) -> IO [(Node, Node)]
query trees fun = do 
  t <- trees
  res <- filter (\(t, e) -> matchEdges fun e) (edges t)
  return res
    
learnPattern cfg cnc gr smarts pat name pattern pos typs = do
    
    putStrLn ("== " ++ name ++ " ==" )

    let (a_ty, n_ty) = typs
    {-let (pos1, pos2) = mapPOS pattern
    a_ty <- lookupResDef gr (cnc,identS pos1)
    n_ty <- lookupResDef gr (cnc,identS pos2)-}
    
    let RecType nTs = n_ty
    let RecType aTs = a_ty
    let nountypes = []


    
    let ap = identS (idx (snd pattern))
        cn = identS (idx (fst pattern))
        patts = do  -- query edges matching AdjCN
          (n1, n2) <- pat
          return [(n1,Vr cn,n_ty),(n2,Vr ap,a_ty)]
        
        cctxt = CodeContext [(Vr ap,a_ty),(Vr cn,n_ty)] -- argument types
                            patts                       -- matching patterns
                            smarts
    --putStrLn ("Patterns: ")
    --mapM_ (print . hsep . punctuate (pp ';') . map (\(n,_,_) -> ppNode n)) patts

    
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

    fieldsM <- forM (Map.toList m) $ \((t0,dim_dataset,dim_inh),dataset) -> do
     -- examples
      when (cfgVerbose cfg) $ do
        putStrLn "Pattern: "
       --print dataset
        print (ppTerm Unqualified 0 t0)

        forM_ dataset $ \(vs,inh) ->
          print (hsep (map (\(_,t,_) -> ppTerm Unqualified 10 t) vs) <+> pp '|' <+>
                                        hsep (punctuate ";" (map (\(t1,t2,ty) -> pp t1 <> pp '=' <> pp t2 <+> pp ':' <+> pp ty) inh)))

      let (freq, accuracy,_,t, dt) = instantiate dim_dataset dataset t0 []
      if dim_inh > 0 && accuracy > cfgSyntaxStopping cfg
        then do when (cfgVerbose cfg) $ do
                  putStrLn ""
                  putStrLn ("Found term with accuracy "++show accuracy++":")
                  -- print (catMaybes dt)
                  print (pp t)
                  putStrLn ""
                return ((t, freq), getIdent (unpackT t))                

        else let types = map (\(_,_,ty)->ty) (fst (head dataset))
                 res   = (sortOn (\(_,acc,_,_,_)-> -acc))
                            (map (\varIndex ->
                                    let var      = freshVar [] (types !! varIndex)
                                        subst0   = [(varIndex+1,Vr var)]
                                        dataset' = map (selectVars subst0) dataset
                                    in instantiate dim_dataset dataset' t0 subst0)
                                [0..dim_dataset-1])
            in case res of
                  ((freq,accuracy,subst0,t,dt):rest)
                    | null rest || accuracy > cfgSyntaxStopping cfg -> do
                          when (cfgVerbose cfg) $ do
                            putStrLn ""
                            putStrLn ("Found term with accuracy "++show accuracy++":")
                            print (pp t)     
                            putStrLn ""
                          return ((t, freq), getIdent (unpackT t))
                          
                    | otherwise -> do
                          cross_breed dim_dataset dataset t0 subst0 rest
                  [] -> do 

                    --print res
                    return ((Empty,0), [])
    let (trees, fsM) = unzip fieldsM 
    if (null trees) then do
      putStrLn "No examples of such construction"
      return ([], [], Empty)
    else do 
      let keepN = filterFields cn (getFields nTs) (concat fsM)
      let keepA = filterFields ap (getFields aTs) (concat fsM)
      let lincat = if pos == 0 then RecType (filter (\(LIdent idx, _, _) -> showRawIdent idx `elem` keepN) nTs) else RecType (filter (\(LIdent idx, _, _) -> showRawIdent idx `elem` keepA) aTs)
      
      let trees' = filter (\(t,_) -> t /= Empty) trees
      let mappedTrees = map (\t -> (getOneIdent (unpackT (fst t)), t)) trees'

      return (mappedTrees, [(showIdent cn, keepN), (showIdent ap, keepA)], lincat)
    
       

  where
    -- | helper functions to collect information about used fields    

    filterFields cl t inp = filter (\x -> (showIdent cl, x) `elem` inp) t

    getFields ts = map (\(LIdent idx, _, _) -> showRawIdent idx) ts

    getOneIdent (C s1 s2) = getOneIdent s1 ++ getOneIdent s2
    getOneIdent (S s1@(S _ _) s2@(S _ _)) = getOneIdent s1
    getOneIdent (S s@(S _ _) _) = getOneIdent s
    getOneIdent (S s@(P _ _) _) = getOneIdent s
    getOneIdent (S _ s@(S _ _)) = getOneIdent s
    getOneIdent (S _ s@(P _ _)) = getOneIdent s
    getOneIdent (P p@(P _ _) _) = getOneIdent p
    getOneIdent (P (Vr idx) (LIdent field)) = [(showIdent idx, showRawIdent field)]
    getOneIdent (S (T _ t) _) = concat (map getTerm t)
    getOneIdent _  = []

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
          cs0 = [(p,t2) | (t1,t2) <- ys, Ok p <- [term2patt t1]]
      in case (xs,ys) of
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

    substitute subst (Meta i) =
      case lookup i subst of
        Just t  -> t
        Nothing -> Meta i
    substitute subst t = composSafeOp (substitute subst) t

    instantiate dim_dataset dataset t0 subst0 =
      let t = foldr (\(_,Vr var) t -> T TRaw [(PV var,t)]) (substitute subst t0) subst0
      in (length dataset', fromIntegral (length dataset')/fromIntegral (length dataset), subst0, t, dts)
      where
        attrs            = zipWith (\i (t,_,ty) -> A (TermName t (\(_,v,_) -> v)) ((!!i) . snd)) [0..] (snd (head dataset))
        (subst, dts)    = unzip substAll
        (dataset',substAll) =
            mapAccumL
              (\dataset i ->
                   case lookup i subst0 of
                     Just t  -> (dataset, ((i,t),Nothing))
                     Nothing -> let (dataset',dt) = build attrs [(d,t) | d@(vs,inh) <- dataset, (i',t,_) <- vs, i'==i]
                                in (map fst dataset',((i,decisionTree2term dt), Just dt)))
              dataset
              [1..dim_dataset]

    cross_breed dim_dataset dataset t0 subst0 ((_,_,subst0',_,_):rest) =
       let subst0'' = subst0++subst0'
           dataset' = map (selectVars subst0'') dataset
           (freq,accuracy,_, t, dt) = instantiate dim_dataset dataset' t0 subst0''
       in if null rest || accuracy > cfgSyntaxStopping cfg
            then do when (cfgVerbose cfg) $ do
                      putStrLn ""
                      putStrLn ("Found term with accuracy "++show accuracy++":")
                      print (pp t)
                      --print (dt)
                      putStrLn ""
                    return ((t, freq), getIdent (unpackT t))
            else cross_breed dim_dataset dataset t0 subst0'' rest


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

readCONLL :: Config -> String -> IO [Tree Node]
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
  fmap concat $ forM fs $ \f -> do
     if takeExtension f == ".conllu"
       then do ls <- fmap lines $ readFile (fpath </> f)
               return (map (toTree "0" (0,"root","",[],"")) (stanzas ls))
       else return []
  where
    stanzas []           = []
    stanzas (('#':_):ls) = stanzas ls
    stanzas ([]:ls)      = [] : stanzas ls
    stanzas (l:ls)       = case stanzas ls of
                             []     -> [[split '\t' l]]
                             (s:ss) -> (split '\t' l:s):ss

    toTree id lbl stanza =
      Node lbl [toTree (l!!0) (read(l!!0),l!!1,l!!3,toAttrs (l!!5),l!!7) stanza | l <- stanza, id==l!!6]

    toAttrs "_" = []
    toAttrs s   = map toAttr (split '|' s)
      where
        toAttr s =
          case break (=='=') s of
            (x,'=':y) -> (x,y)

matchEdges (p1, p2) (n1@(_,_,pos1,m1,rel1),n2@(_,_,pos2,m2,rel2)) = checkPos (pos p1) pos1 && 
                    checkPos (pos p2) pos2 && checkRel (rel p1) rel1 && checkRel (rel p2) rel2 && checkMorpho (morpho p1) m1 && checkMorpho (morpho p2) m2
  where checkRel p r = isNothing p || fromJust p == r
        checkMorpho q e = isNothing q || and (map (\m -> checkFeat m e) (fromJust q))
        checkFeat (feat, Match val) m2 = (feat, val) `elem` m2
        checkFeat (feat, Not val) m2 = (feat, val) `notElem` m2
        checkPos pos p = isNothing pos || p `elem` (fromJust pos)
   
   
-- | functions to create a correct representation of syntax
--getPosFun :: String -> String -> Map.Map String [String] -> Term
getPosFun lang modmap name pos n ffs = getF (Map.lookup n ffs)
  
  where 
    getF Nothing = Nothing 
    getF (Just res) = Just (fromJust (Map.lookup pos modmap), [getFun name [pos] (getRecord pos res)])
    getRecord pos res = R (map (\x -> mapWithType x pos) res)
    mapWithType x pos = (LIdent (rawIdentS x), (Nothing, P (Vr (identS pos)) (LIdent (rawIdentS x))))


getFun :: String -> [String] -> Term -> (Ident, Info)
getFun name args f = (identS name, CncFun (Nothing) (Just (L NoLoc (getArgs args f))) Nothing Nothing)
  where 
    getArgs [] f = f 
    getArgs (arg:args) f = Abs Explicit (identS arg) (getArgs args f) 

getModule cfg cnc_mn abs_mn jments =
  ppModule Unqualified (cnc_mn,mo)
  where
    mo =
      ModInfo
        { jments = Map.fromList jments
        , msrc=""
        , mstatus = MSComplete
        , mextend = [(cfgLangModuleName cfg "Cat", MIAll)]
        , mwith=Nothing
        , mopens=[OSimple (cfgLangModuleName cfg "Res")]
        , mexdeps=[]
        , mflags = noOptions
        , mtype = MTConcrete abs_mn
        }

-- creates a placeholder 
artFields fields def | "Species" `elem` fields = [(theLinLabel, ((Nothing, Empty), Sort cStr)), (LIdent (rawIdentS "sp"), ((Nothing, getType def), (Sort (identS ("Species")))))] 
  where 
    w = words def 

    getType def | length w == 2 = App (Cn (identS (w !! 0))) (Cn (identS (w !! 1)))
    getType def | otherwise = Cn (identS def)

artFields fields def | otherwise               = [(theLinLabel, ((Nothing, Empty), Sort cStr))]

createPlaceholder name fields = (getFun name [] (R fs), tp)
    where (fs, tp) = unzip $ map (\(l, (val, ty)) -> ((l, val), (l, [], ty))) fields

createNum name value = createPlaceholder name fields
    where fields = [(theLinLabel, ((Nothing, Empty), Sort cStr)), (LIdent (rawIdentS "n"), ((Nothing, Cn (identS value)), Sort (identS "Number")))]

createArt name value fields sp | value `elem` sp = createPlaceholder name (artFields fields value)
createArt name value fields sp | otherwise = createPlaceholder name (artFields fields vs)
  where vs = head (filter (\x -> isPrefixOf value x) sp)

indefArt fields sp = createArt "IndefArt" "Indef" fields sp
defArt fields sp = createArt "DefArt" "Def" fields sp
-- concat in s; copy def

detQuant fields num art = (getFun "DetQuant" ["det", "num"] (R (map (\x -> concatFields x ("det", fs2) ("num", fs1)) (nub $ fs1 ++ fs2))), typ)
  where 
    fs typ = map (\(LIdent l, _, _) -> showRawIdent l) typ
    fs1 = fs num 
    fs2 = fs art

    typ = nub $ num ++ art
    
    concatFields f (id1, fs1) (id2, fs2) | f `elem` fs1 && f `elem` fs2 = (LIdent (rawIdentS f), (Nothing, C (P (Vr (identS id1)) (LIdent (rawIdentS f))) (P (Vr (identS id2)) (LIdent (rawIdentS f)))))
    concatFields f (id1, fs1) (id2, fs2) | f `elem` fs1 = getOneField f id1
    concatFields f (id1, fs1) (id2, fs2) | f `elem` fs2 = getOneField f id2

getOneField f arg = (LIdent (rawIdentS f), (Nothing, P (Vr (identS arg)) (LIdent (rawIdentS f))))

-- helper functions
unionPatts (patts1, p1) (patts2, p2) = filter (\p -> (getPosition p p2) `elem` patts1') patts2 
    where 
        getPosition patt 0 = fst patt
        getPosition patt 1 = snd patt

        patts1' = getPosition (unzip patts1) p1


combineTrees cfg funName name wo mod [] argMap argNames = (Nothing, [])
combineTrees cfg funName name wo mod fun argMap argNames = (Just (main, [getFun funName argNames (R fields)]), concat addArgs)
    where 
        modname = cfgLangModuleFileName cfg (fromJust (Map.lookup wo mod))
        main = fromJust (Map.lookup name mod)
        (fields, addArgs) = unzip (map (\x -> matchFields name wo modname x (Map.lookup x varTrees)) (fromJust (Map.lookup name argMap)))
        -- check that variation comes from word order variation
        varTrees = Map.fromListWith (++) (map (\(f, t) -> (snd (head (filter (\(x, y) -> x == name) f)), [(map fst f, t)])) fun)
    

--matchFields :: String -> String -> String -> String -> Maybe Term -> ((IdentTerm, [])
matchFields name wo mod field Nothing       = ((LIdent (rawIdentS field), (Nothing, P (Vr (identS name)) (LIdent (rawIdentS field)))),[])
matchFields name wo mod field (Just [(_,def)])   = ((LIdent (rawIdentS field), (Nothing, fst def)),[])
matchFields name "" mod field (Just defs) = ((LIdent (rawIdentS field), (Nothing, head (map fst (sortOn snd defs')))), [])
  where (order, defs') = unzip defs
matchFields name wo mod field (Just defs) = getDefs name wo mod field (unzip defs)
  where 
        getDefs name wo mod field (order, defs')            | length (nub order) == 1 = ((LIdent (rawIdentS field), (Nothing, head (map fst (sortOn snd defs')))), [])
        getDefs name wo mod field (order1:order2:restO, (def1:def2:rest)) | otherwise               =  ((LIdent (rawIdentS field), (Nothing, S (T TRaw [getPreOrPost wo  order1 (fst def1), getPreOrPost wo order2 (fst def2)]) (P (Vr (identS wo)) (LIdent (rawIdentS "isPre"))))), [(wo, ["isPre"])])

        getPreOrPost wo o def | o !! 0 == wo = (PP (MN (identS "Prelude"), identS "True") [] , def)
        getPreOrPost wo o def | otherwise    = (PP (MN (identS "Prelude"), identS "False") [] , def)

getNewType fields [] base = []       
getNewType fields ((def, tree):funs) base | el `notElem` fields = ((el, [(map fst def, tree)]):(getNewType fields funs base))
  where el = snd (head (filter (\(x, a) -> x == base) def))
getNewType fields ((def, tree):funs) base | otherwise = (("s", [(map fst def, tree)]):(getNewType fields funs base)) 

--getNewType ((LIdent idx, typ):fields) funs base = base

getIds typ = map (\(LIdent idx,_,_) -> showRawIdent idx) typ
getIdent (C s1 s2) = getIdent s1 ++ getIdent s2
getIdent (S s1@(S _ _) s2@(S _ _)) = getIdent s1 ++ getIdent s2
getIdent (S s1@(S _ _) s2@(P _ _)) = getIdent s1 ++ getIdent s2
getIdent (S s1@(P _ _) s2@(S _ _)) = getIdent s1 ++ getIdent s2
getIdent (S s@(S _ _) _) = getIdent s
getIdent (S _ s@(S _ _)) = getIdent s
getIdent (S _ s@(P _ _)) = getIdent s
getIdent (S s@(P _ _) _) = getIdent s
getIdent (P (Vr idx) (LIdent field)) = [(showIdent idx, showRawIdent field)]
getIdent (S (T _ t) _) = concat (map getTerm t)
getIdent _  = []

getTerm (PP _ _, s@(S _ _)) = getIdent s
getTerm _ = []

unpackT (T _ ((_, c@(C _ _)):ts)) = c
unpackT (T _ ((_, t@(T _ _)):ts)) = unpackT t
unpackT c@(C _ _) = c
unpackT t = t

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

freshVar env ty = fresh (letter ty) 1
  where
    letter (QC (_,c)) =
      convert (showIdent c)
    letter (RecType xs) =
      --case [cat | (l,_,_) <- xs, Just cat <- [isLockLabel l]] of
      case [id | (LIdent id,_,_) <- xs, Just _ <- [isLockLabel (LIdent id)]] of
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
