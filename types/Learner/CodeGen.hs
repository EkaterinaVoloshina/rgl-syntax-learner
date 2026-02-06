module Learner.CodeGen
              (readCONLL,Node(..),ppNode,drawTree,
               identS,Ident,
               rawIdentS,RawIdent,
               noSmarts,
               learnPattern, query, getPosFun, 
               getFun, getModule, matchFields,
               combineTrees, getNewType, 
               QueryPattern(..)) where

import Prelude hiding ((<>))
import GF.Infra.Ident
import GF.Infra.Option
import GF.Data.Operations
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
import Data.List (sortOn,mapAccumL,partition,nub)
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
    pos :: String,
    rel :: Maybe String, 
    morpho :: Maybe [Feat],
    idx :: String
} deriving (Show)

mapPOS fun = (mapOne (snd fun), mapOne (fst fun))
  where mapOne f = showIdent (posCat (fromJust (lookupUPOS (pos f))))


--query :: Tree Node -> ((String, Maybe String), (String, Maybe String)) -> IO [(Node, Node)]
query trees fun = do 
  t <- trees
  res <- filter (\(t, e) -> matchEdges fun e) (edges t)
  return res
    
learnPattern cfg cnc gr smarts pat name pattern = do
    let (pos1, pos2) = mapPOS pattern
    putStrLn ("== " ++ name ++ " ==" )
    
    a_ty <- lookupResDef gr (cnc,identS pos1)
    n_ty <- lookupResDef gr (cnc,identS pos2)
    
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

    --mapM_ (print . hsep . punctuate (pp ';') . map (\(n,_,_) -> ppNode n)) patts

    let res = fmap (Map.fromListWith (++)) $ runGenM $ do
                patt <- anyOf (patterns cctxt)

                inh <- runGenM $ do
                          ((_,_,_,morpho,_),t,ty) <- anyOf patt
                          findInh gr cctxt morpho t ty

                (str,vs) <- buildStr [] (sortOn (\((id,_,_,_,_),_,_)->id) patt)
                                        (\vs ((_,_,_,morpho,_),t,ty) -> findStr gr cctxt morpho vs t ty)
                return ((str,length vs,length inh),[(reverse vs,inh)])
    
    m <- case res of
      Ok m    -> return m
      Bad msg -> fail msg
    

    fieldsM <- forM (Map.toList m) $ \((t0,dim_dataset,dim_inh),dataset) -> do
    -- print (ppTerm Unqualified 0 t0)

     -- examples
      when (cfgVerbose cfg) $
        forM_ dataset $ \(vs,inh) ->
          print (hsep (map (\(_,t,_) -> ppTerm Unqualified 10 t) vs) <+> pp '|' <+>
                                        hsep (punctuate ";" (map (\(t1,t2,ty) -> pp t1 <> pp '=' <> pp t2 <+> pp ':' <+> pp ty) inh)))

      let (freq, accuracy,_,t) = instantiate dim_dataset dataset t0 []
      if dim_inh > 0 && accuracy > stopping
        then do when (cfgVerbose cfg) $ do
                  putStrLn ""
                  putStrLn ("=== "++show accuracy)
                  print (pp t)
                return ((t, freq), getIdent (unpackT t))                
                
        else let types = map (\(_,_,ty)->ty) (fst (head dataset))
                 res   = (reverse . sortOn (\(_,acc,_,_)->acc))
                            (map (\varIndex ->
                                    let var      = freshVar [] (types !! varIndex)
                                        subst0   = [(varIndex+1,Vr var)]
                                        dataset' = map (selectVars subst0) dataset
                                    in instantiate dim_dataset dataset' t0 subst0)
                                [0..dim_dataset-1])
            in case res of
                  ((freq,accuracy,subst0,t):rest)
                    | null rest || accuracy > stopping -> do
                          when (cfgVerbose cfg) $ do
                            putStrLn ""
                            putStrLn ("=== "++show accuracy)
                            print (pp t)
                          return ((t, freq), getIdent (unpackT t))
                          
                    | otherwise -> do
                          cross_breed dim_dataset dataset t0 subst0 rest
    let (trees, fsM) = unzip fieldsM 
    if (null trees) then do
      putStrLn "No examples of such construction"
      return ([], [])
    else do 
      let keepN = filterFields cn (getFields nTs) (concat fsM)
      let keepA = filterFields ap (getFields aTs) (concat fsM)
      let lincatCN = RecType (filter (\(LIdent idx,_, _) -> showRawIdent idx `elem` keepN) nTs)
      --print (pp lincatCN)
      -- get fields to be filled 
      -- match with trees
      -- if there exists something else make a new parameter?
      let mappedTrees = map (\t -> (getOneIdent (unpackT (fst t)), t)) trees

      return (mappedTrees, [(showIdent cn, keepN), (showIdent ap, keepA)])
       

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
    getOneIdent (P (Vr idx) (LIdent field)) = [(showIdent idx, showRawIdent field)]
    getOneIdent (S (T _ t) _) = concat (map getTerm t)
    getOneIdent _  = []



    stopping = 0.95

    buildStr s []     f = return (Empty,s)
    buildStr s [x]    f = f s x
    buildStr s (x:xs) f = do (t1,s) <- f s x
                             (t2,s) <- buildStr s xs f
                             return (C t1 t2,s)

    selectVars subst0 ([],               inh) = ([], inh)
    selectVars subst0 (v_ty@(i,v,ty):vs, inh) =
      case lookup i subst0 of
        Just t  -> selectVars subst0 (vs,(t,v,ty):inh)
        Nothing -> let (vs',inh') = selectVars subst0 (vs,inh)
                   in (v_ty:vs',inh')

    decisionTree2term (Leaf t _) = t
    decisionTree2term (Decision (TermName t f) _ children) =
      let (xs,ys) = partition (uncurry (==))
                       [(f k,decisionTree2term dt) | (k,dt) <- Map.toList children]
          cs0 = [(p,t) | (t1,t) <- ys, Ok p <- [term2patt t1]]
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
      in (length dataset', fromIntegral (length dataset')/fromIntegral (length dataset), subst0, t)
      where
        attrs            = zipWith (\i (t,_,ty) -> A (TermName t (\(_,v,_) -> v)) ((!!i) . snd)) [0..] (snd (head dataset))
        (dataset',subst) =
            mapAccumL
              (\dataset i ->
                   case lookup i subst0 of
                     Just t  -> (dataset, (i,t))
                     Nothing -> let (dataset',dt) = build attrs [(d,t) | d@(vs,inh) <- dataset, (i',t,_) <- vs, i'==i]
                                in (map fst dataset',(i,decisionTree2term dt)))
              dataset
              [1..dim_dataset]

    cross_breed dim_dataset dataset t0 subst0 ((_,_,subst0',_):rest) =
       let subst0'' = subst0++subst0'
           dataset' = map (selectVars subst0'') dataset
           (freq,accuracy,_,t) = instantiate dim_dataset dataset' t0 subst0''
       in if null rest || accuracy > stopping
            then do when (cfgVerbose cfg) $ do
                      putStrLn ""
                      putStrLn ("=== "++show accuracy)
                      print (pp t)
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

matchEdges (p1, p2) (n1@(_,_,pos1,_,rel1),n2@(_,_,pos2,_,rel2)) = pos p1 == pos1 && pos p2 == pos2 && checkRel p1 rel1 && checkRel p2 rel2
  where checkRel p r = (rel p == Nothing) || fromJust (rel p) == r
   
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
    

getModule cfg name jments = ppModule Qualified (MN (identS (cfgLangModule cfg name)), ModInfo {jments = funs, msrc="", mstatus = MSComplete, mextend = [(MN (identS (cfgLangModule cfg "Res")), MIAll)], mwith=Nothing, mopens=[], mexdeps=[], mflags = noOptions, mtype = MTConcrete (MN  (identS name))})
  where funs = Map.fromList jments


combineTrees cfg funName name wo mod [] argMap argNames = (Nothing, [])
combineTrees cfg funName name wo mod fun argMap argNames = (Just (main, [getFun funName argNames (R fields)]), concat addArgs)
    where 
        modname = cfgLangModule cfg (fromJust (Map.lookup wo mod))
        main = fromJust (Map.lookup name mod)
        (fields, addArgs) = unzip (map (\x -> matchFields name wo modname x (Map.lookup x varTrees)) (fromJust (Map.lookup name argMap)))
        -- check that variation comes from word order variation
        varTrees = Map.fromListWith (++) (map (\(f, t) -> (snd (head (filter (\(x, y) -> x == name) f)), [(map fst f, t)])) fun)
    

--matchFields :: String -> String -> String -> String -> Maybe Term -> ((IdentTerm, [])
matchFields name wo mod field Nothing       = ((LIdent (rawIdentS field), (Nothing, P (Vr (identS name)) (LIdent (rawIdentS field)))),[])
matchFields name wo mod field (Just [(_,def)])   = ((LIdent (rawIdentS field), (Nothing, fst def)),[])
matchFields name wo mod field (Just defs) = getDefs name wo mod field (unzip defs)
  where 
        
        getDefs name wo mod field (order, defs')            | length (nub order) == 1 = ((LIdent (rawIdentS field), (Nothing, head (map fst (sortOn snd defs')))), [])
        getDefs name wo mod field (order, (def1:def2:rest)) | otherwise               =  ((LIdent (rawIdentS field), (Nothing, S (T TRaw [(PP (MN (identS mod), identS "pre") [] , fst def1), (PP (MN (identS mod), identS "post") [] , fst def2)]) (P (Vr (identS wo)) (LIdent (rawIdentS "wo"))))), [(wo, ["wo"])])

getNewType fields [] base = []       
getNewType fields ((def, tree):funs) base | el `elem` fields = ((el, [(map fst def, tree)]):(getNewType fields funs base))
  where el = snd (head (filter (\(x, a) -> x == base) def))
getNewType fields ((def, tree):funs) base | otherwise = (("s", [(map fst def, tree)]):(getNewType fields funs base))

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
getTerm (PP _ pp, _) = []

unpackT (T _ ((_, c@(C _ _)):ts)) = c
unpackT (T _ ((_, t@(T _ _)):ts)) = unpackT t
unpackT c@(C _ _) = c

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

findStr gr cfg morpho vs t (Sort s)
  | s == cStr                  = return (t,vs)
findStr gr cfg morpho vs t (RecType fs) = do
  (l,_,ty) <- anyOf fs
  morpho <- case [ud_tag t | t <- all_tags, label t==l] of
              (v:_) -> pop v morpho
              _     -> return morpho
  findStr gr cfg morpho vs (P t l) ty
findStr gr cfg morpho vs t (Table arg res) = do
  v <- findParam gr cfg morpho arg
  let i = length vs+1
      p = Meta i
  findStr gr cfg morpho ((i,v,arg):vs) (S t p) res
findStr gr cfg morpho vs t ty = empty

findParam gr cfg morpho (QC q) = do
  (q,ctxt) <- case Map.lookup q (smarts cfg) of
                Just ps -> anyOf ps
                Nothing -> do (mn,info) <- lookupOrigInfo gr q
                              case info of
                                ResParam (Just (L _ ps)) _ -> do (id,ctxt) <- anyOf ps
                                                                 return ((mn,id),ctxt)
                                _                          -> raise $ render (ppQIdent Qualified q <+> "has no parameter values defined")
  morpho <- case [ud_tag t | t <- all_tags, ident t==snd q] of
              (v:_) -> pop v morpho
              _     -> return morpho
  
  foldM (\t (_,_,ty) -> fmap (App t) (findParam gr cfg morpho ty)) (QC q) ctxt

findInh gr cfg morpho t ty@(QC q) = do
  v <- findParam gr cfg morpho ty
  return (t,v,ty)
findInh gr cfg morpho t (RecType fs) = do
  (l,_,ty) <- anyOf fs
  morpho <- case [ud_tag t | t <- all_tags, label t==l] of
              (v:_) -> pop v morpho
              _     -> return morpho
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

pop x  []     = empty
pop x0 (x:xs)
  | x0 == x   = return xs
  | otherwise = do xs <- pop x0 xs
                   return (x:xs)

with q (GenM g) =
  GenM (\s k r -> if Set.member q s
                    then Ok r
                    else g (Set.insert q s) k r)
