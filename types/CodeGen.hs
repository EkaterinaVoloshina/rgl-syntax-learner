module CodeGen(readCONLL,Node(..),ppNode,drawTree,
               loadGrammar,
               identS,Ident,
               rawIdentS,RawIdent,
               noSmarts,
               learn) where

import Prelude hiding ((<>))
import GF.Infra.Ident
--import GF.Infra.Location
import GF.Infra.Option
import GF.Data.Operations
import GF.Text.Pretty hiding (empty)
import GF.Grammar.Lookup
--import GF.Grammar.Parser(runP)
import GF.Grammar.Printer
import GF.Grammar.Predef
import GF.Grammar.Grammar hiding (Rule(..))
import GF.Grammar.Lockfield
import GF.Grammar.Macros (term2patt,composSafeOp,typeFormCnc)
import GF.Compile
import System.FilePath
import System.Directory
import Data.Tree
import Data.Maybe hiding (fromList)
import Data.Char(toLower)
import Data.List (sortOn,mapAccumL,partition)
import Control.Monad
import Control.Applicative hiding (Const)
import qualified Data.Set as Set
import qualified Data.Map as Map
import DecisionTree

type Types = Map.Map String [Label]

-- mapping from UD to GF
posmap = Map.fromList ([("NOUN", "N"), ("ADJ", "A"), ("ADV", "Adv"), ("DET", "Det"), ("ADP", "Prep")])
fun2Type = Map.fromList([("AdjCN", ("cn", "ap")), ("AdAP", ("ada", "adj")), ("AdvAP", ("ap", "adv")), ("AdvCN", ("cn", "adv")), ("PrepNP", ("prep", "np"))])

funsAll = [("AdjCN", (("NOUN", Nothing),("ADJ", Just "mod"))),
         ("AdAP",(("ADJ", Nothing),("ADV", Just "mod"))),
         ("AdvAP",(("ADJ", Nothing),("NOUN", Just "mod"))),
        -- ["DetAP",(("ADJ", Nothing),("DET", Just "det@adj")), ()],
         ("PrepNP", (("ADP", Nothing), ("NOUN", Just "comp:obj"))),
         ("AdvCN",(("NOUN", Nothing), ("NOUN", Just "mod")))] -- preposition: empty or existing
         -- TODO: check how it matches deep dependencies
         -- TODO

-- keep results of the previous functions 

learn cnc gr mapping smarts trees = do

  allFs <- forM funsAll $ \funs -> do 
    let (name, fun) = funs
    let args = fromJust (Map.lookup name fun2Type)
    putStrLn ("== " ++ name ++ " ==" )

    let pos1 = fromJust (Map.lookup (fst (snd fun)) posmap)
    let pos2 = fromJust (Map.lookup (fst (fst fun)) posmap)
    a_ty <- lookupResDef gr (cnc,identS pos1)
    n_ty <- lookupResDef gr (cnc,identS pos2)
    
    let RecType nTs = n_ty
    let RecType aTs = a_ty
    let nountypes = []
    -- prependModule
    
    let ap = identS (snd args)
        cn = identS (fst args)
        patts = do  -- query edges matching AdjCN
          t <- trees
          (n1, n2) <- filter (\e -> matchEdges fun e) (edges t)
          return [(n1,Vr cn,n_ty),(n2,Vr ap,a_ty)]
        
        cfg = Config [(Vr ap,a_ty),(Vr cn,n_ty)] -- argument types
                    patts                       -- matching patterns
                    mapping
                    smarts

    -- mapM_ (print . hsep . punctuate (pp ';') . map (\(n,_,_) -> ppNode n)) patts

    let res = fmap (Map.fromListWith (++)) $ runGenM $ do
                patt <- anyOf (patterns cfg)

                inh <- runGenM $ do
                          ((_,_,_,morpho,_),t,ty) <- anyOf patt
                          findInh gr cfg morpho t ty

                (str,vs) <- buildStr [] (sortOn (\((id,_,_,_,_),_,_)->id) patt)
                                        (\vs ((_,_,_,morpho,_),t,ty) -> findStr gr cfg morpho vs t ty)
                return ((str,length vs,length inh),[(reverse vs,inh)])
    
    m <- case res of
      Ok m    -> return m
      Bad msg -> fail msg
    

    fieldsM <- forM (Map.toList m) $ \((t0,dim_dataset,dim_inh),dataset) -> do
      putStrLn ""
    -- print (ppTerm Unqualified 0 t0)

     -- examples
      --forM_ dataset $ \(vs,inh) ->
      --  print (hsep (map (\(_,t,_) -> ppTerm Unqualified 10 t) vs) <+> pp '|' <+>
      --                                hsep (punctuate ";" (map (\(t1,t2,ty) -> pp t1 <> pp '=' <> pp t2 <+> pp ':' <+> pp ty) inh)))

      let (accuracy,_,t) = instantiate dim_dataset dataset t0 []
      
      if dim_inh > 0 && accuracy > stopping
        then do putStrLn ""
                putStrLn ("=== "++show accuracy)
                
                print (pp t)
                return (t, getIdent (unpackT t))
                
                
                
        else let types = map (\(_,_,ty)->ty) (fst (head dataset))
                 res   = (reverse . sortOn (\(acc,_,_)->acc))
                            (map (\varIndex ->
                                    let var      = freshVar [] (types !! varIndex)
                                        subst0   = [(varIndex+1,Vr var)]
                                        dataset' = map (selectVars subst0) dataset
                                    in instantiate dim_dataset dataset' t0 subst0)
                                [0..dim_dataset-1])
            in case res of
                  ((accuracy,subst0,t):rest)
                    | null rest || accuracy > stopping -> do
                          putStrLn ""
                          putStrLn ("=== "++show accuracy)
                          print (pp t)
                          return (t, getIdent (unpackT t))
                          
                    | otherwise -> do
                          cross_breed dim_dataset dataset t0 subst0 rest
    let (trees, fsM) = unzip fieldsM 
    if (null trees) then do
      putStrLn "No examples of such construction"
      return ([], [])
    else do 
      let keepN = filterFields cn (getFields nTs) (concat fsM)
      let keepA = filterFields ap (getFields aTs) (concat fsM)
      let lincatCN = RecType (filter (\(LIdent idx, _) -> showRawIdent idx `elem` (fst (unzip keepN))) nTs)
      print (pp lincatCN)
      -- get fields to be filled 
      -- match with trees
      -- if there exists something else make a new parameter?
      if (length trees) == 1 then 
        do 
          let [tree] = trees
          let f = CncFun (Nothing) (Just (L NoLoc (Abs Explicit ap (Abs Explicit cn tree)))) Nothing Nothing
        
          return ([(identS name, f)], [(showIdent cn, keepN), (showIdent ap, keepA)])
        else do
          let (tree:rest) = trees
          let f = CncFun (Nothing) (Just (L NoLoc (Abs Explicit ap (Abs Explicit cn tree)))) Nothing Nothing
        
          return ([(identS name, f)], [(showIdent cn, keepN), (showIdent ap, keepA)])
  let (fs, allArgs) = unzip allFs
  let ffs = Map.fromListWith (++) (concat allArgs) -- all filtered fields
  let keepA = fromJust (Map.lookup "ap" ffs)
  let keepN = fromJust (Map.lookup "cn" ffs)


  let positA = CncFun (Nothing) (Just (L NoLoc (getPosFun "a" keepA))) Nothing Nothing
  let usen = CncFun (Nothing) (Just (L NoLoc (getPosFun "n" keepN))) Nothing Nothing
  --print keepN

  let allFunctions = concat fs ++ [(identS "UseN", usen), (identS "PositA", positA)]

  let nounJudgms = Map.fromList allFunctions

  let mod = ppModule Qualified (MN (identS "NounMkd"), ModInfo {jments = nounJudgms, msrc="", mstatus = MSComplete, mextend = [(MN (identS "ResMkd"), MIAll)], mwith=Nothing, mopens=[], mexdeps=[], mflags = noOptions, mseqs = Nothing, mtype = MTConcrete (MN  (identS "Noun"))})
  print (pp mod)

  writeFile "NounMkd.gf" (show (pp mod))
      
    
      
  
         

  where
    -- | helper functions to collect information about used fields

    --getPosFun pos [a] = Vr (identS pos)
    getPosFun pos kTs = Abs Explicit (identS pos) (R (map (\(x,t) -> mapWithType x t pos) kTs))

    mapWithType x t@(Table _ _) pos = (LIdent (rawIdentS x), (Nothing, T TRaw [(lengthTable t (P (Vr (identS pos)) (LIdent (rawIdentS x))))]))
      --(LIdent (rawIdentS x), (Nothing, P (Vr (identS pos)) (LIdent (rawIdentS x))))
    mapWithType x _ pos = (LIdent (rawIdentS x), (Nothing, P (Vr (identS pos)) (LIdent (rawIdentS x))))

   ---lengthTable (Table _ t@(Table _ _)) idx = (PW, T TRaw [(lengthTable t idx)])
    lengthTable _ idx = (PW, idx)

    

    filterFields cl t inp = filter (\(x, y) -> (showIdent cl, x) `elem` inp) t

    getFields ts = map (\(LIdent idx, y) -> (showRawIdent idx, y)) ts

    getIdent (C s1 s2) = getIdent s1 ++ getIdent s2
    getIdent (S s1@(S _ _) s2@(S _ _)) = getIdent s1 ++ getIdent s2
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

    substitute subst (Meta i) =
      case lookup i subst of
        Just t  -> t
        Nothing -> Meta i
    substitute subst t = composSafeOp (substitute subst) t

    instantiate dim_dataset dataset t0 subst0 =
      let t = foldr (\(_,Vr var) t -> T TRaw [(PV var,t)]) (substitute subst t0) subst0
      in (fromIntegral (length dataset') / fromIntegral (length dataset), subst0, t)
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

    cross_breed dim_dataset dataset t0 subst0 ((_,subst0',_):rest) =
       let subst0'' = subst0++subst0'
           dataset' = map (selectVars subst0'') dataset
           (accuracy,_,t) = instantiate dim_dataset dataset' t0 subst0''
       in if null rest || accuracy > stopping
            then do putStrLn ""
                    putStrLn ("=== "++show accuracy)
                    print t
                    print (pp t)
                    return $ (t, getIdent (unpackT t))
            else cross_breed dim_dataset dataset t0 subst0'' rest


type POS  = String
type Node = (Int,String,POS,[(String,String)],String)

data Config
   = Config {
       args     :: [(Term,Type)],
       patterns :: [[(Node,Term,Type)]],
       mapping  :: [(Either RawIdent (Ident,Ident),(String,String))],
       smarts   :: Map.Map QIdent [(QIdent,Context)]
     }

noSmarts = Map.empty

data TermName a = TermName Term (a -> Term)

loadGrammar :: FilePath -> IO (ModuleName,SourceGrammar)
loadGrammar fpath = batchCompile noOptions Nothing [fpath]

readCONLL :: FilePath -> IO [Tree Node]
readCONLL fpath = do
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

matchEdges ((p1, Nothing), (p2, Just r2)) (n1@(_,_,pos1,_,_),n2@(_,_,pos2,_,rel2))    = p1 == pos1 && p2 == pos2 && r2 == rel2               
matchEdges ((p1, Just r1), (p2, Just r2)) (n1@(_,_,pos1,_,rel1),n2@(_,_,pos2,_,rel2)) = p1 == pos1 && p2 == pos2 && r1 == rel2 && r2 == rel2 
matchEdges ((p1, Just r1), (p2, Nothing)) (n1@(_,_,pos1,_,rel1),n2@(_,_,pos2,_,_))    = p1 == pos1 && p2 == pos2 && r1 == rel1              
matchEdges ((p1, Nothing), (p2, Nothing)) (n1@(_,_,pos1,_,_),n2@(_,_,pos2,_,_))       = p1 == pos1 && p2 == pos2                             

nodes t = collect t []
  where
    collect t@(Node n children) ns = foldr collect (n:ns) children

edges t = collect t []
  where
    collect t@(Node n1 children) es = foldr collect ([(n1,n2) | Node n2 _ <- children]++es) children

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
  (l@(LIdent id),ty) <- anyOf fs
  morpho <- case [v | (Left lbl,v) <- mapping cfg, lbl==id] of
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
  morpho <- case [v | (Right (_,con),v) <- mapping cfg, con==snd q] of
              (v:_) -> pop v morpho
              _     -> return morpho
  
  foldM (\t (_,_,ty) -> fmap (App t) (findParam gr cfg morpho ty)) (QC q) ctxt

findInh gr cfg morpho t ty@(QC q) = do
  v <- findParam gr cfg morpho ty
  return (t,v,ty)
findInh gr cfg morpho t (RecType fs) = do
  (l@(LIdent id),ty) <- anyOf fs
  morpho <- case [v | (Left lbl,v) <- mapping cfg, lbl==id] of
              (v:_) -> pop v morpho
              _     -> return morpho
  findInh gr cfg morpho (P t l) ty
findInh gr cfg morpho t ty = empty

freshVar env ty = fresh (letter ty) 1
  where
    letter (QC (_,c)) =
      convert (showIdent c)
    letter (RecType xs) =
      --case [cat | (l,_) <- xs, Just cat <- [isLockLabel l]] of
      case [id | (LIdent id, _) <- xs, isLockLabel (LIdent id)] of
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

