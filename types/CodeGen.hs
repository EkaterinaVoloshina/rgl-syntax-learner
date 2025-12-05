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
import GF.Compile
import System.FilePath
import System.Directory
import System.Environment
import Text.JSON hiding (Result(..))
import qualified Text.JSON as JSON
import Data.Tree
import Data.Char(toLower)
import Data.List (sortOn)
import Control.Monad
import Control.Applicative hiding (Const)
import qualified Data.Set as Set
import qualified Data.Map as Map
import DecisionTree hiding (Node)
import Debug.Trace

data Rule = Rule [Atom] Atom deriving Show
data Atom
       = NotEqual ATerm ATerm
       | Equal    ATerm ATerm
       | PreOrder
       | PostOrder
       deriving Show
type Value = String
type Feature = String
data ATerm
       = Head Feature
       | Dep  Feature
       | Const Value
       deriving Show

readRules lang fpath = do
  files <- getDirectoryContents fpath
  fmap (Map.fromListWith (++) . concat) $ forM files $ \fname -> do
     case split '_' (takeBaseName fname) of
       [l,fun,x,feat,_] | l == lang -> do
              s <- readFile (fpath</>fname)
              case decode s of
                JSON.Ok res    -> return [(fun,map (toRule [x,feat]) res)]
                JSON.Error msg -> fail msg
       [l,fun,"wordOrder",_] | l == lang -> do
              s <- readFile (fpath</>fname)
              case decode s of
                JSON.Ok res    -> return [(fun,map (toRule ["wordOrder"]) res)]
                JSON.Error msg -> fail msg
       _ -> return []
  where
    toRule x xs = Rule (map toPremise (init xs)) (toConsequent x (last xs))

    toPremise ["head","feats",feat,'!':value] = NotEqual (Head feat)  (Const value)
    toPremise ["head","feats",feat,    value] = Equal    (Head feat)  (Const value)
    toPremise ["head","pos",       '!':value] = Equal    (Head "pos") (Const value)
    toPremise ["head","pos",           value] = Equal    (Head "pos") (Const value)
    toPremise ["dep", "feats",feat,'!':value] = NotEqual (Dep  feat)  (Const value)
    toPremise ["dep", "feats",feat,    value] = Equal    (Dep  feat)  (Const value)
    toPremise ["dep","pos",        '!':value] = Equal    (Dep "pos")  (Const value)
    toPremise ["dep","pos",            value] = Equal    (Dep "pos")  (Const value)
    toPremise ["position", "!True"]           = PreOrder
    toPremise ["position",  "True"]           = PostOrder
    toPremise ["position", "False"]           = PreOrder
    toPremise ["position", "!False"]          = PostOrder
    toPremise s = error (show s)

    toConsequent ["head",feat] ["rule", '!':val] = NotEqual (Head feat) (Const val)
    toConsequent ["head",feat] ["rule",     val] = Equal (Head feat) (Const val)
    toConsequent ["dep", feat] ["rule", '!':val] = NotEqual (Dep  feat) (Const val)
    toConsequent ["dep", feat] ["rule",     val] = Equal (Dep  feat) (Const val)
    toConsequent ["agr", feat] ["rule",   "Yes"] = Equal (Head feat) (Dep  feat)
    toConsequent ["agr", feat] ["rule",    "No"] = NotEqual (Head feat) (Dep  feat)
    toConsequent ["wordOrder"] ["rule",   "Yes"] = PostOrder
    toConsequent ["wordOrder"] ["rule",    "No"] = PreOrder

ppRule (Rule []       consequent) = ppAtom consequent <> '.'
ppRule (Rule premises consequent) = ppAtom consequent <+> pp ":-" <+> hsep (punctuate (pp ",") (map ppAtom premises)) <> '.'

ppAtom (NotEqual t1 t2) = pp "ne" <> parens (ppATerm t1 <> pp ',' <+> ppATerm t2)
ppAtom (Equal    t1 t2) = pp "eq" <> parens (ppATerm t1 <> pp ',' <+> ppATerm t2)
ppAtom PreOrder         = pp "preOrder"
ppAtom PostOrder        = pp "postOrder"

ppATerm (Head feat) = pp "head." <> pp feat
ppATerm (Dep  feat) = pp "dep."  <> pp feat
ppATerm (Const val) = pp val

type POS  = String
type Node = (Int,String,POS,[(String,String)],String)

readCONLL :: FilePath -> IO [Tree Node]
readCONLL fpath = do
  ls <- fmap lines $ readFile fpath
  return (map (toTree "0" (0,"root","",[],"")) (stanzas ls))
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

nodes (Node n1 children) =
  n1 : concatMap nodes children

edges (Node n1 children) =
  [(n1,n2) | Node n2 _ <- children] ++
  concatMap edges children

ppNode (id,lemma,pos,morph,rel) = pp id <+> pp lemma <+> pp pos <+> pp (show morph) <+> pp rel

ppEdge (n1,n2) = ppNode n1 <+> pp "->" <+> ppNode n2

split sep []     = []
split sep (c:cs)
  | c == sep  = [] : split sep cs
  | otherwise = case split sep cs of
                  []     -> [[c]]
                  (x:xs) -> (c:x):xs

main = do
  let lang  = "Macedonian"
      fpath = "../data/"
  rules <- readRules lang fpath
  writeFile (lang++"_rules.txt") $
    show (vcat (map (\(fun,rules) -> (fun <+> vcat (map ppRule rules))) (Map.toList rules)))
  args <- getArgs

  trees <- readCONLL "../SUD_Macedonian-MTB/mk_mtb-sud-test.conllu"
  mapM_ (putStrLn . drawTree . fmap (show . ppNode)) trees

  (cnc,gr) <- batchCompile noOptions Nothing args
  abs <- abstractOfConcrete gr cnc
  abs_ty <- lookupFunType gr abs (identS "AdjCN")
  cnc_ty <- linTypeOfType gr cnc abs_ty

  let params = Map.fromListWith (++) $ do
        t <- trees
        (_,_,_,morph,_) <- nodes t
        (ty,v) <- morph
        return (ty,[v])

      patterns = do  -- query edges matching AdjCN
        t <- trees
        (n1@(_,_,"NOUN",_,_),n2@(_,_,"ADJ",_,"mod")) <- edges t
        return [n1,n2]

  mapM_ (print . hsep . punctuate (pp ';') . map ppNode) patterns

  -- we need to add genNum as a helper function in the environment 
  let q = (moduleNameS "ResMkd",identS "genNum")
  ty <- lookupResType gr q

  ts <- runGenM (codeGen gr cnc (Config [("ADJ",1),("NOUN",2)] rules patterns) [(Q q,ty)] cnc_ty)
  mapM_ (print . ppTerm Unqualified 0) ts

data Config
   = Config {
       args     :: [(POS,Int)],
       rules    :: Map.Map String [Rule],
       patterns :: [[Node]]
     }

codeGen :: SourceGrammar -> ModuleName -> Config -> [(Term,Type)] -> Type -> GenM Term
codeGen gr cnc cfg env (Prod bt _ arg res) = do
  let x = freshVar env arg
  t <- codeGen gr cnc cfg ((Vr x,arg):env) res
  return (Abs bt x t)
codeGen gr cnc cfg env (RecType fs) = do
  fs <- forM fs $ \(lbl,ty) -> do
          t <- codeGen gr cnc cfg env ty
          return (lbl,(Nothing,t))
  return (R fs)
codeGen gr cnc cfg env (Table arg res) = do
  let x = freshVar env arg
  t <- codeGen gr cnc cfg ((Vr x,arg):env) res
  return (T TRaw [(PV x,t)])
codeGen gr cnc cfg env ty0@(QC q) = with q $
  do (t,ty) <- anyOf env
     find env t ty ty0
codeGen gr cnc cfg env ty0@(Sort s)
  | s == cStr = do
    ((t,num_param,num_inh),constrs) <-
         group (Meta 0,0,0) $ do
           patt <- anyOf (patterns cfg)

           inh <- runGenM $ do
                    (pos,i) <- anyOf (args cfg)
                    let morpho = head [morpho | (_,_,pos',morpho,_) <- patt, pos==pos']
                    let (t,ty) = reverse env !! i
                    findInh gr morpho t ty

           (str,vs) <- buildStr [] (sortOn (\(id,_,_,_,_)->id) patt) $ \vs (_,_,pos,morpho,_) ->
               case lookup pos (args cfg) of
                 Just i  -> do let (t,ty) = reverse env !! i
                               findStr gr morpho vs t ty
                 Nothing -> fail ("Missing "++pos++" argument")

           return ((str,length vs,length inh),(vs,inh))
    trace (show (pp t)) $ return ()

    attrs <- forM (zip [0..] (snd (head constrs))) $ \(i,(_,_,ty)) -> do
               ts <- allParamValues gr ty
               return (A (show (pp ty)) (!!i) ts)

    forM constrs $ \(vs,inh) -> do
      trace (show ((hsep . map (ppTerm Unqualified 9 . fst) . reverse) vs <+> pp '|' <+> hsep (map (\(c,d,_) -> pp c <> pp '=' <> pp d) inh))) $ return ()

    forM [0..num_param-1] $ \i -> do
      let (accuracy,dt) = build attrs [(map (\(_,v,_) -> v) inh,fst (vs !! i)) | (vs,inh) <- constrs]
      trace (show accuracy++"\n"++drawDecisionTree dt) $ return ()

    return t
  where
    buildStr s []     f = return (Empty,s)
    buildStr s [x]    f = f s x
    buildStr s (x:xs) f = do (t1,s) <- f s x
                             (t2,s) <- buildStr s xs f
                             return (C t1 t2,s)


find env t ty ty0
  | ty == ty0           = return t
find env t (RecType fs) ty0 = do
  (l,ty) <- anyOf fs
  find env (P t l) ty ty0
find env t (Table arg@(QC q) res) ty0 = do
  p <- with q $ do
         (t,ty) <- anyOf env
         find env t ty arg
  find env (S t p) res ty0
find env t (Prod bt _ arg@(QC q) res) ty0 = do
  p <- with q $ do
         (t,ty) <- anyOf env
         find env t ty arg
  find env (App t p) res ty0
find env t ty _ = empty

mapping =
  [ (identS "count_form", ("Number","Count"))
  , (identS "vocative",   ("Case","Voc"))
  , (identS "adverb",     ("Adverb","Yes"))
  , (identS "Sg",         ("Number","Sing"))
  , (identS "Pl",         ("Number","Plur"))
  , (identS "GSg",        ("Number","Sing"))
  , (identS "GPl",        ("Number","Plur"))
  , (identS "Masc",       ("Gender","Masc"))
  , (identS "Fem",        ("Gender","Fem"))
  , (identS "Neuter",     ("Gender","Neut"))
  , (identS "Indef",      ("Definite","Ind"))
  , (identS "Def",        ("Definite","Def"))
  , (identS "Proximal",   ("Distance","Proximal"))
  , (identS "Distal",     ("Distance","Distal"))
  ]

findStr gr morpho vs t (Sort s)
  | s == cStr                  = return (t,vs)
findStr gr morpho vs t (RecType fs) = do
  (l@(LIdent id),ty) <- anyOf fs
  morpho <- case lookup (identC id) mapping of
              Just v  -> pop v morpho
              Nothing -> return morpho
  findStr gr morpho vs (P t l) ty
findStr gr morpho vs t (Table arg res) = do
  v <- findParam gr morpho arg
  let p = Meta (length vs+1)
  findStr gr morpho ((v,arg):vs) (S t p) res
findStr gr morpho vs t ty = empty

findParam gr morpho (QC q) = do
  (mn,info) <- lookupOrigInfo gr q
  (id,ctxt) <- case info of
                 ResParam (Just (L _ ps)) _ -> anyOf ps
                 _                          -> raise $ render (ppQIdent Qualified q <+> "has no parameter values defined")
  morpho <- case lookup id mapping of
              Just v  -> pop v morpho
              Nothing -> return morpho
  foldM (\t (_,_,ty) -> fmap (App t) (findParam gr morpho ty)) (QC (mn,id)) ctxt

findInh gr morpho t ty@(QC q) = do
  v <- findParam gr morpho ty
  return (t,v,ty)
findInh gr morpho t (RecType fs) = do
  (l@(LIdent id),ty) <- anyOf fs
  morpho <- case lookup (identC id) mapping of
              Just v  -> pop v morpho
              Nothing -> return morpho
  findInh gr morpho (P t l) ty
findInh gr morpho t ty = empty

linTypeOfType :: ErrorMonad m => Grammar -> ModuleName -> Type -> m Type
linTypeOfType gr cnc (Prod bt x arg res) = do
  arg <- linTypeOfType gr cnc arg
  res <- linTypeOfType gr cnc res
  return (Prod bt x arg res)
linTypeOfType gr cnc (Q (abs,q)) = do
  lookupResDef gr (cnc,q)

freshVar env ty = fresh (letter ty) 1
  where
    letter (QC (_,c)) =
      convert (showIdent c)
    letter (RecType xs) =
      case [cat | (l,_) <- xs, Just cat <- [isLockLabel l]] of
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

group :: Ord a => a -> GenM (a,b) -> GenM (a,[b])
group def (GenM g) =
  case g Set.empty (\(x,y) m -> Ok (Map.insertWith (++) x [y] m)) Map.empty of
    Ok m
      | Map.null m -> return (def,[])
      | otherwise  -> anyOf (Map.toList m)
    Bad msg        -> fail msg

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

