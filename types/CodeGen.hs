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
import Control.Monad
import Control.Applicative hiding (Const)
import qualified Data.Set as Set
import qualified Data.Map as Map


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

type Node = (Int,String,String,[(String,String)],String)

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
  mapM_ (putStrLn . show . ppEdge) [(n1,n2) | (n1@(_,_,pos1,_,_),n2@(_,_,pos2,_,rel)) <- concatMap edges trees, pos1=="NOUN", pos2=="ADJ", rel=="mod"]

  (cnc,gr) <- batchCompile noOptions Nothing args
  abs <- abstractOfConcrete gr cnc
  abs_ty <- lookupFunType gr abs (identS "AdjCN")
  cnc_ty <- linTypeOfType gr cnc abs_ty
  let q = (moduleNameS "ResMkd",identS "genNum")
  ty <- lookupResType gr q
  ts <- runGenM (codeGen gr cnc (Config [1,2] rules trees) [(Q q,ty)] cnc_ty)
  mapM_ (print . ppTerm Unqualified 0) ts

data Config
   = Config {
       order  :: [Int],
       rules  :: Map.Map String [Rule],
       corpus :: [Tree Node]
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
  | s == cStr = do ts <- forM (order cfg) $ \i -> do
                           let (t,ty) = reverse env !! i
                           find env t ty ty0
                   return (foldl1 C ts)

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

anyOf xs = GenM (choose xs)
  where
    choose []     s k r = Ok r
    choose (x:xs) s k r = case k x r of
                            Ok r    -> choose xs s k r
                            Bad msg -> Bad msg

with q (GenM g) =
  GenM (\s k r -> if Set.member q s
                    then Ok r
                    else g (Set.insert q s) k r)
