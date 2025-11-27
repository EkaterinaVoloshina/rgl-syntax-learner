import GF.Infra.Ident
import GF.Infra.Option
import GF.Data.Operations
import GF.Grammar.Lookup
import GF.Grammar.Printer
import GF.Grammar.Predef
import GF.Grammar.Grammar
import GF.Grammar.Lockfield
import GF.Compile
import System.Environment
import Data.Char(toLower)
import Control.Monad
import Control.Applicative
import qualified Data.Set as Set

main = do
  args <- getArgs
  (cnc,gr) <- batchCompile noOptions Nothing args
  abs <- abstractOfConcrete gr cnc
  abs_ty <- lookupFunType gr abs (identS "AdjCN")
  cnc_ty <- linTypeOfType gr cnc abs_ty
  ts <- runGenM (codeGen gr cnc (Config [0,1]) [] cnc_ty)
  mapM_ (print . ppTerm Unqualified 0) ts

data Config
   = Config {
       order :: [Int]
     }

codeGen :: SourceGrammar -> ModuleName -> Config -> [(Ident,Type)] -> Type -> GenM Term
codeGen gr cnc cfg env (Prod bt _ arg res) = do
  let x = freshVar env arg
  t <- codeGen gr cnc cfg ((x,arg):env) res
  return (Abs bt x t)
codeGen gr cnc cfg env (RecType fs) = do
  fs <- forM fs $ \(lbl,ty) -> do
          t <- codeGen gr cnc cfg env ty
          return (lbl,(Nothing,t))
  return (R fs)
codeGen gr cnc cfg env (Table arg res) = do
  let x = freshVar env arg
  t <- codeGen gr cnc cfg ((x,arg):env) res
  return (T TRaw [(PV x,t)])
codeGen gr cnc cfg env ty0@(QC q) = with q $ do
  (x,ty) <- anyOf env
  find env (Vr x) ty ty0
codeGen gr cnc cfg env ty0@(Sort s)
  | s == cStr = do ts <- forM (order cfg) $ \i -> do
                           let (x,ty) = reverse env !! i
                           find env (Vr x) ty ty0
                   return (foldl1 C ts)

find env t ty ty0
  | ty == ty0           = return t
find env t (RecType fs) ty0 = do
  (l,ty) <- anyOf fs
  find env (P t l) ty ty0
find env t (Table arg@(QC q) res) ty0 = do
  p <- with q $ do
         (x,ty) <- anyOf env
         find env (Vr x) ty arg
  find env (S t p) res ty0
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
      in case [x | (x,_) <- env, x == v] of
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
