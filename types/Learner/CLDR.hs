module Learner.CLDR where

import Data.Maybe
import GF.Data.XML
import GF.Infra.Ident
import GF.Grammar.Predef
import qualified GF.Grammar.Grammar as G
import Control.Monad
import Control.Applicative
import System.Exit
import System.Process
import System.Directory
import Learner.LCS
import Learner.Config(cfgIso2)

readCLDR cfg = do
  let fpath  = "data/cldr"
  exist <- doesDirectoryExist fpath
  when (not exist) $ do
    createDirectoryIfMissing True fpath
    res <- system ("curl https://unicode.org/Public/cldr/48/core.zip -o data/core.zip")
    case res of
      ExitSuccess -> do res <- system ("unzip data/core.zip -d "++fpath)
                        case res of
                          ExitSuccess -> return ()
                          _           -> exitWith res
      _           -> exitWith res
  s <- readFile ("data/cldr/common/main/"++cfgIso2 cfg++".xml")
  let formatting = do
        doc <- parseXML s
        symbols <- path ["ldml","numbers","symbols"] doc
        decimal <- default_val "." ((children >=> select "decimal")   symbols >>= children >>= get_text)
        group   <- default_val " " ((children >=> select "group")     symbols >>= children >>= get_text)
        minus   <- default_val "-" ((children >=> select "minusSign") symbols >>= children >>= get_text)
        return (decimal,group,minus)
      (decimal,group,minus) =
         case formatting of
           x:_ -> x
           _   -> (","," ","-")
  s <- readFile ("data/cldr/common/rbnf/"++cfgIso2 cfg++".xml")
  let rbnf = do
        doc <- parseXML s
        ruleset <- (path ["ldml","rbnf","rulesetGrouping"] >=> attr "type" "SpelloutRules" >=> children >=> select "ruleset") doc
        cat <- get_attr "type" ruleset
        rules <- collect ruleset $ \n -> do
                   rule  <- (children >=> select "rbnfrule") n
                   value <- get_attr "value" rule
                   guard (value /= "-x" && value /= "x.x")
                   text  <- (children >=> get_text) rule
                   case reads value of
                     [(v,"")] -> return (v :: Int,parseRule cat text)
                     _        -> mzero
        return (cat,rules)
  return (decimal,group,minus,rbnf)

select :: String -> XML -> [XML]
select t xml@(Tag t' _ _) | t == t' = [xml]
select t xml@(ETag t' _)  | t == t' = [xml]
select t _                          = []

children :: XML -> [XML]
children xml@(Tag _ _ ns) = ns
children _                = []

path []     xml = [xml]
path [t]    xml = select t xml
path (t:ts) xml = select t xml >>= children >>= path ts

attr a v xml@(Tag _ as _) | lookup a as == Just v = [xml]
attr a v xml@(ETag _ as)  | lookup a as == Just v = [xml]
attr a v _                                        = []

get_attr a (Tag _ as _) = maybeToList (lookup a as)
get_attr a (ETag _ as)  = maybeToList (lookup a as)
get_attr a _            = []

get_text (Data t) = [t]
get_text _        = []

collect x f = [f x]

default_val x [] = [x]
default_val _ xs = xs

data Symbol
  = Up   String
  | Down String
  | Eq   String
  | Bracket [Symbol]
  | Plural [(PluralKeyword,String)]
  | Space
  | Word String
  deriving Show

data PluralKeyword
  = Zero
  | One
  | Two
  | Few
  | Many
  | Other
  deriving Show

parseRule cat []           = []
parseRule cat ('→':cs) = 
  case break (=='→') cs of
    ([],'→':cs) -> Down cat    : parseRule cat cs
    (id,'→':cs) -> Down (p id) : parseRule cat cs
parseRule cat ('←':cs)     =
  case break (=='←') cs of
    ([],'←':cs) -> Up   cat : parseRule cat cs
    (id,'←':cs) -> Up   (p id) : parseRule cat cs
parseRule cat ('=':cs)     =
  case break (=='=') cs of
    (id,'=':cs) -> Eq   (p id) : parseRule cat cs
    _           -> []
parseRule cat ('[':cs)     =
  case break (==']') cs of
    (xs,']':cs) -> Bracket (parseRule cat xs) : parseRule cat cs
parseRule cat ('$':'(':cs)     =
  case break (==')') cs of
    (xs,')':'$':cs) -> Plural (parsePlural xs) : parseRule cat cs
parseRule cat (' '    :cs) = Space  : parseRule cat cs
parseRule cat ('\173' :cs) = parseRule cat cs
parseRule cat (';'    :cs) = []
parseRule cat cs       =
  let (w,cs') = break (flip elem " ←→[]=;") cs
  in Word w : parseRule cat cs'

parsePlural s = parse (tail (dropWhile (/=',') s))
  where
    parse [] = []
    parse s  =
      case break (=='{') s of
        (x,'{':s) -> case break (=='}') s of
                       (y,'}':s) -> case lookup x kwds of
                                      Just x -> (x,y) : parse s
    kwds =
      [ ("zero",Zero)
      , ("one",One)
      , ("two",Two)
      , ("few",Few)
      , ("many",Many)
      , ("other",Other)
      ]

p = dropWhile (=='%')

evalRules memo cat m n rbnf =
  case lookup (m,n) memo of
    Just t  -> return (m,n,t)
    Nothing -> do (m,n,syms) <- maybeToList (lookup cat rbnf) >>= lookupRule [] m n
                  let m' = floor10 m
                      n' = floor10 n
                      d  = if m'==n'
                             then m'
                             else error "m'/=n'"
                  eval m n d syms
  where
    lookupRule res m n [] = fmap ((,,) m n) res
    lookupRule res m n ((i,syms):rules)
      | m < i      = fmap ((,,) m (min n (i-1))) res
                     <|>
                     (if i > n
                        then empty
                        else lookupRule [syms] i n rules)
      | otherwise  = lookupRule [syms] m n rules

    eval m n d []              = return (m,n,[])
    eval m n d (Up   cat:syms) = do (p,q,s1) <- evalRules memo cat (m `div` d) (n `div` d) rbnf
                                    let r1 = (p + m `mod` d, q + n `mod` d, s1)
                                    r2 <- eval m n d syms
                                    return (plus' r1 r2)
    eval m n d (Down cat:syms) = do (p,q,s1) <- evalRules memo cat (m `rem` d) (n `rem` d) rbnf
                                    let r1 = (m `div` d + p, n `div` d + q, s1)
                                    r2 <- eval m n d syms
                                    return (plus' r1 r2)
    eval m n d (Eq   cat:syms) = do (p1,q1,s1) <- evalRules memo cat m n rbnf
                                    (p2,q2,s2) <- eval m n d syms
                                    return (max p1 p2,min q1 q2,s1++s2)
    eval m n d (Space   :syms) = do (m,n,s) <- eval m n d syms
                                    return (m,n,G.K " " : s)
    eval m n d (Bracket syms':syms)
      | m `rem` d == 0     = eval m m d syms
                             <|>
                             (if m == n
                                then empty
                                else do (p,q,s1) <- eval (m+1) n d syms'
                                        (m,n,s2) <- eval (m+1) n d syms
                                        return (m,n,s1++s2))
      | otherwise          = do (p,q,s1) <- eval m n d syms'
                                (m,n,s2) <- eval m n d syms
                                return (m,n,s1++s2)
    eval m n d (Plural ks:syms) = do (m,n,s) <- eval m n d syms
                                     return (m,n,G.K (plural (n `div` d) ks) : s)
    eval m n d (Word w   :syms) = do (m,n,s) <- eval m n d syms
                                     return (m,n,G.K w : s)

    plural 0 ((Zero, w):ks) = w
    plural 1 ((One , w):ks) = w
    plural 2 ((Two,  w):ks) = w
    plural n ((Few,  w):ks)
      | n > 1               = w
    plural n ((Many, w):ks)
      | n > 1               = w
    plural n ((Other,w):ks) = w
    plural n (_        :ks) = plural n ks

    plus' (m1,n1,ts1) (m2,n2,ts2) = (max m1 m2, min n1 n2, ts1++ts2)

    floor10 n
      | n < 10    = 1
      | otherwise = floor10 (n `div` 10) * 10


data Numeral
  = Pot01
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9
  | Pot110
  | Pot111
  | Pot1to19 Ident
  | Pot1     Ident
  | Pot1Plus Ident Ident
  | Pot2     Ident
  | Pot2Plus Ident Ident
  | Pot3     Ident
  | Pot3Plus Ident Ident
  | Pot4     Ident
  | Pot4Plus Ident Ident
  | Pot5     Ident
  | Pot5Plus Ident Ident

eqNumeral Pot01           1 = True
eqNumeral N2              2 = True
eqNumeral N3              3 = True
eqNumeral N4              4 = True
eqNumeral N5              5 = True
eqNumeral N6              6 = True
eqNumeral N7              7 = True
eqNumeral N8              8 = True
eqNumeral N9              9 = True
eqNumeral Pot110         10 = True
eqNumeral Pot111         11 = True
eqNumeral (Pot1to19 _)    n = n >= 12 && n <= 19
eqNumeral (Pot1 _)        n = n `mod` 10 == 0 && n >= 20 && n <= 99
eqNumeral (Pot1Plus _ _)  n = n `mod` 10 /= 0 && n >= 20 && n <= 99
eqNumeral (Pot2 _)        n = n `mod` 100 == 0 && n >= 100 && n <= 999
eqNumeral (Pot2Plus _ _)  n = n `mod` 100 /= 0 && n >= 100 && n <= 999
eqNumeral (Pot3 _)        n = n `mod` 1000 == 0 && n >= 1000 && n <= 9999
eqNumeral (Pot3Plus _ _)  n = n `mod` 1000 /= 0 && n >= 1000 && n <= 9999
eqNumeral (Pot4 _)        n = n `mod` 1000000000 == 0 && n >= 1000000000 && n <= 9999999999
eqNumeral (Pot4Plus _ _)  n = n `mod` 1000000000 /= 0 && n >= 1000000000 && n <= 9999999999
eqNumeral (Pot5 _)        n = n `mod` 1000000000000 == 0 && n >= 1000000000000 && n <= 9999999999999
eqNumeral (Pot5Plus _ _)  n = n `mod` 1000000000000 /= 0 && n >= 1000000000000 && n <= 9999999999999

evalRules2 memo cat n rbnf = do
  undefined {-(m,n,syms) <- maybeToList (lookup cat rbnf) >>= lookupRule [] m n
                  let m' = floor10 m
                      n' = floor10 n
                      d  = if m'==n'
                             then m'
                             else error "m'/=n'"
                  eval m n d syms-}
  where
{-    lookupRule res m n [] = fmap ((,,) m n) res
    lookupRule res m n ((i,syms):rules)
      | m < i      = fmap ((,,) m (min n (i-1))) res
                     <|>
                     (if i > n
                        then empty
                        else lookupRule [syms] i n rules)
      | otherwise  = lookupRule [syms] m n rules

    eval m n d []              = return (m,n,[])
    eval m n d (Up   cat:syms) = do (p,q,s1) <- evalRules2 memo cat (m `div` d) (n `div` d) rbnf
                                    let r1 = (p + m `mod` d, q + n `mod` d, s1)
                                    r2 <- eval m n d syms
                                    return (plus' r1 r2)
    eval m n d (Down cat:syms) = do (p,q,s1) <- evalRules2 memo cat (m `rem` d) (n `rem` d) rbnf
                                    let r1 = (m `div` d + p, n `div` d + q, s1)
                                    r2 <- eval m n d syms
                                    return (plus' r1 r2)
    eval m n d (Eq   cat:syms) = do (p1,q1,s1) <- evalRules2 memo cat m n rbnf
                                    (p2,q2,s2) <- eval m n d syms
                                    return (max p1 p2,min q1 q2,s1++s2)
    eval m n d (Space   :syms) = do (m,n,s) <- eval m n d syms
                                    return (m,n,G.K " " : s)
    eval m n d (Bracket syms':syms)
      | m `rem` d == 0     = eval m m d syms
                             <|>
                             (if m == n
                                then empty
                                else do (p,q,s1) <- eval (m+1) n d syms'
                                        (m,n,s2) <- eval (m+1) n d syms
                                        return (m,n,s1++s2))
      | otherwise          = do (p,q,s1) <- eval m n d syms'
                                (m,n,s2) <- eval m n d syms
                                return (m,n,s1++s2)
    eval m n d (Plural ks:syms) = do (m,n,s) <- eval m n d syms
                                     return (m,n,G.K (plural (n `div` d) ks) : s)
    eval m n d (Word w   :syms) = do (m,n,s) <- eval m n d syms
                                     return (m,n,G.K w : s)

    plural 0 ((Zero, w):ks) = w
    plural 1 ((One , w):ks) = w
    plural 2 ((Two,  w):ks) = w
    plural n ((Few,  w):ks)
      | n > 1               = w
    plural n ((Many, w):ks)
      | n > 1               = w
    plural n ((Other,w):ks) = w
    plural n (_        :ks) = plural n ks

    plus' (m1,n1,ts1) (m2,n2,ts2) = (max m1 m2, min n1 n2, ts1++ts2)

    floor10 n
      | n < 10    = 1
      | otherwise = floor10 (n `div` 10) * 10
-}

concatTerms ts = snd (concat ts)
  where
    concat []           = (False,G.Empty)
    concat [t]          = (True, t)
    concat (G.K " ":ts) = (False,concatTerms ts)
    concat (t      :ts)
      | needsBind = (True,G.C t (G.C (G.Q (cPredef,cBIND)) t'))
      | otherwise = (True,G.C t t')
      where
        (needsBind, t') = concat ts

