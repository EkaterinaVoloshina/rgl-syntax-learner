module Learner.CLDR where

import Data.Maybe
import GF.Data.XML
import Control.Monad
import System.Exit
import System.Process
import System.Directory
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

evalRule cat n rbnf = do
  (n',syms) <- lookup cat rbnf >>= lookupRule Nothing n
  eval (floor10 n') syms
  where
    lookupRule res n [] = res
    lookupRule res n (rule@(n',syms):rules)
      | n < n'      = res
      | otherwise   = lookupRule (Just rule) n rules

    eval n' []              = return ""
    eval n' (Up   cat:syms) = liftM2 (++) (evalRule cat (n `div` n') rbnf) (eval n' syms)
    eval n' (Down cat:syms) = liftM2 (++) (evalRule cat (n `rem` n') rbnf) (eval n' syms)
    eval n' (Eq   cat:syms) = liftM2 (++) (evalRule cat n rbnf) (eval n' syms)
    eval n' (Space   :syms) = do s <- eval n' syms
                                 return (' ':s)
    eval n' (Bracket syms':syms)
      | n `rem` n' == 0     = eval n' syms
      | otherwise           = liftM2 (++) (eval n' syms') (eval n' syms)
    eval n' (Plural ks:syms) = fmap (plural (n `div` n') ks++) (eval n' syms)
    eval n' (Word w  :syms) = do s <- eval n' syms
                                 return (w++s)

    plural 0 ((Zero, w):ks) = w
    plural 1 ((One , w):ks) = w
    plural 2 ((Two,  w):ks) = w
    plural n ((Few,  w):ks)
      | n > 1               = w
    plural n ((Many, w):ks)
      | n > 1               = w
    plural n ((Other,w):ks) = w
    plural n (_        :ks) = plural n ks

    floor10 n
      | n < 10    = 1
      | otherwise = floor10 (n `div` 10) * 10
