module Learner.Config(module Learner.Config, Ident, identS, rawIdentS) where

import GF.Infra.Ident
import System.IO

data Config
  = Config
      { cfgIso2     :: String
      , cfgIso3     :: String
      , cfgLangName :: String
      , cfgVerbose  :: Bool
      , cfgMinForms :: Int
      , cfgUpdTags  :: [String] -> [[String]]
      , cfgTreebanks:: [String]
      }

defaultConfig iso2 iso3 name =
  Config { cfgIso2 = iso2
         , cfgIso3 = iso3
         , cfgLangName = name
         , cfgVerbose = False
         , cfgMinForms = 5
         , cfgUpdTags = (:[])
         , cfgTreebanks = []
         }

withStatus msg f = do
  hPutStr stdout (msg++" ...")
  hFlush stdout
  x <- f
  hPutStr stdout "\n"
  return x

data POS
  = POS
      { posTag  :: String
      , posOper :: Ident
      , posCat  :: Ident
      }

pos_tags =
  [POS "adj"  (identS "Adj")          (identS "A")
  ,POS "adv"  (identS "Adverb")       (identS "Adv")
  ,POS "conj" (identS "Conjunct")     (identS "Conj")
  ,POS "det"  (identS "Detm")         (identS "Det")
  ,POS "intj" (identS "Intj")         (identS "Interj")
  ,POS "name" (identS "Name")         (identS "PN")
  ,POS "noun" (identS "Noun")         (identS "N")
  ,POS "prep" (identS "Preposition")  (identS "Prep")
  ,POS "verb" (identS "Verb")         (identS "V")
  ]
  
lookupPOS :: String -> Maybe POS
lookupPOS tag = lookup pos_tags
  where
    lookup []             = Nothing
    lookup (pos:poss)
      | posTag pos == tag = Just pos
      | otherwise         = lookup poss

data Tag
  = Tag 
      { wikt_tag   :: String
      , umorph_tag :: (String,String)
      , ident      :: Ident
      , typ        :: Ident
      , order      :: Int
      }

instance Eq Tag where
  t1 == t2 = order t1 == order t2

instance Ord Tag where
  compare t1 t2 = compare (order t1) (order t2)
  
instance Show Tag where
  show t = showIdent (ident t)



all_tags = flip (zipWith id) [0..] $
  [tag "positive"        ("Degree","Pos")           "Posit"       "Degree"
  ,tag "comparative"     ("Degree","Cmp")           "Compar"      "Degree"
  ,tag "superlative"     ("Degree","Sup")           "Superl"      "Degree"
  ,tag "infinitive"      ("","")                    "Inf"         "NonFinite"
  ,tag "past_participle" ("","")                    "PastPart"    "Participle"
  ,tag "participle"      ("VerbForm","Part")        "Part"        "Participle"
  ,tag "indefinite"      ("Definite","Ind")         "Indef"       "Species"
  ,tag "definite"        ("Definite","Def")         "Def"         "Species"
  ,tag "unspecified"     ("Distance","Unspecified") "Unspecified" "Distance"
  ,tag "proximal"        ("Distance","Proximal")    "Proximal"    "Distance"
  ,tag "distal"          ("Distance","Distal")      "Distal"      "Distance"
  ,tag "present"         ("Tense","Pres")           "Present"     "Tense"
  ,tag "past"            ("Tense","Past")           "Past"        "Tense"
  ,tag "aorist"          ("","")                    "Aorist"      "Tense"
  ,tag "imperfect"       ("Tense","Imp")            "Imperfect"   "Tense"
  ,tag "perfect"         ("","")                    "Perfect"     "Tense"
  ,tag "pluperfect"      ("","")                    "Pluperf"     "Tense"
  ,tag "past-perfect"    ("","")                    "PastPerfect" "Tense"
  ,tag "future"          ("","")                    "Future"      "Tense"
  ,tag "past-future"     ("","")                    "PastFuture"  "Tense"
  ,tag "future-perfect"  ("","")                    "FuturePerfect" "Tense"
  ,tag "imperfective"    ("","")                    "Imperf"      "Aspect"
  ,tag "perfective"      ("","")                    "Perf"        "Aspect"
  ,tag "imperative"      ("","")                    "Imperative"  "Mood"
  ,tag "conditional"     ("","")                    "Cond"        "Mood"
  ,tag "masculine"       ("Gender","Masc")          "Masc"        "Gender"
  ,tag "feminine"        ("Gender","Fem")           "Fem"         "Gender"
  ,tag "neuter"          ("Gender","Neut")          "Neuter"      "Gender"
  ,tag "nominative"      ("Case","Nom")             "Nom"         "Case"
  ,tag "accusative"      ("Case","Acc")             "Acc"         "Case"
  ,tag "dative"          ("Case","Dat")             "Dat"         "Case"
  ,tag "genitive"        ("Case","Gen")             "Gen"         "Case"
  ,tag "locative"        ("Case","Loc")             "Loc"         "Case"
  ,tag "terminative"     ("Case","Trm")             "Term"        "Case"
  ,tag "ablative"        ("Case","Abl")             "Abl"         "Case"
  ,tag "instrumental"    ("Case","Ins")             "Ins"         "Case"
  ,tag "comitative"      ("Case","Com")             "Com"         "Case"
  ,tag "abessive"        ("Case","Abe")             "Abes"        "Case"
  ,tag "comparative"     ("Case","Cmp")             "Cmp"         "Case"
  ,tag "causative"       ("Case","Cau")             "Causative"   "Case"
  ,tag "benefactive"     ("Case","Bnf")             "Benef"       "Case"
  ,tag "associative"     ("Case","Ass")             "Assoc"       "Case"
  ,tag "distributive"    ("Case","Dis")             "Distr"       "Case"
  ,tag "exclusive"       ("Case","Exc")             "Excl"        "Case"
  ,tag "vocative"        ("Case","Voc")             "Voc"         "Case"
  ,tag "singular"        ("Number","Sing")          "Sg"          "Number"
  ,tag "plural"          ("Number","Plur")          "Pl"          "Number"
  ,tag "first-person"    ("Person","1")             "P1"          "Person"
  ,tag "second-person"   ("Person","2")             "P2"          "Person"
  ,tag "third-person"    ("Person","3")             "P3"          "Person"
  ,tag "count-form"      ("Number","Count")         "count_form"  "CountForm"
  ]
  where
    tag wikt_tag umorph_tag ident typ = Tag wikt_tag umorph_tag (identS ident) (identS typ)
