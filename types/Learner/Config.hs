module Learner.Config(module Learner.Config, Ident, identS, rawIdentS) where

import GF.Infra.Ident
import GF.Grammar.Grammar(Label,ident2label)
import Data.Char (toUpper)
import System.IO

data Config
  = Config
      { cfgIso2     :: String
      , cfgIso3     :: String
      , cfgLangName :: String
      , cfgVerbose  :: Bool
      , cfgMinForms :: Int
      , cfgAnalyticTenses :: Bool
      , cfgUpdPOS   :: String -> String -> String
      , cfgUpdForms :: String -> String -> [([String],String)] -> [([String],String)]
      , cfgTreebanks:: [String]
      }

cfgLangModule cfg pref =
  case cfgIso3 cfg of
    []     -> pref
    (c:cs) -> pref++toUpper c:cs

defaultConfig iso2 iso3 name =
  Config { cfgIso2 = iso2
         , cfgIso3 = iso3
         , cfgLangName = name
         , cfgVerbose = False
         , cfgMinForms = 5
         , cfgAnalyticTenses = False
         , cfgUpdPOS = \word pos -> pos
         , cfgUpdForms = \word pos forms -> forms
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
      { posWiktTag :: String
      , posUDTag   :: String
      , posOper    :: Ident
      , posCat     :: Ident
      }

pos_tags =
-- The following table describes how different features are described
-- in Wiktionary, UD and GF.
--
--     Wiktionary  UD     GF Oper                 GF lincat
  [POS "adj"       "ADJ"  (identS "Adj")          (identS "A")
  ,POS "adv"       "ADV"  (identS "Adverb")       (identS "Adv")
  ,POS "conj"      "CONJ" (identS "Conjunct")     (identS "Conj")
  ,POS "det"       "DET"  (identS "Detm")         (identS "Det")
  ,POS "intj"      "INTJ" (identS "Intj")         (identS "Interj")
  ,POS "name"      "NAME" (identS "Name")         (identS "PN")
  ,POS "noun"      "NOUN" (identS "Noun")         (identS "N")
  ,POS "prep"      "ADP"  (identS "Preposition")  (identS "Prep")
  ,POS "verb"      "VERB" (identS "Verb")         (identS "V")
  ,POS "num"       "NUM"  (identS "Numrl")        (identS "Numeral")
  ]

lookupPOS :: String -> Maybe POS
lookupPOS tag = lookup pos_tags
  where
    lookup []                 = Nothing
    lookup (pos:poss)
      | posWiktTag pos == tag = Just pos
      | otherwise             = lookup poss

lookupUPOS :: String -> Maybe POS
lookupUPOS tag = lookup pos_tags
  where
    lookup []               = Nothing
    lookup (pos:poss)
      | posUDTag pos == tag = Just pos
      | otherwise           = lookup poss

data Tag
  = Tag 
      { wikt_tag :: String
      , ud_tag   :: (String,String)
      , label    :: Label
      , ident    :: Ident
      , typ      :: Ident
      , order    :: Int
      }

instance Eq Tag where
  t1 == t2 = order t1 == order t2

instance Ord Tag where
  compare t1 t2 = compare (order t1) (order t2)
  
instance Show Tag where
  show t = showIdent (ident t)



all_tags = flip (zipWith id) [0..] $
-- The following table describes how different features are described
-- in Wiktionary, UD and GF. In GF, we have two ways to represent
-- a feature - either as a record field or as a parameter constructor.
--
--     Wiktionary        UD                         GF record fld.  GF param constr.  GF param type  
  [tag "positive"        ("Degree","Pos")           "posit"         "Posit"           "Degree"
  ,tag "comparative"     ("Degree","Compar")        "compar"        "Compar"          "Degree"
  ,tag "superlative"     ("Degree","Sup")           "superl"        "Superl"          "Degree"
  ,tag "infinitive"      ("","")                    "inf"           "Inf"             "NonFinite"
  ,tag "past_participle" ("","")                    "past_part"     "PastPart"        "Participle"
  ,tag "participle"      ("VerbForm","Part")        "part"          "Part"            "Participle"
  ,tag "indefinite"      ("Definite","Ind")         "indef"         "Indef"           "Species"
  ,tag "definite"        ("Definite","Def")         "def"           "Def"             "Species"
  ,tag "unspecified"     ("Distance","Unspecified") "unspecified"   "Unspecified"     "Distance"
  ,tag "proximal"        ("Distance","Proximal")    "proximal"      "Proximal"        "Distance"
  ,tag "distal"          ("Distance","Distal")      "distal"        "Distal"          "Distance"
  ,tag "present"         ("Tense","Pres")           "present"       "Present"         "Tense"
  ,tag "past"            ("Tense","Past")           "past"          "Past"            "Tense"
  ,tag "aorist"          ("","")                    "aorist"        "Aorist"          "Tense"
  ,tag "imperfect"       ("Tense","Imp")            "imperfect"     "Imperfect"       "Tense"
  ,tag "perfect"         ("","")                    "perfect"       "Perfect"         "Tense"
  ,tag "pluperfect"      ("","")                    "pluperf"       "Pluperf"         "Tense"
  ,tag "past-perfect"    ("","")                    "past_perf"     "PastPerfect"     "Tense"
  ,tag "future"          ("","")                    "future"        "Future"          "Tense"
  ,tag "past-future"     ("","")                    "past_future"   "PastFuture"      "Tense"
  ,tag "future-perfect"  ("","")                    "future_perf"   "FuturePerfect"   "Tense"
  ,tag "imperfective"    ("","")                    "imperf"        "Imperf"          "Aspect"
  ,tag "perfective"      ("","")                    "perf"          "Perf"            "Aspect"
  ,tag "continuative"    ("","")                    "cont"          "Cont"            "Aspect"
  ,tag "imperative"      ("","")                    "imperative"    "Imperative"      "Mood"
  ,tag "conditional"     ("","")                    "conditional"   "Cond"            "Mood"
  ,tag "optative"        ("","")                    "optative"      "Opt"             "Mood"
  ,tag "masculine"       ("Gender","Masc")          "masculine"     "Masc"            "Gender"
  ,tag "feminine"        ("Gender","Fem")           "feminine"      "Fem"             "Gender"
  ,tag "neuter"          ("Gender","Neut")          "neuter"        "Neuter"          "Gender"
  ,tag "nominative"      ("Case","Nom")             "nominative"    "Nom"             "Case"
  ,tag "accusative"      ("Case","Acc")             "accusative"    "Acc"             "Case"
  ,tag "dative"          ("Case","Dat")             "dative"        "Dat"             "Case"
  ,tag "genitive"        ("Case","Gen")             "genitive"      "Gen"             "Case"
  ,tag "locative"        ("Case","Loc")             "locative"      "Loc"             "Case"
  ,tag "terminative"     ("Case","Trm")             "terminative"   "Term"            "Case"
  ,tag "ablative"        ("Case","Abl")             "ablative"      "Abl"             "Case"
  ,tag "instrumental"    ("Case","Ins")             "instrumental"  "Instr"           "Case"
  ,tag "comitative"      ("Case","Com")             "comitative"    "Com"             "Case"
  ,tag "abessive"        ("Case","Abe")             "abessive"      "Abes"            "Case"
  ,tag "essive"          ("Case","Ess")             "essive"        "Ess"             "Case"
  ,tag "inessive"        ("Case","Ine")             "inessive"      "Ine"             "Case"
  ,tag "comparative-case"("Case","Cmp")             "comparative"   "Comp"            "Case"
  ,tag "causative"       ("Case","Cau")             "causative"     "Causative"       "Case"
  ,tag "benefactive"     ("Case","Bnf")             "benefactive"   "Benef"           "Case"
  ,tag "associative"     ("Case","Ass")             "associative"   "Assoc"           "Case"
  ,tag "distributive"    ("Case","Dis")             "distributive"  "Distr"           "Case"
  ,tag "exclusive"       ("Case","Exc")             "exclusive"     "Excl"            "Case"
  ,tag "objective"       ("Case","Obj")             "objective"     "Obj"             "Case"
  ,tag "partitive"       ("Case","Par")             "partitive"     "Par"             "Case"
  ,tag "illative"        ("Case","Ill")             "illative"      "Ill"             "Case"
  ,tag "oblique"         ("Case","Obl")             "oblique"       "Obl"             "Case"
  ,tag "vocative"        ("Case","Voc")             "vocative"      "Voc"             "Case"
  ,tag "singular"        ("Number","Sing")          "singular"      "Sg"              "Number"
  ,tag "plural"          ("Number","Plur")          "plural"        "Pl"              "Number"
  ,tag "dual"            ("Number","Dual")          "dual"          "Dl"              "Number"
  ,tag "singular"        ("Number[psor]","Sing")    "singular"      "Sg"              "Number"
  ,tag "plural"          ("Number[psor]","Plur")    "plural"        "Pl"              "Number"
  ,tag "dual"            ("Number[psor]","Dual")    "dual"          "Dl"              "Number"
  ,tag "first-person"    ("Person","1")             "first_person"  "P1"              "Person"
  ,tag "second-person"   ("Person","2")             "second_person" "P2"              "Person"
  ,tag "third-person"    ("Person","3")             "third_person"  "P3"              "Person"
  ,tag "fourth-person"   ("Person","4")             "fourth_person" "P4"              "Person"
  ,tag "first-person"    ("Person[psor]","1")       "first_person"  "P1"              "Person"
  ,tag "second-person"   ("Person[psor]","2")       "second_person" "P2"              "Person"
  ,tag "third-person"    ("Person[psor]","3")       "third_person"  "P3"              "Person"
  ,tag "fourth-person"   ("Person[psor]","4")       "fourth_person" "P4"              "Person"
  ,tag "count-form"      ("Number","Count")         "count_form"    "Count"           "CountForm"
  ,tag ""                ("","")                    "rel"           ""                ""
  ,tag ""                ("","")                    "adverb"        ""                ""
  ,tag ""                ("Number","Sing")          "gsg"           "GSg"             "GenNum"
  ,tag ""                ("Number","Sing")          "gpl"           "GPl"             "GenNum"
  ,tag ""                ("Number","Sing")          "asg"           "ASg"             "AForm"
  ,tag ""                ("Number","Sing")          "apl"           "APl"             "AForm"
  ,tag ""                ("RelType","Adv")          ""              "AdvMod"          "NRelType"
  ,tag ""                ("RelType","Adj")          ""              "AdjMod"          "NRelType"
  ,tag ""                ("RelType","Pref")         ""              "Pref"            "NRelType"
  ,tag "informal"        ("Polite","Form")          "informal"      "Informal"        "Formality"
  ,tag "colloquial"      ("Polite","Form")          "colloquial"    "Colloquial"      "Formality"
  ,tag "formal"          ("Polite","Form")          "formal"        "Formal"          "Formality"
  ,tag "animate"         ("Animacy","Anim")         "animate"       "Animate"         "Animacy"
  ,tag "inanimate"       ("Animacy","Inam")         "inanimate"     "Inanimate"       "Animacy"
  ]
  where
    tag wikt_tag umorph_tag label ident typ = Tag wikt_tag umorph_tag (ident2label (identS "label")) (identS ident) (identS typ)


def_tags = filter_defaults identW all_tags
  where
    filter_defaults prev [] = []
    filter_defaults prev (t:ts)
      | prev /= typ t = t : filter_defaults (typ t) ts
      | otherwise     = filter_defaults prev ts
