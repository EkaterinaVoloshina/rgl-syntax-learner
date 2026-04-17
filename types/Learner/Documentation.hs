module Learner.Documentation where

import Learner.Config
import Learner.RGL
import System.FilePath
import System.Console.GetOpt
import qualified Data.Map as Map
import GF.Infra.Ident
import GF.Infra.Option
import GF.Data.Operations
import GF.Text.Pretty
import GF.Grammar.Predef
import GF.Grammar.Lookup
import GF.Grammar.Grammar
import GF.Grammar.Printer
import GF.Grammar.Macros

-- add output folder
options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
  ]

-- | main function to learn different parts of a grammar
learn cfg = do
  (cnc,gr) <- loadGrammar ("src" </> cfgLangName cfg </> cfgLangModuleFileName cfg "Lang" ++".gf")
  let t = identS "t"
      i = identS "i"
      d = identS "d"
      e = identS "e"
      s1 = identS "s1"
      s2 = identS "s2"
      mi = ModInfo {
             mtype   = MTConcrete (moduleNameS "Documentation"),
             mstatus = MSComplete,
             mflags  = noOptions,
             mextend = [(cfgLangModuleName cfg "Cat",MIAll)],
             mwith   = Nothing,
             mopens  = [OSimple (cfgLangModuleName cfg "Res")
                       ,OSimple (moduleNameS "Prelude")
                       ,OSimple (moduleNameS "HTML")
                       ],
             mexdeps = [],
             msrc    = "",
             jments  = Map.fromList
                          (mkLinCat (identS "Inflection") (RecType
                                                             [(ident2label t, [],typeStr)
                                                             ,(ident2label s1,[],typeStr)
                                                             ,(ident2label s2,[],typeStr)
                                                             ]) ++
                           mkLinCat (identS "Definition") defLinType ++
                           mkLinCat (identS "Document") defLinType ++
                           mkLinCat (identS "Tag") defLinType ++
                           mkLin (identS "NoDefinition") (Abs Explicit t (Vr t)) ++
                           mkLin (identS "MkDefinition") (Abs Explicit t (Abs Explicit d (R [assign theLinLabel (C (K "<p><b>Definition:</b>") (C (P (Vr t) theLinLabel) (C (P (Vr d) theLinLabel) (K "</p>"))))]))) ++
                           mkLin (identS "MkDefinitionEx") (Abs Explicit t (Abs Explicit d (Abs Explicit e (R [assign theLinLabel (C (K "<p><b>Definition:</b>") (C (P (Vr t) theLinLabel) (C (P (Vr d) theLinLabel) (C (K "<p><b>Example:</b>") (C (P (Vr t) theLinLabel) (K "</p>"))))))])))) ++
                           mkLin (identS "MkTag") (Abs Explicit i (R [assign theLinLabel (P (Vr i) (ident2label t))])) ++
                           mkLin (identS "MkDocument") (Abs Explicit d (Abs Explicit i (Abs Explicit e (R [assign theLinLabel (P (Vr i) (ident2label s1))])))) ++
                           mkInflection gr cnc "n" "Noun" (identS "noun") "N"  ++
                           mkInflection gr cnc "n" "Noun" (identS "noun") "N2" ++
                           mkInflection gr cnc "n" "Noun" (identS "noun") "N3" ++
                           mkInflection gr cnc "pn" "Noun" (identS "pn") "PN" ++
                           mkInflection gr cnc "pn" "Noun" (identS "pn") "LN" ++
                           mkInflection gr cnc "pn" "Noun" (identS "pn") "GN" ++
                           mkInflection gr cnc "pn" "Noun" (identS "pn") "SN" ++
                           mkInflection gr cnc "a" "Noun" (identS "adj") "A" ++
                           mkInflection gr cnc "a" "Noun" (identS "adj") "A2" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "V" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "V2" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "VV" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "VS" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "VQ" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "VA" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "V3" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "V2V" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "V2S" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "V2Q" ++
                           mkInflection gr cnc "v" "Noun" (identS "v") "V2A" ++
                           mkInflection gr cnc "adv" "Noun" (identS "adv") "Adv" ++
                           mkInflection gr cnc "adv" "Noun" (identS "adv") "AdV" ++
                           mkInflection gr cnc "adv" "Noun" (identS "adv") "AdA" ++
                           mkInflection gr cnc "adv" "Noun" (identS "adv") "AdN" ++
                           mkInflection gr cnc "prep" "Noun" (identS "prep") "Prep")
           }
  print (ppModule Unqualified (cfgLangModuleName cfg "Documentation", mi))


mkLinCat cat ty = [(cat,CncCat (Just (noLoc ty)) Nothing Nothing Nothing Nothing)]

mkLin fun t = [(fun, CncFun Nothing (Just (noLoc t)) Nothing Nothing)]

mkInflection gr cnc tag tag_heading var cat =
  case lookupResDef gr (cnc,identS cat) of
    Ok ty -> let t  = K tag
                 s1 = heading1 (K tag_heading)
                 s2 = frameTable (foldr1 C (mkTerm empty (Vr var) ty))
                 body = Abs Explicit var
                               (R [assign (ident2label (identS "t")) t
                                  ,assign (ident2label (identS "s1")) s1
                                  ,assign (ident2label (identS "s2")) s2
                                  ])
             in mkLin (identS ("Inflection"++cat)) body
    Bad _ -> []
  where
    mkTerm path t (RecType lts) =
      [t | (l,_,ty) <- lts
         , let path' = if l == theLinLabel
                         then path
                         else path <+> pp l
         , t <- mkTerm path' (P t l) ty]
    mkTerm path t (Table arg res) =
      case allParamValues gr arg of
        Ok vs -> [t | v <- vs
                    , t <- mkTerm (path <+> ppTerm Unqualified 10 v) (S t v) res]
        Bad _ -> []
    mkTerm path t (Sort s)
      | s == cStr = [tr (C (th (K (show path))) (td t))]
    mkTerm path t _ = []

heading1 = App (Cn (identS "heading1"))
frameTable = App (Cn (identS "frameTable"))
th = App (Cn (identS "th"))
td = App (Cn (identS "td"))
tr = App (Cn (identS "tr"))
