module Learner.Numerals(options, learn) where

import GF.Grammar
import Control.Monad(when,liftM2,liftM4)
import System.Console.GetOpt
import qualified Data.Map as Map
import Learner.RGL
import Learner.Config hiding (POS)
import Learner.CLDR

options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
  ]

learn cfg = do
  rbnf <- readCLDR cfg
  rgl <- readGrammar cfg
  let lincatDig   = RecType [(ident2label s,[],Sort cStr),(ident2label n,[],Cn number)]
  let lincatDigit = RecType [(ident2label s,[],Sort cStr),(ident2label teen,[],typeStr),(ident2label ten,[],typeStr),(ident2label hundred,[],Sort cStr)]
  rgl <- addLincat rgl "Dig" lincatDig
  rgl <- addLincat rgl "Digit" lincatDigit
  rgl <- addLincat rgl "Sub10"  (RecType [(ident2label s,[],Sort cStr),(ident2label hundred,[],Sort cStr)])
  rgl <- addLincat rgl "Sub100" (RecType [(ident2label s,[],Sort cStr)])
  rgl <- addOper rgl "mkDigit" (Prod Explicit identW typeStr (Prod Explicit identW typeStr (Prod Explicit identW typeStr (Prod Explicit identW typeStr lincatDigit))))
                               (Abs Explicit s (Abs Explicit teen (Abs Explicit ten (Abs Explicit hundred
                                  (R [assign (ident2label s) (Vr s)
                                     ,assign (ident2label teen) (Vr teen)
                                     ,assign (ident2label ten) (Vr ten)
                                     ,assign (ident2label hundred) (Vr hundred)])))))
  rgl <- addOper rgl "mkDig" (Prod Explicit identW typeStr (Prod Explicit identW (Cn number) lincatDig))
                             (Abs Explicit s (Abs Explicit n (R [assign (ident2label s) (Vr s),assign (ident2label n) (Vr n)])))
  rgl <- transferRule1 rgl rbnf "pot01"  1
  rgl <- transferRule10 rgl rbnf "pot110" 10
  rgl <- transferRule10 rgl rbnf "pot111" 11
  rgl <- transferRule rgl rbnf "n2" 2
  rgl <- transferRule rgl rbnf "n3" 3
  rgl <- transferRule rgl rbnf "n4" 4
  rgl <- transferRule rgl rbnf "n5" 5
  rgl <- transferRule rgl rbnf "n6" 6
  rgl <- transferRule rgl rbnf "n7" 7
  rgl <- transferRule rgl rbnf "n8" 8
  rgl <- transferRule rgl rbnf "n9" 9
  rgl <- addRule rgl "pot0" (Abs Explicit n (R [assign (ident2label s) (P (Vr n) (ident2label s))
                                               ,assign (ident2label hundred) (P (Vr n) (ident2label hundred))]))
  rgl <- addRule rgl "pot1" (Abs Explicit n (R [assign (ident2label s) (P (Vr n) (ident2label ten))]))
  rgl <- addRule rgl "pot2" (Abs Explicit n (R [assign (ident2label s) (P (Vr n) (ident2label hundred))]))
  rgl <- addRule rgl "pot0as1" (Abs Explicit n (Vr n))
  rgl <- addRule rgl "pot1as2" (Abs Explicit n (Vr n))
  rgl <- addRule rgl "pot2as3" (Abs Explicit n (Vr n))
  rgl <- addRule rgl "pot3as4" (Abs Explicit n (Vr n))
  rgl <- addRule rgl "pot4as5" (Abs Explicit n (Vr n))
  rgl <- addRule rgl "D_0" (App (App (Vr (identS "mkDig")) (K "0")) (Cn pl))
  rgl <- addRule rgl "D_1" (App (App (Vr (identS "mkDig")) (K "1")) (Cn sg))
  rgl <- addRule rgl "D_2" (App (App (Vr (identS "mkDig")) (K "2")) (Cn pl))
  rgl <- addRule rgl "D_3" (App (App (Vr (identS "mkDig")) (K "3")) (Cn pl))
  rgl <- addRule rgl "D_4" (App (App (Vr (identS "mkDig")) (K "4")) (Cn pl))
  rgl <- addRule rgl "D_5" (App (App (Vr (identS "mkDig")) (K "5")) (Cn pl))
  rgl <- addRule rgl "D_6" (App (App (Vr (identS "mkDig")) (K "6")) (Cn pl))
  rgl <- addRule rgl "D_7" (App (App (Vr (identS "mkDig")) (K "7")) (Cn pl))
  rgl <- addRule rgl "D_8" (App (App (Vr (identS "mkDig")) (K "8")) (Cn pl))
  rgl <- addRule rgl "D_9" (App (App (Vr (identS "mkDig")) (K "9")) (Cn pl))
  writeGrammar cfg rgl
  where
    s = identS "s"
    teen = identS "teen"
    ten = identS "ten"
    hundred = identS "hundred"
    n = identS "n"
    number = identS "Number"
    pl = identS "Pl"
    sg = identS "Sg"

    addLincat rgl cat typ = do
      let m = rglNumeral rgl
          jments' = Map.insert (identS cat) (CncCat (Just (noLoc typ)) Nothing Nothing Nothing Nothing) (jments m)
      return rgl{rglNumeral=m{jments=jments'}}

    addOper rgl fn typ def = do
      let m = rglNumeral rgl
          jments' = Map.insert (identS fn) (ResOper (Just (noLoc typ)) (Just (noLoc def))) (jments m)
      return rgl{rglNumeral=m{jments=jments'}}

    transferRule1 rgl rbnf rule i =
      case liftM2 (,) (evalRule "spellout-numbering" i rbnf) (evalRule "spellout-numbering" (i*100) rbnf) of
        Nothing    -> do putStrLn ("No rbnf found for "++cfgLangName cfg++" number 1")
                         return rgl
        Just (s_val,hundred_val)
                   -> do addRule rgl rule (R [assign (ident2label s) (K s_val), assign (ident2label hundred) (K hundred_val)])

    transferRule10 rgl rbnf rule i =
      case evalRule "spellout-numbering" i rbnf of
        Nothing    -> do putStrLn ("No rbnf found for "++cfgLangName cfg++" number 1")
                         return rgl
        Just s_val -> do addRule rgl rule (R [assign (ident2label s) (K s_val)])

    transferRule rgl rbnf rule i =
      case liftM4 (,,,) (evalRule "spellout-numbering" i rbnf)
                        (evalRule "spellout-numbering" (i+10) rbnf)
                        (evalRule "spellout-numbering" (i*10) rbnf)
                        (evalRule "spellout-numbering" (i*100) rbnf) of
        Nothing  -> do putStrLn ("No rbnf found for "++cfgLangName cfg++" number "++show i)
                       return rgl
        Just (s,teen,ten,hundred)
                 -> do addRule rgl rule (App (App (App (App (Vr (identS "mkDigit")) (K s)) (K teen)) (K ten)) (K hundred))

    addRule rgl fn def = do
      let m = rglNumeral rgl
          jments' = Map.insert (identS fn) (CncFun Nothing (Just (noLoc def)) Nothing Nothing) (jments m)
      return rgl{rglNumeral=m{jments=jments'}}
