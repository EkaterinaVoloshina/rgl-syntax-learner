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
  (decimal,group,minus,rbnf) <- readCLDR cfg

  rgl <- readGrammar cfg

  rgl <- addCatLincat rgl "Digits"  (RecType [(ident2label s,[],Sort cStr),(ident2label n,[],Cn number),(ident2label tl,[],Cn dtail)])
  rgl <- addCatLincat rgl "Decimal" (RecType [(ident2label s,[],Sort cStr),(ident2label n,[],Cn number),(ident2label hasDot,[],Cn cBool)])

  let lincatDig   = RecType [(ident2label s,[],Sort cStr),(ident2label n,[],Cn number)]
  let lincatDigit = RecType [(ident2label s,[],Sort cStr)
                            ,(ident2label teen,[],typeStr)
                            ,(ident2label ten,[],typeStr)
                            ,(ident2label hundred,[],Sort cStr)
                            ]
  rgl <- addLincat rgl "Dig" lincatDig
  rgl <- addLincat rgl "Digit" lincatDigit
  rgl <- addLincat rgl "Sub10"  (RecType [(ident2label s,[],Sort cStr)
                                         ,(ident2label hundred,[],Sort cStr)
                                         ,(ident2label n,[],Cn number)
                                         ])
  rgl <- addLincat rgl "Sub100" (RecType [(ident2label s,[],Sort cStr)
                                         ,(ident2label n,[],Cn number)
                                         ])
  rgl <- addOper rgl "mkDigit" (Prod Explicit identW typeStr (Prod Explicit identW typeStr (Prod Explicit identW typeStr (Prod Explicit identW typeStr lincatDigit))))
                               (Abs Explicit s (Abs Explicit teen (Abs Explicit ten (Abs Explicit hundred
                                  (R [assign (ident2label s) (Vr s)
                                     ,assign (ident2label teen) (Vr teen)
                                     ,assign (ident2label ten) (Vr ten)
                                     ,assign (ident2label hundred) (Vr hundred)])))))
  rgl <- addOper rgl "mkDig" (Prod Explicit identW typeStr (Prod Explicit identW (Cn number) lincatDig))
                             (Abs Explicit s (Abs Explicit n (R [assign (ident2label s) (Vr s),assign (ident2label n) (Vr n)])))
  rgl <- transferRule1 rgl rbnf "pot01" 1
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
                                               ,assign (ident2label hundred) (P (Vr n) (ident2label hundred))
                                               ,assign (ident2label n) (Cn pl)]))
  rgl <- addRule rgl "pot1" (Abs Explicit n (R [assign (ident2label s) (P (Vr n) (ident2label ten))
                                               ,assign (ident2label n) (Cn pl)]))
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
  rgl <- addRule rgl "IDig" (Abs Explicit d (ExtR (Vr d) (R [assign (ident2label tl) (Cn t1)])))
  rgl <- addRule rgl "IIDig" (Abs Explicit d (Abs Explicit ds (R [assign (ident2label s) (C (P (Vr d) (ident2label s))
                                                                                            (C (S (T TRaw [(PC t3     [],group_term group)
                                                                                                          ,(PC identW [],Cn cBIND)
                                                                                                          ])
                                                                                                  (P (Vr ds) (ident2label tl)))
                                                                                               (P (Vr ds) (ident2label s))))
                                                                 ,assign (ident2label n) (Cn pl)
                                                                 ,assign (ident2label tl) (App (Cn inc) (P (Vr ds) (ident2label tl)))])))
  rgl <- addRule rgl "PosDecimal" (Abs Explicit ds (ExtR (Vr ds) (R [assign (ident2label hasDot) (Cn cFalse)])))
  rgl <- addRule rgl "NegDecimal" (Abs Explicit ds (R [assign (ident2label s) (C (K minus)
                                                                                 (C (Cn cBIND)
                                                                                    (P (Vr ds) (ident2label s))))
                                                      ,assign (ident2label n) (Cn pl)
                                                      ,assign (ident2label hasDot) (Cn cFalse)]))
  rgl <- addRule rgl "IFrac" (Abs Explicit ds (Abs Explicit d (R [assign (ident2label s) (C (S (T TRaw [(PC cTrue  [],P (Vr ds) (ident2label s))
                                                                                                       ,(PC cFalse [],C (P (Vr ds) (ident2label s)) (C (Cn cBIND) (K decimal)))
                                                                                                       ])
                                                                                               (P (Vr ds) (ident2label hasDot)))
                                                                                            (C (Cn cBIND)
                                                                                               (P (Vr d) (ident2label s))))
                                                                 ,assign (ident2label n) (Cn pl)
                                                                 ,assign (ident2label hasDot) (Cn cTrue)])))
  writeGrammar cfg rgl
  where
    s = identS "s"
    teen = identS "teen"
    ten = identS "ten"
    hundred = identS "hundred"
    n = identS "n"
    d = identS "d"
    ds = identS "ds"
    number = identS "Number"
    pl = identS "Pl"
    sg = identS "Sg"
    hasDot = identS "hasDot"
    tl = identS "tail"
    dtail = identS "DTail"
    t1 = identS "T1"
    t2 = identS "T2"
    t3 = identS "T3"
    inc = identS "inc"

    group_term " " = Cn cSOFT_SPACE
    group_term cs  = C (Cn cBIND) (C (K cs) (Cn cBIND))

    addCatLincat rgl cat typ = do
      let m = rglCat rgl
          jments' = Map.insert (identS cat) (CncCat (Just (noLoc typ)) Nothing Nothing Nothing Nothing) (jments m)
      return rgl{rglCat=m{jments=jments'}}

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
                   -> do addRule rgl rule (R [assign (ident2label s) (K s_val)
                                             ,assign (ident2label hundred) (K hundred_val)
                                             ,assign (ident2label n) (Cn sg)
                                             ])

    transferRule10 rgl rbnf rule i =
      case evalRule "spellout-numbering" i rbnf of
        Nothing    -> do putStrLn ("No rbnf found for "++cfgLangName cfg++" number "++show i)
                         return rgl
        Just s_val -> do addRule rgl rule (R [assign (ident2label s) (K s_val)
                                             ,assign (ident2label n) (Cn pl)])

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
