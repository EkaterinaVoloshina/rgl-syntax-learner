module Learner.Numerals(options, learn) where

import GF.Grammar.Grammar
import Control.Monad(when)
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
  case evalRule "spellout-numbering" 1 rbnf of
    Nothing -> do putStrLn ("No rbnf found for "++cfgLangName cfg)
    Just s  -> do rgl <- readGrammar cfg
                  let m = rglNumerals rgl
                      jments' = Map.insert (identS "pot01") (CncFun Nothing (Just (noLoc (K s))) Nothing Nothing) (jments m)
                  writeGrammar cfg rgl{rglNumerals=m{jments=jments'}}
