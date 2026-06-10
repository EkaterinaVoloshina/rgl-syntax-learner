module Learner.Structural(options, learn) where

import GF.Grammar
import GF.Text.Pretty
import System.Console.GetOpt
import Learner.Config hiding (POS)
import Learner.CodeGen
import Learner.DecisionTree
import Data.List
import Data.Char
import Control.Monad

options =
  [ Option "v" [] (NoArg (\cfg->cfg{cfgVerbose=True})) "verbose output"
  ]

learn cfg = do
  treebanks <- forM (cfgTreebanks cfg) $ \treebank -> do
                 (train,_) <- readCONLL cfg treebank
                 return train
  let forms = [(map toLower w,morpho) | treebank <- treebanks
                   , t <- treebank
                   , n@(_,w,pos,morpho,_) <- nodes t
                   , pos == "PRON"
                   , ("PronType","Prs") `elem` morpho
                   ]
  let i_Pron     = (identS "i_Pron",     select [("Person","1"),("Number","Sing")] forms)
      youSg_Pron = (identS "youSg_Pron", select [("Person","2"),("Number","Sing")] forms)
      he_Pron    = (identS "he_Pron",    select [("Person","3"),("Number","Sing"),("Gender","Masc")] forms)
      she_Pron   = (identS "she_Pron",   select [("Person","3"),("Number","Sing"),("Gender","Fem")]  forms)
      it_Pron    = (identS "it_Pron",    select [("Person","3"),("Number","Sing"),("Gender","Neut")] forms)
      we_Pron    = (identS "we_Pron",    select [("Person","1"),("Number","Plur")] forms)
      youPl_Pron = (identS "youPl_Pron", select [("Person","2"),("Number","Plur")] forms)
      they_Pron  = (identS "they_Pron",  select [("Person","3"),("Number","Plur")] forms)
      youPol_Pron= (identS "youPol_Pron",select [("Person","2"),("Polite","Form")] forms)
      prons = [i_Pron,youSg_Pron,he_Pron,she_Pron,it_Pron,we_Pron,youPl_Pron,they_Pron,youPol_Pron]

  forM_ prons $ \(id,forms) -> do
    print (pp id)
    let forms' = nub [(w,foldl (convert morpho) [] (cfgMapping cfg)) | (w,morpho) <- forms]
    forM_ forms' $ \(w,tags) -> do
      putStrLn (w ++ " " ++ show tags)

    return ()
  where
    convert morpho tags tag
      | null (ud_tag tag)                   = tags
      | all (flip elem morpho) (ud_tag tag) = tag:tags
      | otherwise                           = tags

    select spec prons =
      case lookup "Gender" spec of
        Just g  -> -- not all languages have gender
                   case [x | x@(w,morpho) <- matching, elem ("Gender",g) morpho] of
                     []       -> matching
                     matching -> matching
        Nothing -> matching
      where
        matching = [x | x@(w,morpho) <- prons, all (\x -> elem x morpho || fst x == "Gender") spec]
