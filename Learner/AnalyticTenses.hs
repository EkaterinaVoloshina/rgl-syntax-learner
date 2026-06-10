module Learner.AnalyticTenses where

import Control.Monad
import qualified Data.Map as Map
import Data.Containers.ListUtils (nubOrd)
import Learner.Config


analiticForms :: [(String,[String],[([String],String)])] -> IO ()
analiticForms dict = do
  let mwes = Map.filter (\ws -> length ws == 1) $ fmap nubOrd $ Map.fromListWith (++) $ concatMap
                  (\(word,tags,forms) ->
                      let (forms',mwes) = partition forms
                          mwes' = map (\(tags,ws) -> (tags,[map (analyse forms') ws])) mwes
                      in mwes')
                  dict
  mapM_ (putStrLn . showMwe) (Map.toList mwes)
  where
    partition [] = ([],[])
    partition ((tags,form):forms) =
      case ws of
        [w] -> ((tags,form) : forms',mwes)
        ws  -> (forms', (tags, ws) : mwes)
      where
        ws = words form
        (forms',mwes) = partition forms

    analyse forms w =
      case [tags | (tags,form) <- forms, form == w] of
        (tags:_) -> Left tags
        []       -> Right w

    showMwe (tags,wss) = show tags ++ " -> " ++ unwords ["("++unwords (map showW ws)++")" | ws <- wss]
    showW (Left tags) = show tags
    showW (Right   w) = w
