{-# LANGUAGE LambdaCase #-}
import Learner.Shell

main = learnerMain ((defaultConfig "mk" "mkd" "macedonian")
                       { cfgUpdForms  =
                            \word pos ->
                              case pos of
                                "adj" -> concatMap (\(tags,w) ->
                                             if elem "diminutive" tags || elem "abstract-noun" tags
                                               then []
                                               else let tags1 =
                                                          if not (elem "adverb" tags) && not (elem "comparative" tags) && not (elem "superlative" tags)
                                                            then "positive":tags
                                                            else tags
                                                        tags2 =
                                                          if not (elem "adverb" tags) && not (elem "singular" tags) && not (elem "plural" tags)
                                                            then "singular":tags1
                                                            else tags1
                                                    in [(tags2,no_stress w)])
                                "name"-> concatMap (\(tags,w) ->
                                             if elem "count-form" tags
                                               then [(["count-form"],no_stress w)]
                                               else [(tags,no_stress w)])
                                "noun"-> concatMap (\(tags,w) ->
                                             if elem "diminutive" tags || elem "collective" tags || elem "augmentative" tags
                                               then []
                                               else if elem "count-form" tags
                                                      then [(["count-form"],no_stress w)]
                                                      else [(tags,no_stress w)])
                                "verb"-> concatMap (\(tags,w) ->
                                             if elem "diminutive" tags
                                               then []
                                               else [(tags,no_stress w)])
                                _ -> map (\(tags,w) -> (tags,no_stress w))
                       , cfgAnalyticTenses = True
                       , cfgTreebanks = ["SUD_Macedonian-MTB"]
                       })
  where
    no_stress []      = []
    no_stress ('́':cs) =     no_stress cs
    no_stress (c :cs) = c : no_stress cs
