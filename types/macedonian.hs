{-# LANGUAGE LambdaCase #-}
import Learner.Shell

main = learnerMain ((defaultConfig "mk" "mkd" "macedonian")
                       { cfgUpdForms  =
                            \_ _ ->
                              map (\case
                                      (["comparative"],w) -> (["indefinite","masculine","comparative"],w)
                                      (["superlative"],w) -> (["indefinite","masculine","superlative"],w)
                                      x                   -> x)
                       , cfgAnalyticTenses = True
                       , cfgTreebanks = ["SUD_Macedonian-MTB"]
                       })
