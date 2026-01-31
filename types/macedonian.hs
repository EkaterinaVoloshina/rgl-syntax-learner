{-# LANGUAGE LambdaCase #-}
import Learner.Shell

main = learnerMain ((defaultConfig "mk" "mkd" "macedonian")
                       { cfgUpdTags   = \case
                                           ["comparative"] -> [["indefinite","masculine","comparative"]]
                                           ["superlative"] -> [["indefinite","masculine","superlative"]]
                                           tags            -> [tags]
                       , cfgAnalyticTenses = True
                       , cfgTreebanks = ["SUD_Macedonian-MTB"]
                       })
