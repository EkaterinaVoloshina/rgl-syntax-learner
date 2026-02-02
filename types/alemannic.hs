import Learner.Shell

main = learnerMain ((defaultConfig "gsw" "gsw" "alemannic")
                       { cfgUpdForms =
                           \lemma pos forms ->
                                case pos of
                                  "noun" -> (["singular"],lemma) : forms
                                  "adj"  -> map insertSingular forms
                                  _      -> forms
                       , cfgTreebanks = ["SUD_Alemannic-DIVITAL"]
                       })
  where
    insertSingular (tags,w)
      | (elem "strong" tags || elem "weak" tags) &&
        not (elem "plural" tags)
                  = ("singular":tags,w)
      | otherwise = (tags,w)
