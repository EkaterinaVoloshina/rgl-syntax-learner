import Learner.Shell

main = learnerMain ((defaultConfig "gsw" "gsw" "alemannic")
                       { cfgUpdForms =
                           \lemma pos forms ->
                                case pos of
                                  "noun" -> (["singular"],lemma) : forms
                                  _      -> forms
                       , cfgTreebanks = ["SUD_Alemannic-DIVITAL"]
                       })
