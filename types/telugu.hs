import Learner.Shell

main = learnerMain ((defaultConfig "te" "tel" "telugu")
                       { cfgTreebanks = ["SUD_Telugu_English-TECT","SUD_Telugu-MTG"]
                       })
