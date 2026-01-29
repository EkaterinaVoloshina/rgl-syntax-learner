import Learner.Shell

main = learnerMain ((defaultConfig "uk" "ukr" "ukranian")
                       { cfgTreebanks = ["SUD_Ukrainian-IU","SUD_Ukrainian-ParlaMint"]
                       })
