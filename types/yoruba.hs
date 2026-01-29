import Learner.Shell

main = learnerMain ((defaultConfig "yo" "yor" "yoruba")
                       { cfgTreebanks = ["SUD_Yoruba-YTB"]
                       })
