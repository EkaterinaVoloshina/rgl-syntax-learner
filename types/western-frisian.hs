import Learner.Shell

main = learnerMain ((defaultConfig "fy" "fry" "western-frisian")
                       { cfgTreebanks = ["SUD_Frisian_Dutch-Fame"]
                       })
