import Learner.Shell

main = learnerMain ((defaultConfig "stq" "stq" "saterland-frisian")
                       { cfgTreebanks = ["SUD_Frisian_Dutch-Fame"]
                       })
