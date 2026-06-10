import Learner.Shell

main = learnerMain ((defaultConfig "sah" "sah" "sakha")
                       { cfgTreebanks = ["SUD_Yakut-YKTDT"]
                       })
