import Learner.Shell

main = learnerMain ((defaultConfig "gsw" "gsw" "alemannic")
                       { cfgTreebanks = ["SUD_Alemannic-DIVITAL"]
                       })
