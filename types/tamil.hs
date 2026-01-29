import Learner.Shell

main = learnerMain ((defaultConfig "ta" "tam" "tamil")
                       { cfgTreebanks = ["SUD_Tamil-MWTT","SUD_Tamil-TTB"]
                       })
