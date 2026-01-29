import Learner.Shell

main = learnerMain ((defaultConfig "nds" "nds" "low-german")
                       { cfgTreebanks = []
                       })
