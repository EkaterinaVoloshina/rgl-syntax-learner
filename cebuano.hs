import Learner.Shell

main = learnerMain ((defaultConfig "ceb" "ceb" "cebuano")
                       { cfgTreebanks = ["SUD_Cebuano-GJA"]
                       })
