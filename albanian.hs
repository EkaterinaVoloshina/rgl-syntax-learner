import Learner.Shell

main = learnerMain ((defaultConfig "sq" "sqi" "albanian")
                       { cfgTreebanks = ["SUD_Albanian-STAF","SUD_Albanian-TSA"]
                       })
