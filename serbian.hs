import Learner.Shell

main = learnerMain ((defaultConfig "sr" "srp" "serbian")
                       { cfgTreebanks = ["SUD_Serbian-SET"]
                       })
