import Learner.Shell

main = learnerMain ((defaultConfig "bn" "ben" "bengali")
                       { cfgTreebanks = ["SUD_Bengali-BRU"]
                       })
