import Learner.Shell

main = learnerMain ((defaultConfig "ml" "mal" "malayalam")
                       { cfgTreebanks = ["SUD_Malayalam-UFAL"]
                       })
