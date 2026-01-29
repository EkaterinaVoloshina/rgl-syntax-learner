import Learner.Shell

main = learnerMain ((defaultConfig "sa" "san" "sanskrit")
                       { cfgTreebanks = ["SUD_Sanskrit-UFAL","SUD_Sanskrit-Vedic"]
                       })
