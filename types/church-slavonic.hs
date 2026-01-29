import Learner.Shell

main = learnerMain ((defaultConfig "cu" "chu" "church-slavonic")
                       { cfgTreebanks = ["SUD_Old_Church_Slavonic-PROIEL"]
                       })
