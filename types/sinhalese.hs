import Learner.Shell

main = learnerMain ((defaultConfig "si" "sin" "sinhalese")
                       { cfgTreebanks = ["SUD_Sinhala-STB"]
                       })
