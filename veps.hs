import Learner.Shell

main = learnerMain ((defaultConfig "vep" "vep" "veps")
                       { cfgTreebanks = ["SUD_Veps-VWT"]
                       })
