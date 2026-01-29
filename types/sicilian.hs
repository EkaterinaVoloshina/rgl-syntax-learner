import Learner.Shell

main = learnerMain ((defaultConfig "scn" "scn" "sicilian")
                       { cfgTreebanks = ["SUD_Sicilian-STB"]
                       })
