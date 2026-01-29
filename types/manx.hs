import Learner.Shell

main = learnerMain ((defaultConfig "gv" "glv" "manx")
                       { cfgTreebanks = ["SUD_Manx-Cadhan"]
                       })
