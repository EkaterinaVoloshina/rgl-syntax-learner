import Learner.Shell

main = learnerMain ((defaultConfig "nv" "nav" "navaho")
                       { cfgTreebanks = ["SUD_Navaho-RB"]
                       })
