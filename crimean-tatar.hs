import Learner.Shell

main = learnerMain ((defaultConfig "crh" "crh" "crimean-tatar")
                       { cfgTreebanks = ["SUD_Tatar-NMCTT"]
                       })
