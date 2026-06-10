import Learner.Shell

main = learnerMain ((defaultConfig "nap" "nap" "neapolitan")
                       { cfgTreebanks = ["SUD_Neapolitan-RB"]
                       })
