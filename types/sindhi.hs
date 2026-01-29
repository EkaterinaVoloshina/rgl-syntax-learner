import Learner.Shell

main = learnerMain ((defaultConfig "sd" "snd" "sindhi")
                       { cfgTreebanks = ["SUD_Sindhi-Isra"]
                       })
