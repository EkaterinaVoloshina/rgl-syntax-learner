import Learner.Shell

main = learnerMain ((defaultConfig "kv" "kpv" "komi-zyrian")
                       { cfgTreebanks = ["SUD_Komi_Zyrian-IKDP","SUD_Komi_Zyrian-Lattice"]
                       })
