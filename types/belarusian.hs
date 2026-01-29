import Learner.Shell

main = learnerMain ((defaultConfig "be" "bel" "belarusian")
                       { cfgTreebanks = ["SUD_Belarusian-HSE"]
                       })
