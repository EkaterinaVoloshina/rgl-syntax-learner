import Learner.Shell

main = learnerMain ((defaultConfig "ht" "hat" "haitian-creole")
                       { cfgTreebanks = ["SUD_Haitian_Creole-Adolphe","SUD_Haitian_Creole-Autogramm"]
                       })
