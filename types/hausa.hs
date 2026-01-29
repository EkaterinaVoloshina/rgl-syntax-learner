import Learner.Shell

main = learnerMain ((defaultConfig "ha" "hau" "hausa")
                       { cfgTreebanks = ["SUD_Hausa-NorthernAutogramm","SUD_Hausa-SouthernAutogramm","SUD_Hausa-WesternAutogramm"]
                       })
