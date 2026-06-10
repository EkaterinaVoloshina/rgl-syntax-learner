import Learner.Shell

main = learnerMain ((defaultConfig "uz" "uzb" "uzbek")
                       { cfgTreebanks = ["SUD_Uzbek-TueCL","SUD_Uzbek-UT","SUD_Uzbek-UzUDT"]
                       })
