import Learner.Shell

main = learnerMain ((defaultConfig "ky" "kir" "kyrgyz")
                       { cfgTreebanks = ["SUD_Kyrgyz-KTMU","SUD_Kyrgyz-TueCL"]
                       })
