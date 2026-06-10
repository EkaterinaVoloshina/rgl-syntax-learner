import Learner.Shell

main = learnerMain ((defaultConfig "ug" "uig" "uyghur")
                       { cfgTreebanks = ["SUD_Uyghur-UDT"]
                       })
