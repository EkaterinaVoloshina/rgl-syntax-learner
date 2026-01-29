import Learner.Shell

main = learnerMain ((defaultConfig "ka" "kat" "georgian")
                       { cfgTreebanks = ["SUD_Georgian-GLC","SUD_Georgian-GNC"]
                       })
