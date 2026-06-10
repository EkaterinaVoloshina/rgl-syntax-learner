import Learner.Shell

main = learnerMain ((defaultConfig "vi" "vie" "vietnamese")
                       { cfgTreebanks = ["SUD_Vietnamese-TueCL","SUD_Vietnamese-VTB"]
                       })
