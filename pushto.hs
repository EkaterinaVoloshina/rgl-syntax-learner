import Learner.Shell

main = learnerMain ((defaultConfig "ps" "pus" "pushto")
                       { cfgTreebanks = ["SUD_Pashto-Sikaram"]
                       })
