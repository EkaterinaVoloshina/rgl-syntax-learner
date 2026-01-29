import Learner.Shell

main = learnerMain ((defaultConfig "cy" "Cym" "welsh")
                       { cfgTreebanks = ["SUD_Welsh-CCG"]
                       })
