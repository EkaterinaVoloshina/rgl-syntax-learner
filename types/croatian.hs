import Learner.Shell

main = learnerMain ((defaultConfig "hr" "hrv" "croatian")
                       { cfgTreebanks = ["SUD_Croatian-SET"]
                       })
