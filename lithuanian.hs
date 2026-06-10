import Learner.Shell

main = learnerMain ((defaultConfig "lt" "lit" "lithuanian")
                       { cfgTreebanks = ["SUD_Lithuanian-ALKSNIS","SUD_Lithuanian-HSE"]
                       })
