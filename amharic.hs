import Learner.Shell

main = learnerMain ((defaultConfig "am" "amh" "amharic")
                       { cfgTreebanks = ["SUD_Amharic-ATT"]
                       })
