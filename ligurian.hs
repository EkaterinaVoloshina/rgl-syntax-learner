import Learner.Shell

main = learnerMain ((defaultConfig "lij" "lij" "ligurian")
                       { cfgTreebanks = ["SUD_Ligurian-GLT"]
                       })
