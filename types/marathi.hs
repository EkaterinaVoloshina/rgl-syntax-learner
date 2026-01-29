import Learner.Shell

main = learnerMain ((defaultConfig "mr" "mar" "marathi")
                       { cfgTreebanks = ["SUD_Marathi-UFAL"]
                       })
