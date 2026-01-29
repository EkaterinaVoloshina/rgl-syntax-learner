import Learner.Shell

main = learnerMain ((defaultConfig "lb" "ltz" "luxembourgish")
                       { cfgTreebanks = ["SUD_Luxembourgish-LuxBank"]
                       })
