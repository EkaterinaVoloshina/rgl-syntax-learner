import Learner.Shell

main = learnerMain ((defaultConfig "bar" "bar" "bavarian")
                       { cfgTreebanks = ["SUD_Bavarian-MaiBaam"]
                       })
