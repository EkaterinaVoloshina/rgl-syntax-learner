import Learner.Shell

main = learnerMain ((defaultConfig "mdf" "mdf" "moksha")
                       { cfgTreebanks = ["SUD_Moksha-JR"]
                       })
