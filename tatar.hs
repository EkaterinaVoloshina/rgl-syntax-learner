import Learner.Shell

main = learnerMain ((defaultConfig "tt" "tat" "tatar")
                       { cfgTreebanks = ["SUD_Tatar-NMCTT"]
                       })
