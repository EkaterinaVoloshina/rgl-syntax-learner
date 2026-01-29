import Learner.Shell

main = learnerMain ((defaultConfig "sms" "sms" "skolt-sami")
                       { cfgTreebanks = ["SUD_Skolt_Sami-Giellagas"]
                       })
