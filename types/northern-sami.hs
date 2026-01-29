import Learner.Shell

main = learnerMain ((defaultConfig "se" "sme" "northern-sami")
                       { cfgTreebanks = ["SUD_North_Sami-Giella"]
                       })
