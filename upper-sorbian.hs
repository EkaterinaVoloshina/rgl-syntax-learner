import Learner.Shell

main = learnerMain ((defaultConfig "hsb" "hsb" "upper-sorbian")
                       { cfgTreebanks = ["SUD_Upper_Sorbian-UFAL"]
                       })
