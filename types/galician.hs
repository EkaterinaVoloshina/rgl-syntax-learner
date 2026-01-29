import Learner.Shell

main = learnerMain ((defaultConfig "gl" "glg" "galician")
                       { cfgTreebanks = ["SUD_Galician-CTG","SUD_Galician-PUD","SUD_Galician-TreeGal"]
                       })
