import Learner.Shell

main = learnerMain ((defaultConfig "cs" "Cze" "czech")
                       { cfgTreebanks = ["SUD_Czech-CAC","SUD_Czech-CLTT","SUD_Czech-FicTree","SUD_Czech-PDTC","SUD_Czech-Poetry","SUD_Czech-PUD"]
                       })
