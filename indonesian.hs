import Learner.Shell

main = learnerMain ((defaultConfig "id" "ind" "indonesian")
                       { cfgTreebanks = ["SUD_Indonesian-CSUI","SUD_Indonesian-GSD","SUD_Indonesian-PUD"]
                       })
