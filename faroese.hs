import Learner.Shell

main = learnerMain ((defaultConfig "fo" "fao" "faroese")
                       { cfgTreebanks = ["SUD_Faroese-FarPaHC","SUD_Faroese-OFT"]
                       })
