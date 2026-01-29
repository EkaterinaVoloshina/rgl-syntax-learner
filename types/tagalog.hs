import Learner.Shell

main = learnerMain ((defaultConfig "tl" "tgl" "tagalog")
                       { cfgTreebanks = ["SUD_Tagalog-TRG","SUD_Tagalog-Ugnayan"]
                       })
