import Learner.Shell

main = learnerMain ((defaultConfig "eo" "epo" "esperanto")
                       { cfgTreebanks = ["SUD_Esperanto-Cairo","SUD_Esperanto-Prago"]
                       })
