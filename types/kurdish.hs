import Learner.Shell

main = learnerMain ((defaultConfig "ku" "kur" "kurdish")
                       { cfgTreebanks = ["SUD_Southern_Kurdish-Garrusi","SUD_Northern_Kurdish-Kurmanji","SUD_Central_Kurdish-Mukri"]
                       })
