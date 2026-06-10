import Learner.Shell

main = learnerMain ((defaultConfig "gd" "gla" "gaelic")
                       { cfgTreebanks = ["SUD_Scottish_Gaelic-ARCOSG"]
                       })
