import Learner.Shell

main = learnerMain ((defaultConfig "ga" "gle" "irish")
                       { cfgTreebanks = ["SUD_Irish-Cadhan","SUD_Irish-IDT","SUD_Irish-TwittIrish"]
                       })
