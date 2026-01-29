import Learner.Shell

main = learnerMain ((defaultConfig "hy" "hye" "armenian")
                       { cfgTreebanks = ["SUD_Armenian-ArmTDP","SUD_Armenian-BSUT"]
                       })
