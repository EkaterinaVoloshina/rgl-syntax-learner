import Learner.Shell

main = learnerMain ((defaultConfig "got" "got" "gothic")
                       { cfgTreebanks = ["SUD_Gothic-PROIEL"]
                       })
