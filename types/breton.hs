import Learner.Shell

main = learnerMain ((defaultConfig "br" "bre" "breton")
                       { cfgTreebanks = ["SUD_Breton-KEB"]
                       })
