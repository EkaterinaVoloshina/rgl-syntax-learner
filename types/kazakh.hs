import Learner.Shell

main = learnerMain ((defaultConfig "kk" "kaz" "kazakh")
                       { cfgTreebanks = ["SUD_Kazakh-KTB"]
                       })
