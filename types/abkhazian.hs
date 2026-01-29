import Learner.Shell

main = learnerMain ((defaultConfig "ab" "abk" "abkhazian")
                       { cfgTreebanks = ["SUD_Abkhaz-AbNC"]
                       })
