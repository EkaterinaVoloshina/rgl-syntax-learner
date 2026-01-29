import Learner.Shell

main = learnerMain ((defaultConfig "yi" "yid" "yiddish")
                       { cfgTreebanks = ["SUD_Yiddish-YiTB"]
                       })
