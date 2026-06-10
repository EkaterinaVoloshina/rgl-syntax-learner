import Learner.Shell

main = learnerMain ((defaultConfig "gu" "guj" "gujarati")
                       { cfgTreebanks = ["SUD_Gujarati-GujTB"]
                       })
