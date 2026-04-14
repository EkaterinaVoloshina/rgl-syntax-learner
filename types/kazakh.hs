import Learner.Shell
import Data.Map as Map

main = learnerMain ((defaultConfig "kk" "kaz" "kazakh")
                       { cfgTreebanks = ["SUD_Kazakh-KTB"],
                         cfgDefaults = [("Number", "Sing")]
                       })
