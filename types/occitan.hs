import Learner.Shell

main = learnerMain ((defaultConfig "oc" "oci" "occitan")
                       { cfgTreebanks = ["SUD_Occitan-TTB"]
                       })
