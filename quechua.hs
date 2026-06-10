import Learner.Shell
import Debug.Trace

main = learnerMain ((defaultConfig "qu" "que" "quechua")
                       { cfgTreebanks = []
                       , cfgUpdForms  =
                            \word pos forms ->
                              case pos of
                                "noun"-> fix_annotations True forms
                                "name"-> fix_annotations True forms
                                _     -> forms
                       , cfgFilterLemmas =
                            \word pos ->
                              case pos of
                                "adv" -> False
                                _     -> True
                       })
  where
    fix_annotations flag [] = []
    fix_annotations flag ((tags,w):forms)
      | elem "distributive" tags =
           if flag
             then ("singular":tags,w) : fix_annotations False forms
             else ("plural"  :tags,w) : fix_annotations False forms
      | otherwise =
           (map comp_case tags, w) : fix_annotations flag forms

    comp_case "comparative" = "comparative-case"
    comp_case x             = x
