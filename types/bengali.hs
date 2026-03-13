import Learner.Shell

main = learnerMain ((defaultConfig "bn" "ben" "bengali")
                       { cfgUpdPOS =
                           \lemma pos ->
                                case (lemma,pos) of
                                  ("ফরমানো", "noun") -> "verb"
                                  ("বিশ্লেষণ করা", "noun") -> "verb"
                                  _                 -> pos
                       , {-cfgUpdForms =
                           \lemma pos forms ->
                                case pos of
                                  "adj"  -> (["masculine"],lemma) : forms
                                  "adv"  -> (["s"],lemma) : forms
                                  "conj" -> (["s"],lemma) : forms
                                  "det"  -> (["s"],lemma) : forms
                                  "intj" -> (["s"],lemma) : forms
                                  "name" -> map injectIndefinite ((["nominative"],lemma) : forms)
                                  "noun" -> map injectIndefinite ((["nominative"],lemma) : forms)
                                  _      -> forms
                       , -} cfgTreebanks = ["SUD_Bengali-BRU"]
                       })
  where
    injectIndefinite (["nominative"],w) = (["indefinite","nominative"],w)
    injectIndefinite (["genitive"],w)   = (["indefinite","genitive"],w)
    injectIndefinite (["locative"],w)   = (["indefinite","locative"],w)
    injectIndefinite (["objective"],w)  = (["indefinite","objective"],w)
    injectIndefinite x = x
