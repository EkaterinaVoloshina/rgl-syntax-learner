import grewpy
from grewpy import Corpus, Request
import pandas as pd
from typing import Dict, List

grewpy.set_config("sud")
feats = ["Number", "Gender", "Case", "Person"] # TODO: automatically extract it

def form_pattern(pattern: str,
                 deprel: str,
                 feat: str = None,
                 binary: bool = True) -> Dict[str, str]:
    patterns = {}
    pattern = pattern.replace("[deprel]", f"[{deprel}]")
    if binary:
        if feat: # equality or unequality of a feature
            pattern = pattern.replace("[feat]", feat)
            patterns["0"] = pattern.replace("[rel]", "<>")  # without feature
            patterns["1"] = pattern.replace("[rel]", "=") # with feature
        else:
            patterns["1"] = pattern + "; X << Y"
            patterns["0"] = pattern + "; X >> Y"
    else:

        req7 = Request().pattern(pattern)
        values = corpus.count(req7, clustering_parameter=[f"Y.{feat}"])
        for v in values:
            patterns[v] = pattern + f";Y[{feat}={v}]"
    return patterns


def form_data(corpus: Corpus,
              ids: list,
              target: str,
              nodes: list = ["X", "Y"],
              exclude: list = ["textform", "wordform", "form", "SpaceAfter"]) -> List[str]:
    data = []
    for idx in ids:
        sent_data = {}
        sentence = corpus.get(idx["sent_id"])
        match = idx["matching"]["nodes"]
        for node in nodes:
            feats = {f"{k}_{node}":v for k, v in sentence[match[node]].items()
                     if k not in exclude}
            sent_data.update(feats)
            sent_data["target"] = target
            sent_data["idx"] = idx["sent_id"]

        data.append(sent_data)
    return data



# idea: store patterns as json file with rule name as key and pattern as value
# we need to a tree how to progress to find all rules

# question 1: what to do with Number[abs] etc?
# question 2: how to make it to grandparents etc?
# question 3: how to add wordnet integration here?
# note 1: keep extracted rules in json file
# note 2: maybe some filtering is necessary for training data



treebank_path = "SUD_Russian-SynTagRus/ru_syntagrus-sud-test.conllu"
corpus = Corpus(treebank_path)
n_sentencens = len(corpus)
sent_ids = corpus.get_sent_ids()

print(f"{n_sentencens = }")
print(f"{sent_ids[0] = }")


# how to find all deprels in the corpus?
req6 = Request().pattern("e: X->Y")
all_dels = corpus.count(req6, clustering_parameter=["e.label"])


# case marking
patterns = form_pattern(pattern="X-[deprel]->Y; X[upos=VERB]; Y[upos=NOUN]",
                             deprel="comp:obj",
                             feat="Case",
                             binary=False)

# word order
patterns = form_pattern(pattern="X-[deprel]->Y",
                             deprel="subj",
                             binary=True)


# agreement by number
#patterns = form_pattern(pattern="X -[deprel]-> Y; X.[feat] [rel] Y.[feat]",
#                        deprel="subj",
#                        feat="Number",
#                        binary=True)

dfs = []
for num, pattern in patterns.items():
    req = Request().pattern(pattern)
    ids = corpus.search(req)
    df = pd.DataFrame(form_data(corpus, ids, num))
    dfs.append(df)

df = pd.concat(dfs, ignore_index=True)
df.to_csv("ru_case_marking.csv")