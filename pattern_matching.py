import grewpy
from grewpy import Corpus, Request
import pandas as pd
from typing import Dict, List
from tqdm.auto import tqdm

grewpy.set_config("sud")
exclude = ["textform", "wordform", "form", "SpaceAfter"]
lang = "ru"

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
              exclude: list = exclude) -> List[str]:
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

def search_patterns(corpus: Corpus,
                    patterns: dict,
                    feat: str = None) -> List[pd.DataFrame]:
    dfs = []
    for num, pattern in patterns.items():
        req = Request().pattern(pattern)
        ids = corpus.search(req)
        if len(ids) == 0:
            return []
        df = pd.DataFrame(form_data(corpus, ids, num, exclude=exclude+[feat]))
        dfs.append(df)
    return dfs



# idea: store patterns as json file with rule name as key and pattern as value
# we need to a tree how to progress to find all rules

# question 1: what to do with Number[abs] etc?
# question 2: how to make it to grandparents etc?
# question 3: how to add wordnet integration here?
# note 1: keep extracted rules in json file
# note 2: maybe some filtering is necessary for training data

treebank_path = "SUD_Russian-SynTagRus/ru_syntagrus-sud-test.conllu"
#treebank_path = "abq_atb-sud-test.conllu"
corpus = Corpus(treebank_path)
n_sentencens = len(corpus)
sent_ids = corpus.get_sent_ids()

print(f"{n_sentencens = }")
print(f"{sent_ids[0] = }")


# how to find all deprels in the corpus?
req = Request().pattern("e: X->Y")
all_dels = corpus.count(req, clustering_parameter=["e.label"])
all_feats = corpus.count_feature_values(exclude=["xpos","lemma","form","wordform",
                                                 "textform","SpaceAfter", "Gloss"])
pos = all_feats.pop("upos")
feats = all_feats

for deprel in tqdm(all_dels):
    temp_req = Request().pattern(f"X-[{deprel}]->Y")
    X_pos = corpus.count(temp_req, clustering_parameter=["X.upos"])
    Y_pos = corpus.count(temp_req, clustering_parameter=["Y.upos"])

    wordOrderPatterns = form_pattern(pattern="X-[deprel]->Y",
                            deprel=deprel,
                            binary=True)

    wordOrderData = search_patterns(corpus, wordOrderPatterns)
    if not wordOrderData:
        print(f"{deprel} doesn't have a variation in word order")
    else:
        df = pd.concat(wordOrderData, ignore_index=True)
        df.to_csv(f"{lang}_wordOrder.csv")

    # TODO : write a rule out of it

    # dependent marking
    """
    for feat in feats:
        temp_req = Request().pattern(f"X-[{deprel}]->Y")
        featCount = corpus.count(temp_req, clustering_parameter=[f"Y.{feat}"])
        if len(featCount) > 1:
            depMarkingPattern = form_pattern(pattern="X-[deprel]->Y",
                                         deprel=deprel,
                                         feat=feat,
                                         binary=False)
            featData = search_patterns(corpus, wordOrderPatterns, feat=feat)
            # TODO: check that case marking is working as needed
            df = pd.concat(featData, ignore_index=True)
            df.to_csv(f"{lang}_dep_{feat}.csv")

    # head marking
    for feat in feats:
        temp_req = Request().pattern(f"X-[{deprel}]->Y")
        featCount = corpus.count(temp_req, clustering_parameter=[f"X.{feat}"])
        if len(featCount) > 1:
            depMarkingPattern = form_pattern(pattern="X-[deprel]->Y",
                                         deprel=deprel,
                                         feat=feat,
                                         binary=False)
            featData = search_patterns(corpus, wordOrderPatterns, feat=feat)
            # TODO: check that case marking is working as needed
            df = pd.concat(featData, ignore_index=True)
            df.to_csv(f"{lang}_head_{feat}.csv")
    """
    # agreement
    for feat in feats:
        if "__" not in feat:
            agrPatterns = form_pattern(pattern="X -[deprel]-> Y; X.[feat] [rel] Y.[feat]",
                                    deprel=deprel,
                                    feat=feat,
                                    binary=True)
            featData = search_patterns(corpus, wordOrderPatterns, feat=feat)
            if not featData:
                print(f"No agreement for {deprel} and {feat}")
            else:
                df = pd.concat(featData, ignore_index=True)
                df.to_csv(f"{lang}_agr_{feat}.csv")