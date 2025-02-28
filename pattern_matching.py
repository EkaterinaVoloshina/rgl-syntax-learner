import grewpy
from grewpy import Corpus, Request
import pandas as pd
from typing import Dict, List
from tqdm.auto import tqdm

grewpy.set_config("sud")
exclude = ["textform", "wordform", "form", "SpaceAfter"]


def form_data(corpus: Corpus,
              ids: dict,
              nodes: list = ["head", "dep"],
              exclude: list = exclude) -> pd.DataFrame:
    data = []
    for val, idx in ids.items():
        for ids in idx:
            sent_data = {}
            sentence = corpus.get(ids["sent_id"])
            match = idx["matching"]["nodes"]
            for node in nodes:
                feats = {f"{k}_{node}": v for k, v in sentence[match[node]].items()
                         if k not in exclude}
                sent_data.update(feats)
                sent_data["target"] = val
                sent_data["idx"] = idx["sent_id"]
            data.append(sent_data)
    return pd.DataFrame(data)

def extract(treebank_path):
    corpus = Corpus(treebank_path)
    # how to find all deprels in the corpus?
    req = Request().pattern("e: head->dep")
    all_dels = corpus.count(req, clustering_parameter=["e.label"])
    all_feats = corpus.count_feature_values(exclude=["xpos","lemma","form","wordform",
                                                     "textform","SpaceAfter", "Gloss"])
    pos = all_feats.pop("upos")
    feats = all_feats

    for deprel in tqdm(all_dels):
        pattern = Request().pattern(f"head-[{deprel}]->dep")

        wordOrderData = corpus.search(pattern, clustering_parameter=["{ head << dep}"])
        if len(wordOrderData) == 1:
            print(f"{deprel} doesn't have a variation in word order")
        else:
            df = form_data(corpus, wordOrderData)
            df.to_csv(f"{lang}_{deprel}_wordOrder.csv")

        # TODO : write a rule out of it

        # dependent marking
        for feat in feats:
            excludeList = exclude + [feat]
            featDepData = corpus.search(pattern, clustering_parameter=[f"dep.{feat}"])
            featHeadData = corpus.search(pattern, clustering_parameter=[f"head.{feat}"])

            if len(featHeadData) > 1:
                df = form_data(corpus, featHeadData,
                               exclude=excludeList)
                df.to_csv(f"{lang}_head_{feat}.csv")

            if len(featDepData) > 1:
                df = form_data(corpus, featDepData,
                               exclude=excludeList)
                df.to_csv(f"{lang}_dep_{feat}.csv")

        # agreement
            if "__" not in feat: # these mark head marking or something similar
                featData = corpus.search(pattern, clustering_parameter=[f"{{ head.{feat} <> dep.{feat} }}"])
                if not featData:
                    print(f"No agreement for {deprel} and {feat}")
                else:
                    df = form_data(corpus, featData, excludeList)
                    df.to_csv(f"{lang}_agr_{feat}.csv")



# idea: store patterns as json file with rule name as key and pattern as value
# we need to a tree how to progress to find all rules

# question 1: what to do with Number[abs] etc?
# question 2: how to make it to grandparents etc?
# question 3: how to add wordnet integration here?
# note 1: keep extracted rules in json file

lang = "ru"
treebank_path = "SUD_Russian-SynTagRus/ru_syntagrus-sud-test.conllu"
#treebank_path = "abq_atb-sud-test.conllu"
extract(treebank_path)