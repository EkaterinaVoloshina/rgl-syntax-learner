import glob

import grewpy
from grewpy import Corpus, Request
import pandas as pd
from typing import Dict, List
from tqdm.auto import tqdm
import json
import pickle
import glob

grewpy.set_config("sud")
exclude = ["textform", "wordform", "form", "SpaceAfter", "Translit", "LTranslit"]

def toJSON(pattern: str,
           deprel: str,
           rule: str
           ) -> Dict[str, str]:
    rule = {
        "pattern": pattern,
        "rule": rule,
        "deprel": deprel,
        "head": {
            "pos": None,
            "feats": None,
            "lemma": None,
            "sem": None
        },
        "dep": {
            "pos": None,
            "feats": None,
            "lemma": None,
            "sem": None
        },
    }
    return rule


def form_data(corpus: Corpus,
              ids: dict,
              exclude : list =["textform", "wordform", "form", "SpaceAfter"]) -> List[dict]:
    if exclude is None:
        exclude = exclude
    data = []
    for val, idx in ids.items():
        for i in idx:
            sent_data = {}
            sentence = corpus.get(i["sent_id"])
            match = i["matching"]["nodes"]
            for node, position in match.items():
                feats = {f"{k}_{node}": v for k, v in sentence[position].items()
                         if k not in exclude}
                sent_data.update(feats)
                sent_data["target"] = val
                sent_data["idx"] = i["sent_id"]
                if "position" not in exclude: # to relative order
                    sent_data[f"position_{node}"] = position
            data.append(sent_data)
    return data

def extract(treebank_path: str, lang: str):
    corpus = Corpus(treebank_path)
    req = Request().pattern("e: head->dep")
    all_dels = corpus.count(req, clustering_parameter=["e.label"])
    all_feats = corpus.count_feature_values(exclude=["xpos","lemma","form","wordform",
                                                     "textform","SpaceAfter", "Gloss"])
    pos = all_feats.pop("upos")
    feats = all_feats
    rules = []
    # TODO: check linear order
    datasets = {}

    for deprel in tqdm(all_dels):
        pattern = Request().pattern(f"head-[{deprel}]->dep")
        param = "{ head << dep}"
        wordOrderData = corpus.search(pattern, clustering_parameter=[param,])
        if len(wordOrderData) == 1:
            rules.append(toJSON(param,
                                deprel,
                                next(iter(wordOrderData))))
        else:
            datasets[f"{lang}_{deprel}_wordOrder"] = form_data(corpus, wordOrderData,
                                                               exclude=exclude + ["position"])

        # dependent marking
        for feat in feats:
            excludeList = exclude + [feat]
            depParam = f"dep.{feat}"
            headParam = f"head.{feat}"
            featDepData = corpus.search(pattern, clustering_parameter=[depParam,])
            featHeadData = corpus.search(pattern, clustering_parameter=[headParam,])

            if len(featHeadData) > 1:
                datasets[f"{lang}_{deprel}_head_{feat}"] = form_data(corpus, featHeadData,
                               exclude=excludeList)
            elif next(iter(featHeadData)) != "__undefined__":
                rules.append(toJSON(headParam, deprel, next(iter(featHeadData))))

            if len(featDepData) > 1:
                datasets[f"{lang}_{deprel}_dep_{feat}"] = form_data(corpus, featDepData,
                               exclude=excludeList)
            elif next(iter(featDepData)) != "__undefined__":
                rules.append(toJSON(depParam, deprel, next(iter(featDepData))))
        # agreement
            if "__" not in feat: # these mark head marking or something similar
                featData = corpus.search(pattern, clustering_parameter=[f"{{ head.{feat} <> dep.{feat} }}"])
                if not featData:
                    print(f"No agreement for {deprel} and {feat}")
                else:
                    datasets[f"{lang}_{deprel}_agr_{feat}"] = form_data(corpus, featData, excludeList)
    return rules, datasets

def parse(treebank):
    lang = treebank.split("_")[1]
    treebank_path = glob.glob(f"{treebank}/*.conllu")[0]
    rules, datasets = extract(treebank_path, lang)

    with open(f"{treebank}_rules.json", "w") as f:
        f.write(json.dumps(rules))

    with open(f"{treebank}_datasets.pkl", "wb") as f:
        pickle.dump(datasets, f)