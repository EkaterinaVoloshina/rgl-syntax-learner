from collections import defaultdict
from typing import Dict, List
from tqdm.auto import tqdm
import json
import pickle
import glob
import conllu as cn
from syntax_learner.utils import getParams

exclude_feat = ["Gloss", "Translit", "LTranslit"]

def toJSON(pattern: str,
           deprel: str,
           rule: str
           ) -> Dict[str, str]:
    rule = {
        "pattern": pattern,
        "rule": rule,
        "deprel": None,
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


def process(deprel, dep, head, headId, depId, deep=False):
    sentData = {}

    sentData["position"] = headId < depId
    depFeats = {f"{k}_dep": v for k, v in dep["feats"].items()} if dep["feats"] else {}
    headFeats = {f"{k}_head": v for k, v in head["feats"].items()} if head["feats"] else {}
    sentData.update(depFeats)
    sentData.update(headFeats)
    if deep:
        sentData["deprel"] = deprel.split("@")[0]
    else:
        sentData["deprel"] = deprel
    return sentData


def extract(treebank_path: str, lang: str, deep: bool = False):
    params = getParams(lang.split("-")[0])
    with open(treebank_path, "r") as f:
        data = f.read()
    sentences = cn.parse(data)
    dataDict = defaultdict(list)
    for sentence in tqdm(sentences):
        groups = defaultdict(list)
        for token in sentence:
            deprel = token["deprel"]
            head = token["head"]
            # WORD ORDER
            if isinstance(token["id"], int) and deprel != "root" and deprel != "punct":
                headData = sentence.filter(id=head)[0]
                output = process(deprel, token, headData, head, token["id"])
                groups[head].append((output, token["id"]))
                if head < token["id"]:
                    output["target"] = "Yes"
                    dataDict["wordOrder"].append(output)
                else:
                    output["target"] = "No"
                    dataDict["wordOrder"].append(output)
                    # MATCHING

                if token["feats"]:
                    for feat, val in token["feats"].items():
                        if not params or feat in params:
                            if headData["feats"] and feat in headData["feats"]:
                                if val == headData["feats"][feat]:
                                    output["target"] = "Yes"
                                    dataDict[f"agr_{feat}"].append(output)
                                else:
                                    output["target"] = "No"
                                    dataDict[f"agr_{feat}"].append(output)
                        # FEATURE MARKING
                            output["target"] = val
                            dataDict[f"dep_{feat}"].append(output)
                if headData["feats"]:
                    for feat, val in headData["feats"].items():
                        if not params or feat in params:
                            output["target"] = val
                            dataDict[f"dep_{feat}"].append(output)

        # LINEAR RULES
        for head, group in groups.items():
            position = 0
            for num, (token, idx) in enumerate(group):
                if num == position and head > idx:
                    position += 1
                token["target"] = position
                dataDict["linearOrder"].append(token)
                position += 1

    return dataDict

def parse(treebank, deep=False):
    lang = treebank.split("_")[1]
    treebanks = glob.glob(f"{treebank}/*.conllu")
    for treebank_path in treebanks:
        subset = treebank_path.rsplit("-")[-1].replace(".conllu", "")
        datasets = extract(treebank_path, lang, deep)
        #with open(f"data/{treebank}_{subset}_rules.json", "w") as f:
        #    f.write(json.dumps(rules))

        with open(f"data/{treebank}_{subset}_datasets.pkl", "wb") as f:
            pickle.dump(datasets, f)