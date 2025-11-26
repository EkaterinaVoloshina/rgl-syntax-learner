from collections import defaultdict
from typing import Dict, List
from tqdm.auto import tqdm
import json
import pickle
import glob
import conllu as cn
import pandas as pd
import numpy as np
from syntax_learner.utils import getParams, features, ud2gfPOS

exclude_feat = ["Gloss", "Translit", "LTranslit", "Shared"]

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


def process(deprel, dep, head, headId, depId, idx, deep=False):
    sentData = {}

    sentData["position"] = headId < depId
    depFeats = {f"{k}_dep": v for k, v in dep["feats"].items()} if dep["feats"] else {}
    headFeats = {f"{k}_head": v for k, v in head["feats"].items()} if head["feats"] else {}
    sentData.update(depFeats)
    sentData.update(headFeats)
    sentData["pos_dep"] = dep["upos"]
    sentData["pos_head"] = head["upos"]
    if deep:
        sentData["deprel"] = deprel.split("@")[0]
    else:
        sentData["deprel"] = deprel
    sentData["sent_id"] = "-".join([idx, str(headId), str(depId)])
    return sentData


def extract(treebank_path: str, lang: str, deep: bool = False, funs=None):
    inh_params, params = getParams(lang.split("-")[0])
    with open(treebank_path, "r") as f:
        data = f.read()
    sentences = cn.parse(data)
    dataDict = defaultdict(list)
    for sentence in tqdm(sentences):
        idx = sentence.metadata["sent_id"]
        groups = defaultdict(list)
        subj = False
        root = None
        for token in sentence:
            deprel = token["deprel"]
            head = token["head"]
            # WORD ORDER
            if deprel == "root":
                root = {f"{k}_dep": v for k, v in token["feats"].items()} if token["feats"] else {}
            if isinstance(token["id"], int) and deprel != "root" and deprel != "punct":
                headData = sentence.filter(id=head)[0]
                if headData["upos"] in ud2gfPOS and token["upos"] in ud2gfPOS:
                    output = process(deprel, token, headData, head, token["id"], idx)
                    groups[head].append((output, token["id"]))
                    if deprel == "subj":  # omitting subject: possible ??
                        subj = True
                    if head < token["id"]:
                        data = output.copy()
                        data["target"] = "Yes"
                        
                    else:
                        data = output.copy()
                        data["target"] = "No"
                    if funs:
                        for fname, fun in funs.items():
                            if fun["deprel"] and data["deprel"] == fun["deprel"]:
                                if (not fun["hpos"] or fun["hpos"] == data["pos_head"]) and (not fun["dpos"] or fun["dpos"] == data["pos_dep"]):
                                    dataDict[f"{fname}_wordOrder"].append(data)
                    else:            
                        dataDict["wordOrder"].append(data)
                        # MATCHING

                    if token["feats"]:
                        for feat, val in token["feats"].items():
                            if feat in features and headData["feats"] and feat in headData["feats"]: 
                                if val == headData["feats"][feat]:
                                    data = output.copy()
                                    data["target"] = "Yes"
                                else:
                                    data = output.copy()
                                    data["target"] = "No"
                                    
                                if funs:
                                    for fname, fun in funs.items():
                   
                                        if fun["deprel"] and data["deprel"] == fun["deprel"]:
                                            if (not fun["hpos"] or fun["hpos"] == data["pos_head"]) and (not fun["dpos"] or fun["dpos"] == data["pos_dep"]):
                                                dataDict[f"{fname}_agr_{feat}"].append(data)
                                else:
                                    dataDict[f"agr_{feat}"].append(data)

                            
                            # FEATURE MARKING
                            if feat in features and feat not in inh_params.get(ud2gfPOS[token["upos"]], []):
                                data = output.copy()
                                data["target"] = val
                                if funs:
                                    for fname, fun in funs.items():
                   
                                        if fun["deprel"] and data["deprel"] == fun["deprel"]:
                                            if (not fun["hpos"] or fun["hpos"] == data["pos_head"]) and (not fun["dpos"] or fun["dpos"] == data["pos_dep"]):
                                                dataDict[f"{fname}_dep_{feat}"].append(data)
                                else:             
                                    dataDict[f"dep_{feat}"].append(data)
                    if headData["feats"]:
                        
                        for feat, val in headData["feats"].items():

                            if feat in features and feat not in inh_params.get(ud2gfPOS[headData["upos"]], []):
                                data = output.copy()
                                data["target"] = val
                                if funs:
                                    for fname, fun in funs.items():
                   
                                        if fun["deprel"] and data["deprel"] == fun["deprel"]:
                                            if (not fun["hpos"] or fun["hpos"] == data["pos_head"]) and (not fun["dpos"] or fun["dpos"] == data["pos_dep"]):

                                                dataDict[f"{fname}_head_{feat}"].append(data)
                                else:
                                    dataDict[f"head_{feat}"].append(data)

        if subj:
            root["target"] = "Yes"
            dataDict["subj_exists"].append(root)
        else:
            root["target"] = "No"
        
            dataDict["subj_exists"].append(root)

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

def parse(treebank : str, deep : bool = False):
    lang = treebank.split("_",1)[1].split("-")[0]
    treebanks = glob.glob(f"{treebank}/*.conllu")
    fun_df = pd.read_csv("syntax_learner/dep2fun.csv")[["function", "dependency", "dpos", "hpos"]]
    fun_df = fun_df.fillna(np.nan).replace([np.nan], [None])

    funs = {}
    for num, row in fun_df.iterrows():
        if row["dependency"]:
            funs[row["function"]] = {"deprel": row["dependency"],
                                        "dpos": row["dpos"],
                                        "hpos": row["hpos"]}
            

    for treebank_path in treebanks:
        subset = treebank_path.rsplit("-")[-1].replace(".conllu", "")
        datasets = extract(treebank_path, lang, deep, funs)
        #with open(f"data/{treebank}_{subset}_rules.json", "w") as f:
        #    f.write(json.dumps(rules))

        with open(f"data/{treebank}_{subset}_datasets.pkl", "wb") as f:
            pickle.dump(datasets, f)
