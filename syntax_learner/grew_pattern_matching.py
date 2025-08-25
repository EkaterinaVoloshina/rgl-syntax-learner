import grewpy
from grewpy import Corpus, Request

import pickle

grewpy.set_config("sud")
exclude = ["textform", "wordform", "form", "SpaceAfter"]

def form_data(corpus: Corpus,
              ids: dict,
              exclude : list =["textform", "wordform", "form", "SpaceAfter"],
              deep : bool = False) -> List[dict]:
    data = []
    for val, deps in ids.items():
        for deprel, idx in deps.items():
            deprel = eval(deprel)
            for i in idx:
                sent_data = {}
                sentence = corpus.get(i["sent_id"])
                match = i["matching"]["nodes"]
                for node, position in match.items():
                    if node in ["head", "dep"]:
                        feats = {f"{k}_{node}": v for k, v in sentence[position].items()
                                 if k not in exclude}
                        sent_data.update(feats)
                sent_data["target"] = val
                sent_data["idx"] = i["sent_id"]
                if "position" not in exclude: # to relative order
                    sent_data["position"] = 1 if match["head"] > match["dep"] else 0
                if isinstance(deprel, str):
                    sent_data["deprel"] = deprel
                else:
                    if "2" in deprel:
                        base = f"{deprel["1"]}:{deprel["2"]}"
                    else:
                        base = deprel["1"]
                    if deep:
                        sent_data["deprel"] = base + "@" + deprel["deep"]
                    sent_data["deprel"] = base
            data.append(sent_data)
    return data

def parse(treebank_path, pattern, parameter, name):
    corpus = Corpus(treebank_path)
    rel = corpus.search(Request().pattern(pattern), clustering_parameter=parameter)
    dataset = form_data(corpus, rel, exclude=exclude)
    with open(f"{treebank_path}_{name}.pkl", "wb") as f:
        pickle.dump(dataset, f)