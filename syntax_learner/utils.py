import json
from collections import defaultdict

lang2gf = {"Macedonian": "Mkd",
           "Albanian": "Sqi",
           "Kazakh" : "Kaz",
           "Russian": "Rus"}

ud2gfPOS = {"NOUN": "N",
            "VERB": "V",
            "AUX": "V",
            "ADJ": "A",
            "PROPN": "PN",
            "PRON": "Pron",
            "ADV": "Adv",
            "DET": "Det",
            "SCONJ": "Complementizer",
            "CCONJ": "Conj",
            "INTJ": "Interj",
            "ADP": "Prep",
            "NUM": "Digit",
            "PART": "Particle",
}


features = ["PronType", "Gender", "VerbForm", "NumType", "Animacy", "Mood", 
            "Poss", "NounClass", "Tense", "Reflex", "Number", "Aspect",
            "Case", "Voice", "Definite", "Evident", "Deixis", "Polarity",
            "Ref", "Person", "ExtPos", "Degree", "Polite", "Clusivity", "Variant"]

def get_inherent_params(d):
    params = []
    for k, v in d.items():
        if k != "s" and "con" in v:
            if v["con"] == "Bool":
                params.append(k)
            else:
                params.append(v["con"])
    return params

def getParams(lang):
    lang = lang2gf[lang]
    with open(f"data/gf/All{lang}Abs.json") as f:
        data = json.load(f)
    langData = data[f"All{lang}"]
    
    params = {}
    inh_params = {}
    for fun, val in langData["jments"].items():
        if "lintype" in val:
            if rec := val["lintype"].get("rectype", None):
                inh_params[fun] = get_inherent_params(rec)
        if "params" in val:
            vals = [x["id"] for x in val["params"]]
            params[fun] = vals
    return inh_params, params

