import json
from collections import defaultdict

lang2gf = {"Macedonian": "Mkd",
           "Albanian": "Sqi",
           "Kazakh" : "Kaz",
           "Russian": "Rus"}

features = ["PronType", "Gender", "VerbForm", "NumType", "Animacy", "Mood", 
            "Poss", "NounClass", "Tense", "Reflex", "Number", "Aspect",
            "Case", "Voice", "Abbr", "Definite", "Evident", "Deixis", "Polarity",
            "Ref", "Person", "ExtPos", "Degree", "Polite", "Clusivity"]


def getParams(lang):
    lang = lang2gf[lang]
    with open(f"data/gf/All{lang}Abs.json") as f:
        data = json.load(f)
    langData = data[f"All{lang}"]
    params = {}
    for fun, val in langData["jments"].items():
        if "params" in val:
            vals = [x["id"] for x in val["params"]]
            params[fun] = vals
    return params
