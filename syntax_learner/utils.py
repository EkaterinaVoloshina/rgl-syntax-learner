import re
from collections import defaultdict

lang2gf = {"macedonian": "Mkd",
           "albanian": "Sqa",
           "kazakh" : "Kaz"}


def getParams(lang):
    lang = lang.lower()
    if lang in lang2gf:
        filepath = f"../gf-rgl/src/{lang}/Res{lang2gf[lang]}.gf"
    else:
        print(f"{lang} is not supported in GF")
        return None

    with open(filepath, "r") as f:
        text = f.read().splitlines()

    params = [t for t in text if t.startswith("param")]

    paramDict = defaultdict()
    for p in params:
        par, vals = re.findall(r"param (.+) = (.*) ;", p)[0]
        vals = [v.split()[0] for v in vals.split(" | ")]
        paramDict[par] = vals
    return paramDict
