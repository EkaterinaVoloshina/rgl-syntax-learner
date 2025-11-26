import json
from collections import defaultdict

boilerplate = defaultdict()

lang2gf = {"Macedonian": "Mkd",
           "Albanian": "Sqi",
           "Kazakh" : "Kaz",
           "Scottish_Gaelic": "Gla",
           "Russian": "Rus"}

pos2gf = {"Noun": "N",
          "Verb": "V",
          "Adj": "A",
          "Compl": "Complementizer"}

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

fun2loc = {'PositA': 'Adjective',
    'UseN': 'Noun',
    'AdjCN': 'Noun',
    'AdAP': 'Adjective',
    'AdAdv': 'Adverb',
    'AdNum': 'Noun',
    'AdVVP': 'Verb',
    'AdVVPSlash': 'Verb',
    'AddAdvQVP': 'Question',
    'AdjDAP': 'Noun',
    'AdjOrd': 'Adjective',
    'AdnCAdv': 'Adverb',
    'AdvAP': 'Adjective',
    'AdvCN': 'Noun',
    'AdvIAdv': 'Question',
    'AdvIP': 'Question',
    'AdvNP': 'Noun',
    'AdvQVP': 'Question',
    'AdvS': 'Sentence',
    'AdvSlash': 'Sentence',
    'AdvVP': 'Verb',
    'AdvVPSlash': 'Verb',
    'ApposCN': 'Noun',
    'BaseAP': 'Conjunction',
    'BaseAdV': 'Conjunction',
    'BaseAdv': 'Conjunction',
    'BaseCN': 'Conjunction',
    'BaseDAP': 'Conjunction',
    'BaseIAdv': 'Conjunction',
    'BaseNP': 'Conjunction',
    'BaseRS': 'Conjunction',
    'BaseS': 'Conjunction',
    'CAdvAP': 'Adjective',
    'CleftAdv': 'Idiom',
    'CleftNP': 'Idiom',
    'CompAP': 'Verb',
    'CompAdv': 'Verb',
    'CompCN': 'Verb',
    'CompIAdv': 'Question',
    'CompIP': 'Question',
    'CompNP': 'Verb',
    'ComparA': 'Adjective',
    'ComparAdvAdj': 'Adverb',
    'ComparAdvAdjS': 'Adverb',
    'ComplA2': 'Adjective',
    'ComplN2': 'Noun',
    'ComplN3': 'Noun',
    'ComplSlash': 'Verb',
    'ComplSlashIP': 'Question',
    'ComplVA': 'Verb',
    'ComplVQ': 'Verb',
    'ComplVS': 'Verb',
    'ComplVV': 'Verb',
    'ConjAP': 'Conjunction',
    'ConjAdV': 'Conjunction',
    'ConjAdv': 'Conjunction',
    'ConjCN': 'Conjunction',
    'ConjDet': 'Conjunction',
    'ConjIAdv': 'Conjunction',
    'ConjNP': 'Conjunction',
    'ConjRS': 'Conjunction',
    'ConjS': 'Conjunction',
    'ConsAP': 'Conjunction',
    'ConsAdV': 'Conjunction',
    'ConsAdv': 'Conjunction',
    'ConsCN': 'Conjunction',
    'ConsDAP': 'Conjunction',
    'ConsIAdv': 'Conjunction',
    'ConsNP': 'Conjunction',
    'ConsRS': 'Conjunction',
    'ConsS': 'Conjunction',
    'CountNP': 'Noun',
    'DetCN': 'Noun',
    'DetDAP': 'Noun',
    'DetNP': 'Noun',
    'DetQuant': 'Noun',
    'DetQuantOrd': 'Noun',
    'EmbedQS': 'Sentence',
    'EmbedS': 'Sentence',
    'EmbedVP': 'Sentence',
    'ExistIP': 'Idiom',
    'ExistIPAdv': 'Idiom',
    'ExistNP': 'Idiom',
    'ExistNPAdv': 'Idiom',
    'ExtAdvNP': 'Noun',
    'ExtAdvS': 'Sentence',
    'ExtAdvVP': 'Verb',
    'FunRP': 'Relative',
    'GenericCl': 'Idiom',
    'IDig': 'Numeral',
    'IIDig': 'Numeral',
    'IdetCN': 'Question',
    'IdetIP': 'Question',
    'IdetQuant': 'Question',
    'ImpP3': 'Idiom',
    'ImpPl1': 'Idiom',
    'ImpVP': 'Sentence',
    'ImpersCl': 'Idiom',
    'MassNP': 'Noun',
    'NumCard': 'Noun',
    'NumDigits': 'Noun',
    'NumNumeral': 'Noun',
    'OrdDigits': 'Noun',
    'OrdNumeral': 'Noun',
    'OrdNumeralSuperl': 'Noun',
    'OrdSuperl': 'Noun',
    'PConjConj': 'Phrase',
    'PPartNP': 'Noun',
    'PartNP': 'Noun',
    'PhrUtt': 'Phrase',
    'PositAdAAdj': 'Adverb',
    'PositAdvAdj': 'Adverb',
    'PossNP': 'Noun',
    'PossPron': 'Noun',
    'PredSCVP': 'Sentence',
    'PredVP': 'Sentence',
    'PredetNP': 'Noun',
    'PrepIP': 'Question',
    'PrepNP': 'Adverb',
    'ProgrVP': 'Idiom',
    'QuestCl': 'Question',
    'QuestIAdv': 'Question',
    'QuestIComp': 'Question',
    'QuestQVP': 'Question',
    'QuestSlash': 'Question',
    'QuestVP': 'Question',
    'ReflA2': 'Adjective',
    'RelCN': 'Noun',
    'RelCl': 'Relative',
    'RelNP': 'Noun',
    'RelS': 'Sentence',
    'RelSlash': 'Relative',
    'RelVP': 'Relative',
    'SSubjS': 'Sentence',
    'SelfAdVVP': 'Idiom',
    'SelfAdvVP': 'Idiom',
    'SelfNP': 'Idiom',
    'SentAP': 'Adjective',
    'SentCN': 'Noun',
    'Slash2V3': 'Verb',
    'SlashPrep': 'Sentence',
    'SlashV2A': 'Verb',
    'SlashV2Q': 'Verb',
    'SlashV2S': 'Verb',
    'SlashV2V': 'Verb',
    'SlashV2VNP': 'Verb',
    'SlashV2a': 'Verb',
    'SlashVP': 'Sentence',
    'SlashVS': 'Sentence',
    'SlashVV': 'Verb',
    'SubjS': 'Adverb',
    'TTAnt': 'Tense',
    'Use2N3': 'Noun',
    'Use3N3': 'Noun',
    'UseA2': 'Adjective',
    'UseCl': 'Sentence',
    'UseComp': 'Verb',
    'UseComparA': 'Adjective',
    'UsePN': 'Noun',
    'UsePron': 'Noun',
    'UseQCl': 'Sentence',
    'UseRCl': 'Sentence',
    'UseSlash': 'Sentence',
    'UseV': 'Verb',
    'UttAP': 'Phrase',
    'UttAdv': 'Phrase',
    'UttCN': 'Phrase',
    'UttCard': 'Phrase',
    'UttIAdv': 'Phrase',
    'UttIP': 'Phrase',
    'UttImpPl': 'Phrase',
    'UttImpPol': 'Phrase',
    'UttImpSg': 'Phrase',
    'UttInterj': 'Phrase',
    'UttNP': 'Phrase',
    'UttQS': 'Phrase',
    'UttS': 'Phrase',
    'UttVP': 'Phrase',
    'VPSlashPrep': 'Verb',
    'VocNP': 'Phrase'}


features = ["PronType", "Gender", "VerbForm", "NumType", "Animacy", "Mood", 
            "Poss", "NounClass", "Tense", "Reflex", "Number", "Aspect",
            "Case", "Voice", "Definite", "Evident", "Deixis", "Polarity",
            "Ref", "Person", "ExtPos", "Degree", "Polite", "Clusivity", "Variant"]

def get_inherent_params(d):
    params = []
    for k, v in d.items():
        if k != "s" and "qc" in v:
            if v["qc"] == "Bool":
                params.append(k)
            else:
                params.append(v["qc"])
    return params
    
def getParams(lang):
    lang = lang2gf[lang]
    with open(f"data/gf/Res{lang}.json") as f:
        data = json.load(f)
    langData = data[f"Res{lang}"]
    
    params = {}
    inh_params = {}
    for fun, val in langData["jments"].items():
        if "operdef" in val:
            if rec := val["operdef"].get("rectype", None):
                inh_params[fun] = get_inherent_params(rec)
        if "params" in val:
            vals = [x["id"] for x in val["params"]]
            params[fun] = vals
    return inh_params, params


def get_order(val):
    order = []
    if "tblhypo" in val:
        order.append(val["tblhypo"]["qc"])
        order.extend(get_order(val["tblres"]))
    return order


def get_record(lang, params):
    lang = lang2gf[lang]
    with open(f"data/gf/Res{lang}.json") as f:
        data = json.load(f)
    params_values = {v1:k for k,v in params.items() for v1 in v}
    pos2params = defaultdict(list)
    jments = data[f"Res{lang}"]["jments"]
    for pos,v in jments.items():
        if "sort" in v.get("opertype", {}) and v["opertype"]["sort"] == "Type":
            inhParams2name = {}
            rest = []
            for i, val in jments[pos]["operdef"]["rectype"].items():
                order = []
                if "qc" in val and val["qc"] != "Bool":
                    inhParams2name[val["qc"]] = i
                elif i == "s":
                    order = get_order(val)
                elif i in params_values:
                    print(val)
                else:
                    rest.append(i)
            x = pos2gf.get(pos, pos)
            pos2params[x] = (inhParams2name, order, rest)
    return pos2params
