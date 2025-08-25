
from collections import defaultdict
import pandas as pd

def get_dep2fun():
    df = pd.read_csv("dep2fun.csv")
    df = df.fillna("-")
    dep2fun = defaultdict(list)
    for _, x in df.iterrows():

        args = []
        dep = x["dependency"] if x["dependency"] != "-" else None
        dep2fun[dep].append({
            "function": x["function"],
            "dpos": x["dpos"] if x["dpos"] != "-" else None, 
            "hpos": x["hpos"] if x["hpos"] != "-" else None,
            "args": [x.strip() for x in x["args"].split(",")]
        })
    return dep2fun

def filter_by_pos(fun_pos, rule_pos):
    if fun_pos:
        if isinstance(rule_pos, str):
            if fun_pos == rule_pos or (fun_pos == "NOUN" and rule_pos == "PRON"):
                return True
            else:
                return False
        elif isinstance(rule_pos, list):
            for rule in rule_pos:
                if rule[1:] == fun_pos:
                    return False
    return True

def process_function(function, dep, rule_mark, rule_class, hpos, dpos, dfeats, hfeats):
    rs = []
    if rule_mark == "wordOrder":
        if len(function["args"]) > 1:
            if rule_class == "Yes":
                rs = ["head", dep]
            else:
                rs = [dep, "head"]
    elif rule_mark.startswith("dep"):
        _, feat = rule_mark.split("_")
        rs = {dep: {feat: rule_class}}
    elif rule_mark.startswith("head"):
        _, feat = rule_mark.split("_")
        rs = {"head": {feat: rule_class}}
    elif rule_mark.startswith("agr"):
        _, feat = rule_mark.split("_")
        if rule_class == "Yes":
            rs = {"agr": feat}
    elif rule_mark == "linearOrder":
        pass
    else: 
        raise ValueError("Unknown feature")
    
    if rs: 
        return {
            "left-side": [function["function"],] + function["args"], 
            "right-side": rs, 
            "conditions": {
                    "hpos": hpos,
                    "dpos": dpos,
                    "dfeats": dfeats,
                    "hfeats": hfeats
                    }   
            }


def process_subrules(rule):
    deprel, not_deprel = [], []
    head_pos, not_head_pos = [], []
    dep_pos, not_dep_pos = [], []
    head_feats = defaultdict(list)
    dep_feats = defaultdict(list)
    rule_class = None
    
    for subrule in rule:
        if "deprel" in subrule:
            if "!" in subrule[-1]:
                not_deprel.append(subrule[-1])
            else:
                deprel.append(subrule[-1])  
                    
        elif subrule[0] == "dep":
            if "pos" in subrule:
                if "!" in subrule[-1]:
                    not_dep_pos.append(subrule[-1])
                else:
                    dep_pos.append(subrule[-1])
            elif "feats" in subrule:
                    dep_feats[subrule[-2]].append(subrule[-1])
                        
        elif subrule[0] == "head":
            if "pos" in subrule:
                if "!" in subrule[-1]:
                    not_head_pos.append(subrule[-1])
                else:
                    head_pos.append(subrule[-1])
            elif "feats" in subrule:
                head_feats[subrule[-2]].append(subrule[-1])
        elif "rule" in subrule:
                rule_class = subrule[-1]
            
                    
    if deprel:
        dep = deprel[0]
    else:
        dep = not_deprel
    
    if head_pos:
        hpos = head_pos[0]
    else:
        hpos = not_head_pos
    
    if dep_pos:
        dpos = dep_pos[0]
    else:
        dpos = not_dep_pos

    dfeats = {}
    if dep_feats:
        for feat, values in dep_feats.items():
            exist = [x for x in values if not x.startswith("!")]
            if exist:
                dfeats[feat] = exist
            else: 
                dfeats[feat] = values
    
    hfeats = {}
    if head_feats:
        for feat, values in head_feats.items():
            exist = [x for x in values if not x.startswith("!")]
            if exist:
                hfeats[feat] = exist
            else: 
                hfeats[feat] = values

    return dep, hpos, dpos, hfeats, dfeats
    

def get_rules(rules, rule_mark):
    grammar_rules = []
    dep2fun = get_dep2fun()

    for rule in rules:
        
        dep, hpos, dpos, hfeats, dfeats,  = process_subrules(rule)
                    
        if isinstance(dep, str) and dep in dep2fun:
            functions = dep2fun[dep]
            for function in functions:
                if filter_by_pos(function["dpos"], dpos) and filter_by_pos(function["hpos"], hpos):
                    gr_rule = process_function(function, dep, rule_mark, rule_class, hpos, dpos, dfeats, hfeats)
                    if gr_rule:
                        grammar_rules.append(gr_rule)
                           
        elif isinstance(dep, list):
            dep = [i[1:] for i in dep]
            for fun, val in dep2fun.items(): 
                if fun not in dep:
                    for function in val:
                        if filter_by_pos(function["dpos"], dpos) and filter_by_pos(function["hpos"], hpos):
                            gr_rule = process_function(function, dep, rule_mark, rule_class, hpos, dpos, dfeats, hfeats)
                            if gr_rule:
                                grammar_rules.append(gr_rule)      
        
    return grammar_rules