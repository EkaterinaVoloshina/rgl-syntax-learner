
from collections import defaultdict
import pandas as pd
from tqdm.auto import tqdm

def get_dep2fun():
    df = pd.read_csv("syntax_learner/dep2fun.csv")
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
        raise ValueError(f"Unknown feature: {rule_mark}")
    
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
                dfeats[feat] = exist[0]
            else: 
                dfeats[feat] = values
    
    hfeats = {}
    if head_feats:
        for feat, values in head_feats.items():
            exist = [x for x in values if not x.startswith("!")]
            if exist:
                hfeats[feat] = exist[0]
            else: 
                hfeats[feat] = values

    return dep, hpos, dpos, hfeats, dfeats, rule_class
    

def get_rules(rules, rule_mark, dep2fun):
    grammar_rules = defaultdict(list)

    for rule in rules:
        
        dep, hpos, dpos, hfeats, dfeats, rule_class  = process_subrules(rule)

                    
        if isinstance(dep, str) and dep in dep2fun:
            functions = dep2fun[dep]
            for function in functions:
                if filter_by_pos(function["dpos"], dpos) and filter_by_pos(function["hpos"], hpos):
                    gr_rule = process_function(function, dep, rule_mark, rule_class, hpos, dpos, dfeats, hfeats)
                    if gr_rule:
                        grammar_rules[function["function"]].append(gr_rule)
                           
        elif isinstance(dep, list):
            dep = [i[1:] for i in dep]
            for fun, val in dep2fun.items(): 
                if fun not in dep:
                    for function in val:
                        if filter_by_pos(function["dpos"], dpos) and filter_by_pos(function["hpos"], hpos):
                            gr_rule = process_function(function, fun, rule_mark, rule_class, hpos, dpos, dfeats, hfeats)
                            if gr_rule:
                                grammar_rules[function["function"]].append(gr_rule)      
        
    return grammar_rules


def compare_cond(val1, val2):
    match = True
    conditions = None
    if isinstance(val1, str): # if val1 is a positive value, for example, NOUN
        if "!"+val1 in val2 or (isinstance(val2, str) and val1 != val2):
            match = False
        else:
            conditions = val1
    elif isinstance(val1, list):
        if (isinstance(val2, str) and "!"+val2 in val1):
            match = False
        elif isinstance(val2, str):
            conditions = val2
        else:
            conditions = (list(set(val1 + val2)))
    return match, conditions

def compare_rules(f1, rules_f1, f2, rules_f2):
    new_rules = []
    i = 0
    for r1 in rules_f1:
        for r2 in rules_f2:
            matchFound = True
            conditions = {}
            for cond1, val1 in r1["conditions"].items():
                if val1:
                    if val2 := r2["conditions"].get(cond1):
                        if isinstance(val1, dict):
                            subconds = {}
                            for c1, v1 in val1.items():
                                if v2 := val2.get(c1):
                                        matchFound, cond = compare_cond(v1, v2)
                                        if not matchFound:
                                            break
                                        else:
                                            subconds[c1] = cond
                                else:
                                    subconds[c1] = v1
                            diff = val2.keys() - val1.keys()
                            for d in diff:
                                subconds[d] = val2[d]
                                
                            if not matchFound:
                                break
                            else:
                                conditions[cond1] = subconds
                        else:
                            matchFound, cond = compare_cond(val1, val2)
                            if not matchFound:
                                break
                            else:
                                conditions[cond1] = cond
                    else:
                        conditions[cond1] = val1
                else:
                    conditions[cond1] = r2["conditions"][cond1]
    
            if matchFound:
                new_rule = {}
                new_rule["left-side"] = r1["left-side"]
                if f1:
                    new_rule["right-side"] = {f1: r1["right-side"],
                                          f2: r2["right-side"]}
                else:
                    new_rule["right-side"] = r1["right-side"]
                    new_rule["right-side"][f2] = r2["right-side"]
                new_rule["conditions"] = conditions
                i += 1
                new_rules.append(new_rule)
            if f2:
                new_rule_2 = {}
                new_rule_2["left-side"] = r2["left-side"]
                new_rule_2["right-side"] = {f2: r2["right-side"]}
                new_rule_2["conditions"] = r2["conditions"]
            else:
                new_rules.append(r2)
            
    
        if f1:
            new_rule_1 = {}
            new_rule_1["left-side"] = r1["left-side"]
            new_rule_1["right-side"] = {f1: r1["right-side"]}
            new_rule_1["conditions"] = r1["conditions"]
        else:
            new_rules.append(r1)
    return new_rules


def get_matches(dep2fun, rules, data):
    results = defaultdict(dict)
    for fun, val in tqdm(dep2fun.items()): 
        results[fun] = {}
        for feat_name, subrules in rules[fun].items(): 
            if fun in feat_name:
                all_matches = []
                correct = defaultdict(list)
                covered = defaultdict(list)
                for sent in data[feat_name]:
                    feat = feat_name.split("_", maxsplit=1)[-1]
                    if val["deprel"] == sent["deprel"]:
                        if (not val["hpos"] or val["hpos"] == sent["pos_head"] or "!" + sent["pos_head"] not in val["hpos"]) and (not val["dpos"] or val["dpos"] == sent["pos_dep"] or "!" + sent["pos_dep"] not in val["dpos"]):
                            all_matches.append(sent["sent_id"])
                            for num, subrule in enumerate(subrules):
                                mismatch = False
                                if not val["hpos"]:
                                    if subrule["conditions"]["hpos"]:
                                        if isinstance(subrule["conditions"]["hpos"], str) and sent["pos_head"] != subrule["conditions"]["hpos"]:
                                            mismatch = True 
                                            break
                                        elif isinstance(subrule["conditions"]["hpos"], list) and "!" + sent["pos_head"] in subrule["conditions"]["hpos"]:
                                            mismatch = True 
                                            break
                                    if subrule["conditions"]["dpos"]:
                                        if isinstance(subrule["conditions"]["dpos"], str) and sent["pos_dep"] != subrule["conditions"]["dpos"]:
                                            mismatch = True 
                                            break
                                        elif isinstance(subrule["conditions"]["dpos"], list) and "!" + sent["pos_dep"] in subrule["conditions"]["dpos"]:
                                            mismatch = True 
                                            break
                                    
                                if not mismatch: 
                                    for f_name, f_val in subrule["conditions"]["hfeats"].items():
                                        if f"{f_name}_head" in sent:
                                            if isinstance(f_val, str) and sent[f"{f_name}_head"] != f_val:
                                                mismatch = True
                                                
                                                break 
                                            elif isinstance(f_val, list) and "!" + sent[f"{f_name}_head"] in f_val:
                                                mismatch = True
                                                
                                                break
                                if not mismatch: 
                                    for f_name, f_val in subrule["conditions"]["dfeats"].items():
                                        if f"{f_name}_dep" in sent:
                                            if isinstance(f_val, str) and sent[f"{f_name}_dep"] != f_val:
                                                mismatch = True
                                                
                                                break 
                                            elif isinstance(f_val, list) and "!" + sent[f"{f_name}_dep"] in f_val:
                                                mismatch = True
                                                
                                                break
        

                                if not mismatch:
                                    covered[num].append(sent["sent_id"])
                                    if feat == "wordOrder":
                                        if sent["target"] == "No" and subrule["right-side"].index("head") == 1:
                                            correct[num].append(sent["sent_id"])
                                        elif sent["target"] == "Yes" and subrule["right-side"].index("head") == 0:
                                            correct[num].append(sent["sent_id"])
                                    elif "agr" in feat and sent["target"] == "Yes":
                                        correct[num].append(sent["sent_id"])
                                    elif "dep" in feat:
                                        feature = feat.replace("dep_", "")
                                        _, v = next(iter(subrule["right-side"].items()))
                                        if v[feature] == sent["target"]:
                                            correct[num].append(sent["sent_id"])
                                    elif "head" in feat:
                                        feature = feat.replace("head_", "")
                                        if subrule["right-side"]["head"][feature] == sent["target"]:
                                            correct[num].append(sent["sent_id"])

                    
                    results[fun][feat_name] = {"all_matches": all_matches,
                                    "correct" : correct,
                                    "covered": covered}
    return results

def get_scores(rule_name, rules, matches):
    feats = defaultdict(list)
    for f in rules[rule_name].keys():
        if "_" in f and "wordOrder" not in f:
            feats[f.split("_")[-1]].append(f)


    feat_0 = f"{rule_name}_wordOrder"
    if feat_0 not in matches[rule_name]:
        feat_0 = list(matches[rule_name].keys())[0]
       # print(matches[rule_name].keys())
    ss = set(matches[rule_name][feat_0]["all_matches"])
    correct_0 = {((feat_0, k),):v for k, v in matches[rule_name][feat_0]["correct"].items()}
    covered_0 = {((feat_0, k),):v for k, v in matches[rule_name][feat_0]["covered"].items()}
    subset_0 = {((feat_0, k),): ss for k, v in matches[rule_name][feat_0]["covered"].items()}


    rule_score = {}


    for _, fts in tqdm(feats.items()):
        new_covered = {}
        new_correct = {}
        new_subset = {}
        for feat in fts:
            if feat in matches[rule_name]:
                subset_1  = set(matches[rule_name][feat]["all_matches"])
                if subset_1:
                    correct_1 = {((feat, k),):v for k, v in matches[rule_name][feat]["correct"].items()}
                    covered_1 =  {((feat, k),):v for k, v in matches[rule_name][feat]["covered"].items()}

                    new_covered |= covered_1
                    new_correct |= correct_1

                    for num0, cov0 in covered_0.items():
                        for num1, cov1 in covered_1.items():
                            new_subset[num1] = subset_1
                            inter_set = set(subset_0[num0]).intersection(subset_1)
            
                            if inter_set:
                                cor0 = correct_0.get(num0,[])
                                cor1 = correct_1.get(num1,[])

                                new_cor = set(cor0).intersection(set(cor1))
                                new_cov = set(cov0).intersection(set(cov1))

                                
                                n0 = list(num0)
                                n0.append(num1[0])
                                num = tuple(n0)
                                

                                new_covered[num] = new_cov
                                new_correct[num] = new_cor
                                new_subset[num] = inter_set

                

                                coverage = len(new_cov)/len(inter_set) if inter_set else 0
                                precision = len(new_cor)/len(new_cov) if new_cov else 0

                                fscore = (2 * precision * coverage) / (precision + coverage) if coverage > 0 else 0
                                rule_score[num] = (coverage, precision, fscore)
                    


        correct_0 |= new_correct
        covered_0 |= new_covered
        subset_0 |= new_subset
    d = {"covered": covered_0,
        "correct": correct_0}

    return rule_score, d

def get_rule_description(example_rule, rules):
			rule_description = []
			hpos = []
			dpos = []
			dfeats = {}
			hfeats = {}
			for i, num in example_rule:
				fun, rule = i.split("_", maxsplit=1)
				rule_d = rules[fun][i][num]
				if isinstance(rule_d["conditions"]["hpos"], str):
					hpos.append(rule_d["conditions"]["hpos"])
				else:
					hpos += rule_d["conditions"]["hpos"]
				if isinstance(rule_d["conditions"]["dpos"], str):
					dpos.append(rule_d["conditions"]["dpos"])
				else:
					dpos += rule_d["conditions"]["dpos"]
				dfeats |= rule_d["conditions"]["dfeats"]
				hfeats |= rule_d["conditions"]["hfeats"]
				rule_description.append(rule_d)
			n_rules = len(rule_description)
			n_cond = 0 

			if hpos:
				n_cond += 1
			if dpos:
				n_cond += 1
			n_cond += len(dfeats) + len(hfeats)
			return rule_description, n_rules, n_cond