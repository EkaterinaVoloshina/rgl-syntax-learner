from syntax_learner.utils import getParams

def generate_negative_rule(rule, dep, head):
    for num, cond in rule:
        if "wordOrder" in num[0]:
            if cond["right-side"].index("mod") == 0:
                return head + " " + dep
            else:
                return dep + " " +  head
            

def generate_rule(rule, params):
    rule_string = ""
    dep = ""
    head = ""
    rule_string += " ".join(rule[0][1]["left-side"]) + " = {\ns = " # definition of the function
    if rule[0][1]["left-side"].index("head") == 2:
        dep_name = rule[0][1]["left-side"][1]
    else:
        dep_name = rule[0][1]["left-side"][2]
    head_name = "head"

    dep += dep_name
    head += head_name
    deps = []
    heads = []
    agr = []
    wo = []
    for num, cond in rule:
        _, num = num[0].split("_", maxsplit=1)
        if num.startswith("dep"):
            d = num.split("_")[1]
            deps.append((d, cond))
        elif num.startswith("head"):
            h = num.split("_")[1]
            heads.append((h, cond))
        elif num.startswith("agr"):
            a = num.split("_")[1]
            agr.append((a, cond))
        elif "wordOrder" in num:
            wo.append(cond)

    optional = ""
    # first dep
    for name, cond in deps:
        dep += " ! " + cond["right-side"][dep_name][name]
    
    # head
    for name, cond in heads:
        head += " ! " + cond["right-side"]["head"][name]
   
    # agr
    for num, (name, cond) in enumerate(agr):
        if num == 0:
            rule_string += "\\\\"
        if num != len(agr)-1:
            rule_string += name.lower() + ","
        else:
            rule_string += name.lower() + " => "
        
        if name in params[0]:
            optional += name.lower() + " = " + name.lower() + ";\n"
            head +=  " ! "  +  dep_name + "." + name.lower() 
        elif name in params[1]:
            optional += name.lower() + " = head." + name.lower() + ";\n"
            dep += " ! "  + head_name + "." + name.lower()
        else:
            dep += " ! "  + name.lower()
            head +=  " ! "  + name.lower() 
        
    
    # word order
    if wo and wo[0]["right-side"].index(dep_name) == 1: # assupmtion here
        rule_string += head + " ++ " + dep
    else:
        rule_string += dep + " ++ " + head
        
    if optional:
        rule_string += ";\n" + optional[:-2]
    rule_string += "\n}"

    return rule_string

