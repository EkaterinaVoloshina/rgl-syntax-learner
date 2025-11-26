from syntax_learner.utils import getParams, boilerplate
import os


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
    rule_string += " ".join(rule[0][1]["left-side"]) + " = {\n" # definition of the function
    head_idx = rule[0][1]["left-side"].index("head") - 1
    if rule[0][1]["left-side"].index("head") == 2:
        dep_idx = 1
    else:
        dep_idx = 2
    dep_name = rule[0][1]["left-side"][dep_idx]
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
    if params and params[dep_idx-1] and params[dep_idx-1][1]:
        param_list = params[dep_idx-1][1]
        deps = sorted(deps, key=lambda item: (item[0] not in param_list, param_list.index(item[0]) if item[0] in param_list else 0))
    for name, cond in deps:
        if cond["right-side"][dep_name][name] not in dep:
                dep += " ! " + cond["right-side"][dep_name][name]
    
    # head
    if params and params[head_idx] and params[head_idx][1]:
        heads = sorted(heads, key=lambda item: (item[0] not in params[head_idx][1], params[head_idx][1].index(item[0]) if item[0] in params[head_idx][1] else 0))
    for name, cond in heads:
        if cond["right-side"]["head"][name] not in head:
            head += " ! " + cond["right-side"]["head"][name]
   
    # agr
    if params and params[head_idx] and params[head_idx][1]:
        agr = sorted(agr, key=lambda item: (item[0] not in params[head_idx][1], params[head_idx][1].index(item[0]) if item[0] in params[head_idx][1] else 0))
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

    if params and params[head_idx]:
            if params[head_idx][1]: # s exists
                rule_string += "\ts = "
                if wo and wo[0]["right-side"].index(dep_name) == 1: # assupmtion here
                    rule_string += head + " ++ " + dep + ";\n"
                else:
                    rule_string += dep + " ++ " + head + ";\n"
                for i in params[head_idx][-1]:
                    optional += "\t" + i + " = head." + i + ";\n"
                    
            else:
                for i in params[head_idx][-1]:
                    if wo and wo[0]["right-side"].index(dep_name) == 1: # assupmtion here
                        rule_string += "\t" + i + " = " + head + " ++ " + dep + ";\n"
                    else:
                        rule_string += "\t" + i + " = " + head + " ++ " + dep + ";\n"
    else:
        # word order
        rule_string += "\ts = "
        if wo and wo[0]["right-side"].index(dep_name) == 1: # assupmtion here
            rule_string += head + " ++ " + dep + ";\n"
        else:
            rule_string += dep + " ++ " + head + ";\n"
        
    if optional:
        rule_string += optional[:-2]
    rule_string = rule_string + "\n}"

    return rule_string

def generate_grammar(grammar, lang):
    if not os.path.exists(f"output/{lang.lower()}"):
        os.makedirs(f"output/{lang.lower()}")
    for name, funs in grammar.items():
        text = f"concrete {name}{lang.title()} of {name} = " + boilerplate.get(name, "") + " {\n" 
        text += "flags coding=utf8 ; optimize=all ;\n"
        text += "lin\n\n" 
        text += "\n\n".join(set(funs))
        text += "\n}"
        with open(f"output/{lang.lower()}/{name}{lang.title()}.gf", "w") as f:
            f.write(text)


