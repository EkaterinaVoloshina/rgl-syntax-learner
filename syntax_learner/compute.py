import glob
import numpy as np
from scipy.sparse import vstack
from .loader import DataLoader
from sklearn.model_selection import PredefinedSplit, GridSearchCV
from sklearn.tree import DecisionTreeClassifier, export_text
import skglm
import scipy
import json
from collections import defaultdict
import bambi as bmb
import arviz as az
from scipy.stats import chisquare


def train_tree(dl):
    x = vstack([dl.X_train, dl.X_test])
    y = np.concatenate([dl.y_train, dl.y_test])
    test_fold = np.concatenate([
        np.full(dl.X_train.shape[0], -1, dtype=np.int8),
        np.zeros(dl.X_test.shape[0], dtype=np.int8)
    ])
    cv = PredefinedSplit(test_fold)
    criterion = ['gini', 'entropy']
    parameters = {'criterion': criterion, 'max_depth': np.arange(6, 15), 'min_impurity_decrease': [1e-3]}
    decision_tree = DecisionTreeClassifier()
    model = GridSearchCV(decision_tree, parameters, cv=cv)
    model.fit(x, y)

    trainleave_id = model.best_estimator_.apply(x)

    uniqueleaves = set(trainleave_id)
    uniqueleaves = sorted(uniqueleaves)
    leafcount = {}
    for i, leaf in enumerate(uniqueleaves):
        leafcount[i] = round(np.count_nonzero(trainleave_id == leaf) * 100 / len(trainleave_id), 2)

    tree_rules = export_text(model.best_estimator_, feature_names=dl.feature_names,
                             max_depth=model.best_params_["max_depth"])
    return tree_rules

def format_tree_rules(rules, loader, feature=None):
    all_rules = []
    rules = rules.split("\n")
    curDict = []
    for num, rule in enumerate(rules):
        rule = rule.replace("-","").split()
        level = rule.count("|")
        rule = list(filter(lambda x: x != "|", rule))
        if num > 0 and "class:" in rules[num-1]:
            curDict = curDict[:level-1]
        if rule and ("head" in rule[0] or "dep" in rule[0] or "position" in rule[0]) : # change if grandchildren?
            if "<=" in rule:
                if "deprel" in rule[0]:
                    curDict.append(("deprel", "!" + rule[0].split("_")[1]))
                elif "position" in rule[0]:
                    curDict.append(("position", "!" + rule[0].split("_")[1]))
                elif "pos_head" in rule[0]:
                    curDict.append(("head", "pos", "!" + rule[0].split("_")[-1]))
                elif "pos_dep" in rule[0]:
                    curDict.append(("dep", "pos", "!" + rule[0].split("_")[-1]))
                else:
                    feat, node, value = rule[0].rsplit("_", maxsplit=2)
                    if feat != "lemma" and feat != "upos":
                        curDict.append((node, "feats", feat, "!" + value))
                    elif feat == "upos":
                        curDict.append((node, "pos", "!" + value))
                    else:
                        curDict.append((node, feat, "!" + value))
            elif ">" in rule and "_" in rule[0]:
                if "deprel" in rule[0]:
                    curDict.append(("deprel", rule[0].split("_")[1]))
                elif "position" in rule[0]:
                    curDict.append(("position", rule[0].split("_")[1]))
                elif "pos_head" in rule[0]:
                    curDict.append(("head", "pos", rule[0].split("_")[-1]))
                elif "pos_dep" in rule[0]:
                    curDict.append(("dep", "pos", rule[0].split("_")[-1]))
                else:
                    feat, node, value = rule[0].rsplit("_", maxsplit=2)
                    if feat != "lemma" and feat != "upos":
                        curDict.append((node, "feats", feat, value))
                    elif feat == "upos":
                        curDict.append((node, "pos", value))
                    else:
                        curDict.append((node, feat, value))
        elif "class:" in rule:
            d = curDict.copy()
            d.append(("rule",  loader.labels[int(rule[1])]))

            all_rules.append(d)
        else:
            print(rule)
    return all_rules

def filter_rules(rules, loader, significance_level=0.01):
    filtered_rules = []
 
    for rule in rules:
        sub_df = loader.df.copy()
        for subrule in rule:
            value = subrule[-1]
            #print(sub_df.Number_dep.value_counts())
            if "deprel" in subrule:
                if value.startswith("!"):
                    sub_df = sub_df[sub_df["deprel"] != value]
                else:
                    sub_df = sub_df[sub_df["deprel"] == value]
           # print(sub_df.shape)
            elif "head" in subrule and "feats" in subrule:
                feat = subrule[-2]
                if value.startswith("!"):
                    if value == "nan":
                        sub_df = sub_df[sub_df[f"{feat}_dep"].notna()]
                    else:
                        sub_df = sub_df[sub_df[f"{feat}_head"] != value]
                else:
                    if value == "nan":
                        sub_df = sub_df[sub_df[f"{feat}_head"].isna()]
                    else:
                        sub_df = sub_df[sub_df[f"{feat}_head"] == value]
               # print(sub_df.shape)
            elif "dep" in subrule:
                feat = subrule[-2]
                if value.startswith("!"):
                    if value == "!nan":
                        sub_df = sub_df[sub_df[f"{feat}_dep"].notna()]
                    else:
                        sub_df = sub_df[sub_df[f"{feat}_dep"] != value]
                else:
                    if value == "nan":
                        sub_df = sub_df[sub_df[f"{feat}_dep"].isna()]
                    else:
                        sub_df = sub_df[sub_df[f"{feat}_dep"] == value]
               # print(sub_df.shape)

        num_examples = sub_df.shape[0]

        

        if loader.feature.startswith("agr"):
            # Observed frequencies
            O_yes = sub_df[sub_df["target"] == "Yes"].shape[0]
            O_no = sub_df[sub_df["target"] == "No"].shape[0]

            # Expected frequencies
            dist = loader.getFeatureDistribution()
            E_yes = num_examples * dist["Yes"]
            E_no = num_examples * dist["No"]
            
            res = chisquare([O_yes, O_no], [E_yes, E_no])

        else:
            E_yes = num_examples * 0.5
            E_no = num_examples * 0.5
            res = chisquare(sub_df.target.value_counts())

        effect_size = res.statistic / num_examples
        if min(E_no, E_yes) > 5 and res.pvalue < 0.05 and effect_size > 0.5:
            filtered_rules.append(rule)
        #else:
        #    print(res.pvalue, effect_size, E_no, E_yes)
            
    return filtered_rules

def train_sparse_logreg(loader,
                        alpha_start=0.1,
                        alpha_end=0.001,
                        alpha_num=100
                        ):
    alphas = np.linspace(alpha_start, alpha_end, alpha_num)
    X, y = loader.X_train, loader.y_train
    all_rules = set()
    ordered_rules = list()
    filtered_deps_len = loader.getDeps()
    n_yes = int(y.sum())

    for j, alpha in enumerate(alphas):
        model = skglm.SparseLogisticRegression(
            alpha=alpha,
            fit_intercept=True,
            max_iter=20,
            max_epochs=1000,
        )
        model.fit(X, y)
        for idx, (name, value) in enumerate(zip(loader.feature_names, model.coef_[0])):
            if name not in all_rules:
                all_rules.add(name)
                col = np.asarray(X[:, idx].todense())
                idx_col = col.squeeze(1)

                with_feature_selector = idx_col > 0
                without_feature_selector = np.logical_not(with_feature_selector)

                matched = y[with_feature_selector]
                n_matched = len(matched)
                n_pattern_positive_occurence = matched.sum()
                n_pattern_negative_occurence = n_matched - n_pattern_positive_occurence

                mu = (n_yes / filtered_deps_len)
                a = (n_pattern_positive_occurence / n_matched)
                gstat = 2 * n_matched * (
                        ((a * np.log(a)) if a > 0 else 0) - a * np.log(mu)
                        + (((1 - a) * np.log(1 - a)) if (1 - a) > 0 else 0) - (1 - a) * np.log(1 - mu)
                )
                p_value = 1 - scipy.stats.chi2.cdf(gstat, 1)
                cramers_phi = np.sqrt((gstat / n_matched))

                expected = (n_matched * n_yes) / filtered_deps_len
                delta_observed_expected = n_pattern_positive_occurence - expected

                if n_pattern_positive_occurence / n_matched > int(y.sum()) / filtered_deps_len:
                    decision = 'yes'
                    coverage = (n_pattern_positive_occurence / n_yes) * 100
                    presicion = (n_pattern_positive_occurence / n_matched) * 100
                else:
                    decision = 'no'
                    coverage = (n_pattern_negative_occurence / (filtered_deps_len - n_yes)) * 100
                    presicion = (n_pattern_negative_occurence / n_matched) * 100

                ordered_rules.append({
                    "pattern": ",".join(sorted(name.split(","))),
                    "n_pattern_occurence": idx_col.sum(),
                    "n_pattern_positive_occurence": n_pattern_positive_occurence,
                    "decision": decision,
                    "alpha": alpha,
                    "value": value,
                    "coverage": coverage,
                    "precision": presicion,
                    "delta": delta_observed_expected,
                    "g-statistic": gstat,
                    "p-value": p_value,
                    "cramers_phi": cramers_phi
                })
    return ordered_rules

def format_logreg_rules(rules):
    new_rules = []
    for rule in rules:
        #if rule["p-value"] <= 100:
            new_rule = defaultdict()
            new_rule["rule"] = rule["decision"]
            for pattern in rule["pattern"].split(","):
                if "deprel" in pattern:
                    new_rule["deprel"] = pattern.split("_")[1]
                elif "position" in pattern:
                    new_rule["position"] = pattern.split("_")[1]
                elif "head" in pattern or "dep" in pattern:
                    feat, node, value = pattern.rsplit("_", maxsplit=2)
                    if node in new_rule:
                        new_rule[node][feat] = value
                    else:
                        new_rule[node] = {feat: value}
                else:
                    print(pattern)
            new_rules.append(new_rule)
    return new_rules


def train_bayesian_model(data, binary=True):
    data = data.fillna("-")
    feats = " + ".join([x for x in data.columns if x != "target" and x != "idx"])
    if binary:
        model = bmb.Model(f"target[Yes] ~ {feats}", data, family="bernoulli")
    else:
        model = bmb.Model(f"target ~ {feats}", data, family="categorial")
    idata = model.fit(draws=3000)
    return az.summary(idata)["sd"].to_dict()




def compute(treebank, feat, model, deps=None):
    lang = treebank.split("_")[1]
   # paths = glob.glob(f"data/{treebank}_*_datasets.pkl")
    paths = glob.glob(f"data/{treebank}_train_datasets.pkl")
    loader = DataLoader(paths, feat, deps=deps) # TODO: fix more files
    #print(loader.X_train)
    if model == "tree": # based on AutoLEX paper
        all_rules = format_tree_rules(train_tree(loader), loader)
        # print(all_rules)
        rules = filter_rules(all_rules, loader)
    elif model == "logreg": # based on GREX paper
        rules = format_logreg_rules(train_sparse_logreg(loader))
    else:
        raise AttributeError(f"Model {model} not implemented yet")
    with open(f"data/{lang}_{feat}_{model}.json", "w") as outfile:
        json.dump(rules, outfile)
    print("Done!")