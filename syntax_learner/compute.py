import glob

import numpy as np
from scipy.sparse import vstack
from .loader import DataLoader
from sklearn.model_selection import PredefinedSplit, GridSearchCV
from sklearn.tree import DecisionTreeClassifier, export_text
import skglm
import json
from collections import defaultdict

def train_tree(dl):
    x = vstack([dl.X_train, dl.X_dev])
    y = np.concatenate([dl.y_train, dl.y_dev])
    test_fold = np.concatenate([
        np.full(dl.X_train.shape[0], -1, dtype=np.int8),
        np.zeros(dl.X_dev.shape[0], dtype=np.int8)
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

def format_rules(rules, loader, feature=None):
    all_rules = []
    rules = rules.split("\n")
    curDict = defaultdict(dict)
    for rule in rules:
        rule = list(filter(lambda x: not x.startswith("|"), rule.split()))
        print(rule)
        if rule and ("head" in rule[0] or "dep" in rule[0]): # change if grandchildren?
            if "<=" in rule:
                feat, node, value = rule[0].split("_")
                if feat != "lemma" and feat != "upos":
                    if "feats" in curDict[node]:
                        curDict[node]["feats"][feat] = "!" + value
                    else:
                        curDict[node]["feats"] = {feat: "!"+value}
                elif feat == "upos":
                    curDict[node]["pos"] = "!" + value
                else:
                    curDict[node][feat] = "!" + value
            elif ">" in rule and "_" in rule[0]:
                feat, node, value = rule[0].split("_")
                if feat != "lemma" and feat != "upos":
                    if "feats" in curDict[node]:
                        curDict[node]["feats"][feat] = value
                    else:
                        curDict[node]["feats"] = {feat: value}
                elif feat == "upos":
                    curDict[node]["pos"] = value
                else:
                    curDict[node][feat] = value
        elif "class:" in rule:
            curDict["rule"] = loader.labels[int(rule[1])]
            all_rules.append(curDict)
            curDict = defaultdict(dict)
        else:
            print(rule)
    return all_rules

def train_sparse_logreg(dl,
                        alpha_start=0.1,
                        alpha_end=0.001,
                        alpha_num=100
                        ):
    alphas = np.linspace(alpha_start, alpha_end, alpha_num)

    all_rules = set()
    ordered_rules = list()
    filtered_deps_len = 0 # TODO: fix that
    n_yes = int(dl.y_train.sum())

    for j, alpha in enumerate(alphas):
        model = skglm.SparseLogisticRegression(
            alpha=alpha,
            fit_intercept=True,
            max_iter=20,
            max_epochs=1000,
        )
        model.fit(dl.X_train, dl.y_train)
    return []

def compute(treebank, feat, model):
    lang = treebank.split("_")[1]
    feature = f"{lang}_{feat}"
    paths = glob.glob(f"data/{treebank}_datasets.pkl")[0]
    loader = DataLoader([paths], feature) # TODO: fix more files
    if model == "tree": # based on AutoLEX paper
        rules = format_rules(train_tree(loader), loader)
    elif model == "logreg": # based on GREX paper
        rules = train_sparse_logreg(loader)
    else:
        raise AttributeError(f"Model {model} not implemented yet")

    with open(f"data/{feature}.json", "w") as outfile:
        json.dump(rules, outfile)