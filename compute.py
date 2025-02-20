import numpy as np
from scipy.sparse import vstack
from loader import DataLoader
from sklearn.model_selection import PredefinedSplit, GridSearchCV
from sklearn.tree import DecisionTreeClassifier, export_text

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


def compute(paths, model):
    loader = DataLoader(paths)
    if model == "tree": # based on AutoLEX paper
        rules = train_tree(loader)
    elif model == "logreg":
        pass
    elif model == "bayesian_logreg":
        pass
    else:
        raise AttributeError(f"Model {model} not implemented yet")