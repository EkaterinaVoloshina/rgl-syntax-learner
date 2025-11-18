import pandas as pd
import numpy as np
import pickle
from collections import defaultdict

from sklearn.preprocessing import LabelEncoder, OneHotEncoder
from sklearn.model_selection import train_test_split


class DataLoader:
    def __init__(self,
                 feature,
                 encoding="one-hot",
                 df = None, 
                 paths = [],
                 deps=None):
        self.encoding = encoding
        self.feature = feature
        self.deps = deps
        if paths:
            self.df = self.read_data(paths[0])
        else:
            self.df = df
        

        if len(paths) == 3:
            train, dev, test = None, None, None
            for path in paths:
                if "train" in path:
                    train = path
                elif "dev" in path:
                    dev = path
                elif "test" in path:
                    test = path 
            self.X_train, self.y_train = self.transform(self.read_data(train))
           # self.X_dev, self.y_dev = self.transform(self.read_data(dev)) # TODO: should be just transform
           # self.X_test, self.y_test = self.transform(self.read_data(test)) # TODO: should be just tranform
        else:
            dfs = []
           # for path in paths:
           #     dfs.append(self.read_data(path))
          #  df = pd.concat(dfs)
            X, y = self.transform(df)
           # X_train_dev, self.X_test, y_train_dev, self.y_test = train_test_split(X, y)
           # self.X_train, self.X_dev, self.y_train, self.y_dev = train_test_split(X_train_dev, y_train_dev)
            if X.shape[0] * 0.75 > 3:
                self.X_train, self.X_test, self.y_train, self.y_test = train_test_split(X, y)
            else:
                self.X_train, self.X_test, self.y_train, self.y_test = None, None, None, None

    def read_data(self, path):
        with open(path, 'rb') as f:
            data = pickle.load(f)
        if self.feature in data:
            df = pd.DataFrame(data[self.feature])
        else:
            raise AttributeError(f"{self.feature} is not in the list of features")
        if self.feature.startswith("agr") or self.feature.startswith("dep") or self.feature.startswith("head"):
            _, feat = self.feature.split("_", maxsplit=1)
        
        if self.deps:
            df = df[df.deprel.isin(self.deps)]
        return df

    def getDeps(self):
        return self.df["deprel"].nunique()

    def getFeatureDistribution(self):
        p = defaultdict(float)
        if self.feature.startswith("agr"):
            _, feat2 = self.feature.split("_")
            dep = self.df[f"{feat2}_dep"].value_counts().to_dict()
            head = self.df[f"{feat2}_head"].value_counts().to_dict()
            all_dep = sum(dep.values())
            all_head = sum(head.values())
            for val, freq in dep.items():
                p["Yes"] += (freq/all_dep) * (head[val]/all_head)
                p["No"] += (freq/all_dep) * ((all_head - head[val])/all_head)
        elif self.feature.startswith("dep"):
            _, feat2 = self.feature.split("_")
            dep = self.df[f"{feat2}_dep"].value_counts().to_dict()
            all_dep = sum(dep.values())
            for val, freq in dep.items():
                p[val] = (freq / all_dep)
        elif self.feature.startswith("head"):
            _, feat2 = self.feature.split("_")
            head = self.df[f"{feat2}_head"].value_counts().to_dict()
            all_dep = sum(head.values())
            for val, freq in head.items():
                p[val] = (freq / all_dep)
        elif self.feature.startswith("wordOrder"):
            data = self.df["target"].value_counts().to_dict()
            all_dep = sum(data.values())
            for val, freq in data.items():
                p[val] = (freq / all_dep)
        elif self.feature.startswith("linearOrder"):
            data = self.df["target"].value_counts().to_dict()
            all_dep = sum(data.values())
            for val, freq in data.items():
                p[val] = (freq / all_dep)
        else:
            raise ValueError(f"Unknown feature: {self.feature}")
        return p

    def transform(self, df):
        if "agr" in self.feature or "head" in self.feature or "dep" in self.feature:
            feat = self.feature.split("_")[-1]
            if f"{feat}_head" in df.columns:
                df = df.drop([f"{feat}_head"], axis=1)
            if f"{feat}_dep" in df.columns:
                df = df.drop(f"{feat}_dep", axis=1)
        elif self.feature.endswith("Order"):
            df = df.drop(["position"], axis=1)
        else:
            print(f"Unknown feature: {self.feature}")
        X = []
        encoder = LabelEncoder()
        y = encoder.fit_transform(df["target"].array)
        self.labels = encoder.classes_
        df = df.drop(["target", "sent_id"], axis=1)

        if self.encoding == "one-hot":
            encoder = OneHotEncoder()
            X = encoder.fit_transform(df)
            self.feature_names = encoder.get_feature_names_out()
        elif self.encoding == "label":
            for i in df.columns:
                encoder = LabelEncoder()
                X.append(encoder.fit_transform(df[i].array))
            X = np.asarray(X).T
            self.feature_names = df.columns # needs to be fixed
        else:
            raise AttributeError(f"Encoding {self.encoding} is not supported yet")
        return X, y