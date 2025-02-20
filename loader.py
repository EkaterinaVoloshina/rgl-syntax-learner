import pandas as pd
import numpy as np

from sklearn.preprocessing import LabelEncoder, OneHotEncoder
from sklearn.model_selection import train_test_split


class DataLoader:
    def __init__(self, paths, encoding="one-hot"):
        self.encoding = encoding

        if len(paths) == 3:
            self.X_train, self.y_test = self.transform(self.read_data(paths[0]))
            self.X_dev, self.y_dev = self.transform(self.read_data(paths[1])) # TODO: should be just transform
            self.X_test, self.y_test = self.transform(self.read_data(paths[2])) # TODO: should be just tranform
        else:
            dfs = []
            for path in paths:
                dfs.append(self.read_data(path))
            df = pd.concat(dfs)
            X, y = self.transform(df)
            X_train_dev, self.X_test, y_train_dev, self.y_test = train_test_split(X, y)
            self.X_train, self.X_dev, self.y_train, self.y_dev = train_test_split(X_train_dev, y_train_dev)
            #self.X_train, self.X_test, self.y_train, self.y_test = train_test_split(X, y)

    def read_data(self, path):
        df = pd.read_csv(path)
        return df

    def transform(self, df):
        X = []
        y = LabelEncoder().fit_transform(df["target"].array)
        df = df.drop("Unnamed: 0", axis=1)
        df = df.drop("target", axis=1)
        if self.encoding == "one-hot":
            encoder = OneHotEncoder()
            X = encoder.fit_transform(df)
            self.feature_names = encoder.get_feature_names_out()
        elif self.encoding == "label":
            for i in df.columns:
                X.append(LabelEncoder().fit_transform(df[i].array))
            X = np.asarray(X).T
            self.feature_names = df.columns
        else:
            raise AttributeError(f"Encoding {self.encoding} is not supported yet")
        return X, y