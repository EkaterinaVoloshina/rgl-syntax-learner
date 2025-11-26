from syntax_learner.rule_processor import *
from syntax_learner.compute import compute
from collections import defaultdict
import json
import glob
import pickle
from tqdm.auto import tqdm
import os
import pandas as pd
import numpy as np
from syntax_learner.utils import getParams
from syntax_learner.code_generation import generate_rule

class GrammarGenerator():
	def __init__(self, model, data):
		self.model = model 
		self.treebank = data
		if os.path.exists("data/SUD_{self.treebank}_train_datasets.pkl"):
			path = f"data/SUD_{self.treebank}_train_datasets.pkl"
		else: 
			path = glob.glob(f"data/SUD_{self.treebank}_*_datasets.pkl")[0]
		self.lang = self.treebank.split("-")[0]
		with open(path, 'rb') as f:
			self.dataset = pickle.load(f)

		self.dep2fun = self.__read_dep__("syntax_learner/dep2fun.csv")
		self.inParams, self.params = getParams(self.treebank.split("-")[0])


		self.types = self.__read_json__("test_generation/fun2type.json")

		self.pos = ['A', 'A2', 'CAdv', 'Conj', 'Dig', 'IQuant', 'Interj', 'N3', 'Numeral',
       'PN', 'Predet','Prep', 'Pron', 'Subj', 'V', 'V2', 'V2A', 'Num',
       'V2Q', 'V2S','V2V', 'V3', 'VA', 'VQ', 'VS','VV', 'N', 'N2', 'AdV']
			

	def __read_dep__(self, filename):
		df = pd.read_csv(filename)[["function", "dependency", "dpos", "hpos"]]
		df = df.fillna(np.nan).replace([np.nan], [None])
		dep2fun = {}
		for num, row in df.iterrows():
			if row["dependency"]:
				dep2fun[row["function"]] = {"deprel": row["dependency"],
											"dpos": row["dpos"],
											"hpos": row["hpos"]}
		return dep2fun


	def __read_json__(self, filename):
		with open(filename) as f:
			rules = json.load(f)
		return rules
		 
	def extract_all(self):
		"""
		Extracts rules with a chosen model
		"""
		for feature, data in tqdm(self.dataset.items()): # TODO: fix features 
			if feature != "subj_exists":
				compute(data, feature, self.model, deps=None, lang=self.lang)
		 
	def generate_grammar(self):
		"""
		Takes all extracted rules and maps them to GF function
		"""
		
		grammar_rules = defaultdict(dict) # rule -> all possible constraints
		dep2fun = get_dep2fun()
		for feature in tqdm(self.dataset):
			p = f"data/{self.lang}_{feature}_{self.model}.json"
			if os.path.exists(p):
				rules = self.__read_json__(p)
				if feature.count("_") > 1 or (feature.count("_") > 0 and feature.endswith("wordOrder")): # if trees are divided by function
					fname, feat = feature.split("_", maxsplit=1) 
				else:
					feat = feature

				
				rules = get_rules(rules, feat, dep2fun)
				for k, v in rules.items():
					grammar_rules[k].update({feature : v})
        
		with open(f"data/{self.treebank}_rules.json", "w") as f:
			json.dump(grammar_rules, f)
        
		return grammar_rules
	
	def match_rules(self):
		"""
		The function combines subrules to a rule that corresponds to a function
		"""
		with open(f"data/{self.treebank}_rules.json") as f:
			rules = json.load(f)
		
		matches = get_matches(self.dep2fun, rules, self.dataset)
		dep2scores = {}
		for rule_name in self.dep2fun:
			if matches[rule_name]:
				print(len(matches[rule_name]))
				dep2scores[rule_name] = get_scores(rule_name, rules, matches)
		
		with open("subsets.pickle", "wb") as f:
			pickle.dump(dep2scores, f)

		with open("results.json", "w") as f:
			json.dump(matches, f)

	def rank_rules(self):
		"""
		Ranks based on their coverage and precision
		"""
		with open("subsets.pickle", "rb") as f:
			subsets = pickle.load(f)

		with open("results.json") as f:
			results = json.load(f)

		with open(f"data/{self.treebank}_rules.json") as f:
			rules = json.load(f)

		def get_weighted_results(coverage, precision, n_fules, n_conditions):
			return 0.25 * coverage + 0.25 * precision + 0.25 * n_fules + 0.25 * 1/(n_conditions+1)
		
		top_rules = {}
		for rule_name, (rule_score, d) in subsets.items():
			scores = {}
			# rule_score[num] = (coverage, precision, fscore)
			for rule, (coverage, precision, fscore) in rule_score.items():
				print(rule)
				desc, n_rules, n_cond = get_rule_description(rule, rules)
				weighted = get_weighted_results(coverage, precision, n_rules, n_cond)
				scores[rule] = (coverage, precision, fscore, n_rules, n_cond, weighted, desc)


			# Best combination by f-score
			#rule_res = sorted(scores.items(), key=lambda x: -x[1][2])
			#print(rule_res[0], "\n")
			# Best combination by coverage
			#rule_res = sorted(scores.items(), key=lambda x: -x[1][0])
			#print(rule_res[0], "\n")
			# Best combination by precision
			#rule_res = sorted(scores.items(), key=lambda x: -x[1][1])
			#print(rule_res[0], "\n")
			# Best by combination
			if scores:
				top_rules[rule_name] = sorted(scores.items(), key=lambda x: -x[1][2])[0]
		
		with open("top_rules.json", "w") as f:
			json.dump(top_rules, f)



	def generate_code(self):
		"""
		Takes the top rules and generates a grammar
		"""
		# TODO: change to the correct order
		top_rules = self.__read_json__("top_rules.json")
		grammar = ""
		for fun, rule in top_rules.items(): 
			if not fun.startswith("Use"):
				if fun not in self.types:
					continue
				inp, out = self.types[fun]
				inp_params = get_type(inp, self.types, self.inParams, self.pos)
				print(rule)
				rule_string = generate_rule(list(zip(rule[0],rule[1][6])), inp_params)
				grammar += rule_string + "\n\n"
		with open(f"{self.treebank}_grammar.txt", "w") as f:
			f.write(grammar)

def get_type(inp, types, params, pos):
	p = []
	for i in inp:
		param = []
		if i in pos:
			param =params[i]
		else:
			for inp, out in types.values():
				if i == out:
					if len(inp) == 1 and inp[0] in pos:
						param = params[inp[0]]
						break
		p.append(param)
	return p

"""
textfile = "rule\tcoverage\tprecision\tfscore\n"
for rule, scores in rule_score.items():
     textfile += str(rule) + "\t" + "\t".join(map(str,scores)) + "\n"

with open("rule_scores.csv", "w") as f:
    f.write(textfile)






"""
