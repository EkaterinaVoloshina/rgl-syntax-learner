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
from syntax_learner.utils import getParams, fun2loc, get_record
from syntax_learner.code_generation import generate_rule, generate_grammar
from submodlib.functions.graphCut import GraphCutFunction #, SetCover
from scipy.stats import chisquare, fisher_exact, wilcoxon



class GrammarGenerator():
	def __init__(self, model : str, data : str, funs : bool =True):
		"""
		model : the name of the model, i.e. "tree" for Decision Tree or 
		
		"""
		self.model = model 
		self.treebank = data
		self.funs = funs
		if os.path.exists("data/SUD_{self.treebank}_train_datasets.pkl"):
			path = f"data/SUD_{self.treebank}_train_datasets.pkl"
		else: 
			path = glob.glob(f"data/SUD_{self.treebank}_*_datasets.pkl")[0]
		self.lang = self.treebank.split("_")[0]
		with open(path, 'rb') as f:
			self.dataset = pickle.load(f)

		self.dep2fun = self.__read_dep__("syntax_learner/dep2fun.csv")
		self.inParams, self.params = getParams(self.treebank.split("-")[0])
		self.params_values = get_record(self.treebank.split("-")[0], self.params)


		self.types = self.__read_json__("test_generation/fun2type.json")
		
		self.pos = ['A', 'A2', 'CAdv', 'Conj', 'Dig', 'IQuant', 'Interj', 'N3', 'Numeral',
       'PN', 'Predet','Prep', 'Pron', 'Subj', 'V', 'V2', 'V2A', 'Num',
       'V2Q', 'V2S','V2V', 'V3', 'VA', 'VQ', 'VS','VV', 'N', 'N2', 'AdV']
		
		self.predifined = ["Ant", "Pol", "Tense"]
		
		# FOR GRAMMAR GENERATION
		self.grammar = defaultdict(list)
		self.type2record = defaultdict(list)

			

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
			if data and feature != "subj_exists":
				compute(data, feature, self.model, deps=None, lang=self.lang)

	def calculate_significance(self):

		feature_distribution = defaultdict(dict)
		distribution_by_function = defaultdict(dict)
		for i, examples in self.dataset.items():
				if "_" in i:
						fun, feat = i.split("_", maxsplit=1)
						if "dep_" or "head_" in feat:
								feat_name = feat.split("_")[-1]
						else: feat_name = feat
						for rule in examples:
								target = rule["target"]
								if "sent_id" in rule:
										if target in feature_distribution[feat_name]:
												feature_distribution[feat_name][target].append(rule["sent_id"])
										else:
												feature_distribution[feat_name][target] = [rule["sent_id"],]
										if target in distribution_by_function[feat]:
												if fun in distribution_by_function[feat][target]:
														distribution_by_function[feat][target][fun].append(rule["sent_id"])
												else:
														distribution_by_function[feat][target][fun] = [rule["sent_id"],]
										else:
												distribution_by_function[feat][target] = {fun: [rule["sent_id"],]}


		row_1 = []
		row_2 = []

		rule2feat = defaultdict(list)
		for rule in self.dep2fun:
				for feat, fund in distribution_by_function.items():
						if "dep_" or "head_" in feat:
								feat_name = feat.split("_")[-1]
						else: feat_name = feat
						d = feature_distribution[feat_name]
						row_1 = []
						row_2 = []

						for i,v in fund.items():
								row_2.append(len(set(d[i])))
								row_1.append(len(set(v.get(rule, []))))

						if row_2 and sum(row_1) != 0 and len(row_1) > 1:
								data = np.array([row_1, row_2])
								data[1] = data[1] - data[0]
								res = fisher_exact(data)
		
								if res.pvalue <= 0.05:
										rule2feat[rule].append(feat)

						
		return rule2feat
		 
	def generate_grammar(self):
		"""
		Takes all extracted rules and maps them to GF function
		"""
		
		grammar_rules = defaultdict(dict) # rule -> all possible constraints
		dep2fun = get_dep2fun()
		rule2feat = self.calculate_significance()

		print(rule2feat)
		
		for feature in tqdm(self.dataset):
			p = f"data/{self.treebank}_{feature}_{self.model}.json"
			if os.path.exists(p):
				rules = self.__read_json__(p)
				if feature.count("_") > 1 or (feature.count("_") > 0 and feature.endswith("wordOrder")): # if trees are divided by function
					fname, feat = feature.split("_", maxsplit=1) 
				else:
					feat = feature
					fname = None
				
				rules = get_rules(rules, feat, dep2fun, fname)
				for k, v in rules.items():
					if (not fname or feature.startswith(k)) and (feat in rule2feat[k] or feat.startswith("agr") or feat == "wordOrder"):
						grammar_rules[k].update({feature : v})
        
		with open(f"data/{self.treebank}_rules.json", "w") as f:
			json.dump(grammar_rules, f)
        
		return grammar_rules
	
	def match_rules(self):
		"""
		The function combines subrules to a rule that corresponds to a function
		"""
		# SOMETHING IS WRONG HERE
		with open(f"data/{self.treebank}_rules.json") as f:
			rules = json.load(f)

		
		matches = get_matches(self.dep2fun, rules, self.dataset, funs=self.funs)
		dep2scores = {}
		for rule_name in self.dep2fun:
			if matches[rule_name]:
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


		with open(f"data/{self.treebank}_rules.json") as f:
			rules = json.load(f)

		def get_weighted_results(coverage, precision, n_fules, n_conditions):
			return 0.25 * coverage + 0.25 * precision + 0.25 * n_rules + 0.25 * 1/(n_conditions+1)
		
		top_rules = {}
		rule_subsets = defaultdict(list)
		for rule_name, (rule_score, d) in tqdm(subsets.items()):
			scores = {}
			# rule_score[num] = (coverage, precision, fscore)
			for rule, (coverage, precision, fscore) in rule_score.items():
				#print(rule)
				desc, n_rules, n_cond = get_rule_description(rule_name, rule, rules, funs=self.funs)
				weighted = get_weighted_results(coverage, precision, n_rules, n_cond)
				scores[rule] = (coverage, precision, fscore, n_rules, n_cond, weighted, desc)


			# Best combination by f-score
			#rule_res = sorted(scores.items(), key=lambda x: -x[1][2])
			# Best combination by coverage
			#rule_res = sorted(scores.items(), key=lambda x: -x[1][0])
			# Best combination by precision
			#rule_res = sorted(scores.items(), key=lambda x: -x[1][1])
			# Best by combination
			if rule_name == "PredVP":
				print(scores)
			if scores:
				top_rules[rule_name] = sorted(scores.items(), key=lambda x: (-x[1][-2]))[0]

				
				rule_subset = sorted(scores.items(), key=lambda x: -x[1][2])[:1000]
				
				#if self.funs:
				#	full_length = len(self.dataset[rule_name])
				#else:
			#	full_length = len(d["covered"])
			#	mtrx = get_mtrx(dict(rule_subset), d, full_length)

			#	if len(rule_subset) > 1:
			#		obj = GraphCutFunction(n=len(rule_subset), mode = 'dense', lambdaVal=0.2, separate_rep=False, mgsijs=mtrx, data=None, metric='cosine', num_neighbors=None)
			#		greedyLis = obj.maximize(budget = 1, optimizer = 'NaiveGreedy',stopIfZeroGain=True, stopIfNegativeGain=True)

			#		best_set = []
			#		rule_found = []
			#		for i in greedyLis:
			#			best_set.append(i[0])
			##			rule_found.append((rule_subset[i[0]][0], rule_subset[i[0]][1][-1]))
			#		print("Best set:")
			#		rule_subsets[rule_name].append(rule_found)
			#	print(rule_subset)
				# e = rule_subset
				
		
		with open("top_rules.json", "w") as f:
			json.dump(top_rules, f)

		with open("rule_subsets.json", "w") as f:
			json.dump(rule_subsets, f)

	def get_next(self, typ):
		next_fun = []
		for fun, types in self.types.items():
			in_types = all([x in typ for x in types[0]]) # check that we can produce all types of this function
			if in_types:
				next_fun.append(fun)
		return next_fun
	
	def get_type(self, inp, out):
		p = []
		for num, i in enumerate(inp):
			if i in self.pos:

				record = self.params_values.get(i, [])
				if num == len(inp) -1:
					self.type2record[out] = record
				#print(params.get(i, []))
			else:
				record = self.type2record.get(i,[])
			p.append(record)
			
		return p
	
	def generate_functions(self, funs, top_rules):
		for fun in funs: 
			if fun in self.types: # and fun not in self.grammar:
				inp, out = self.types[fun]
				loc = fun2loc.get(fun, "Extra")
				if fun in top_rules:
					rule = top_rules[fun]
					inp_params = self.get_type(inp, out)
					rule_string = generate_rule(list(zip(rule[0],rule[1][6])), inp_params, self.predifined)
					self.grammar[loc].append(rule_string) 
				elif len(inp) == 1 and fun.startswith("Use"):
					inp_params = self.get_type(inp, out)
					rule_string = fun + " "+ inp[0].lower() + " = " + inp[0].lower()
					self.grammar[loc].append(rule_string) 
				#elif fun in FUNS:
				#	inp_params = self.get_type(inp, out)
				#	self.grammar[loc].append(" ".join([fun, FUNS[fun]]))
		

	def generate_code(self):
		"""
		Takes the top rules and generates a grammar
		"""
		# TODO: change to the correct order
		top_rules = self.__read_json__("top_rules.json")
		
		seen_functions = set()
		
		seen_types = set(self.pos)
		seen_types |= set(self.params.keys())
		
		all_types = set([x[1] for x in self.types.values() if x[1] not in x[0]] + self.pos)
		
		fst_funs = self.get_next(seen_types)
		self.generate_functions(fst_funs, top_rules)
		
		seen_functions |= set(fst_funs)
		new_types = [self.types[f][1] for f in fst_funs]
		seen_types |= set(new_types)
        #while seen_types != all_types:
		for i in range(5):
			snd_funs = self.get_next(list(seen_types))
			self.generate_functions(snd_funs, top_rules)
			new_types = [self.types[f][1] for f in snd_funs]
			seen_functions |= set(snd_funs)
			seen_types = seen_types | set(new_types)
		generate_grammar(self.grammar, "Kaz")
		
		
