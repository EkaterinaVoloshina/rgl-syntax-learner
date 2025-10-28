from syntax_learner.rule_processor import get_rules
from syntax_learner.compute import compute
from collections import defaultdict
import json
import glob
import pickle
from tqdm.auto import tqdm
import os

class GrammarGenerator():
	def __init__(self, model, data):
		self.model = model 
		self.treebank = data
		path = glob.glob(f"data/SUD_{self.treebank}_train_datasets.pkl")[0]
		with open(path, 'rb') as f:
			self.dataset = pickle.load(f)

	def __read_rules__(self, filename):
		with open(filename) as f:
			rules = json.load(f)
		return rules
		 
	def extract_all(self):
		"""
		Extracts rules with a chosen model
		"""
		for feature in tqdm(self.dataset): # TODO: fix features 
			if feature != "subj_exists":
				compute(f"SUD_{self.treebank}", feature, self.model, deps=None)
		 
	def generate_grammar(self):
		"""
		Takes all extracted rules and maps them to GF function
		"""
		
		grammar_rules = defaultdict(dict) # rule -> all possible constraints
		for feature in tqdm(self.dataset):
			p = f"data/{self.treebank}_{feature}_{self.model}.json"
			if os.path.exists(p):
				rules = self.__read_rules__(p)
				rules = get_rules(rules, feature)
				for k, v in rules.items():
					grammar_rules[k].update({feature : v})
		return grammar_rules
	
	def match_rules(self):
		"""
		The function combines subrules to a rule that corresponds to a function
		"""
		pass

	def rank_rules(self):
		"""
		Ranks based on their coverage and precision
		"""
		pass


