from syntax_learner.grammar_generator import GrammarGenerator
import json
from syntax_learner.rule_processor import compare_rules
from tqdm.auto import tqdm


# head features? 
generator = GrammarGenerator("tree", "Russian-SynTagRus")
#generator.extract_all()
rules = generator.generate_grammar()


with open("test.json", "w") as f:
    json.dump(rules,f)

feats = list(rules["AdjCN"].keys())
f1 = feats[0]
rules_0 = rules["AdjCN"][f1]
#for feat in tqdm(feats[1:]):
#    print(f1, feat)
#    rules_1 = rules["AdjCN"][feat]
#    rules_0 = compare_rules(f1, rules_0, feat, rules_1)
#    f1 = None

