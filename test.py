from syntax_learner.grammar_generator import GrammarGenerator
import json
from syntax_learner.rule_processor import compare_rules
from tqdm.auto import tqdm


# head features? 
generator = GrammarGenerator("logreg", "Macedonian-MTB")
#generator.extract_all()
#rules = generator.generate_grammar()
generator.match_rules()
generator.rank_rules()
generator.generate_code()



