from wordnet import *
from Levenshtein import distance
import json
from tqdm.auto import tqdm
from collections import defaultdict
import random
from itertools import permutations, product


class TestGenerator():
    def __init__(self, fun2type, pos_synsets, exclude=None):
        self.fun2type = fun2type
        self.pos = ['A', 'A2', 'CAdv', 'Conj', 'Dig', 'IQuant', 'Interj', 'N3', 'Numeral',
       'PN', 'Predet','Prep', 'Pron', 'Subj', 'V', 'V2', 'V2A',
       'V2Q', 'V2S','V2V', 'V3', 'VA', 'VQ', 'VS','VV', 'N', 'N2', 'AdV']
        self.params = {"Pol": [w.PPos, w.PNeg],
                      "Ant": [w.ASimul, w.AAnter],
                      "Tense": [w.TPres, w.TPast, w.TPastSimple,
                            w.TFut, w.TCond]
                 }
        self.pos_synsets = pos_synsets
        self.exclude = exclude

        self.results = defaultdict(list)
        self.pos_sample = {}
        self.neg_sample = {}
        self.seen = []

    def get_next(self, typ):
        next_fun = []
        for fun, types in self.fun2type.items():
            in_types = all([x in typ for x in types[0]]) # check that we can produce all types of this function
            if in_types:
                next_fun.append(fun)
        return next_fun

    def get_examples_by_pos(self, pos):
        examples = []
        for x in self.pos_synsets[pos]:
            try:
                examples.append(lexeme(x[1]))
            except:
                pass
        return examples

    def get_type_examples(self, typ):
        if typ in self.pos:
            return self.get_examples_by_pos(typ)
        elif typ in self.results:
            return self.results[typ]
        elif typ in self.params:
            return self.params[typ]

    def call_functions(self, funs, lang, n):
        pos, neg = {}, {}
        for f in tqdm(funs):
        
            if f not in self.exclude and f not in self.seen:
                print(f)
                self.seen.append(f)
                input_types, output_type = self.fun2type[f]
                
                if len(input_types) == 1:
                    lists = self.get_type_examples(input_types[0])
                    if lists:
                        es, _ = sample_one(n, lang, lists, eval("w." + f))
                        self.results[output_type].extend(es)  
                    else:
                        print(input_types)
                elif f == "TTAnt":
                    for p in product(self.params["Ant"], self.params["Tense"]):
                        self.results[output_type].append(w.TTAnt(*p))
                else:
                    lists = []
                    for inp_t in input_types:
                        lists.append(self.get_type_examples(inp_t))
                    if all(lists):
                        es, ps, ns = sample(n, lang, lists, eval("w." + f))   
                        self.results[output_type].extend(es)
                        self.pos_sample[f] = ps
                        self.neg_sample[f] = ns
                        
                    else:
                        print(input_types)
                


    def build_hierarchy(self, lang, n):
        seen_functions = set()
        
        
        seen_types = set()
        seen_types |= set(self.pos)
        seen_types |= set(self.params.keys())
    
        all_types = set([x[1] for x in self.fun2type.values() if x[1] not in x[0]] + self.pos)
        
        
        fst_funs = self.get_next(seen_types)
        
        self.call_functions(fst_funs,lang, n)
        
        seen_functions |= set(fst_funs)
        new_types = [self.fun2type[f][1] for f in fst_funs]
        seen_types |= set(new_types)
        #while seen_types != all_types:
        for i in range(5):
            snd_funs = self.get_next(list(seen_types))
            self.call_functions(snd_funs, lang, n)
            new_types = [self.fun2type[f][1] for f in snd_funs]
            seen_functions |= set(snd_funs)
     
            seen_types = seen_types | set(new_types)

def decode_line(line):
    if "\t" in line:
        rest, desc = line.split("\t", maxsplit=1)
        rest, wd_id = rest.split("--")
        wd_id = wd_id.strip()
    else:
        rest = line
        wd_id, desc = None, None
    pos = rest.split()[-2]
    fun = rest.split()[1]
    return (wd_id, pos, fun, desc)

def get_files():
    with open("fun2type.json") as f:
        fun2type = json.load(f)

    with open("WordNet.gf") as f:
        wn_raw = f.read().splitlines()

    
    pos_synsets = defaultdict(list)
    for i in wn_raw[2:-2]:
        wd_id, pos_name, fun, desc = decode_line(i)
        pos_synsets[pos_name].append((wd_id, fun, desc))
    return fun2type, pos_synsets

def sample_one(n, lang, l, posF):
    pos_samples = []
    exprs = []
    if n < len(l):
        chosen = random.sample(l, n)
    else: 
        chosen = l
    for c in chosen:
        pos_sam = []
        if isinstance(l[0], Lexeme):
            c = c.expression()
            lin = linearize(lang, c)
            while "[" in lin and len(lin.split()) > 1: 
                c = random.choice(l).expression()
                lin = linearize(lang, c)
        else:
            lin = linearize(lang, c)
        pos_sam.append(c)
        expr = posF(*pos_sam)
        exprs.append(expr)
        try:
            pos_s = linearize(lang, expr)
            pos_samples.append(pos_s)
        except Exception as e:
            print(e)
            print(expr)
            print(pos_sam)
    return exprs, pos_samples



def sample(n, lang, lists, posF):
    """
    n: number of samples
    lang: language 
    lists: lists of lexemes
    posF: function to generate positive samples
    """
    # get all possible combinations and choose the one furthest from the correct one
    pos_samples = []
    neg_samples = []
    exprs = []
    for _ in range(n):
        pos_sam = []
        neg_sam = []
        for l in lists:
            if isinstance(l[0], Lexeme):
                chosen = random.choice(l).expression()
                lin = linearize(lang, chosen)
                n_iter = 0
                while lin and "[" in lin and len(lin.split()) > 1: 
                    n_iter += 1
                    chosen = random.choice(l).expression()
                    lin = linearize(lang, chosen)
                    if n_iter > 5:
                        break
            else:
                chosen = random.choice(l)
                lin = linearize(lang, chosen)
            pos_sam.append(chosen)
            neg_sam.append(lin)
        expr = posF(*pos_sam)
        try:
            pos_s = linearize(lang, expr) 
            pos_samples.append(pos_s)
            ex_list = list(permutations(neg_sam))
            max_dist = -1
            neg_s = None
            
            for i in ex_list:
                e = " ".join(i)
                d = distance(pos_s, e)
                if d > max_dist:
                    max_dist = d
                    neg_s = e
            neg_samples.append(neg_s)
            exprs.append(expr)
        except Exception as e:
            print(e)
            print(expr)
            print(pos_sam)

    return exprs, pos_samples, neg_samples
        
    
if __name__ == "__main__":
    fun2type, pos_synsets = get_files()
    generator = TestGenerator(fun2type, pos_synsets, exclude=["AdjOrd", "ComparAdvAdj",
                                                               "ComparAdvAdjS", "DetNP",
                                                               "NumDigits","PPartNP", "CAdvAP",
                                                               "ReflA2",
                                                               "PhrUtt", "PredSCVP", "SentAP",
                                                               "SentCN"])
    res = generator.build_hierarchy("eng", 5)
    with open("pos_samples.json", "w") as f:
        json.dump(generator.pos_sample, f)

    with open("neg_samples.json", "w") as f:
        json.dump(generator.neg_sample, f)