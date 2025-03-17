import re

filepath = "../gf-rgl/src/macedonian/ResMkd.gf"

with open(filepath, "r") as f:
    text = f.read().splitlines()

params = [t for t in text if t.startswith("param")]

for p in params:
    par, vals = re.findall(r"param (.+) = (.*) ;", p)[0]
    vals = [v.split()[0] for v in vals.split(" | ")]
    print(par, vals)
