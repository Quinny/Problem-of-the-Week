

#!/usr/bin/python3

from urllib.request import urlretrieve
from statistics import mean
import json

urlretrieve("http://potw.quinnftw.com/api/solution_languages", "./...")
with open("./...") as fresh_api_data:
    solutions = json.load(fresh_api_data)
solutions = [(s.get('week'), s.get('language')) for s in solutions['data']]

solutions.sort()
n_weeks = solutions[-1][0]
weeks = [[sol[1] for sol in solutions if sol[0] == week+1] for week in range(n_weeks)]
languages = {lang for lang in {s[1] for s in solutions}}

historical_shares = {lang: [week.count(lang)/len(week) for week in weeks] for lang in languages}
lang_means = {lang: mean(historical_shares[lang]) for lang in languages}

trendy = list()
for week in range(n_weeks):
    trending = "None"
    uptick = 0.0
    for lang in languages:
        lang_share = historical_shares[lang][week]
        deviance = lang_share - lang_means[lang]
        if deviance > uptick:
            uptick = deviance
            trending = lang
    trendy.append(trending)

for week, trend in enumerate(trendy):
    print("Week " + str(week) + ": " + trend)



