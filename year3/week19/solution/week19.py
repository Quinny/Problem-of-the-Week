import requests
from collections import Counter, defaultdict

def groupby(seq, key=lambda e: e):
    groups = defaultdict(list)
    for e in seq:
        groups[key(e)].append(e)
    return groups

def getkey(key): return lambda x: x[key]

def main():
    solutions = requests\
            .get("http://potw.quinnftw.com/api/solution_languages")\
            .json()["data"]

    global_means = {
        language: count / len(solutions)
        for language, count in
        Counter(map(getkey("language"), solutions)).most_common()
    }

    solutions_by_week = groupby(solutions, getkey("week"))
    for week, solutions in solutions_by_week.items():
        trending_scores = [
            (language, (count / len(solutions)) / global_means[language])
            for language, count in
            Counter(map(getkey("language"), solutions)).most_common()
        ]
        print(week, max(trending_scores, key=getkey(1))[0])

main()
