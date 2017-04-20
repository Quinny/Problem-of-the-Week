import sys
from collections import Counter

def parse_vote(line):
    candidate, district = line.strip().split(" ")
    return candidate, int(district)

def consume_vote(district_counts, vote):
    candidate, district = vote
    if district_counts[district] is None:
        district_counts[district] = (candidate, 0)

    current_leader, count = district_counts[district]
    if current_leader == candidate:
        district_counts[district] = (current_leader, count + 1)
    else:
        district_counts[district] = (candidate, 1)

def main():
    n_votes, n_districts = map(int, input().strip().split(" "))
    votes = map(parse_vote, sys.stdin.readlines())
    district_counts = [None for _ in range(n_districts)]
    for v in votes: consume_vote(district_counts, v)
    winners = (x[0] for x in district_counts if x is not None)
    print(Counter(winners).most_common(1)[0][0])

main()
