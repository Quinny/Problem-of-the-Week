import random
from faker       import Factory
from collections import Counter
fake = Factory.create()

def votes_per_district(n_votes, n_districts):
    # there must be at least one vote per district
    ret = [1 for _ in xrange(n_districts)]
    n_votes -= n_districts

    while n_votes != 0:
        for i in xrange(n_districts):
            if n_votes == 0:
                break
            additional = random.randint(0, n_votes)
            ret[i] += additional
            n_votes -= additional
    return ret

def main():
    n_districts = random.randint(10, 1000)
    n_votes = random.randint(n_districts, 10 * n_districts)

    vpd = votes_per_district(n_votes, n_districts)
    assert sum(vpd) == n_votes

    overall_winner = fake.first_name()
    district_winners = [overall_winner] * n_districts

    lost_districts = random.sample(range(n_districts), (n_districts / 2) - 1)
    for i in lost_districts:
        district_winners[i] = fake.first_name()
    assert Counter(district_winners).most_common(1)[0][0] == overall_winner

    votes = []
    for i in range(n_districts):
        votes.extend([district_winners[i] + " " + str(i)\
                for _ in xrange((vpd[i] / 2) + 1)])

        votes.extend([fake.first_name() + " " + str(i)\
                for _ in xrange(vpd[i] - ((vpd[i] / 2) + 1))])
    assert len(votes) == n_votes

    random.shuffle(votes)

    print " ".join([str(n_districts), str(n_votes)])
    print "\n".join(votes)

main()
