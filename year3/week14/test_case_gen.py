import random

VALID_CHARACTERS = map(chr, range(ord('A'), ord('Z') + 1))

def random_genome():
    return ''.join([random.choice(VALID_CHARACTERS) for _ in xrange(4)])

def change_one(x):
    i = random.randint(0, 3)
    return x[0:i] + random.choice(VALID_CHARACTERS) + x[i+1:]

def fitness(x):
    return sum(a == b for a,b in zip(x, "GEEK"))

def main():
    start = random_genome()
    knowns = [start]
    while "GEEK" not in knowns:
        most_fit = max(knowns, key=fitness)
        knowns.append(change_one(most_fit))

    print start
    print len(knowns)
    print '\n'.join(knowns)

main()
