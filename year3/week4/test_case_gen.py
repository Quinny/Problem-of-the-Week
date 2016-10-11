import random
from faker import Faker

fake = Faker()

def generate_fake_names(n):
    names = set()
    while len(names) != n:
        name = fake.name().split(" ")[0].lower()
        if "." not in name:
            names.add(name)
    return list(names)

def names_used(s):
    ret = set()
    for n1, n2 in s:
        ret.add(n1)
        ret.add(n2)
    return list(ret)

def main():
    n_friendships = random.randint(10, 1000)
    n_queries = random.randint(10, 100)
    names = generate_fake_names(n_friendships)

    print n_friendships
    friendships_used = set()
    while len(friendships_used) != n_friendships:
        n1, n2 = random.choice(names), random.choice(names)
        if n1 != n2 and (n1, n2) not in friendships_used:
            print n1 + " " + n2
            friendships_used.add((n1, n2))

    print n_queries
    queries_used = set()
    names = names_used(friendships_used)
    while len(queries_used) != n_queries:
        n1, n2 = random.choice(names), random.choice(names)
        if n1 != n2 and (n1, n2) not in queries_used:
            print n1 + " " + n2
            queries_used.add((n1, n2))


main()
