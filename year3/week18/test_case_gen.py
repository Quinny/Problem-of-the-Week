from faker import Factory
import random

fake = Factory.create()
words = [x.strip() for x in open("/usr/share/dict/words").readlines()]

def random_name():
    return fake.first_name()

def n_random_activities(n):
    return random.sample(words, n)

def main():
    global words
    n_people = random.randint(10, 1000)
    print(n_people)

    words = random.sample(words, n_people)

    people = ["Dave"] + [random_name() for _ in range(n_people - 1)]
    for person in people:
        print(" ".join([person] +\
                n_random_activities(random.randint(1, n_people - 1))))

main()
