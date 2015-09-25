import autochecker
import random
from faker import Factory
fake = Factory.create()
words = open("/usr/share/dict/words").readlines()

for i in range(100):
    nwords = random.randint(100, 1000)
    contents = str(nwords) + "\n"
    word_set = [random.choice(words).strip() for _ in range(nwords)]
    for w in word_set:
        contents += w + "\n"
    nsentences = random.randint(5, 50)
    contents += str(nsentences) + "\n"
    for _ in range(nsentences):
        nwords = random.randint(4, 10)
        for j in range(nwords):
            entrop = random.randint(0, 10)

            if entrop == 7:
                contents += fake.word()
            else:
                contents += random.choice(word_set)
        contents += "\n"

    autochecker.make_test(i, contents)

