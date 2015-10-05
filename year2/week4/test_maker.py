from faker import Factory
import random
import autochecker
fake = Factory.create()



for i in range(100):
    n = random.randint(10, 100)
    m = random.randint(10, 100)
    contents = str(n) + " " + str(m) + "\n"
    for row in range(n):
        contents += " ".join([str(random.randint(-10000, 10000)) for col in range(m)])
        contents += "\n"
    autochecker.make_test(i, contents)
