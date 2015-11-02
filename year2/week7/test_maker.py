from faker import Factory
import random
import autochecker
fake = Factory.create()

for i in range(100):
    n = random.randint(100, 10000)
    x = [random.randint(1, n) for _ in range (n)]
    contents = ""
    contents += str(n - 1) + "\n"
    contents += "\n".join(map(str, x))

    autochecker.make_test(i, contents)
