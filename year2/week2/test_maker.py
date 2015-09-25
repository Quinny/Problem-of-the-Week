from faker import Factory
import random
import autochecker
fake = Factory.create()



for i in range(100):
    x = random.randint(10, 1000)
    contents = str(x) + "\n"
    names = [fake.first_name() for _ in range(200)]
    names.append("Quinn")
    has_quinn = False
    for d in range(x - 1):
        n1 = random.choice(names)
        n2 = random.choice(names)
        has_quinn = has_quinn or (n1 == "Quinn") or (n2 == "Quinn")
        contents += n1 + " " + n2 + "\n"
    if not has_quinn:
        contents += "Quinn " + random.choice(names) + "\n"
    else:
        contents += random.choice(names) + " " + random.choice(names) + "\n"
    autochecker.make_test(i, contents)
