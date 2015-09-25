from faker import Factory
import random
import autochecker

fake = Factory.create()
for i in range(100):
    x = random.randint(10, 100)
    contents = str(x) + "\n"
    for d in range(x):
        contents += fake.first_name() +\
                " " + str(random.randint(0, 4294967294)) + "\n"
    autochecker.make_test(i, contents)
