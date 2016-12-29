import autochecker
import random

autochecker.assert_test_dir()

# sum -> desired sum
# n   -> numbers that sum to
# m   -> vector size
# m numbers -> contents of vector

for i in range(100):
    m = random.randint(10, 20)
    n = random.randint(3, 5)
    vector = [random.randint(-1000, 1000) for _ in range(m)]
    sum_to = random.randint(-1000, 1000)
    contents = str(sum_to) + "\n"
    contents += str(n) + "\n"
    contents += str(m) + "\n"
    contents += "\n".join(map(str, vector))
    autochecker.make_test(i, contents)

