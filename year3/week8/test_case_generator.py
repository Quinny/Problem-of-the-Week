import random

def strip_newline(s):
    return s.strip()

def n_random_words(n):
    words = map(strip_newline, open("/usr/share/dict/words").readlines())
    return (random.choice(words) for _ in range(n))

def n_random_domains(n):
    extensions = ["ca", "com", "co", "net"]
    return map(lambda x: x + "." + random.choice(extensions), n_random_words(n))

def main():
    n = random.randint(10, 1000)
    print n
    for i in n_random_domains(n):
        print i


    m = random.randint(100, 10000)
    print m
    for i in n_random_domains(m):
        print i

main()
