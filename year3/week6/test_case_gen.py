import random

def random_with_one_repeat(size):
    repeat = random.randint(1, size - 1)
    ret = []
    while len(ret) != size:
        n = random.randint(1, size - 1)
        if n not in ret:
            ret.append(n)
        elif n in ret and n == repeat:
            ret.append(n)
    return ret

def main():
    size = random.randint(100, 10000)
    print size
    print " ".join(map(str, random_with_one_repeat(size)))

main()
