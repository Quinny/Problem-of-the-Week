import random

def rand_char():
    return chr(random.randrange(ord(' '), ord('~')))

def rand_str(length):
    return ''.join([rand_char() for _ in range(length)])

def maybe_change(c, rate):
    if random.randrange(1, 100) < rate:
        return rand_char()
    return c

def mutate(s, rate):
    return ''.join(map(lambda c: maybe_change(c, rate), s))

def hamming_distance(s1, s2):
    return len(filter(lambda (x, y): x == y, zip(s1, s2)))

def main():
    mutation_rate = 30
    target  = raw_input()
    current = rand_str(len(target))

    while (current != target):
        print current
        next_gen = [mutate(current, mutation_rate) for _ in range(100)]
        current = max(next_gen, key=lambda s: hamming_distance(s, target))
    print target

if __name__ == "__main__":
    main()
