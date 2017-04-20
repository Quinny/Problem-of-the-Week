import random

def make_row(i):
    return " ".join([str(random.randint(-100, 100)) for _ in range(2 ** i)])

def main():
    n_rows = random.randint(2, 10)
    print n_rows

    for i in range(n_rows):
        print make_row(i)

main()
