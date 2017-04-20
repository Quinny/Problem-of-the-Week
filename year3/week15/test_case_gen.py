import random

def empty_spreadsheet(n):
    return [[None for _ in range(n)] for __ in range(n)]

def random_non_empty(spreadsheet):
    x = len(spreadsheet)
    gx, gy = random.randint(0, x - 1), random.randint(0, x - 1)
    while (spreadsheet[gx][gy] is None):
        gx, gy = random.randint(0, x - 1), random.randint(0, x - 1)
    return chr(gx + ord('A')), chr(gy + ord('A'))

def random_empty(spreadsheet):
    x = len(spreadsheet)
    gx, gy = random.randint(0, x - 1), random.randint(0, x - 1)
    while (spreadsheet[gx][gy] is not None):
        gx, gy = random.randint(0, x - 1), random.randint(0, x - 1)
    return gx, gy

def concat(seq):
    return reduce(lambda a,b: a+b, seq)

def populate_empty(spreadsheet):
    for i in range(len(spreadsheet)):
        for j in range(len(spreadsheet)):
            if spreadsheet[i][j] is None:
                n_terms = random.randint(2, 10)
                terms = "+".join([concat(random_non_empty(spreadsheet))\
                        for _ in range(n_terms)])
                spreadsheet[i][j] = terms
                return True
    return False


def main():
    n = random.randint(2, 26)
    n_seed_values = random.randint(1, (n * n) / 2)
    spreadsheet = empty_spreadsheet(n)
    for i in range(n_seed_values):
        gx, gy = random_empty(spreadsheet)
        spreadsheet[gx][gy] = random.randint(-10, 10)

    while populate_empty(spreadsheet):
        pass

    print n
    for row in spreadsheet:
        print ",".join(map(str, row))

main()
