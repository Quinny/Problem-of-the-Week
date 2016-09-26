import random

MIN_COORD = 0
MAX_COORD = 10000

def rand_coord():
    return random.randint(MIN_COORD, MAX_COORD)

def str_coord((x, y)):
    return str(x) + " " + str(y)

def main():
    wx, wy = rand_coord(), rand_coord()
    n_women_to_find = random.randint(10, 1000)
    n_women_in_seattle = random.randint(n_women_to_find, 10000)
    women_locations = [(rand_coord(), rand_coord())
            for _ in range(n_women_in_seattle)]

    print str_coord((wx, wy))
    print n_women_to_find
    print n_women_in_seattle
    for coord in women_locations:
        print str_coord(coord)

main()
