import random

NOT_CONNECTED = 0

def empty_dogenet(n):
    return [[NOT_CONNECTED for i in range(n)] for j in range(n)]

def random_coordinate(n):
    return random.randint(0, n-1), random.randint(0, n-1)

def can_reach_all(net, start_row):
    seen = set()

    def visit(row):
        for doge, weight in enumerate(row):
            if weight != NOT_CONNECTED and doge not in seen:
                seen.add(doge)
                visit(net[doge])

    visit(net[start_row])
    return len(seen) == len(net)

def is_connected(net):
    return all(can_reach_all(net, row) for row in range(len(net)))

def randomly_connected_net(n):
    net = empty_dogenet(n)
    while not is_connected(net):
        x, y = random_coordinate(n)
        if x != y:
            w = random.randint(1, 1000)
            net[x][y] = w
            net[y][x] = w
    return net

def main():
    net_size = random.randint(2, 100)
    print net_size
    net = randomly_connected_net(net_size)
    for row in net:
        print " ".join(map(str, row))

main()
