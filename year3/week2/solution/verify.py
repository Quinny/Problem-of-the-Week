def distance_squared((x1, y1), (x2, y2)):
    return (x1 - x2) ** 2 + (y1 - y2) ** 2

def smallest_k(seq, k, william):
    return sorted(seq, key=lambda p: distance_squared(william, p))[:k]

def read_point():
    return tuple(map(int, raw_input().split(" ")))

def main():
    william = read_point()
    k = int(raw_input())
    n = int(raw_input())
    points = [read_point() for _ in range(n)]

    for (x, y) in smallest_k(points, k, william):
        print str(x) + " " + str(y)

main()
