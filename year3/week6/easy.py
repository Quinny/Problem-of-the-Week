def get_repeated(seq):
    seen = set()
    for i in seq:
        if i in seen:
            return i
        seen.add(i)
    raise Exception("bad test case man")

def main():
    n = int(raw_input())
    ns = (int(x) for x in raw_input().split(" "))
    print get_repeated(ns)

main()
