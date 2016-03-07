import sys

def majority_candidate(votes):
    def count((candidate, count), name):
        if count == 0:
            return name, 1
        if name == candidate:
            return candidate, count + 1
        return candidate, count - 1

    candidate, _ = reduce(count, votes, (votes[0], 0))
    return candidate

def is_majority(value, iterable):
    return iterable.count(value) > len(iterable) / 2

def majority_element(iterable):
    x = majority_candidate(iterable)
    if is_majority(x, iterable):
        return x
    return None

def main():
    votes = sys.stdin.readlines()
    prez = majority_element(votes)
    print prez or "no majority"

if __name__ == "__main__":
    main()
