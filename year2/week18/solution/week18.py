import sys

def groups_of(groups, n):
    for i in range(0, len(groups), n):
        yield groups[i:i+n]

tokens = sys.stdin.read().split()
meta   = tokens[0:4]
data   = tokens[4:]

print '\n'.join(meta)
for i in groups_of(data, 3):
    r, g, b = map(int, i)
    avg = (r + g + b) / 3
    print " ".join(map(str, [avg, avg, avg]))
