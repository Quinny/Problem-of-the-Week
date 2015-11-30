import sys

def evaluate(expr, ops):
    toks = expr.split()
    stack = []

    for term in toks:
        if term.isdigit():
            stack.append(int(term))
        else:
            n2, n1 = int(stack.pop()), int(stack.pop())
            stack.append(ops[term](n1, n2))
    return stack[0]

ops = {}
ops["+"] = lambda x, y: x + y
ops["*"] = lambda x, y: x * y
ops["/"] = lambda x, y: x / y
ops["-"] = lambda x, y: x - y

for line in sys.stdin.readlines()[1:]:
    print evaluate(line.strip(), ops)
