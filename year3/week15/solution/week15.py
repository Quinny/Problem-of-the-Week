import itertools

# A cell is represented either by a single value, or a summation of its
# dependancies.
class Cell:
    def __init__(self, value = None, deps = []):
        self.value = value
        self.deps = deps

    def evaluate(self, spreadsheet):
        if self.value is None:
            self.value = sum(spreadsheet[row][col].value for row, col in self.deps)
        return self.value

def int_or_none(s):
    try:
        return int(s)
    except:
        return None

def parse_cell(s):
    v = int_or_none(s)
    if v is not None:
        return Cell(value = v)

    return Cell(deps = [(ord(row) - ord('A'), ord(col) - ord('A'))\
            for [row, col] in s.split("+")])

# Returns a topological ordering of the cells of the spreadsheet such that
# all summation dependancies will be satisified.
def evaluation_order(spreadsheet):
    seen = set()

    def not_seen(pos):
        return pos not in seen

    def visit(pos):
        row, col = pos
        if not_seen(pos):
            map(visit, spreadsheet[row][col].deps)
            seen.add(pos)
            yield pos

    cells = itertools.product(range(len(spreadsheet)), repeat=2)
    return itertools.chain(map(visit, filter(not_seen, cells)))

def main():
    n = int(input())
    cells = [input().strip().split(",") for _ in range(n)]
    spreadsheet = [[parse_cell(cell) for cell in row] for row in cells]

    for row in spreadsheet:
        print(",".join(str(cell.evaluate(spreadsheet)) for cell in row))

main()
