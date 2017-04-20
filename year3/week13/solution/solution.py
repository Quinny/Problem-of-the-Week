import sys

# Given a line of space separated integers, return the tree row representation.
def make_tree_row(line):
    return [int(x) for x in line.split(" ")]

# Return the children indicies of a given row and column.
def children(row, col):
    return (row + 1, 2 * col), (row + 1, 2 * col + 1)

# Check if the row is a leaf row.
def is_leaf(tree, row):
    return row == len(tree)

# Determine if there exists a root to leaf path which sums to 13 in the given
# tree.
def is_lucky(tree, row = 0, col = 0, path_sum = 0):
    if is_leaf(tree, row):
        return path_sum == 13

    lc, rc = children(row, col)
    return is_lucky(tree, *lc, path_sum + tree[row][col]) or\
           is_lucky(tree, *rc, path_sum + tree[row][col])

def main():
    n_rows = int(input())
    tree = [make_tree_row(line) for line in sys.stdin.readlines()]
    if is_lucky(tree):
        print("lucky")
    else:
        print("not lucky")

main()
