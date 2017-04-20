from heapq import heappush, heappop

# Given the special tiles map and the current state, return all possible
# neighbour states.  The turn number is included in the state since it may
# be possible to reach a given tile multiple different ways.
def move_function(tiles, state):
    position, turn = state
    for roll in range(1, 7, 1):
        if position + roll <= 100:
            if position + roll in tiles:
                yield tiles[position + roll], turn + 1
            else:
                yield position + roll, turn + 1

# Perform astar search using the provided functions.  This implmentation is
# intentionally generic, so that it could be used to explore any search space.
def astar(start, done, expand, transition_cost, heuristic_cost):
    # Storing the whole path in the queue is really not a great idea,
    # it uses a lot of memory and is likely slower but is easier to implement.
    to_explore = [(heuristic_cost(start), (start, [start]))]
    path_cost = {start: 0}

    while to_explore:
        _, (state, path) = heappop(to_explore)
        if done(state):
            return path

        for successor_state in expand(state):
            new_cost = path_cost[state] + transition_cost(successor_state)
            if successor_state not in path_cost or\
                    new_cost < path_cost[successor_state]:
                path_cost[successor_state] = new_cost
                heappush(to_explore, (new_cost + heuristic_cost(successor_state),\
                        (successor_state, path + [successor_state])))

    # The goal was not reached.
    return None

def main():
    n_tiles = int(input())
    tiles = {}
    for i in range(n_tiles):
        start, end = list(map(int, input().strip().split()))
        tiles[start] = end

    # Partial application of the move function with the tiles bound.
    def expand(state):
        yield from move_function(tiles, state)

    def done(state):
        position, _ = state
        return position == 100

    # Rolling the dice has a constant cost of 1.
    def transition_cost(_):
        return 1

    # The absolute distance to the goal.
    def heuristic_cost(state):
        position, _ = state
        return 100 - position

    def fst(p):
        return p[0]

    path = map(fst, astar((1, 0), done, expand, transition_cost, heuristic_cost))
    print(" ".join(map(str, path)))

main()
