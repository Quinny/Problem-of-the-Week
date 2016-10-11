class Graph:
    def __init__(self):
        self._g = {}

    def connect(self, u, v):
        if u not in self._g:
            self._g[u] = set()
        self._g[u].add(v)

    def connect_bidirectional(self, u, v):
        self.connect(u, v)
        self.connect(v, u)

    def __getitem__(self, u):
        return self._g[u]

def rolls_with(g, u, v):
    seen = set()
    to_visit = [u]
    while len(to_visit) != 0:
        current = to_visit.pop()
        if current == v:
            return True

        for friend in g[current]:
            if friend not in seen:
                seen.add(friend)
                to_visit.append(friend)

    return False

def main():
    friend_graph = Graph()
    n_friendships = int(raw_input())
    for i in range(n_friendships):
        n1, n2 = raw_input().split(" ")
        friend_graph.connect_bidirectional(n1, n2)

    n_queries = int(raw_input())
    for i in range(n_queries):
        u, v = raw_input().split(" ")
        if rolls_with(friend_graph, u, v):
            print "yes"
        else:
            print "no"

main()
