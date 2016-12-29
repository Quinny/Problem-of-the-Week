#include <iostream>
#include <queue>
#include <vector>

template <typename T>
using matrix = std::vector<std::vector<T>>;

template <typename T>
matrix<T> make(std::size_t m, std::size_t n, T fill) {
    matrix<T> ret;
    for (std::size_t i = 0; i < m; ++i)
        ret.emplace_back(n, fill);
    return ret;
}

bool bfs(matrix<int> rgraph, int source, int target, std::vector<int>& parent) {
    std::vector<bool> visited(rgraph.size(), false);
    std::queue<int> q;
    q.push(source);
    visited[source] = true;
    parent[source] = -1;

    while (!q.empty()) {
        auto u = q.front();
        q.pop();

        for (int v = 0; v < rgraph[u].size(); ++v) {
            if (!visited[v] && rgraph[u][v] > 0) {
                q.push(v);
                parent[v] = u;
                visited[v] = true;
            }
        }
    }

    return visited[target];
}

std::size_t max_flow(matrix<int> graph, int source, int target) {
    matrix<int> rgraph = graph;
    std::vector<int> parent(graph.size(), 0);

    int ret = 0;

    while (bfs(rgraph, source, target, parent)) {
        int flow = 1 << 30;
        for (int v = target; v != source; v = parent[v]) {
            int u = parent[v];
            flow = std::min(flow, rgraph[u][v]);
        }

        for (int v = target; v != source; v = parent[v]) {
            int u = parent[v];
            rgraph[u][v] -= flow;
            rgraph[v][u] += flow;
        }

        ret += flow;
    }
    return ret;
}

int main() {
   // vertex u is connected to v with capacity m[u][v]
    matrix<int> g = { {0, 16, 13, 0, 0, 0},
        {0, 0, 10, 12, 0, 0},
        {0, 4, 0, 0, 14, 0},
        {0, 0, 9, 0, 0, 20},
        {0, 0, 0, 7, 0, 4},
        {0, 0, 0, 0, 0, 0}
    };

    std::cout << max_flow(g, 0, 5) << std::endl;
}
