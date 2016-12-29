#include <map>
#include <vector>
#include <iostream>
#include <queue>
#include <set>

// Simple graph representation.
// Maps a friend to their list of friends, or more specifically
// maps a node to a list of adjacent nodes
struct friend_graph {
    std::map<std::string, std::vector<std::string>> friends_;

    void make_friendship(const std::string& f1, const std::string& f2) {
        friends_[f1].push_back(f2);
        friends_[f2].push_back(f1);
    }

    std::vector<std::string> friends_list(const std::string& f) {
        return friends_[f];
    }
};

int main() {
    int n;
    std::cin >> n;
    friend_graph g;

    // Keep track of a sorted collection of all names entered
    std::set<std::string> name_set;
    for (int i = 0; i < n; ++i) {
        std::string f1, f2;
        std::cin >> f1 >> f2;
        g.make_friendship(f1, f2);
        name_set.insert(f1); name_set.insert(f2);
    }

    // Map name to qdist value.  Doubles as a way to record qdist values
    // and mark nodes as beeing visited
    std::map<std::string, int> visited;

    // Queue to keep track of nodes to process in breadth first search
    // stores a pair denoting an edge from person1 to person2
    std::queue<std::pair<std::string, std::string>> q;

    // Set Quinn's Qdist level as 0 and add all of his immediate friends to the
    // queue
    auto level1 = g.friends_list("Quinn");
    visited["Quinn"] = 0;
    for (auto i : level1)
        q.push({"Quinn", i});

    while (!q.empty()) {
        auto t = q.front();
        q.pop();

        // if we havent already visited this node, then assign its qdist
        // value as one plus its parent nodes value
        // by doing this we are also marking the node as visited
        if (visited.find(t.second) == visited.end())
            visited[t.second] = visited[t.first] + 1;

        // add each of the nodes unvisited children to the queue for processing
        for (auto i : g.friends_list(t.second)) {
            if (visited.find(i) == visited.end())
                q.push({t.second, i});
        }
    }

    // Iterate through the sorted collection of names obtained earlier
    for (auto i : name_set) {
        std::cout << i << " ";
        // if we never visited that name, then that node is unreachable
        // from quinn and they are uncool
        if (visited.find(i) == visited.end())
            std::cout << "uncool";
        else
            std::cout << visited[i];
        std::cout << std::endl;
    }
    return 0;
}
