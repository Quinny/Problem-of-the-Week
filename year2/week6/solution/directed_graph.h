#include <map>
#include <set>
#include <queue>
#include <iostream>

// A directed graph implementation for contest use
// Members are intentionally left public for ease of use in
// contest style programming

template <typename T>
struct directed_graph {
    // maps vertex to set of neighbour nodes
    std::map<T, std::set<T>> g_;
    // keeps track of the number of incoming edges for a given vertex
    // useful for topological sort
    std::map<T, std::size_t> incoming_count_;

    void connect(const T& u, const T& v) {
        // insure each vertex is represented in the incoming map
        incoming_count_.insert({u, 0});
        g_[u].insert(v);
        ++incoming_count_[v];
    }

    void disconnect(const T& u, const T& v) {
        g_[u].erase(v);
        --incoming_count_[v];
    }

    std::set<T> operator[] (const T& u) {
        return g_[u];
    }
};
