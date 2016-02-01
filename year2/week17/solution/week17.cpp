#include <iostream>
#include <map>
#include <queue>
#include <vector>
#include <functional>

template <typename T>
using histogram = std::map<T, std::size_t>;

template <typename T>
struct huffman_node {
    T data;
    std::size_t frequency;
    std::vector<huffman_node> children;

    huffman_node(const T& d, std::size_t f)
        : data(d), frequency(f) {}

    huffman_node(std::initializer_list<huffman_node> init): frequency(0) {
        children = init;
        for (auto i: init)
            frequency += i.frequency;
    }

    bool operator < (const huffman_node& rhs) const {
        return frequency > rhs.frequency;
    }
};

template <typename T>
histogram<T> build_table(std::istream& is) {
    T token;
    histogram<T> ret;
    while (is >> token)
        ++ret[token];
    return ret;
}

template <typename T>
huffman_node<T> build_tree(std::istream& is) {
    auto table = build_table<T>(is);
    std::priority_queue<huffman_node<T>> pq;

    for (auto i: table)
        pq.push(huffman_node<T>(i.first, i.second));

    while (pq.size() > 1) {
        auto n1 = pq.top(); pq.pop();
        auto n2 = pq.top(); pq.pop();

        huffman_node<T> parent{n1, n2};
        pq.push(parent);
    }
    return pq.top();
}

template <typename T>
using callback_fn = std::function<void(T, std::string)>;

template <typename T>
void traverse(huffman_node<T> root, std::string s, callback_fn<T> cb) {
    if (root.children.empty())
        cb(root.data, s);
    else {
        traverse(root.children[0], s + "0", cb);
        traverse(root.children[1], s + "1", cb);
    }
}

template <typename T>
void traverse(huffman_node<T> root, callback_fn<T> cb) {
    traverse(root, "", cb);
}

int main() {
    auto tree = build_tree<std::string>(std::cin);
    auto cb = [](std::string d, std::string c) {
        std::cout << d << " " << c << std::endl;
    };
    traverse<std::string>(tree, cb);
    return 0;
}
