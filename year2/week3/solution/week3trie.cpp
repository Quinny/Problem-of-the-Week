#include <iostream>
#include <map>
#include <queue>

struct trie_node {
    bool word_end = false;
    std::map<char, trie_node*> children;
};

struct trie {
    trie_node* root;

    trie(): root(new trie_node()) {};

    void insert(const std::string& s) {
        auto cur = root;
        for (auto i: s) {
            auto check = cur->children.insert({i, nullptr});
            if (check.second)
                check.first->second = new trie_node();
            cur = cur->children[i];
        }
        cur->word_end = true;
    }

    void words() {
        std::queue<std::pair<std::string, trie_node*>> q;
        q.push({"", root});
        while (!q.empty()) {
            auto t = q.front();
            q.pop();
            if (t.second->word_end)
                std::cout << t.first << std::endl;

            for (auto i: t.second->children)
                q.push({t.first + i.first, t.second->children[i.first]});
        }
    }

    std::vector<std::string> prefix_matches(std::string s) const {
        auto cur = root;
        std::vector<std::string> ret;
        std::string build;
        for (auto i: s) {
            if (cur->word_end)
                ret.push_back(build);
            if (cur->children.find(i) == cur->children.end())
                break;
            build += i;
            cur = cur->children[i];
        }
        if (s == build && cur->word_end)
            ret.push_back(s);
        return ret;
    }
};

bool is_valid(const trie& t, std::string s) {
    if (s.size() == 0)
        return true;
    auto p = t.prefix_matches(s);
    bool f = false;
    for (auto i: p)
        f = f || is_valid(t, s.substr(i.size()));
    return f;
}

int main() {
    trie t;
    int m;
    std::cin >> m;
    for (int i = 0; i < m; ++i) {
        std::string s;
        std::cin >> s;
        t.insert(s);
    }

    int n;
    std::cin >> n;
    for (int i = 0; i < n; ++i) {
        std::string sentence;
        std::cin >> sentence;
        auto check = is_valid(t, sentence);
        std::cout << check << std::endl;
    }

    return 0;
}
