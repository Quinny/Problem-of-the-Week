#include <iostream>
#include <map>
#include <set>
#include <vector>
#include <iterator>
#include <sstream>
#include <algorithm>
#include <cassert>

using coverage_map = std::map<std::string, std::set<int>>;

std::vector<std::string> split_space(const std::string& s) {
    std::istringstream ss(s);
    return std::vector<std::string> {
        std::istream_iterator<std::string>(ss),
        std::istream_iterator<std::string>()
    };
}

void mark_coverage(coverage_map& cm,
                   const std::string& word,
                   const std::vector<std::string>& sentences) {
    auto it = cm.find(word);
    if (it != cm.end()) return;

    it = cm.insert({word, std::set<int>()}).first;
    for (auto i = 0UL; i < sentences.size(); ++i) {
        if (sentences[i].find(word) != std::string::npos)
            it->second.insert(i);
    }
}

void invalidate(coverage_map& m, const std::set<int>& inv) {
    for (auto& i: m) {
        for (const auto& e: inv) {
            i.second.erase(e);
        }
    }
}

bool coverage_compare(const coverage_map::value_type& x,
                      const coverage_map::value_type& y) {
    return x.second.size() < y.second.size();
}

int main() {
    std::vector<std::string> sentences;

    for (std::string s; std::getline(std::cin, s); /* nothing */)
        sentences.emplace_back(s);

    coverage_map m;
    for (const auto& sentence: sentences) {
        auto words = split_space(sentence);
        for (const auto& word: words) {
            if (word.size() >= 5)
                mark_coverage(m, word, sentences);
        }
    }

    std::size_t covered = 0, n = 0;

    while (covered < sentences.size()) {
        auto max = std::max_element(m.begin(), m.end(), coverage_compare);
        covered += max->second.size();
        invalidate(m, max->second);
        //std::cout << max->first << std::endl;
        ++n;
    }
    std::cout << n << std::endl;
    return 0;
}
