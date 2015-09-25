#include <iostream>
#include <unordered_set>

using Dictionary = std::unordered_set<std::string>;

bool is_valid(std::string letters, const Dictionary& d) {
    if (letters.size() == 0)
        return true;

    std::string tmp;
    for (std::size_t i = 0; i < letters.size(); ++i) {
        tmp += letters[i];
        if (d.find(tmp) != d.end()) {
            auto check = is_valid(letters.substr(i + 1), d);
            if (check) return true;
        }
    }
    return false;
}

int main() {
    Dictionary d;
    int n;
    std:: cin >> n;
    for (int i = 0; i < n; ++i) {
        std::string s;
        std::cin >> s;
        d.insert(s);
    }
    int m;
    std::cin >> m;
    for (int i = 0; i < m; ++i) {
        std::string sentence;
        std::cin >> sentence;
        std::cout << is_valid(sentence, d) << std::endl;
    }

    return 0;
}
