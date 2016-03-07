#include <iostream>
#include <algorithm>
#include <iterator>
#include <vector>

template <typename T>
T majority_element(T first, T last) {
    auto o_first = first, candidate = first;
    std::size_t count = 1;

    while (++first != last) {
        if (count == 0)
            candidate = first;
        *first == *candidate ? ++count : --count;
    }

    if (std::count(o_first, last, *candidate) > std::distance(o_first, last) / 2)
        return candidate;
    return last;
}

int main() {
    std::vector<std::string> votes {
        std::istream_iterator<std::string>(std::cin),
        std::istream_iterator<std::string>()
    };

    auto me = majority_element(votes.begin(), votes.end());
    if (me != votes.end())
        std::cout << *me << std::endl;
    else
        std::cout << "no majority" << std::endl;

    return 0;
}
