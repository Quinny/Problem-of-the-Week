#include <iostream>
#include <vector>
#include <functional>
#include <algorithm>
#include <random>

const int mutation_rate = 30;

// Hamming distance between two strings, number of differing characters
std::size_t hamming_distance(std::string cur, std::string target) {
    std::size_t ret = 0;
    for (auto i = 0UL; i < cur.size(); ++i) {
        if (cur[i] == target[i])
            ++ret;
    }
    return ret;
}

// Generate uniform random integral within a range
template <typename T>
T random_range(T first, T last) {
    static std::default_random_engine e;
    std::uniform_int_distribution<T> d(first, last);
    return d(e);
}

// Mutate a string by randomly changing its characters at a certain
// probability
std::string mutate(std::string s) {
    for (auto i = 0UL; i < s.size(); ++i) {
        if (random_range(1, 100) < mutation_rate)
            s[i] = random_range(' ', '~');
    }
    return s;
}

// Evolve a string to a target and show the successor from each
// population
template <typename Species, typename MutateFn, typename FitnessFn>
void evolve(Species current, Species target, MutateFn mutate, FitnessFn fitness) {
    while (current != target) {
        std::cout << current << std::endl;
        std::vector<Species> next_generation;
        for (int i = 0; i < 100; ++i)
            next_generation.push_back(mutate(current));

        auto successor = std::max_element(next_generation.begin(), next_generation.end(),
                [&](const Species& a, const Species& b) { return fitness(a) < fitness(b); });
        current = *successor;
    }
}

int main() {
    std::string target;
    std::cin >> target;

    std::string init;
    for (auto i =  0UL; i < target.size(); ++i)
        init += random_range(' ', '~');

    evolve<std::string>(
        init, target, mutate,
        [&] (std::string s) { return hamming_distance(s, target); }
    );
    std::cout << target << std::endl;
}
