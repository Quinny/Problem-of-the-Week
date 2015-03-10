/*
 * Given a list of weights of stones where all stones are the same weight
 * except one is heavier, find the index of the heaviest stone.
 *
 * The only function which can be used to compare stones is heavier, which takes
 * two sets of iterators and returns 1 if the stones in the first set are heavier,
 * 2 if the stones in the second set are heavier, and 0 if they are the same
 */


#include <iostream>
#include <numeric>
#include <iterator>
#include <vector>

template <typename T>
int heavier(T first1, T last1, T first2, T last2);
template <typename T>
T heavy_marble(T first, T last);
template <typename T>
std::pair<T, T> third(T first, T last);

template <typename T>
int heavy_marble_index_to_make_nusko_happy(T first, T last);

int main() {
    std::vector<int> marbles {1, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1};
    auto i = heavy_marble_index_to_make_nusko_happy(marbles.begin(), marbles.end());
    std::cout << i << std::endl;
    return 0;
}

template <typename T>
int heavy_marble_index_to_make_nusko_happy(T first, T last) {
    auto i = heavy_marble(first, last);
    return std::distance(first, i);
}

template <typename T>
std::pair<T, T> thirds(T first, T last) {
    auto size = std::distance(first, last);
    return std::make_pair(first + (size / 3), first + (2 * size / 3));
}

template <typename T>
T heavy_marble(T first, T last) {
    auto size = std::distance(first, last);
    if (size == 1)
        return first;
    auto splits = thirds(first, last);
    auto r = heavier(first, splits.first, splits.first, splits.second);
    if (r == 0)
        return heavy_marble(splits.second - 1, last);
    if (r == 1)
        return  heavy_marble(first, splits.first);
    else
        return heavy_marble(splits.first, splits.second);
}

template<typename T>
int heavier(T first1, T last1, T first2, T last2) {
    auto a_w = std::accumulate(first1, last1, 0);
    auto b_w = std::accumulate(first2, last2, 0);
    if (a_w > b_w)
        return 1;
    if (b_w > a_w)
        return 2;
    return 0;
}


