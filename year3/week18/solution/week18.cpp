#include <algorithm>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>

template <typename T>
int intersection_size(const std::unordered_set<T>& s1,
                      const std::unordered_set<T>& s2) {
  int ret = 0;
  for (const auto& e : s1) {
    ret += s2.find(e) != s2.end();
  }
  return ret;
}

class Roommate {
 public:
  const std::string name;
  const std::unordered_set<std::string> activities;

  Roommate(const std::string& name,
           const std::unordered_set<std::string>& activities)
      : name(name), activities(activities) {}

  double jaccard_similarity(const Roommate& other) const {
    int intersection = intersection_size(activities, other.activities);
    int union_size = activities.size() + other.activities.size() - intersection;
    return intersection / static_cast<double>(union_size);
  }
};

int main() {
  int n_people;
  std::cin >> n_people;
  std::cin.ignore(1, '\n');

  std::vector<Roommate> people;
  for (std::string line; std::getline(std::cin, line); /* empty */) {
    std::istringstream ss(line);

    std::string name;
    ss >> name;

    std::unordered_set<std::string> activities(
        std::istream_iterator<std::string>{ss},
        std::istream_iterator<std::string>{});

    people.emplace_back(name, activities);
  }

  const auto& dave = people.front();
  const auto most_compatible = std::max_element(
      people.begin() + 1, people.end(),
      [&dave](const auto& p1, const auto& p2) {
        return p1.jaccard_similarity(dave) < p2.jaccard_similarity(dave);
      });

  std::cout << most_compatible->name << std::endl;
}
