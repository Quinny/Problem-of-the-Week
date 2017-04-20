#include <algorithm>
#include <iostream>
#include <unordered_map>
#include <vector>

// Calculates the majority element of a stream in constant time using the boyer-
// moore majority vote algorithm.
template <typename T>
class StreamingMajorityVote {
 public:
  StreamingMajorityVote() : candidate_{}, votes_(0) {}

  void consume(const T& v) {
    v == candidate_ ? ++votes_ : --votes_;
    if (votes_ <= 0) {
      candidate_ = v;
      votes_ = 0;
    }
  }

  const T& candidate() const { return candidate_; }

 private:
  T candidate_;
  int votes_;
};

template <typename T>
std::unordered_map<T, int> histogram(const std::vector<T>& v) {
  std::unordered_map<T, int> h;
  for (const auto& i : v) {
    ++h[i];
  }
  return h;
}

int main() {
  std::ios_base::sync_with_stdio(false);

  int n_districts, n_votes;
  std::cin >> n_districts >> n_votes;

  // Calculate the majority vote for each district.
  std::vector<StreamingMajorityVote<std::string>> districts(n_districts);
  int district;
  std::string candidate;
  while (std::cin >> candidate >> district) {
    districts[district].consume(candidate);
  }

  // Grab each candidates name.
  std::vector<std::string> district_winners(n_districts);
  std::transform(districts.begin(), districts.end(), district_winners.begin(),
                 [](const auto& d) { return d.candidate(); });

  // Count how many districts each candidate won and output the one who took
  // the most.
  const auto tally = histogram(district_winners);
  const auto winner = std::max_element(
      tally.begin(), tally.end(),
      [](const auto& lhs, const auto& rhs) { return lhs.second < rhs.second; });

  std::cout << winner->first << std::endl;
}
