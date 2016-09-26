#include <algorithm>
#include <iostream>
#include <vector>

struct Point {
  int x, y;
  Point(int x_, int y_) : x(x_), y(y_){};
};

int squared(int x) { return x * x; }

// Since we don't care about the actual distance only the relative ordering
// between distances, we can skip the expensive call to sqrt.
int distance_squared(const Point& p1, const Point& p2) {
  return squared(p1.x - p2.x) + squared(p1.y - p2.y);
}

// Partially sorts the elements in [first, last) such that:
//   - The nth element in the new range is whatever element would occur
//     in that position if [first, last) was sorted using c (a.k.a. the nth
//     smallest element).
//   - All elements before the nth element are less than or equal too all
//     elements after the nth element.
template <typename Iter, typename Comp>
Iter quick_select(Iter first, Iter last, int n, Comp c) {
  while (true) {
    // The range contains only one element.
    if (first == last) {
      return first;
    }

    auto pivot = *(last - 1);
    // Partition the range on the last element.
    auto pivot_iter = std::partition(
        first, last - 1,
        [&pivot, &c](const decltype(pivot)& v) { return c(v, pivot); });
    std::iter_swap(last - 1, pivot_iter);

    auto index = std::distance(first, pivot_iter);
    // The pivot is in its final sorted position.
    if (index == n) {
      return pivot_iter;
      // The pivot is too large, check the left half of the range.
    } else if (n < index) {
      last = pivot_iter;
      // The pivot is too small.  The left half of the range is in place,
      // so we offset n and move to the right half.
    } else {
      n -= index + 1;
      first = pivot_iter + 1;
    }
  }
}

int main() {
  Point william(0, 0);
  std::cin >> william.x >> william.y;

  int k, n;
  std::cin >> k >> n;

  int x, y;
  std::vector<Point> ladies;
  while (std::cin >> x >> y) {
    ladies.emplace_back(x, y);
  }

  auto distance_from_william_comparator = [&william](const Point& a,
                                                     const Point& b) {
    return distance_squared(a, william) < distance_squared(b, william);
  };

  auto kth_it = quick_select(ladies.begin(), ladies.end(), k - 1,
                             distance_from_william_comparator);

  std::sort(ladies.begin(), kth_it + 1, distance_from_william_comparator);
  for (auto i = ladies.begin(); i != kth_it + 1; ++i) {
    std::cout << i->x << " " << i->y << std::endl;
  }

  return 0;
}
