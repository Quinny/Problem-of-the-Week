#include <unordered_map>
#include <iostream>
#include <numeric>

struct pixel {
    int r, g, b, t;

    pixel() = default;
    pixel(int rr, int gg, int bb, int tt): r(rr), g(gg), b(bb), t(tt) {};

    // Compare two pixels based on tolerance
    bool operator == (const pixel& p) const {
        return (r / t == p.r / t) && (g / t == p.g / t) && (b / t == p.b / t);
    }
};

// read a pixel from a stream
std::istream& operator >> (std::istream& is, pixel& p) {
    pixel check;
    is >> check.r >> check.g >> check.b;

    if (is)
        p = std::move(check);
    return is;
}

// write to a stream
std::ostream& operator << (std::ostream& os, const pixel& p) {
    os << p.r << " " << p.g << " " << p.b;
    return os;
}

// specialize hash struct for pixel struct. hash based on the tolerance value,
// we want collisions to happen on similar pixels
namespace std {

template <>
struct hash<pixel> {
    size_t operator ()(const pixel& p) const {
        // knuth magic
        size_t hash = 0x811c9dc5;
        hash = (hash * 0x01000193) ^ (p.r / p.t);
        hash = (hash * 0x01000193) ^ (p.g / p.t);
        hash = (hash * 0x01000193) ^ (p.b / p.t);
        return hash;
    }
};

};

int main() {
    int n, t;
    std::cin >> n >> t;
    std::unordered_map<pixel, int> occur;

    for (int i = 0; i < n; ++i) {
        pixel p;
        std::cin >> p;
        p.t = t;
        ++occur[p];
    }

    // same as reduce in python
    // applies the binary op to each element in the sequence and carrys
    // the result
    std::pair<pixel, int> init(pixel(), 0);
    auto m = std::accumulate(occur.begin(), occur.end(), init,
            [](std::pair<pixel, int> p1, std::pair<pixel, int> p2) {
                return p1.second > p2.second ? p1 : p2;
            }
        );


    std::cout << m.first << std::endl;
    return 0;
}
