#include <vector>
#include <iostream>
#include <algorithm>
#include <iterator>

struct rgb_pixel {
    int r, g, b;
};

using image = std::vector<std::vector<rgb_pixel>>;

std::istream& operator >> (std::istream& is, rgb_pixel& p) {
    is >> p.r >> p.g >> p.b;
    return is;
}

std::ostream& operator << (std::ostream& os, const rgb_pixel& p) {
    os << p.r << " " << p.g << " " << p.b;
    return os;
}

std::istream& operator >> (std::istream& is, image& im) {
    std::string type;
    int width, height, scale;
    is >> type >> width >> height >> scale;

    image ret;
    for (int i = 0; i < width; ++i) {
        ret.emplace_back();
        for (int j = 0; j < height; ++j) {
            rgb_pixel p;
            is >> p;
            ret[i].push_back(p);
        }
    }
    if (is)
        im = std::move(ret);
    return is;
}

std::ostream& operator << (std::ostream& os, const image& im) {
    os << "P3" << std::endl;
    os << im.size() << " " << im.front().size() << std::endl;
    os << 255 << std::endl;
    for (auto i: im) {
        std::copy(
                i.begin(),
                i.end(),
                std::ostream_iterator<rgb_pixel>(std::cout, "\n")
        );
    }
    return os;
}

rgb_pixel grayscale(rgb_pixel x) {
    int avg = (x.r + x.g + x.b) / 3;
    return {avg, avg, avg};
}

image to_grayscale(image im) {
    std::transform(im.begin(), im.end(), im.begin(),
            [&](std::vector<rgb_pixel>& v) {
                std::transform(v.begin(), v.end(), v.begin(), grayscale);
                return v;
            });
    return im;
}

int main() {
    image im;
    std::cin >> im;
    auto new_image = to_grayscale(im);
    std::cout << new_image;
    return 0;
}
