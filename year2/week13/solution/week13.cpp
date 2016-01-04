#include "action_buffer.h"
#include <iostream>
#include <random>

void print_s(std::ostream& out, std::string s) {
    out << s << std::endl;
}

std::future<void> append_image(std::string s, qap::action_buffer& ab) {
    return std::async([s, &ab]() {
            // simulate loading of the image
            std::this_thread::sleep_for(std::chrono::seconds(rand() % 3));
            ab.defer(std::bind(print_s, std::ref(std::cout), s));
        });
}

int main() {
    srand(time(NULL));
    std::vector<std::string> urls = {
        "http://google.com",
        "http://yahoo.com",
        "http://bighouse.com",
        "http://yaya.com",
        "http://avvo.com",
        "http://quinnftw.com",
        "http://duder.com",
        "http://myman.com",
        "http://eh.com",
        "http://ehhhh.com",
    };

    qap::action_buffer ab(5);
    std::vector<std::future<void>> v;
    for (auto url: urls)
        v.push_back(append_image(url, ab));
    return 0;
}
