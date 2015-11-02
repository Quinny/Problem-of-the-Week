#include <iostream>

struct node {
    bool taken;
    int value;

    node(): taken{false}, value{0} {};
};

class hashset {
private:
    // array of nodes
    node* table_;
    int size_;

    int hash_fn(int v) {
        return v % size_;
    }
public:
    hashset(int size) {
        table_ = new node[int(size * 1.6)];
        size_ = int(size * 1.6);
    }


    bool insert(int v) {
        int h = hash_fn(v);
        // loop while the spot is taken
        while (table_[h].taken) {
            // if the value is already found return false
            if (table_[h].value == v)
                return false;
            // linear probe ahead one space
            h = hash_fn(h + 1);
        }
        table_[h].taken = true;
        table_[h].value = v;
        return true;
    }

    ~hashset() {
        delete[] table_;
    }
};

int main() {

    int n;
    std::cin >> n;
    hashset hs(n);
    for (int i = 0; i < n; ++i) {
        int tmp;
        std::cin >> tmp;
        if (!hs.insert(tmp))
            std::cout << tmp << std::endl;
    }
}
