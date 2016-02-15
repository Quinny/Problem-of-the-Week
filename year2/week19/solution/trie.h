#ifndef TRIE_H
#define TRIE_H

#include <type_traits>
#include <vector>
#include <string>
#include <list>
#include <forward_list>
#include <map>
#include <queue>

namespace qp {

    // Traits for identifying containers which can be used in a trie
    // Any sequence container can be used, which will be defined by the
    // is_sequence_container trait.
    // I may have missed a few, but you get the point.

    namespace traits {

    using True  = std::integral_constant<bool, true>;
    using False = std::integral_constant<bool, false>;

    // If none of the below patterns match, then "return" false
    template <typename T>
    struct is_sequence_container {
        using type = False;
    };

    // All containers which _will_ be storeable in our trie
    template <typename T>
    struct is_sequence_container<std::vector<T>> {
        using type = True;
    };

    template <typename T>
    struct is_sequence_container<std::basic_string<T>> {
        using type = True;
    };

    template <typename T>
    struct is_sequence_container<std::list<T>> {
        using type = True;
    };

    template <typename T>
    struct is_sequence_container<std::forward_list<T>> {
        using type = True;
    };

} // Traits namespace


template <typename>
struct trie_iterator;

// Node of a trie
template <typename T>
struct trie_node {
    bool end;
    std::map<T, trie_node<T>*> children;
};

// templated templates, this allows us to do stuff like
// trie<std::vector, int> to create a trie which stores
// vectors of ints
template <template <typename... Args> class Container, typename... Args>
class trie {

// Assert at compile time that the container specified is one that can be
// stored in a trie
static_assert(traits::is_sequence_container<Container<Args...>>::type::value,
        "Container must be a sequence container");

public:
    using container  = Container<Args...>;
    using value_type = typename container::value_type;
    using iterator   = trie_iterator<value_type>;

private:
    trie_node<value_type>* root;

public:
    trie(): root(new trie_node<value_type>()) {};

    // Insert elements in a range
    template <typename T>
    trie(T first, T last): root(new trie_node<value_type>()) {
        for (auto i = first; i != last; ++i)
            insert(*i);
    }

    // Insert a sequence container into the trie
    void insert(const container& c) {
        auto cur = root;
        for (auto i: c) {
            // check if an edge labeled "i" already exists
            auto check = cur->children.insert({i, nullptr});
            // if the insert was successful, then the edge did not exist
            // and it must be created
            if (check.second)
                check.first->second = new trie_node<value_type>();
            cur = cur->children[i];
        }
        cur->end = true;
    }

    trie_iterator<value_type> begin() {
        return trie_iterator<value_type>(root);
    }

    trie_iterator<value_type> end() {
        return trie_iterator<value_type>();
    }

    ~trie() {
        std::queue<trie_node<value_type>*> q;
        q.push(root);
        while (!q.empty()) {
            auto t = q.front(); q.pop();
            for (auto i: t->children)
                q.push(i.second);
            delete t;
        }
    }
};

// iterate through a trie
// As it stands, this iterator is kind of useless since
// it doesnt provide any sort of path, you only get the nodes
// in a level order manner.  Providing a path in the specified container
// at this point would be easy though, since we allow only containers
// which support push_back to be stored
template <typename T>
struct trie_iterator {
    std::queue<trie_node<T>*> q;

    trie_iterator(trie_node<T>* cur = nullptr) {
        q.push(cur);
    }

    bool operator == (const trie_iterator& rhs) const {
        return q.front() == rhs.q.front();
    }

    bool operator != (const trie_iterator& rhs) const {
        return q.front() != rhs.q.front();
    }

    trie_node<T> operator * () {
        return *(q.front());
    }

    trie_iterator& operator ++ () {
        if (q.front() == nullptr)
            return *this;

        auto tmp = q.front(); q.pop();
        for (auto i: tmp->children)
            q.push(i.second);
        if (q.empty())
            q.push(nullptr);
        return *this;
    }
};


} // qp namespace

// std::any_of requires iterators to have a category
namespace std {
    template<typename T>
    struct iterator_traits<qp::trie_iterator<T>> {
        using iterator_category = std::forward_iterator_tag;

    };
}
#endif /* TRIE_H */
