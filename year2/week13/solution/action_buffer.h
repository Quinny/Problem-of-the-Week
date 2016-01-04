#ifndef QAP_ACTION_BUFFER_H
#define QAP_ACTION_BUFFER_H

#include <functional>
#include <queue>
#include <iostream>
#include <mutex>
#include <future>

namespace qap {

// An action is a function which returns nothing and takes nothing.
// generally these would be generated using bind i.e.
// std::bind(fib, 1) will generate the function
//
// void f() {
//     fib(1);
// }
using action = std::function<void()>;

// A thread safe action buffer  A maximum buffer size may be specified in the
// constructor.  Actions will only be executed once the size reaches
// the defined maximum
//
// The main two functions are:
//     - void defer(f)
//          - defers a function and increases the size of the buffer,
//          this could cause a flush if the size reaches a maximum
//
//     - void flush()
//          - flushes the buffer and executes all functions within
class action_buffer {
private:
    std::mutex m_;
    std::queue<action> defered_;
    int waiting_;
    const int reset_;

    // do_* functions assume that a lock has already been grabbed.
    // this means that they can freely call each other without having to
    // worry about race conditions
    void do_defer(const action& f) {
        --waiting_;
        defered_.push(f);
        if (waiting_ == 0)
            do_flush();
    }

    void do_flush() {
        while (!defered_.empty()) {
            defered_.front()();
            defered_.pop();
        }
        waiting_ = reset_;
    }

public:
    action_buffer(int n): waiting_(n), reset_(n) {};

    void defer(const action& f) {
        std::lock_guard<std::mutex> lock(m_);
        do_defer(f);
    }

    void flush() {
        std::lock_guard<std::mutex> lock(m_);
        do_flush();
    }
};

}

#endif
