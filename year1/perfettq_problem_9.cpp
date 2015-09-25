/*
 * Given the head of 2 singly linked lists, and the fact that they are gaurunteed
 * to intersect at some point, find that point of intersection.
 *
 * Expected Runtime: O(n)
 * Expected Memory:  O(1)
 */

#include <iostream>
#include <math.h>

struct node {
    int datum_;
    node* next_;
    node(int n = 0, node* nex = nullptr) : datum_(n), next_(nex) {};
};

int len(node* head);
node* intersect_point(node* first1, node* first2);

int main() {
    node* inter = new node(18);
    
    node head1;
    head1.next_ = new node(5);
    head1.next_->next_ = new node(6);
    head1.next_->next_->next_ = inter;

    node head2;
    head2.next_ = new node(8);
    head2.next_->next_ = new node(9);
    head2.next_->next_->next_ = new node(11);
    head2.next_->next_->next_->next_ = inter;
    
    auto x = intersect_point(&head1, &head2);
    std::cout << x->datum_ << std::endl;
    return 0;
}

int len(node* head) {
    int i = 0;
    while (head != nullptr) {
        head = head->next_;
        ++i;
    }
    return i;
}

node* intersect_point(node* first1, node* first2) {
    int len1 = len(first1);
    int len2 = len(first2);

    if (len1 > len2) {
        for (int i = 0; i < abs(len1 - len2); ++i)
            first1 = first1->next_;
    }
    else {
        for (int i = 0; i < abs(len1 - len2); ++i)
            first2 = first2->next_;
    }
    while (first1 != first2) {
        first1 = first1->next_;
        first2 = first2->next_;
    }
    return first1;
}
