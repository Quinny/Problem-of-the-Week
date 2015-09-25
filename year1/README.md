Problem of the week
===============

At the University of Windsor a new problem for CS students to solve is posted each week.  These are my solutions.

Week 1
=======

Given a pattern such as "abba", and a string such as "redbluebluered",
Indicate whether or not the string matches the given pattern

Note that each element of the pattern must match to a distinct string hence, redbluebluered" would not match "aaaa"

Expected runtime: O(2^n)

Week 2
=======

Given 2 sorted arrays, find the median between the 2 as if they were merged.
Note that the arrays can have different lengths

Expected runtime: O(log(m + n))

Week 3
=======

Given a list of m integers, find all pairs that sum to an integer n, and display them in ascending order according to the first number in the pair.

Expected runtime: O(n) for generating pairs, O(nlogn) for sorting them

Week 4
======
Two characters a and b are said to be adjacent if they differ by exactly 1 bit
ex. 1001 and 1011 are adjacent, where as 1001 and 0101 are not.

Given 2 characters a and b, check whether they are adjacent
Note if a and b are the same, they are NOT adjacent

Expected runtime: O(1)

Week 5
======

Given two sorted arrays, find the kth smallest element between them as if theywere merged.
     
Note that no duplicates will occur, and the arrays may have different sizes
     
Expected runtime: O(log(n) + log(m))

Week 6
=======

Given a list of integers such that the i'th element represents the price of a stock on the i'th day, output the maximum profit that can be made over the time period.
     
Note that you may only by 1 unit at a time, and must sell that unit in order to buy again.
     
Expected runtime: O(n)

Week 7
======

Given a list of integers where every element has a duplicate except one,find the non duplicated element.
     
Expected runtime: O(n)
Expected memory:  O(1)

Week 8
========

Given a list of weights of stones where all stones are the same weight except one that is heavier, find the index of that heavy stone.

The only function which can be used to compare stones is "heavier", which takes in two sets of iterators and returns 1 if the stones in the first set are heavier, 2 if the stones in the second set are heavier, and 0 if the weights are the same.

Week 9
===========

Given the heads of 2 singly linked lists, and the fact that they intersect at some point, find that point of intersection.

Expected Runtime: O(n)
Expected Memory:  O(1)
