// There is a lot of functional flare intentionally added to this
// solution.  Functional techniques are important and should be at least
// understood.  An excellent guide / tutorial can be found here:
//
// https://github.com/MostlyAdequate/mostly-adequate-guide


// "class" for generating a prime sieve
// basically a bool array where a[i] => true denotes i is prime
// and a[i] => false denotes i is not prime
//
// https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

function primeSieve(n) {
    var self = this;

    // assume all numbers are prime other than 0 and 1
    var sieve = [];
    for (var i = 0; i < n; ++i)
        sieve.push(true);
    sieve[0] = false; sieve[1] = false;

    var prime = 2;
    // by taking i < sqrt(n) and squaring both sides we get
    // i * i < n.  i * i is much faster to compute than sqrt(n)
    for (var i = 0; i * i < n; ++i) {
        // elimate all multiples of our current prime
        eliminateMultiples(prime);
        // find the next one
        prime = nextPrime(prime);
    }

    function eliminateMultiples(prime) {
        // we can start at prime * prime
        // as the prevous prime will have handled
        // everything up to prime * prime - 1
        for (var i = prime * prime; i < n; i += prime)
            sieve[i] = false;
    }

    function nextPrime(prime) {
        // find the next spot in the array that is true
        while (prime < n && !sieve[++prime]);
        return prime;
    }


    this.isPrime = function(n) {
        return sieve[n];
    }

    // fires the callback for each prime number in the sieve
    // also passes a reference to self (may be ignored)
    this.forEach = function(callback) {
        sieve.forEach(function(v, i) {
            if (v) callback(i, self);
        });
    }
}

// count the occurances of each digit in a number
function occuranceCount(n) {
    ret = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    while (n > 0) {
        ++ret[n % 10];
        n = Math.floor(n / 10);
    }
    return ret;
}

// This technique is called function currying.
// https://github.com/MostlyAdequate/mostly-adequate-guide/blob/master/ch4.md
//
// The function allows for partial arugments and returns a new function which
// will consume and apply the rest

function isPermutation(n1) {
    var o1 = occuranceCount(n1);
    return function(n2) {
        var o2 = occuranceCount(n2);
        return o1.every(function(d, i) {
            return o2[i] == d;
        });
    }
}

// Logical and two predicate functions f and g
function and(f, g)  {
    return function() {
        return f.apply(null, arguments) && g.apply(null, arguments);
    }
}

function increase(s, m) {
    return function(i) {
        return s + (i * m);
    }
}

function check(n, s) {
    var c = [1, 2]
        .map(increase(n, 3330))
        .every(and(s.isPrime, isPermutation(n)))

    if (c)
        console.log(n, n + 3330, n + 6660);
}

function main() {
    s = new primeSieve(10000);
    s.forEach(check);
}

main();
