# uWindsor Problem of the Week

This repository contains the tools used to create problem of the week test cases
and validate solutions.  It also contains solutions to the previous year's problems.

# Using the tools

Within the `bin` folder you will find the utilities used to help automate POTW
related tasks.  I would suggest adding this folder to your `PATH` so you can
use these tools from anywhere.

## Conventions

All POTW tools expect a folder called `tests` in the current working directory.
Test cases should be placed in this `tests` folder with the input contained
in a file named `n.test`, and the corresponding expected output in `n.answer`
(where `n` is the test case number). [See year1 week1 for an example of this structure](https://github.com/Quinny/Problem-of-the-Week/tree/master/year2/week1/tests).

## potw_make_answers

This tool takes the solution program as a command line argument, and generates
the expected output files for the test input files located in the `tests` directory.
For example, if your tests folder had files

<pre>
0.test
1.test
2.test
</pre>


Following the execution of `potw_make_answers solution.py` the folder would contain

<pre>
0.test
0.answer
1.test
1.answer
2.test
2.answer
</pre>

Where the contents of the newly generated answer files are the output of the
provided program when given the test files as input.

## potw_test

Runs a candidate solution against all test cases and verifies that the output
matches the expected.  The candidate solution is passed via a command line argument.
Some POTW's could not be easily checked (ambiguous output, probabilistic algoritms,
etc.), therefore an additional `nocheck` flag may be passed to the program which
will skip the verification phase but still report runtime.  For example:

`potw_test candidate.py` - Run with verification

`potw_test candidate.py nocheck` - Run but don't verify output

## potw_runtime

Runs the candidate solution on all test cases 10 times and returns the average
runtime.  This is used to help curb any variance that may arise in only
running the program once.  Note that this tool does not perform verification
in the interest of speed, so it should be executed *after* `potw_test`.

`potw_runtime candidate.py`

## Adding a new language/changing compilers

The logic for compiling and running the provided program can found in
[autochecker.py](https://github.com/Quinny/Problem-of-the-Week/blob/master/bin/autochecker.py).
The `run_command` function contains a map from file extension to language
handler function.  Each language handler function performs the necessary
compilation work, and the returns an array of the commands needed to execute
the produced program (the command is return as an array as a product of how
the python process API works).  Reading a few of the handler functions should
give you a pretty good idea how they operate.
