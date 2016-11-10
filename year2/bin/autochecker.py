import os
import sys
import subprocess

TEST_DIR = "tests/"

# Check if a test directory exists
def assert_test_dir():
    if not os.path.isdir(TEST_DIR):
        print "tests/ directory required and not found"
        sys.exit(1)

def is_test_file(f):
    return f.endswith(".test")

def is_answer_file(f):
    return f.endswith(".answer")

def prepend_dir(f):
    return TEST_DIR + f

# Get all test files in the tests/ directory
def test_files():
    return map(prepend_dir, filter(
        is_test_file,
        os.listdir(TEST_DIR)
    ))

# get all answer files
def answer_files():
    return map(prepend_dir, filter(
        is_answer_file,
        os.listdir(TEST_DIR)
    ))

# create a test file with the given contents
def make_test(n, contents):
    f = open(TEST_DIR + str(n) + ".test", "w")
    f.write(contents)
    f.close()

def make_answer(n, contents):
    f = open(TEST_DIR + str(n) + ".answer", "w")
    f.write(contents)
    f.close()


'''

All *_handler functions take a file name, perform any compiling nessecary
and then return an array of the commands needed to execute

'''

def cpp_handler(source):
    ret = subprocess.call(["g++", "-std=c++1y", "-O3", source])
    if ret != 0:
        print "Error compiling " + source
        sys.exit(1)
    return ["./a.out"]

def java_handler(source):
    ret = subprocess.call(["javac", source])
    if ret != 0:
        print "Error compiling " + source
        sys.exit(1)
    return ["java", source.split(".")[0]]

def python_handler(source):
    if "python3" in open(source).readline():
        return ["python3", source]
    return ["python", source]

def js_handler(source):
    return ["node", source]

def hs_handler(source):
    ret = subprocess.call(["ghc", "-o", "a.out", source])
    if ret != 0:
        print "Error compiling " + source
        sys.exit(1)
    return ["./a.out"]

def ruby_handler(source):
    return ["ruby", source]

def cs_handler(source):
    ret = subprocess.call(["mcs", source])
    if ret != 0:
        print "Error compiling " + source
        sys.exit(1)
    return ["mono", source.split(".")[0] + ".exe"]

def c_handler(source):
    ret = subprocess.call(["cc", "-O3", source])
    if ret != 0:
        print "Error compiling " + source
        sys.exit(1)
    return ["./a.out"]

def rust_handler(source):
    ret = subprocess.call(["rustc", source, "-o", "a.out", "-O"])
    if ret != 0:
        print "Error compiling " + source
        sys.exit(1)
    return ["./a.out"]

def go_handler(source):
    return ["go", "run", source]

def php_handler(source):
    return ["php", source]

def moon_handler(source):
    return ["moon", source]

# Given a source file, perform compile operations and
# get the run command back
def run_command(source):
    run_handlers = {
        "cpp":  cpp_handler,
        "java": java_handler,
        "py":   python_handler,
        "js":   js_handler,
        "hs":   hs_handler,
        "rb":   ruby_handler,
        "cs":   cs_handler,
        "c":    c_handler,
        "rs":   rust_handler,
        "go":   go_handler,
        "php":  php_handler,
        "moon": moon_handler,
    }
    return run_handlers[source.split(".")[-1]](source)
