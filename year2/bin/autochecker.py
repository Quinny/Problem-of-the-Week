import os
import sys
import subprocess

TEST_DIR = "tests/"

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

def test_files():
    return map(prepend_dir, filter(
        is_test_file,
        os.listdir(TEST_DIR)
    ))

def answer_files():
    return map(prepend_dir, filter(
        is_answer_file,
        os.listdir(TEST_DIR)
    ))

def make_test(n, contents):
    f = open(TEST_DIR + str(n) + ".test", "w")
    f.write(contents)
    f.close()

def make_answer(n, contents):
    f = open(TEST_DIR + str(n) + ".answer", "w")
    f.write(contents)
    f.close()

def cpp_handler(source):
    ret = subprocess.call(["g++", "-std=c++1y", source])
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

def run_command(source):
    run_handlers = {
        "cpp":  cpp_handler,
        "java": java_handler,
        "py":   python_handler,
        "js":   js_handler,
        "hs":   hs_handler,
        "rb":   ruby_handler
    }
    return run_handlers[source.split(".")[-1]](source)
