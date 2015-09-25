#!/usr/bin/env python

import autochecker
import sys
import subprocess

autochecker.assert_test_dir()
answer_source = sys.argv[1]

cmd = autochecker.run_command(answer_source)

for f in autochecker.test_files():
    print f
    output = subprocess.check_output(cmd, stdin=open(f))
    autochecker.make_answer(f.split("/")[1].split(".")[0], output)
