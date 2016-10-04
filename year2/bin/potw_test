#!/usr/bin/env python

import os
import autochecker
import subprocess
import sys
from difflib import Differ
from datetime import datetime, timedelta

autochecker.assert_test_dir()

tests = autochecker.test_files()
answers = autochecker.answer_files()
run_command = autochecker.run_command(sys.argv[1])
skip_verification = sys.argv[2] == "nocheck" if len(sys.argv) == 3 else False
total_run_time = timedelta()

for i in range(len(tests)):
    print "running test " + str(i) + "/" + str(len(tests))
    if skip_verification:
        print "Skipping check"

    before = datetime.now()
    output = subprocess.check_output(run_command, stdin=open(tests[i]))
    after = datetime.now()
    total_run_time += after - before

    expected = open(answers[i]).read()
    if output.strip() != expected.strip() and not skip_verification:
        print "Failed"

        d = Differ()
        diff = list(d.compare(output.strip().splitlines(1),
            expected.strip().splitlines(1)))
        print "".join(diff)

        sys.exit(1)

print "Passed!"
print "Total runtime in seconds: " + str(total_run_time.total_seconds())
