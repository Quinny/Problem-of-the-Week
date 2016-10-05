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
total_run_time = timedelta()
times_to_run = 10

for t in range(times_to_run):
    for i in range(len(tests)):
        before = datetime.now()
        output = subprocess.check_output(run_command, stdin=open(tests[i]))
        after = datetime.now()
        total_run_time += after - before

print "Average runtime in seconds: " +\
        str(total_run_time.total_seconds() / times_to_run)
