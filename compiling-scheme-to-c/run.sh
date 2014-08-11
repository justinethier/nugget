#!/bin/bash

# does not work
echo "(display ((lambda (x) (* x x)) 10))" | huski scheme-to-c.scm > 100.c && gcc 100.c -o 100

# WIP
huski scheme-to-c.scm < tests/square.scm > out.c
