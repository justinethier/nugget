#!/bin/bash
echo "(display ((lambda (x) (* x x)) 10))" | huski jae.scm > 100.c && gcc 100.c -o 100

# WIP
huski jae.scm < tests/square.scm > out.c
