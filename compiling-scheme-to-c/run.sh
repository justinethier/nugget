#!/bin/bash

# does not work
echo "(display ((lambda (x) (* x x)) 10))" | huski scheme-to-c.scm

# WIP
csi scheme-to-c.scm < tests/square.scm > out.c
