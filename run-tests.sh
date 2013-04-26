#!/bin/bash
NMS=90-min-scc/90-min-scc.scm
NMSDIR=90-min-scc

function testScm {
    echo "Testing $1"
    $NMS $1.scm > /dev/null && gcc $1.c && ./a.out
    nsc $1.scm
    $1
    echo ""
}

testScm $NMSDIR/test1
testScm $NMSDIR/test2
testScm $NMSDIR/test3

testScm tests/00-simplest
testScm tests/01-simple
testScm tests/02-basic-function
testScm tests/03-top-level-test1

rm -f a.out
