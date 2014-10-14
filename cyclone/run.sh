#!/bin/bash
#echo "(display ((lambda (x) (* x x)) 10))" | huski compiler.scm > 100.c && gcc 100.c -o 100

CC=./compiler
#CC=huski compiler.scm
if [[ $# -eq 0 ]] ; then
    echo 'usage: run.sh file.scm'
    exit 0
fi

filename=$(basename "$1")
filename="${filename%.*}"

$CC < $1 > $filename.out \
  && cat mta/runtime-header.c > $filename.c \
  && cat $filename.out >> $filename.c \
  && cat mta/runtime-footer.c >> $filename.c \
  && gcc $filename.c -o $filename && ./$filename && rm -f $filename.out
