#!/bin/bash

clang -Xclang -fsyntax-only -Xclang -load -Xclang ./KDFI.so -Xclang -plugin -Xclang KDFI -c vul.c -o vul.txt
#mv testout.txt gzipanalysis/bits.txt
#clang -Xclang -fsyntax-only -Xclang -load -Xclang ./KDFI.so -Xclang -plugin -Xclang KDFI -c /home/kevin/spec2000-all/benchspec/CINT2000/176.gcc/src/varasm.c -o test.txt
#mv testout.txt gzipanalysis/inflate.txt
