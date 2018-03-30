#!/bin/bash

clang -Xclang -fsyntax-only -Xclang -load -Xclang ./KDFI.so -Xclang -plugin -Xclang KDFI -c gzipsrc/bits.c -o test.txt
mv testout.txt gzipanalysis/bits.txt
