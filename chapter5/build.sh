#!/bin/bash

gcc -O2 $1.c -o $1 -I c-code c-code/memory.c c-code/object.c c-code/stack.c c-code/parser.c c-code/scheme.c
