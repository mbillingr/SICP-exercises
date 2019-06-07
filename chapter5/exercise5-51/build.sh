#!/bin/bash

gcc -O2 $1.c -o $1 memory.c object.c stack.c parser.c scheme.c
