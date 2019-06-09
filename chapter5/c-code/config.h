#pragma once
#include <stdio.h>
#include <stdlib.h>

#define MEMORY_SIZE 33554432  // 1 GB, assuming an object size of 16 bytes
#define RESERVED_FOR_GC 10  // reserve a few storage slots for use in the GC
#define INPUT_BUFFER_SIZE 4096
#define TOKEN_BUFFER_SIZE 128
#define SYMBOL_BUFFER_SIZE 40960

void error(const char* msg);
