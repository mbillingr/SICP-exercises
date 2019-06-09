#pragma once
#include "object.h"

void init_memory();
void free_memory();

size_t memory_allocate_pair();
Object* memory_car(size_t idx);
Object* memory_cdr(size_t idx);

Object* fresh_memory_car(size_t idx);
Object* fresh_memory_cdr(size_t idx);

bool collect_garbage();
void gc_reset_root();
void gc_push_root(Object obj);
Object gc_pop_root();

void print_memory(Object* mem, size_t n);
size_t gc_move(size_t from, size_t to);
bool gc_was_moved(size_t addr);
Object gc_relocated_object(size_t addr);
size_t gc_relocate(Object* obj, size_t free);

size_t memory_usage();
