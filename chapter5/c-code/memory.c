#include "memory.h"

#include "config.h"

static Object *fresh_memory;
static Object *list_memory;
static size_t free_ptr = RESERVED_FOR_GC;
static size_t reserved_ptr = 1;
static Object gc_root = NIL;

void init_memory() {
    printf("Object size: %d Bytes\n", sizeof(Object));
    printf("Reserving 2x %d MB of list memory.\n",
           (MEMORY_SIZE * 2 * sizeof(Object))/(1024*1024));

    fresh_memory = malloc(MEMORY_SIZE * 2 * sizeof(Object));
    list_memory = malloc(MEMORY_SIZE * 2 * sizeof(Object));
}


void free_memory() {
    free(fresh_memory);
    free(list_memory);
}

Object* memory_car(size_t idx) {
    return &list_memory[idx * 2];
}

Object* memory_cdr(size_t idx) {
    return &list_memory[idx * 2 + 1];
}

Object* fresh_memory_car(size_t idx) {
    return &fresh_memory[idx * 2];
}

Object* fresh_memory_cdr(size_t idx) {
    return &fresh_memory[idx * 2 + 1];
}

size_t memory_allocate_pair() {
    if(free_ptr >= MEMORY_SIZE) {
        error("out of memory");
    }
    return free_ptr++;
}

void print_memory(Object* mem, size_t n) {
    for(int i=0; i<n; i++) {
        printf("|%4d", i);
    }
    printf("\n");

    for(int i=0; i<n; i++) {
        Object o = mem[i*2];
        switch(o.tag) {
            case BrokenHeart:printf("|----"); break;
            case Nil:printf("| () "); break;
            case Number: printf("|%1.2f", o.number); break;
            case Pointer: printf("|%4d", o.ptr); break;
            default: printf("| ?? "); break;
        }
    }
    printf("\n");

    for(int i=0; i<n; i++) {
        Object o = mem[i*2 + 1];
        switch(o.tag) {
            case BrokenHeart:printf("|----"); break;
            case Nil:printf("| () "); break;
            case Number: printf("|%1.2f", o.number); break;
            case Pointer: printf("|%4d", o.ptr); break;
            default: printf("| ?? "); break;
        }
    }
    printf("\n");
}

size_t gc_move(size_t from, size_t to) {
    *fresh_memory_car(to) = *memory_car(from);
    *fresh_memory_cdr(to) = *memory_cdr(from);
    *memory_car(from) = broken_heart();
    *memory_cdr(from) = pointer(to);
    return to;
}

bool gc_was_moved(size_t addr) {
    return memory_car(addr)->tag == BrokenHeart;
}

Object gc_relocated_object(size_t addr) {
    if(!gc_was_moved(addr))
        error("trying to get new address of unmoved object");
    return *memory_cdr(addr);
}

size_t gc_relocate(Object* obj, size_t free) {
    if(is_pair(*obj)) {
        if(gc_was_moved(obj->ptr)) {
            obj->ptr = gc_relocated_object(obj->ptr).ptr;
        } else {
            obj->ptr = gc_move(obj->ptr, free++);
        }
    }
    return free;
}

void gc_reset_root() {
    if(reserved_ptr != 1  || !is_nil(gc_root)) error("Corrupt GC state");
}

void gc_push_root(Object obj) {
    if(reserved_ptr >= RESERVED_FOR_GC) error("Not enough pairs reserved for GC");
    *memory_car(reserved_ptr) = obj;
    *memory_cdr(reserved_ptr) = gc_root;
    gc_root = pointer(reserved_ptr++);
}

Object gc_pop_root() {
    if(reserved_ptr == 1 || is_nil(gc_root)) error("Pop from empty root");
    if(!is_pair(gc_root)) error("Corrupt GC state");
    reserved_ptr -= 1;
    Object obj = *memory_car(gc_root.ptr);
    gc_root = *memory_cdr(gc_root.ptr);
    return obj;
}

bool collect_garbage() {
    printf("Collecting garbage...\n");
    size_t free = RESERVED_FOR_GC;
    size_t scan = free;

    if(is_nil(gc_root))
        error("Nothing to collect. Did you forget to push roots?");

    *memory_car(0) = nil();
    *memory_cdr(0) = gc_root;

    //print_memory(list_memory, 20);

    size_t root = gc_move(0, free++);

    for(; scan < free; scan++) {
        free = gc_relocate(fresh_memory_car(scan), free);
        free = gc_relocate(fresh_memory_cdr(scan), free);
    }

    Object* tmp = fresh_memory;
    fresh_memory = list_memory;
    list_memory = tmp;
    free_ptr = free;

    gc_root = *memory_cdr(root);

    //print_memory(list_memory, 20);
}

size_t memory_usage() {
    return free_ptr;
}
