#pragma once
#include <stdbool.h>
#include <stdlib.h>

#define NIL { .tag = Nil, .data = 0 }
#define FALSE  { .tag = False, .data = 0 }
#define TRUE  { .tag = True, .data = 0 }
#define BROKEN_HEART  { .tag = BrokenHeart, .data = 0 }

enum TypeTag {
    Nil,
    False,
    True,
    Number,
    Symbol,
    String,
    Pointer,
    Primitive,
    // -- internal use --
    Label,
    BrokenHeart,
};

typedef struct Object {
    enum TypeTag tag;
    union {
        double number;
        const char* symbol;
        const char* str;
        size_t ptr;
        void* label;
        void* data;
    };
} Object;

typedef Object (*PrimitivePtr)(Object args);

void init_symbols();

bool is_eq(Object a, Object b);
bool is_equal(Object a, Object b);

bool is_nil(Object obj);
Object nil();

bool is_true(Object obj);
bool is_false(Object obj);
Object make_true();
Object make_false();
Object boolean(bool x);

bool is_number(Object obj);
Object number(double value);
double expect_number(Object obj);

bool is_symbol(Object obj);
Object symbol(const char* name);

bool is_string(Object obj);
Object string(const char* value);

bool is_pair(Object obj);
Object cons(Object car, Object cdr);
Object* car_ptr(Object obj);
Object* cdr_ptr(Object obj);
Object car(Object obj);
Object cdr(Object obj);
Object car_set(Object obj, Object val);
Object cdr_set(Object obj, Object val);

bool is_pointer(Object obj);
Object pointer(size_t idx);

bool is_label(Object obj);
Object label(void* label);

bool is_primitive_procedure(Object obj);
Object primitive(Object(*proc)(Object));
Object apply_primitive_procedure(Object proc, Object args);

void print_object(Object obj);

Object broken_heart();
