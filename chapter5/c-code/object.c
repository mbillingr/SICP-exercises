#include "object.h"

#include "config.h"
#include "memory.h"
#include <stdio.h>
#include <string.h>

Object S_OK = NIL;
Object S_FALSE = NIL;
Object S_QUOTE = NIL;
Object S_SET_VAR = NIL;
Object S_DEFINE = NIL;
Object S_LAMBDA = NIL;
Object S_IF = NIL;
Object S_PROCEDURE = NIL;
Object S_BEGIN = NIL;
Object S_COND = NIL;
Object S_ELSE = NIL;
Object S_LET = NIL;

void init_symbols() {
    S_OK = symbol("ok");
    S_FALSE = symbol("false");
    S_QUOTE = symbol("quote");
    S_SET_VAR = symbol("set!");
    S_DEFINE = symbol("define");
    S_LAMBDA = symbol("lambda");
    S_IF = symbol("if");
    S_PROCEDURE = symbol("procedure");
    S_BEGIN = symbol("begin");
    S_COND = symbol("cond");
    S_ELSE = symbol("else");
    S_LET = symbol("let");
}

Object broken_heart() {
    Object obj = BROKEN_HEART;
    return obj;
}

Object pointer(size_t idx) {
    Object obj = {
        .tag = Pointer,
        .ptr= idx,
    };
    return obj;
}

bool is_eq(Object a, Object b) {
    return a.tag == b.tag && a.data == b.data;
}

bool is_nil(Object obj) {
  return obj.tag == Nil;
}

Object nil() {
    Object obj = NIL;
    return obj;
}

bool is_true(Object obj) {
    return obj.tag != False;
}

Object make_true() {
    Object obj = TRUE;
    return obj;
}

bool is_false(Object obj) {
    return  obj.tag == False;
}

Object make_false() {
    Object obj = FALSE;
    return obj;
}

Object boolean(bool x) {
    if(x)
        return make_true();
    else
        return make_false();
}

bool is_number(Object obj) {
    return obj.tag == Number;
}

Object number(double value) {
    Object obj = {
        .tag = Number,
        .number = value,
    };
    return obj;
}

double expect_number(Object obj) {
    if(is_number(obj)) return obj.number;
    error("expected number");
}

bool is_symbol(Object obj) {
    return obj.tag == Symbol;
}

Object symbol(const char* name) {
    static char symbol_buffer[SYMBOL_BUFFER_SIZE];
    static char* next_free_symbol = &symbol_buffer[0];

    char* cursor = symbol_buffer;
    while(cursor < next_free_symbol) {
        if(strcmp(cursor, name) == 0) break;
        while(*cursor != 0) {
            cursor++;
        };
        while(*cursor == 0) {
            if(cursor == next_free_symbol) break;
            cursor++;
        };
    }

    if(cursor == next_free_symbol) {
        size_t n = strlen(name) + 1;
        if(next_free_symbol + n >= symbol_buffer + SYMBOL_BUFFER_SIZE)
            error("Symbol buffer overflow");
        strcpy(next_free_symbol, name);
        next_free_symbol += n;
    }

    Object obj = {
        .tag = Symbol,
        .symbol = cursor,
    };
    return obj;
}

bool is_string(Object obj){
    return obj.tag == String;
}

Object string(const char* value) {
    Object obj = {
        .tag = String,
        .str = value,
    };
    return obj;
}

bool is_pair(Object obj) {
  return obj.tag == Pointer;
}

Object cons(Object car, Object cdr) {
    size_t idx = memory_allocate_pair(car, cdr);
    *memory_car(idx) = car;
    *memory_cdr(idx) = cdr;
    return pointer(idx);
}

Object* car_ptr(Object obj) {
  if(obj.tag != Pointer) {
    print_object(obj);
    error("-- not a pair");
  }
  return memory_car(obj.ptr);
}

Object* cdr_ptr(Object obj) {
  if(obj.tag != Pointer) {
    print_object(obj);
    error("-- not a pair");
  }
  return memory_cdr(obj.ptr);
}

Object car(Object obj) {
  return *car_ptr(obj);
}

Object cdr(Object obj) {
  return *cdr_ptr(obj);
}

Object car_set(Object obj, Object val) {
  *car_ptr(obj) = val;
}

Object cdr_set(Object obj, Object val) {
  *cdr_ptr(obj) = val;
}

bool is_equal(Object a, Object b) {
    if(!is_pair(a) && !is_pair(b)) {
        return is_eq(a, b);
    }

    if(is_pair(a) && is_pair(b)) {
        return is_equal(car(a), car(b)) && is_equal(cdr(a), cdr(b));
    }

    return false;
}

void print_list(Object obj) {
  print_object(car(obj));
  obj = cdr(obj);

  while(is_pair(obj)) {
    printf(" ");
    print_object(car(obj));
    obj = cdr(obj);
  }

  if(!is_nil(obj)) {
    printf(" . ");
    print_object(obj);
  }
}

void print_object(Object obj) {
    switch(obj.tag) {
        case Nil: printf("'()"); break;
        case True: printf("#t"); break;
        case False: printf("#f"); break;
        case Number:
            if((int)obj.number == obj.number)
                printf("%d", (int)obj.number);
            else
                printf("%f", obj.number);
            break;
        case Symbol: printf(obj.symbol); break;
        case String: printf(obj.str); break;
        case Pointer:
            printf("(");
            print_list(obj);
            printf(")");
            break;
        case Primitive: printf("<primitive at %p>", obj.label); break;
        case Label: printf("<label %p>", obj.label); break;
        default: printf("<unknown object>"); break;
    }
}

bool is_primitive_procedure(Object obj) {
    return obj.tag == Primitive;
}

Object primitive(Object(*proc)(Object)) {
    Object obj = {
        .tag = Primitive,
        .label = proc,
    };
    return obj;
}

Object apply_primitive_procedure(Object proc, Object args) {
    if(!is_primitive_procedure(proc)) {
        error("not a primitive procedure");
    }
    PrimitivePtr fptr = proc.label;
    return fptr(args);
}

bool is_label(Object obj) {
    return obj.tag == Label;
}

Object label(void* label) {
    Object obj = {
        .tag = Label,
        .label = label,
    };
    return obj;
}

bool is_pointer(Object obj) {
    return obj.tag == Pointer;
}
