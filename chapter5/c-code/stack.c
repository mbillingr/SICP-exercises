#include "stack.h"

#include "config.h"

Object stack;

void stack_initialize() {
    stack = nil();
}

void stack_push(Object obj) {
    stack = cons(obj, stack);
}

Object stack_pop() {
    if(is_nil(stack)) error("Stack underflow");
    Object obj = car(stack);
    stack = cdr(stack);
    return obj;
}

void stack_push_label(void* label) {
    Object obj = { .tag = Label, .label = label };
    stack_push(obj);
}

void* stack_pop_label() {
    Object obj = stack_pop();
    if(obj.tag != Label) error("Type error: expected label");
    return obj.label;
}
