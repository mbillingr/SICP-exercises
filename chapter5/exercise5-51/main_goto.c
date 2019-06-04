#include <stdlib.h>
#include <stdio.h>

#define STACK_SIZE 1024     // I think this is a huge stack

void error(const char* msg) {
    fprintf(stderr, msg);
    exit(EXIT_FAILURE);
}

enum TypeTag {
    Nil,
    Number,
    Symbol,
    Pair,
    // -- internal use --
    Label,
};

typedef struct Object {
    enum TypeTag tag;
    union {
        double number;
        const char* symbol;
        size_t pair;
        void* label;
    };
} Object;

// TODO: I guess list memory should be global (for simplicity)
//       So stack memory could be global too?

size_t memory_new_pair() {
    error("not implemented: memory_new_pair");
}

Object* memory_car() {
    error("not implemented: memory_car");
}

Object* memory_cdr() {
    error("not implemented: memory_cdr");
}

Object nil() {
    Object obj = {
        .tag = Nil,
    };
    return obj;
}

Object number(double value) {
    Object obj = {
        .tag = Number,
        .number = value,
    };
    return obj;
}

Object symbol(const char* name) {
    // todo: intern symbol
    Object obj = {
        .tag = Symbol,
        .symbol = name,
    };
    return obj;
}

Object cons(Object car, Object cdr) {
    size_t idx = memory_new_pair();
    *memory_car(idx) = car;
    *memory_cdr(idx) = cdr;
    Object obj = {
        .tag = Pair,
        .pair = idx,
    };
    return obj;
}

Object label(void* label) {
    Object obj = {
        .tag = Label,
        .label = label,
    };
    return obj;
}

void print_object(Object* obj) {
    switch(obj->tag) {
        case Nil: printf("'()"); break;
        case Number:
            if((int)obj->number == obj->number)
                printf("%d", (int)obj->number);
            else
                printf("%f", obj->number);
            break;
        case Symbol: printf(obj->symbol); break;
        case Pair:
            printf("(");
            printf(".");
            printf(")");
            break;
        case Label: printf("<label %p>", obj->label); break;
        default: printf("<unknown object>"); break;
    }
}

typedef struct Stack {
    Object* start;
    Object* end;
    Object* top;
} Stack;

Stack stack_new(size_t size) {
    void* memory = malloc(size * sizeof(struct Object));
    struct Stack stack =  {
        .start = memory,
        .end = memory + size * sizeof(struct Object),
        .top = memory
    };
    return stack;
}

void stack_destroy(struct Stack* stack) {
    free(stack->start);
}

void stack_initialize(struct Stack* stack) {
    stack->top = stack->start;
}

void stack_push(struct Stack* stack, Object obj) {
    if(stack->top >= stack->end) error("Stack overflow");
    *stack->top++ = obj;
}

Object stack_pop(struct Stack* stack) {
    if(stack->top <= stack->start) error("Stack underflow");
    return *--stack->top;
}

void* stack_pop_label(struct Stack* stack) {
    if(stack->top <= stack->start) error("Stack underflow");
    Object obj = *--stack->top;
    if(obj.tag != Label) error("Type error: expected label");
    return obj.label;
}

Object read() {
    error("not implemented: read");
}

Object get_global_environment() {
    error("not implemented: get_global_environment");
}

int main() {
    void* cont;
    Object exp, env, val, proc, argl, unev;
    Stack stack = stack_new(STACK_SIZE);

    exp = nil();
    print_object(&exp);
    printf("\n");
    exp = symbol("abc");
    print_object(&exp);
    printf("\n");
    exp = number(123456);
    print_object(&exp);
    printf("\n");
    exp = label(&&repl);
    print_object(&exp);
    printf("\n");
    exp = cons(symbol("A"), number(1));
    print_object(&exp);
    printf("\n");
    exp = cons(symbol("A"), cons(number(2), cons(symbol("C"), nil())));
    print_object(&exp);
    printf("\n");

repl:
    stack_initialize(&stack);
    printf("\n;;; C-Eval input:\n");
    exp = read();
    env = get_global_environment();
    cont = &&print_result;
    goto eval_dispatch;
print_result:
    printf(";;; C-Eval value:\n");
    print_object(&val);
    printf("\n");
    goto repl;
unknown_expression_type:
    val = symbol("unknown-expression-type-error");
    goto signal_error;
unknown_procedure_type:
    cont = stack_pop_label(&stack);  // clean up stack (from apply-dispatch)
    val = symbol("unknown-procedure-type-error");
    goto signal_error;
signal_error:
    print_object(&val);
    goto repl;
eval_dispatch:

    stack_destroy(&stack);
    return 0;
}
