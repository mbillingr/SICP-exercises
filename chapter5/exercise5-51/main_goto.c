#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define STACK_SIZE 1024     // I think this is a huge stack
#define MEMORY_SIZE 65536
#define INPUT_BUFFER_SIZE 4096
#define TOKEN_BUFFER_SIZE 128
#define SYMBOL_BUFFER_SIZE 40960

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

void print_object(Object obj);
void print_list(Object obj);

// TODO: I guess list memory should be global (for simplicity)
//       So stack memory could be global too?

static Object list_memory[MEMORY_SIZE][2];
static size_t free_ptr = 0;

size_t memory_allocate_pair() {
    if(free_ptr >= MEMORY_SIZE) error("out of memory");
    return free_ptr++;
}

Object* memory_car(size_t idx) {
    return &list_memory[idx][0];
}

Object* memory_cdr(size_t idx) {
    return &list_memory[idx][1];
}

bool is_nil(Object obj) {
  return obj.tag == Nil;
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
    static char symbol_buffer[SYMBOL_BUFFER_SIZE];
    static char* next_free_symbol = &symbol_buffer[0];

    char* cursor = symbol_buffer;
    while(cursor < next_free_symbol) {
        if(strcmp(cursor, name) == 0) break;
        do {
            cursor++;
        } while(*cursor != 0);
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

bool is_pair(Object obj) {
  return obj.tag == Pair;
}

Object cons(Object car, Object cdr) {
    size_t idx = memory_allocate_pair();
    *memory_car(idx) = car;
    *memory_cdr(idx) = cdr;
    Object obj = {
        .tag = Pair,
        .pair = idx,
    };
    return obj;
}

Object* car_ptr(Object obj) {
  if(obj.tag != Pair) {
    print_object(obj);
    error("-- not a pair");
  }
  return memory_car(obj.pair);
}

Object* cdr_ptr(Object obj) {
  if(obj.tag != Pair) {
    print_object(obj);
    error("-- not a pair");
  }
  return memory_cdr(obj.pair);
}

Object car(Object obj) {
  return *car_ptr(obj);
}

Object cdr(Object obj) {
  return *cdr_ptr(obj);
}

Object label(void* label) {
    Object obj = {
        .tag = Label,
        .label = label,
    };
    return obj;
}

void print_object(Object obj) {
    switch(obj.tag) {
        case Nil: printf("'()"); break;
        case Number:
            if((int)obj.number == obj.number)
                printf("%d", (int)obj.number);
            else
                printf("%f", obj.number);
            break;
        case Symbol: printf(obj.symbol); break;
        case Pair:
            printf("(");
            print_list(obj);
            printf(")");
            break;
        case Label: printf("<label %p>", obj.label); break;
        default: printf("<unknown object>"); break;
    }
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

static char input_buffer[INPUT_BUFFER_SIZE];
static char current_token[TOKEN_BUFFER_SIZE];
static char* input_cursor;

void skip_whitespace() {
    while(*input_cursor == ' ' || *input_cursor == '\n') {
        input_cursor++;
    }
}

void next_token() {
    char* token = current_token;
    *(token++) = *input_cursor;
    switch(*input_cursor) {
        case 0: return;
        case '(':
        case ')':
            input_cursor++;
            *token = 0;
            skip_whitespace();
            return;
    }
    input_cursor++;

    while(*input_cursor != 0) {
        switch(*input_cursor) {
            case ' ':
            case '(':
            case ')':
            case '\n':
                *token = 0;
                skip_whitespace();
                return;
        }
        *(token++) = *(input_cursor++);
    }
}

Object parse_literal();
Object parse_list();
Object parse_expression();

Object parse_list() {
    Object list = nil();
    Object* end = &list;
        next_token();
    while(*current_token != ')') {
        *end = cons(parse_expression(), nil());
        end = cdr_ptr(*end);
    }
    next_token();
    return list;
}

Object parse_literal() {
    Object obj;
    char *end;
    double num = strtod(current_token, &end);

    if(end == current_token) {
        obj = symbol(current_token);
    } else {
        obj = number(num);
    }
    next_token();
    return obj;
}

Object parse_expression() {
    if(*current_token == '(') {
        return parse_list();
    } else {
        return parse_literal();
    }
}

Object read() {
    input_cursor = input_buffer;
    fgets(input_buffer, INPUT_BUFFER_SIZE, stdin);
    skip_whitespace();
    next_token();
    Object expr = parse_expression();
    printf("expr: ");
    print_object(expr);
    printf("\n");
    return expr;
}

Object get_global_environment() {
    error("not implemented: get_global_environment");
}

int main() {
    void* cont;
    Object exp, env, val, proc, argl, unev;
    Stack stack = stack_new(STACK_SIZE);

    exp = nil();
    print_object(exp);
    printf("\n");
    exp = symbol("abc");
    print_object(exp);
    printf("\n");
    exp = number(123456);
    print_object(exp);
    printf("\n");
    exp = label(&&repl);
    print_object(exp);
    printf("\n");
    exp = cons(symbol("A"), number(1));
    print_object(exp);
    printf("\n");
    exp = cons(symbol("A"), cons(number(2), cons(symbol("C"), nil())));
    print_object(exp);
    printf("\n");
    exp = cons(symbol("A"), cons(nil(), cons(symbol("C"), number(2))));
    print_object(exp);
    printf("\n");
    stack_push(&stack, number(1));
    stack_push(&stack, number(2));
    stack_push(&stack, cons(number(3), (number(4))));

    print_object(stack_pop(&stack));
    print_object(stack_pop(&stack));
    print_object(stack_pop(&stack));

repl:
    stack_initialize(&stack);
    printf("\n;;; C-Eval input:\n");
    exp = read();
    env = get_global_environment();
    cont = &&print_result;
    goto eval_dispatch;
print_result:
    printf(";;; C-Eval value:\n");
    print_object(val);
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
    print_object(val);
    goto repl;
eval_dispatch:

    stack_destroy(&stack);
    return 0;
}
