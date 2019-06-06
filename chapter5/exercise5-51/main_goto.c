#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define STACK_SIZE 1024
#define MEMORY_SIZE 33554432  // 1 GB, assuming an object size of 16 bytes
#define INPUT_BUFFER_SIZE 4096
#define TOKEN_BUFFER_SIZE 128
#define SYMBOL_BUFFER_SIZE 40960

#define NIL { .tag = Nil, .data = 0 }
#define FALSE  { .tag = False, .data = 0 }
#define TRUE  { .tag = True, .data = 0 }

void error(const char* msg) {
    fprintf(stderr, msg);
    exit(EXIT_FAILURE);
}

enum TypeTag {
    Nil,
    False,
    True,
    Number,
    Symbol,
    Pair,
    Primitive,
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
        void* data;
    };
} Object;

typedef Object (*PrimitivePtr)(Object args);

void print_object(Object obj);
void print_list(Object obj);

static Object *list_memory;
static size_t free_ptr = 0;

size_t memory_allocate_pair() {
    if(free_ptr >= MEMORY_SIZE) error("out of memory");
    return free_ptr++;
}

Object* memory_car(size_t idx) {
    return &list_memory[idx * 2];
}

Object* memory_cdr(size_t idx) {
    return &list_memory[idx * 2 + 1];
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
        //printf("comparing %s : %s\n", cursor, name);
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

    /*printf("%s -> ", name);
    for(int i=0; i<15; i++) {
        printf("%d ", symbol_buffer[i]);
    }
    printf("\n");*/

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

Object car_set(Object obj, Object val) {
  *car_ptr(obj) = val;
}

Object cdr_set(Object obj, Object val) {
  *cdr_ptr(obj) = val;
}

Object append(Object seq1, Object seq2) {
    if(is_nil(seq1))
        return seq2;
    return cons(car(seq1), append(cdr(seq1), seq2));
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

Object label(void* label) {
    Object obj = {
        .tag = Label,
        .label = label,
    };
    return obj;
}

#define caar(exp) car(car(exp))
#define cadr(exp) car(cdr(exp))
#define cdar(exp) cdr(car(exp))
#define cddr(exp) cdr(cdr(exp))
#define caaar(exp) car(car(car(exp)))
#define caadr(exp) car(car(cdr(exp)))
#define cadar(exp) car(cdr(car(exp)))
#define caddr(exp) car(cdr(cdr(exp)))
#define cdaar(exp) cdr(car(car(exp)))
#define cdadr(exp) cdr(car(cdr(exp)))
#define cddar(exp) cdr(cdr(car(exp)))
#define cdddr(exp) cdr(cdr(cdr(exp)))
#define cadddr(exp) car(cdr(cdr(cdr(exp))))

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
        case Pair:
            printf("(");
            print_list(obj);
            printf(")");
            break;
        case Primitive: printf("<primitive at %p>", obj.label); break;
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

static Object stack_memory[STACK_SIZE];
static Object* stack_end = &stack_memory[STACK_SIZE];
static Object* stack_top = &stack_memory[0];

void stack_initialize() {
    stack_top = stack_memory;
}

void stack_push(Object obj) {
    if(stack_top >= stack_end) error("Stack overflow");
    *(stack_top++) = obj;
}

Object stack_pop() {
    if(stack_top <= stack_memory) error("Stack underflow");
    return *(--stack_top);
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

static Object S_OK = NIL;
static Object S_FALSE = NIL;
static Object S_QUOTE = NIL;
static Object S_SET_VAR = NIL;
static Object S_DEFINE = NIL;
static Object S_LAMBDA = NIL;
static Object S_IF = NIL;
static Object S_PROCEDURE = NIL;
static Object S_BEGIN = NIL;
static Object S_COND = NIL;
static Object S_ELSE = NIL;
static Object S_LET = NIL;

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
        case '\'':
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
    if(*current_token == '\'') {
        next_token();
        return cons(S_QUOTE, cons(parse_expression(), nil()));
    }
    if(*current_token == '(') {
        return parse_list();
    } else {
        return parse_literal();
    }
}

size_t list_length(Object list) {
    size_t count = 0;
    for(Object x=list; !is_nil(x); x=cdr(x)) {
        count += 1;
    }
    return count;
}

Object read() {
    input_cursor = input_buffer;
    fgets(input_buffer, INPUT_BUFFER_SIZE, stdin);
    skip_whitespace();
    next_token();
    return parse_expression();
}

Object enclosing_environment(Object env) { return cdr(env); }
Object first_frame(Object env) { return car(env); }
Object make_frame(Object variables, Object values) {
    return cons(variables, values);
}
Object frame_variables(Object frame) { return car(frame); }
Object frame_values(Object frame) { return cdr(frame); }
void add_binding_to_frame(Object var, Object val, Object frame) {
    car_set(frame, cons(var, car(frame)));
    cdr_set(frame, cons(val, cdr(frame)));
}

Object extend_environment(Object vars, Object vals, Object base_env) {
    if(list_length(vars) == list_length(vals)) {
        return cons (make_frame(vars, vals), base_env);
    } else {
        if(list_length(vars) < list_length(vals))
            error("Too many arguments supplied");
        else
            error("Too few arguments supplied");
    }
}

Object lookup_variable_value(Object var, Object env) {
    while(!is_nil(env)) {
        Object frame = first_frame(env);
        Object vars = frame_variables(frame);
        Object vals = frame_values(frame);
        for(; !is_nil(vars); vars = cdr(vars), vals = cdr(vals)) {
            if(is_eq(var, car(vars))) return car(vals);
        }
        env = enclosing_environment(env);
    }
    error("Unbound variable");
}

void set_variable_value(Object var, Object val, Object env) {
    while(!is_nil(env)) {
        Object frame = first_frame(env);
        Object vars = frame_variables(frame);
        Object vals = frame_values(frame);
        for(; !is_nil(vars); vars = cdr(vars), vals = cdr(vals)) {
            if(is_eq(var, car(vars))) {
                car_set(vals, val);
                return;
            }
        }
        env = enclosing_environment(env);
    }
    error("Unbound variable");
}

void define_variable(Object var, Object val, Object env) {
    Object frame = first_frame(env);
    Object vars = frame_variables(frame);
    Object vals = frame_values(frame);
    for(;;) {
        if(is_nil(vars)) {
            add_binding_to_frame(var, val, frame);
            break;
        }
        if(is_eq(var, car(vars))) {
            car_set(vals, val);
            break;
        }
        vars = cdr(vars);
        vals = cdr(vals);
    }
}

void define_number(const char* var, double val, Object env) {
    define_variable(symbol(var), number(val), env);
}

void define_primitive(const char* var, PrimitivePtr proc, Object env) {
    define_variable(symbol(var), primitive(proc), env);
}

// primitive procedures
Object expect_one_arg(Object args) {
    if(is_nil(args)) error("Too few arguments supplied");
    if(!is_nil(cdr(args))) error("Too many arguments supplied");
    return car(args);
}

Object expect_two_args(Object* a, Object* b, Object args) {
    if(is_nil(args)) error("Too few arguments supplied");
    *a = car(args);
    args = cdr(args);
    if(is_nil(args)) error("Too few arguments supplied");
    *b = car(args);
    if(!is_nil(cdr(args))) error("Too many arguments supplied");
}

Object p_is_null(Object args) {
    return boolean(is_nil(expect_one_arg(args)));
}

Object p_is_number(Object args) {
    return boolean(is_number(expect_one_arg(args)));
}

Object p_is_symbol(Object args) {
    return boolean(is_symbol(expect_one_arg(args)));
}

Object p_is_pair(Object args) {
    return boolean(is_pair(expect_one_arg(args)));
}

Object p_is_eq(Object args) {
    Object a, b;
    expect_two_args(&a, &b, args);
    return boolean(is_eq(a, b));
}

Object p_is_equal(Object args) {
    Object a, b;
    expect_two_args(&a, &b, args);
    return boolean(is_equal(a, b));
}

Object p_cons(Object args) {
    Object a, b;
    expect_two_args(&a, &b, args);
    return cons(a, b);
}

Object p_car(Object args) { return car(expect_one_arg(args)); }
Object p_cdr(Object args) { return cdr(expect_one_arg(args)); }
Object p_set_car(Object args) {
    Object pair, val;
    expect_two_args(&pair, &val, args);
    car_set(pair, val);
    return S_OK;
}
Object p_set_cdr(Object args) {
    Object pair, val;
    expect_two_args(&pair, &val, args);
    cdr_set(pair, val);
    return S_OK;
}

Object p_list(Object args) {
    return args;
}

Object p_is_less(Object args) {
    Object a, b;
    expect_two_args(&a, &b, args);
    return boolean(expect_number(a) < expect_number(b));
}

Object p_is_greater(Object args) {
    Object a, b;
    expect_two_args(&a, &b, args);
    return boolean(expect_number(a) > expect_number(b));
}

Object p_add(Object args) {
    double acc = 0.0;
    for(; !is_nil(args); args = cdr(args)) {
        acc += expect_number(car(args));
    }
    return number(acc);
}

Object p_mul(Object args) {
    double acc = 1.0;
    for(; !is_nil(args); args = cdr(args)) {
        acc *= expect_number(car(args));
    }
    return number(acc);
}

Object p_sub(Object args) {
    if(is_nil(args)) error("Too few arguments supplied");
    double acc = expect_number(car(args));
    if(is_nil(cdr(args))) return number(-acc);
    args = cdr(args);
    for(; !is_nil(args); args = cdr(args)) {
        acc -= expect_number(car(args));
    }
    return number(acc);
}

Object p_div(Object args) {
    if(is_nil(args)) error("Too few arguments supplied");
    double acc = expect_number(car(args));
    if(is_nil(cdr(args))) return number(1.0/acc);
    args = cdr(args);
    for(; !is_nil(args); args = cdr(args)) {
        acc /= expect_number(car(args));
    }
    return number(acc);
}

Object setup_environment() {
    Object initial_env = extend_environment(nil(), nil(), nil());
    define_primitive("null?", p_is_null, initial_env);
    define_primitive("number?", p_is_number, initial_env);
    define_primitive("symbol?", p_is_symbol, initial_env);
    define_primitive("pair?", p_is_pair, initial_env);
    define_primitive("eq?", p_is_eq, initial_env);
    define_primitive("equal", p_is_equal, initial_env);
    define_primitive("cons", p_cons, initial_env);
    define_primitive("car", p_car, initial_env);
    define_primitive("cdr", p_cdr, initial_env);
    define_primitive("set-car!", p_set_car, initial_env);
    define_primitive("set-cdr!", p_set_cdr, initial_env);
    define_primitive("list", p_list, initial_env);
    define_primitive("=", p_is_eq, initial_env);
    define_primitive("<", p_is_less, initial_env);
    define_primitive(">", p_is_greater, initial_env);
    define_primitive("+", p_add, initial_env);
    define_primitive("-", p_sub, initial_env);
    define_primitive("*", p_mul, initial_env);
    define_primitive("/", p_div, initial_env);
    define_variable(symbol("true"), make_true(), initial_env);
    define_variable(symbol("false"), make_false(), initial_env);
    return initial_env;
}

Object get_global_environment() {
    static Object g_env = NIL;
    if(is_nil(g_env))
        g_env = setup_environment();
    return g_env;
}

bool is_tagged_list(Object exp, Object tag) {
    return is_pair(exp) && is_eq(car(exp), tag);
}

bool is_self_evaluating(Object exp) {
    return is_number(exp) || is_nil(exp);
}

bool is_variable(Object exp) {
    return is_symbol(exp);
}

bool is_quoted(Object exp) { return is_tagged_list(exp, S_QUOTE); }
Object text_of_quotation(Object exp) { return car(cdr(exp)); }

bool is_assignment(Object exp) { return is_tagged_list(exp, S_SET_VAR); }
Object assignment_variable(Object exp) { return car(cdr(exp)); }
Object assignment_value(Object exp) { return car(cdr(cdr(exp))); }

bool is_lambda(Object exp) { return is_tagged_list(exp, S_LAMBDA); }
Object lambda_paramaters(Object exp) { return cadr(exp); }
Object lambda_body(Object exp) { return cddr(exp); }
Object make_lambda(Object parameters, Object body) {
    return cons(S_LAMBDA, cons(parameters, body));
}

bool is_compound_procedure(Object proc) {
    return is_tagged_list(proc, S_PROCEDURE);
}
Object make_procedure(Object parameters, Object body, Object env) {
    return cons(S_PROCEDURE, cons(parameters, cons(body, cons(env, nil()))));
}
Object procedure_parameters(Object proc) { return cadr(proc); }
Object procedure_body(Object proc) { return caddr(proc); }
Object procedure_environment(Object proc) { return cadddr(proc); }

bool is_definition(Object exp) { return is_tagged_list(exp, S_DEFINE); }
Object definition_variable(Object exp) {
    if(is_symbol(cadr(exp)))
        return cadr(exp);
    else
        return caadr(exp);
}
Object definition_value(Object exp) {
    if(is_symbol(cadr(exp)))
        return caddr(exp);
    else
        return make_lambda(cdadr(exp), cddr(exp));
}

bool is_if(Object exp) { return is_tagged_list(exp, S_IF); }
Object if_predicate(Object exp) { return cadr(exp); }
Object if_consequent(Object exp) { return caddr(exp); }
Object if_alternative(Object exp) {
    if(is_nil(cdddr(exp)))
        return S_FALSE;
    else
        return cadddr(exp);
}
Object* if_alternative_ptr(Object exp) {
    return car_ptr(cdddr(exp));
}
Object make_if(Object predicate, Object consequent, Object alternative) {
    return cons(S_IF, cons(predicate, cons(consequent, cons(alternative, nil()))));
}

bool is_begin(Object exp) { return is_tagged_list(exp, S_BEGIN); }
Object begin_actions(Object exp) { return cdr(exp); }
bool is_last_exp(Object seq) { return is_nil(cdr(seq)); }
Object first_exp(Object seq) { return car(seq); }
Object rest_exps(Object seq) { return cdr(seq); }
Object make_begin(Object seq) { return cons(S_BEGIN, seq); }
Object sequence_to_exp(Object seq) {
    if(is_nil(seq)) return seq;
    if(is_last_exp(seq)) return first_exp(seq);
    return make_begin(seq);
}

bool is_cond(Object exp) { return is_tagged_list(exp, S_COND); }
Object cond_clauses(Object exp) { return cdr(exp); }
Object cond_predicate(Object clause) { return car(clause); }
Object cond_actions(Object clause) { return cdr(clause); }
bool is_cond_else_clause(Object clause) {
    return is_eq(cond_predicate(clause), S_ELSE);
}
Object cond_to_if(Object exp) {
    Object clauses = cond_clauses(exp);
    Object if_nest = nil();
    Object* cursor = &if_nest;

    while(!is_nil(clauses)) {
        Object first = car(clauses);
        Object rest = cdr(clauses);
        if(is_cond_else_clause(first)) {
            if(is_nil(rest))
                *cursor = sequence_to_exp(cond_actions(first));
            else
                error("ELSE clause isn't last -- COND->IF");
        } else {
            *cursor = make_if(
                cond_predicate(first),
                sequence_to_exp(cond_actions(first)),
                S_FALSE);
            cursor = if_alternative_ptr(*cursor);
        }
        clauses = rest;
    }

    print_object(if_nest);
    return if_nest;
}

bool is_let(Object exp) { return is_tagged_list(exp, S_LET); }
Object let_spec(Object exp) { return cadr(exp); }
Object let_body(Object exp) { return cddr(exp); }
Object let_vars(Object spec) {
    Object seq = nil();
    for(; !is_nil(spec); spec = cdr(spec)) {
        seq = cons(caar(spec), nil());
    }
    return seq;
}
Object let_values(Object spec) {
    Object seq = nil();
    for(; !is_nil(spec); spec = cdr(spec)) {
        seq = cons(cadar(spec), nil());
    }
    return seq;
}
Object let_to_combination(Object exp) {
    Object spec = let_spec(exp);
    return cons(make_lambda(let_vars(spec), let_body(exp)),
                let_values(spec));
}

bool is_application(Object exp) { return is_pair(exp); }
Object operator(Object exp) { return car(exp); }
Object operands(Object exp) { return cdr(exp); }
bool no_operands(Object ops) { return is_nil(ops); }
Object first_operand(Object ops) { return car(ops); }
Object rest_operands(Object ops) { return cdr(ops); }

Object empty_arglist() { return nil(); }
Object adjoin_arg(Object arg, Object arglist) {
    return append(arglist, cons(arg, nil()));
}
bool is_last_operand(Object ops) { return is_nil(cdr(ops)); }

void user_print(Object obj) {
    if(is_compound_procedure(obj)) {
        print_object(
            cons(symbol("compound-procedure"),
                 cons(procedure_parameters(obj),
                      cons(procedure_body(obj),
                           cons(symbol("<procedure-env>"),
                                nil())))));
    } else {
        print_object(obj);
    }
}

int main() {
    void* cont;
    Object exp, env, val, proc, argl, unev;

    printf("Object size: %d Bytes\n", sizeof(Object));
    printf("Reserving %d MB of list memory.\n",
           (MEMORY_SIZE * 2 * sizeof(Object))/(1024*1024));

    list_memory = malloc(MEMORY_SIZE * 2 * sizeof(Object));
    init_symbols();

repl:
    stack_initialize();
    printf("\n;;; C-Eval input:\n");
    exp = read();
    env = get_global_environment();
    cont = &&print_result;
    goto eval_dispatch;
print_result:
    printf(";;; C-Eval value:\n");
    user_print(val);
    printf("\n");
    goto repl;

unknown_expression_type:
    val = symbol("unknown-expression-type-error");
    goto signal_error;
unknown_procedure_type:
    cont = stack_pop_label();  // clean up stack (from apply-dispatch)
    val = symbol("unknown-procedure-type-error");
    goto signal_error;
signal_error:
    user_print(val);
    goto repl;

eval_dispatch:
    if(is_self_evaluating(exp)) goto ev_self_eval;
    if(is_variable(exp)) goto ev_variable;
    if(is_quoted(exp)) goto ev_quoted;
    if(is_assignment(exp)) goto ev_assignment;
    if(is_definition(exp)) goto ev_definition;
    if(is_if(exp)) goto ev_if;
    if(is_lambda(exp)) goto ev_lambda;
    if(is_begin(exp)) goto ev_begin;
    if(is_cond(exp)) goto ev_cond;
    if(is_let(exp)) goto ev_let;
    if(is_application(exp)) goto ev_application;
    goto unknown_expression_type;

ev_self_eval:
    val = exp;
    goto *cont;

ev_variable:
    val = lookup_variable_value(exp, env);
    goto *cont;

ev_quoted:
    val = text_of_quotation(exp);
    goto *cont;

ev_assignment:
    unev = assignment_variable(exp);
    stack_push(unev);
    exp = assignment_value(exp);
    stack_push(env);
    stack_push_label(cont);
    cont = &&ev_assignment_1;
    goto eval_dispatch;
ev_assignment_1:
    cont = stack_pop_label();
    env = stack_pop();
    unev = stack_pop();
    set_variable_value(unev, val, env);
    val = S_OK;
    goto *cont;

ev_definition:
    unev = definition_variable(exp);
    stack_push(unev);
    exp = definition_value(exp);
    stack_push(env);
    stack_push_label(cont);
    cont = &&ev_definition_1;
    goto eval_dispatch;
ev_definition_1:
    cont = stack_pop_label();
    env = stack_pop();
    unev = stack_pop();
    define_variable(unev, val, env);
    val = S_OK;
    goto *cont;

ev_if:
    stack_push(exp);
    stack_push(env);
    stack_push_label(cont);
    cont = &&ev_if_decide;
    exp = if_predicate(exp);
    goto eval_dispatch;
ev_if_decide:
    cont = stack_pop_label();
    env = stack_pop();
    exp = stack_pop();
    if(is_true(val)) goto ev_if_consequent;
ev_if_alternative:
    exp = if_alternative(exp);
    goto eval_dispatch;
ev_if_consequent:
    exp = if_consequent(exp);
    goto eval_dispatch;

ev_lambda:
    unev = lambda_paramaters(exp);
    exp = lambda_body(exp);
    val = make_procedure(unev, exp, env);
    goto *cont;

ev_begin:
    unev = begin_actions(exp);
    stack_push_label(cont);
    goto ev_sequence;

ev_sequence:
    exp = first_exp(unev);
    if(is_last_exp(unev)) goto ev_sequence_last_exp;
    stack_push(unev);
    stack_push(env);
    cont = &&ev_sequence_continue;
    goto eval_dispatch;
ev_sequence_continue:
    stack_pop(env);
    stack_pop(unev);
    unev = rest_exps(unev);
    goto ev_sequence;
ev_sequence_last_exp:
    cont = stack_pop_label();
    goto eval_dispatch;

ev_cond:
    exp = cond_to_if(exp);
    goto eval_dispatch;

ev_let:
    exp = let_to_combination(exp);
    goto eval_dispatch;

ev_application:
    stack_push_label(cont);
    stack_push(env);
    unev = operands(exp);
    stack_push(unev);
    exp = operator(exp);
    cont = &&ev_appl_did_operator;
    goto eval_dispatch;
ev_appl_did_operator:
    unev = stack_pop();
    env = stack_pop();
    argl = empty_arglist();
    proc = val;
    if(no_operands(unev)) goto apply_dispatch;
    stack_push(proc);
ev_appl_operand_loop:
    stack_push(argl);
    exp = first_operand(unev);
    if(is_last_operand(unev)) goto ev_appl_last_arg;
    stack_push(env);
    stack_push(unev);
    cont = &&ev_appl_accumulate_arg;
    goto eval_dispatch;
ev_appl_accumulate_arg:
    unev = stack_pop();
    env = stack_pop();
    argl = stack_pop();
    argl = adjoin_arg(val, argl);
    unev = rest_operands(unev);
    goto ev_appl_operand_loop;
ev_appl_last_arg:
    cont = &&ev_appl_accum_last_arg;
    goto eval_dispatch;
ev_appl_accum_last_arg:
    argl = stack_pop();
    argl = adjoin_arg(val, argl);
    proc = stack_pop();
    goto apply_dispatch;

apply_dispatch:
    if(is_primitive_procedure(proc)) goto primitive_apply;
    if(is_compound_procedure(proc)) goto compound_apply;
    goto unknown_procedure_type;
primitive_apply:
    val = apply_primitive_procedure(proc, argl);
    cont = stack_pop_label();
    goto *cont;
compound_apply:
    unev = procedure_parameters(proc);
    env = procedure_environment(proc);
    env = extend_environment(unev, argl, env);
    unev = procedure_body(proc);
    goto ev_sequence;

done:
    free(list_memory);
    return 0;
}
