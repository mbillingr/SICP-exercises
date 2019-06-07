#include "scheme.h"

#include "config.h"

Object g_env = NIL;

extern Object S_OK, S_FALSE, S_QUOTE, S_SET_VAR, S_DEFINE, S_LAMBDA, S_IF,
              S_PROCEDURE,S_BEGIN,S_COND,S_ELSE,S_LET;

Object append(Object seq1, Object seq2) {
    if(is_nil(seq1))
        return seq2;
    return cons(car(seq1), append(cdr(seq1), seq2));
}

size_t list_length(Object list) {
    size_t count = 0;
    for(Object x=list; !is_nil(x); x=cdr(x)) {
        count += 1;
    }
    return count;
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
