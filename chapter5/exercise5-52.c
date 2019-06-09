
#include "config.h"
#include "memory.h"
#include "object.h"
#include "parser.h"
#include "scheme.h"
#include "stack.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

void error(const char* msg) {
    fprintf(stderr, msg);
    exit(EXIT_FAILURE);
}

extern Object S_OK, S_FALSE, S_QUOTE, S_SET_VAR, S_DEFINE, S_LAMBDA, S_IF,
              S_PROCEDURE,S_BEGIN,S_COND,S_ELSE,S_LET;
Object S_COMPILED_PROCEDURE = NIL;
Object S_UNASSIGNED = NIL;

Object make_compiled_procedure(Object entry, Object env) {
    return cons(S_COMPILED_PROCEDURE, cons(entry, cons(env, nil())));
}
Object compiled_procedure_entry(Object c_proc) { return cadr(c_proc); }
Object compiled_procedure_env(Object c_proc) { return caddr(c_proc); }

void* c_label(Object l) {
    if(!is_label(l)) error("Expected label");
    return l.label;
}


Object nth_env(size_t n, Object env) {
    for(; n > 0; n--) {
        if(is_nil(env))
            error("Lexical frame out of range");
        env = enclosing_environment(env);
    }
    return first_frame(env);
}

Object nth_var(size_t n, Object vals) {
    for(; n > 0; n--) {
        if(is_nil(vals))
            error("Lexical displacement out of range");
        vals = cdr(vals);
    }
    return car(vals);
}

void set_nth_var(size_t n, Object new_val, Object vals) {
    for(; n > 0; n--) {
        if(is_nil(vals))
            error("Lexical displacement out of range");
        vals = cdr(vals);
    }
    car_set(vals, new_val);
}

Object lexical_address_lookup(size_t frame_nr, size_t var_nr, Object env) {
    Object frame = nth_env(frame_nr, env);
    Object val = nth_var(var_nr, frame_values(frame));
    if(is_eq(val, S_UNASSIGNED))
        error("Uninitialized variable -- LEXICAL ADDRESS LOOKUP");
    return val;
}

void lexical_address_set(size_t frame_nr, size_t var_nr, Object val, Object env) {
    Object frame = nth_env(frame_nr, env);
    set_nth_var(var_nr, val, frame_values(frame));
}

extern Object stack, g_env;
static Object env, val, proc, argl, cont;


void gc() {
    if(memory_usage() > MEMORY_SIZE / 10 * 9) {
        gc_reset_root();

        gc_push_root(proc);
        gc_push_root(argl);
        gc_push_root(stack);
        gc_push_root(env);
        gc_push_root(g_env);
        gc_push_root(val);

        collect_garbage();

        val = gc_pop_root();
        g_env = gc_pop_root();
        env = gc_pop_root();
        stack = gc_pop_root();
        argl = gc_pop_root();
        proc = gc_pop_root();
    }
}


int main() {
    double time;

    init_memory();
    init_symbols();
    S_COMPILED_PROCEDURE = symbol("compiled-procedure");
    S_UNASSIGNED = symbol("*unassigned*");

    env = get_global_environment();

    // ---- begin generated code ----
    Object symbols[] = {
        symbol("exp"),
        symbol("env"),
        symbol("self-evaluating?"),
        symbol("variable?"),
        symbol("lookup-variable-value"),
        symbol("quoted?"),
        symbol("text-of-quotation"),
        symbol("assignment?"),
        symbol("eval-assignment"),
        symbol("definition?"),
        symbol("eval-definition"),
        symbol("if?"),
        symbol("eval-if"),
        symbol("lambda?"),
        symbol("make-procedure"),
        symbol("lambda-parameters"),
        symbol("lambda-body"),
        symbol("begin?"),
        symbol("eval-sequence"),
        symbol("begin-actions"),
        symbol("application?"),
        symbol("apply"),
        symbol("eval"),
        symbol("operator"),
        symbol("list-of-values"),
        symbol("operands"),
        symbol("error"),
        symbol("ok"),
        symbol("apply-in-underlying-scheme"),
        symbol("procedure"),
        symbol("arguments"),
        symbol("primitive-procedure?"),
        symbol("apply-primitive-procedure"),
        symbol("compound-procedure?"),
        symbol("procedure-body"),
        symbol("extend-environment"),
        symbol("procedure-parameters"),
        symbol("procedure-environment"),
        symbol("exps"),
        symbol("no-operands?"),
        symbol("cons"),
        symbol("first-operand"),
        symbol("rest-operands"),
        symbol("true?"),
        symbol("if-predicate"),
        symbol("if-consequent"),
        symbol("if-alternative"),
        symbol("last-exp?"),
        symbol("first-exp"),
        symbol("rest-exps"),
        symbol("set-variable-value!"),
        symbol("assignment-variable"),
        symbol("assignment-value"),
        symbol("define-variable!"),
        symbol("definition-variable"),
        symbol("definition-value"),
        symbol("number?"),
        symbol("true"),
        symbol("string?"),
        symbol("false"),
        symbol("symbol?"),
        symbol("tagged-list?"),
        symbol("quote"),
        symbol("cadr"),
        symbol("tag"),
        symbol("pair?"),
        symbol("eq?"),
        symbol("car"),
        symbol("set!"),
        symbol("caddr"),
        symbol("define"),
        symbol("caadr"),
        symbol("make-lambda"),
        symbol("cdadr"),
        symbol("cddr"),
        symbol("lambda"),
        symbol("parameters"),
        symbol("body"),
        symbol("if"),
        symbol("null?"),
        symbol("cdddr"),
        symbol("cadddr"),
        symbol("predicate"),
        symbol("consequent"),
        symbol("alternative"),
        symbol("list"),
        symbol("make-if"),
        symbol("begin"),
        symbol("cdr"),
        symbol("seq"),
        symbol("make-begin"),
        symbol("sequence->exp"),
        symbol("ops"),
        symbol("x"),
        symbol("not"),
        symbol("false?"),
        symbol("p"),
        symbol("enclosing-environment"),
        symbol("first-frame"),
        symbol("the-empty-environment"),
        symbol("variables"),
        symbol("values"),
        symbol("make-frame"),
        symbol("frame"),
        symbol("frame-variables"),
        symbol("frame-values"),
        symbol("var"),
        symbol("val"),
        symbol("set-car!"),
        symbol("set-cdr!"),
        symbol("add-binding-to-frame!"),
        symbol("vars"),
        symbol("vals"),
        symbol("base-env"),
        symbol("="),
        symbol("length"),
        symbol("<"),
        symbol("env-loop"),
        symbol("scan"),
        symbol("*unassigned*"),
        symbol("initial-env"),
        symbol("primitive-procedure-names"),
        symbol("primitive-procedures"),
        symbol("primitive-procedure-objects"),
        symbol("setup-environment"),
        symbol("proc"),
        symbol("primitive"),
        symbol("primitive-implementation"),
        symbol("equal?"),
        symbol(">"),
        symbol("+"),
        symbol("-"),
        symbol("*"),
        symbol("/"),
        symbol("n"),
        symbol("next"),
        symbol("map"),
        symbol("args"),
        symbol("input-prompt"),
        symbol("output-prompt"),
        symbol("prompt-for-input"),
        symbol("input"),
        symbol("output"),
        symbol("announce-output"),
        symbol("user-print"),
        symbol("the-global-environment"),
        symbol("read"),
        symbol("driver-loop"),
        symbol("string"),
        symbol("newline"),
        symbol("display"),
        symbol("object"),
        symbol("compound-procedure"),
        symbol("<procedure-env>"),
        symbol("op"),
        symbol("initial"),
        symbol("sequence"),
        symbol("accumulate"),
        symbol("y"),
    };
    val = make_compiled_procedure(label(&&entry1), env);
    goto after_lambda2; 
entry1:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], cons(symbols[1], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[2], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch6;
compiled_branch7:
    cont = label(&&after_call8);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch6:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call8:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch4;
true_branch3:
    val = lexical_address_lookup(0, 0, env);
    goto *c_label(cont);
false_branch4:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[3], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch12;
compiled_branch13:
    cont = label(&&after_call14);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch12:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call14:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch10;
true_branch9:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[4], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch15;
compiled_branch16:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch15:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call17:
false_branch10:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[5], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch21;
compiled_branch22:
    cont = label(&&after_call23);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch21:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call23:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch19;
true_branch18:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[6], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch24;
compiled_branch25:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch24:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call26:
false_branch19:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[7], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch30;
compiled_branch31:
    cont = label(&&after_call32);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch30:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call32:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch28;
true_branch27:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[8], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch33;
compiled_branch34:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch33:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call35:
false_branch28:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[9], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch39;
compiled_branch40:
    cont = label(&&after_call41);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch39:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call41:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch37;
true_branch36:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[10], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch42;
compiled_branch43:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch42:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call44:
false_branch37:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[11], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch48;
compiled_branch49:
    cont = label(&&after_call50);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch48:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call50:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch46;
true_branch45:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[12], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch51;
compiled_branch52:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch51:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call53:
false_branch46:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[13], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch57;
compiled_branch58:
    cont = label(&&after_call59);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch57:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call59:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch55;
true_branch54:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[14], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(env);
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[16], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch63;
compiled_branch64:
    cont = label(&&after_call65);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch63:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call65:
    argl = stack_pop();
    argl = cons(val, argl);
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[15], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch60;
compiled_branch61:
    cont = label(&&after_call62);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch60:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call62:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch66;
compiled_branch67:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch66:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call68:
false_branch55:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[17], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch72;
compiled_branch73:
    cont = label(&&after_call74);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch72:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call74:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch70;
true_branch69:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[18], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[19], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch75;
compiled_branch76:
    cont = label(&&after_call77);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch75:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call77:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch78;
compiled_branch79:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch78:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call80:
false_branch70:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[20], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch84;
compiled_branch85:
    cont = label(&&after_call86);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch84:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call86:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch82;
true_branch81:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[21], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[24], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[25], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch93;
compiled_branch94:
    cont = label(&&after_call95);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch93:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call95:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch96;
compiled_branch97:
    cont = label(&&after_call98);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch96:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call98:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[23], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch87;
compiled_branch88:
    cont = label(&&after_call89);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch87:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call89:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch90;
compiled_branch91:
    cont = label(&&after_call92);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch90:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call92:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch99;
compiled_branch100:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch99:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call101:
false_branch82:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[26], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    val = string("Unknown expression type -- EVAL");
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch102;
compiled_branch103:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch102:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call104:
after_if83:
after_if71:
after_if56:
after_if47:
after_if38:
after_if29:
after_if20:
after_if11:
after_if5:
after_lambda2:
    define_variable(symbols[22], val, env);
    val = symbols[27];
    stack_push(env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[21], env);
    env = stack_pop();
    define_variable(symbols[28], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry105), env);
    goto after_lambda106;
entry105:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[29], cons(symbols[30], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[31], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch110;
compiled_branch111:
    cont = label(&&after_call112);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch110:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call112:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch108;
true_branch107:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[32], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch113;
compiled_branch114:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch113:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call115:
false_branch108:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[33], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch119;
compiled_branch120:
    cont = label(&&after_call121);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch119:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call121:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch117;
true_branch116:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[18], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[35], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[37], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch128;
compiled_branch129:
    cont = label(&&after_call130);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch128:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call130:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, argl);
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[36], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch125;
compiled_branch126:
    cont = label(&&after_call127);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch125:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call127:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch131;
compiled_branch132:
    cont = label(&&after_call133);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch131:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call133:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[34], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch122;
compiled_branch123:
    cont = label(&&after_call124);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch122:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call124:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch134;
compiled_branch135:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch134:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call136:
false_branch117:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[26], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    val = string("Unknown procedure type -- APPLY");
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch137;
compiled_branch138:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch137:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call139:
after_if118:
after_if109:
after_lambda106:
    define_variable(symbols[21], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry140), env);
    goto after_lambda141;
entry140:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[38], cons(symbols[1], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[39], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch145;
compiled_branch146:
    cont = label(&&after_call147);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch145:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call147:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch143;
true_branch142:
    val = nil();
    goto *c_label(cont);
false_branch143:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[24], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[42], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch154;
compiled_branch155:
    cont = label(&&after_call156);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch154:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call156:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch157;
compiled_branch158:
    cont = label(&&after_call159);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch157:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call159:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[41], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch148;
compiled_branch149:
    cont = label(&&after_call150);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch148:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call150:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch151;
compiled_branch152:
    cont = label(&&after_call153);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch151:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call153:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch160;
compiled_branch161:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch160:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call162:
after_if144:
after_lambda141:
    define_variable(symbols[24], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry163), env);
    goto after_lambda164;
entry163:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], cons(symbols[1], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[43], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[44], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch168;
compiled_branch169:
    cont = label(&&after_call170);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch168:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call170:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch171;
compiled_branch172:
    cont = label(&&after_call173);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch171:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call173:
    argl = cons(val, nil());
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch174;
compiled_branch175:
    cont = label(&&after_call176);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch174:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call176:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch166;
true_branch165:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[45], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch177;
compiled_branch178:
    cont = label(&&after_call179);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch177:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call179:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch180;
compiled_branch181:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch180:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call182:
false_branch166:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[46], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch183;
compiled_branch184:
    cont = label(&&after_call185);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch183:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call185:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch186;
compiled_branch187:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch186:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call188:
after_if167:
after_lambda164:
    define_variable(symbols[12], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry189), env);
    goto after_lambda190;
entry189:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[38], cons(symbols[1], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[47], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch194;
compiled_branch195:
    cont = label(&&after_call196);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch194:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call196:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch192;
true_branch191:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[48], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch197;
compiled_branch198:
    cont = label(&&after_call199);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch197:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call199:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch200;
compiled_branch201:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch200:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call202:
false_branch192:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[48], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch203;
compiled_branch204:
    cont = label(&&after_call205);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch203:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call205:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch206;
compiled_branch207:
    cont = label(&&after_call208);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch206:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call208:
    env = stack_pop();
    cont = stack_pop();
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[18], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[49], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch209;
compiled_branch210:
    cont = label(&&after_call211);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch209:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call211:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch212;
compiled_branch213:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch212:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call214:
after_if193:
after_lambda190:
    define_variable(symbols[18], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry215), env);
    goto after_lambda216;
entry215:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], cons(symbols[1], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[50], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(env);
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[52], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch220;
compiled_branch221:
    cont = label(&&after_call222);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch220:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call222:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch223;
compiled_branch224:
    cont = label(&&after_call225);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch223:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call225:
    argl = stack_pop();
    argl = cons(val, argl);
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[51], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch217;
compiled_branch218:
    cont = label(&&after_call219);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch217:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call219:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch226;
compiled_branch227:
    cont = label(&&after_call228);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch226:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call228:
    cont = stack_pop();
    val = symbols[27];
    goto *c_label(cont);
after_lambda216:
    define_variable(symbols[8], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry229), env);
    goto after_lambda230;
entry229:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], cons(symbols[1], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[53], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(env);
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[55], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch234;
compiled_branch235:
    cont = label(&&after_call236);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch234:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call236:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch237;
compiled_branch238:
    cont = label(&&after_call239);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch237:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call239:
    argl = stack_pop();
    argl = cons(val, argl);
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[54], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch231;
compiled_branch232:
    cont = label(&&after_call233);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch231:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call233:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch240;
compiled_branch241:
    cont = label(&&after_call242);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch240:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call242:
    cont = stack_pop();
    val = symbols[27];
    goto *c_label(cont);
after_lambda230:
    define_variable(symbols[10], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry243), env);
    goto after_lambda244;
entry243:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[56], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch248;
compiled_branch249:
    cont = label(&&after_call250);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch248:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call250:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch246;
true_branch245:
    env = get_global_environment();
    val = lookup_variable_value(symbols[57], env);
    goto *c_label(cont);
false_branch246:
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[58], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch254;
compiled_branch255:
    cont = label(&&after_call256);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch254:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call256:
    cont = stack_pop();
    if(is_false(val)) goto false_branch252;
true_branch251:
    env = get_global_environment();
    val = lookup_variable_value(symbols[57], env);
    goto *c_label(cont);
false_branch252:
    env = get_global_environment();
    val = lookup_variable_value(symbols[59], env);
    goto *c_label(cont);
after_if253:
after_if247:
after_lambda244:
    define_variable(symbols[2], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry257), env);
    goto after_lambda258;
entry257:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[60], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch259;
compiled_branch260:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch259:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call261:
after_lambda258:
    define_variable(symbols[3], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry262), env);
    goto after_lambda263;
entry262:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[61], env);
    env = stack_pop();
    val = symbols[62];
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch264;
compiled_branch265:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch264:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call266:
after_lambda263:
    define_variable(symbols[5], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry267), env);
    goto after_lambda268;
entry267:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch269;
compiled_branch270:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch269:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call271:
after_lambda268:
    define_variable(symbols[6], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry272), env);
    goto after_lambda273;
entry272:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], cons(symbols[64], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[65], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch277;
compiled_branch278:
    cont = label(&&after_call279);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch277:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call279:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch275;
true_branch274:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[66], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch280;
compiled_branch281:
    cont = label(&&after_call282);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch280:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call282:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch283;
compiled_branch284:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch283:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call285:
false_branch275:
    env = get_global_environment();
    val = lookup_variable_value(symbols[59], env);
    goto *c_label(cont);
after_if276:
after_lambda273:
    define_variable(symbols[61], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry286), env);
    goto after_lambda287;
entry286:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[61], env);
    env = stack_pop();
    val = symbols[68];
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch288;
compiled_branch289:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch288:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call290:
after_lambda287:
    define_variable(symbols[7], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry291), env);
    goto after_lambda292;
entry291:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch293;
compiled_branch294:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch293:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call295:
after_lambda292:
    define_variable(symbols[51], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry296), env);
    goto after_lambda297;
entry296:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[69], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch298;
compiled_branch299:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch298:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call300:
after_lambda297:
    define_variable(symbols[52], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry301), env);
    goto after_lambda302;
entry301:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[61], env);
    env = stack_pop();
    val = symbols[70];
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch303;
compiled_branch304:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch303:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call305:
after_lambda302:
    define_variable(symbols[9], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry306), env);
    goto after_lambda307;
entry306:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[60], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch311;
compiled_branch312:
    cont = label(&&after_call313);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch311:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call313:
    argl = cons(val, nil());
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch314;
compiled_branch315:
    cont = label(&&after_call316);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch314:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call316:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch309;
true_branch308:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch317;
compiled_branch318:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch317:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call319:
false_branch309:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[71], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch320;
compiled_branch321:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch320:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call322:
after_if310:
after_lambda307:
    define_variable(symbols[54], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry323), env);
    goto after_lambda324;
entry323:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[60], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch328;
compiled_branch329:
    cont = label(&&after_call330);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch328:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call330:
    argl = cons(val, nil());
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch331;
compiled_branch332:
    cont = label(&&after_call333);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch331:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call333:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch326;
true_branch325:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[69], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch334;
compiled_branch335:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch334:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call336:
false_branch326:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[72], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[74], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch340;
compiled_branch341:
    cont = label(&&after_call342);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch340:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call342:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[73], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch337;
compiled_branch338:
    cont = label(&&after_call339);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch337:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call339:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch343;
compiled_branch344:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch343:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call345:
after_if327:
after_lambda324:
    define_variable(symbols[55], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry346), env);
    goto after_lambda347;
entry346:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[61], env);
    env = stack_pop();
    val = symbols[75];
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch348;
compiled_branch349:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch348:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call350:
after_lambda347:
    define_variable(symbols[13], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry351), env);
    goto after_lambda352;
entry351:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch353;
compiled_branch354:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch353:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call355:
after_lambda352:
    define_variable(symbols[15], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry356), env);
    goto after_lambda357;
entry356:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[74], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch358;
compiled_branch359:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch358:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call360:
after_lambda357:
    define_variable(symbols[16], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry361), env);
    goto after_lambda362;
entry361:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[76], cons(symbols[77], nil())), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch363;
compiled_branch364:
    cont = label(&&after_call365);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch363:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call365:
    argl = cons(val, nil());
    val = symbols[75];
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch366;
compiled_branch367:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch366:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call368:
after_lambda362:
    define_variable(symbols[72], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry369), env);
    goto after_lambda370;
entry369:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[61], env);
    env = stack_pop();
    val = symbols[78];
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch371;
compiled_branch372:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch371:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call373:
after_lambda370:
    define_variable(symbols[11], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry374), env);
    goto after_lambda375;
entry374:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch376;
compiled_branch377:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch376:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call378:
after_lambda375:
    define_variable(symbols[44], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry379), env);
    goto after_lambda380;
entry379:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[69], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch381;
compiled_branch382:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch381:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call383:
after_lambda380:
    define_variable(symbols[45], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry384), env);
    goto after_lambda385;
entry384:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[79], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[80], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch389;
compiled_branch390:
    cont = label(&&after_call391);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch389:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call391:
    argl = cons(val, nil());
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch392;
compiled_branch393:
    cont = label(&&after_call394);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch392:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call394:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch387;
true_branch386:
    val = symbols[59];
    goto *c_label(cont);
false_branch387:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[81], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch395;
compiled_branch396:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch395:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call397:
after_if388:
after_lambda385:
    define_variable(symbols[46], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry398), env);
    goto after_lambda399;
entry398:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[82], cons(symbols[83], cons(symbols[84], nil()))), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, argl);
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    val = symbols[78];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch400;
compiled_branch401:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch400:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call402:
after_lambda399:
    define_variable(symbols[86], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry403), env);
    goto after_lambda404;
entry403:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[61], env);
    env = stack_pop();
    val = symbols[87];
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch405;
compiled_branch406:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch405:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call407:
after_lambda404:
    define_variable(symbols[17], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry408), env);
    goto after_lambda409;
entry408:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch410;
compiled_branch411:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch410:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call412:
after_lambda409:
    define_variable(symbols[19], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry413), env);
    goto after_lambda414;
entry413:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[89], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[79], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch415;
compiled_branch416:
    cont = label(&&after_call417);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch415:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call417:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch418;
compiled_branch419:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch418:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call420:
after_lambda414:
    define_variable(symbols[47], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry421), env);
    goto after_lambda422;
entry421:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[89], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch423;
compiled_branch424:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch423:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call425:
after_lambda422:
    define_variable(symbols[48], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry426), env);
    goto after_lambda427;
entry426:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[89], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch428;
compiled_branch429:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch428:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call430:
after_lambda427:
    define_variable(symbols[49], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry431), env);
    goto after_lambda432;
entry431:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[89], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[79], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch436;
compiled_branch437:
    cont = label(&&after_call438);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch436:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call438:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch434;
true_branch433:
    val = lexical_address_lookup(0, 0, env);
    goto *c_label(cont);
false_branch434:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[47], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch442;
compiled_branch443:
    cont = label(&&after_call444);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch442:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call444:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch440;
true_branch439:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[48], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch445;
compiled_branch446:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch445:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call447:
false_branch440:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[90], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch448;
compiled_branch449:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch448:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call450:
after_if441:
after_if435:
after_lambda432:
    define_variable(symbols[91], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry451), env);
    goto after_lambda452;
entry451:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[89], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    val = symbols[87];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch453;
compiled_branch454:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch453:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call455:
after_lambda452:
    define_variable(symbols[90], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry456), env);
    goto after_lambda457;
entry456:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[65], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch458;
compiled_branch459:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch458:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call460:
after_lambda457:
    define_variable(symbols[20], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry461), env);
    goto after_lambda462;
entry461:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch463;
compiled_branch464:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch463:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call465:
after_lambda462:
    define_variable(symbols[23], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry466), env);
    goto after_lambda467;
entry466:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[0], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch468;
compiled_branch469:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch468:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call470:
after_lambda467:
    define_variable(symbols[25], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry471), env);
    goto after_lambda472;
entry471:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[92], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[79], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch473;
compiled_branch474:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch473:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call475:
after_lambda472:
    define_variable(symbols[39], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry476), env);
    goto after_lambda477;
entry476:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[92], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch478;
compiled_branch479:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch478:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call480:
after_lambda477:
    define_variable(symbols[41], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry481), env);
    goto after_lambda482;
entry481:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[92], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch483;
compiled_branch484:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch483:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call485:
after_lambda482:
    define_variable(symbols[42], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry486), env);
    goto after_lambda487;
entry486:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[93], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[94], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[66], env);
    env = stack_pop();
    stack_push(env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[59], env);
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch488;
compiled_branch489:
    cont = label(&&after_call490);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch488:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call490:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch491;
compiled_branch492:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch491:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call493:
after_lambda487:
    define_variable(symbols[43], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry494), env);
    goto after_lambda495;
entry494:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[93], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[66], env);
    env = stack_pop();
    stack_push(env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[59], env);
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch496;
compiled_branch497:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch496:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call498:
after_lambda495:
    define_variable(symbols[95], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry499), env);
    goto after_lambda500;
entry499:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[76], cons(symbols[77], cons(symbols[1], nil()))), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, argl);
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    val = symbols[29];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch501;
compiled_branch502:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch501:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call503:
after_lambda500:
    define_variable(symbols[14], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry504), env);
    goto after_lambda505;
entry504:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[96], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[61], env);
    env = stack_pop();
    val = symbols[29];
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch506;
compiled_branch507:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch506:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call508:
after_lambda505:
    define_variable(symbols[33], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry509), env);
    goto after_lambda510;
entry509:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[96], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch511;
compiled_branch512:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch511:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call513:
after_lambda510:
    define_variable(symbols[36], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry514), env);
    goto after_lambda515;
entry514:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[96], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[69], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch516;
compiled_branch517:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch516:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call518:
after_lambda515:
    define_variable(symbols[34], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry519), env);
    goto after_lambda520;
entry519:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[96], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[81], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch521;
compiled_branch522:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch521:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call523:
after_lambda520:
    define_variable(symbols[37], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry524), env);
    goto after_lambda525;
entry524:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[1], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch526;
compiled_branch527:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch526:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call528:
after_lambda525:
    define_variable(symbols[97], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry529), env);
    goto after_lambda530;
entry529:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[1], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch531;
compiled_branch532:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch531:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call533:
after_lambda530:
    define_variable(symbols[98], val, env);
    val = symbols[27];
    val = nil();
    define_variable(symbols[99], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry534), env);
    goto after_lambda535;
entry534:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[100], cons(symbols[101], nil())), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch536;
compiled_branch537:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch536:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call538:
after_lambda535:
    define_variable(symbols[102], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry539), env);
    goto after_lambda540;
entry539:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[103], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch541;
compiled_branch542:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch541:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call543:
after_lambda540:
    define_variable(symbols[104], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry544), env);
    goto after_lambda545;
entry544:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[103], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch546;
compiled_branch547:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch546:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call548:
after_lambda545:
    define_variable(symbols[105], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry549), env);
    goto after_lambda550;
entry549:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[106], cons(symbols[107], cons(symbols[103], nil()))), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[108], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch551;
compiled_branch552:
    cont = label(&&after_call553);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch551:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call553:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch554;
compiled_branch555:
    cont = label(&&after_call556);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch554:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call556:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch557;
compiled_branch558:
    cont = label(&&after_call559);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch557:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call559:
    env = stack_pop();
    cont = stack_pop();
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[109], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch560;
compiled_branch561:
    cont = label(&&after_call562);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch560:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call562:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch563;
compiled_branch564:
    cont = label(&&after_call565);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch563:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call565:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch566;
compiled_branch567:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch566:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call568:
after_lambda550:
    define_variable(symbols[110], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry569), env);
    goto after_lambda570;
entry569:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[111], cons(symbols[112], cons(symbols[113], nil()))), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[114], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[115], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch577;
compiled_branch578:
    cont = label(&&after_call579);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch577:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call579:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[115], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch574;
compiled_branch575:
    cont = label(&&after_call576);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch574:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call576:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch580;
compiled_branch581:
    cont = label(&&after_call582);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch580:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call582:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch572;
true_branch571:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[102], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch583;
compiled_branch584:
    cont = label(&&after_call585);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch583:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call585:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch586;
compiled_branch587:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch586:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call588:
false_branch572:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[116], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[115], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch595;
compiled_branch596:
    cont = label(&&after_call597);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch595:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call597:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[115], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch592;
compiled_branch593:
    cont = label(&&after_call594);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch592:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call594:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch598;
compiled_branch599:
    cont = label(&&after_call600);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch598:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call600:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch590;
true_branch589:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[26], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    val = string("Too many arguments supplied");
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch601;
compiled_branch602:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch601:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call603:
false_branch590:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[26], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    val = string("Too few arguments supplied");
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch604;
compiled_branch605:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch604:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call606:
after_if591:
after_if573:
after_lambda570:
    define_variable(symbols[35], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry607), env);
    goto after_lambda608;
entry607:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[106], cons(symbols[1], nil())), argl, env);
    proc = make_compiled_procedure(label(&&entry609), env);
    goto after_lambda610;
entry609:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[117], nil()), argl, env);
    val = make_compiled_procedure(label(&&entry611), env);
    goto after_lambda612;
entry611:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[1], nil()), argl, env);
    proc = make_compiled_procedure(label(&&entry613), env);
    goto after_lambda614;
entry613:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[118], nil()), argl, env);
    val = make_compiled_procedure(label(&&entry615), env);
    goto after_lambda616;
entry615:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[111], cons(symbols[112], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[79], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch620;
compiled_branch621:
    cont = label(&&after_call622);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch620:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call622:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch618;
true_branch617:
    proc = lexical_address_lookup(3, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[97], env);
    env = stack_pop();
    val = lexical_address_lookup(2, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch623;
compiled_branch624:
    cont = label(&&after_call625);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch623:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call625:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch626;
compiled_branch627:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch626:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call628:
false_branch618:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[66], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch632;
compiled_branch633:
    cont = label(&&after_call634);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch632:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call634:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(4, 0, env);
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch635;
compiled_branch636:
    cont = label(&&after_call637);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch635:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call637:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch630;
true_branch629:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch638;
compiled_branch639:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch638:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call640:
false_branch630:
    proc = lexical_address_lookup(1, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch644;
compiled_branch645:
    cont = label(&&after_call646);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch644:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call646:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch641;
compiled_branch642:
    cont = label(&&after_call643);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch641:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call643:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch647;
compiled_branch648:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch647:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call649:
after_if631:
after_if619:
after_lambda616:
    lexical_address_set(0, 0, val, env);
    val = symbols[27];
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[66], env);
    env = stack_pop();
    stack_push(env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[99], env);
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(1, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch653;
compiled_branch654:
    cont = label(&&after_call655);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch653:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call655:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch651;
true_branch650:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[26], env);
    env = stack_pop();
    val = lexical_address_lookup(3, 0, env);
    argl = cons(val, nil());
    val = string("Unbound variable");
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch656;
compiled_branch657:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch656:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call658:
false_branch651:
    proc = make_compiled_procedure(label(&&entry659), env);
    goto after_lambda660;
entry659:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[103], nil()), argl, env);
    proc = lexical_address_lookup(1, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[105], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch664;
compiled_branch665:
    cont = label(&&after_call666);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch664:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call666:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[104], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch661;
compiled_branch662:
    cont = label(&&after_call663);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch661:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call663:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch667;
compiled_branch668:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch667:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call669:
after_lambda660:
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[98], env);
    env = stack_pop();
    val = lexical_address_lookup(1, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch670;
compiled_branch671:
    cont = label(&&after_call672);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch670:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call672:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch673;
compiled_branch674:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch673:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call675:
after_if652:
after_lambda614:
    val = symbols[119];
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch676;
compiled_branch677:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch676:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call678:
after_lambda612:
    lexical_address_set(0, 0, val, env);
    val = symbols[27];
    proc = lexical_address_lookup(0, 0, env);
    val = lexical_address_lookup(1, 1, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch679;
compiled_branch680:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch679:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call681:
after_lambda610:
    val = symbols[119];
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch682;
compiled_branch683:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch682:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call684:
after_lambda608:
    define_variable(symbols[4], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry685), env);
    goto after_lambda686;
entry685:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[106], cons(symbols[107], cons(symbols[1], nil()))), argl, env);
    proc = make_compiled_procedure(label(&&entry687), env);
    goto after_lambda688;
entry687:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[117], nil()), argl, env);
    val = make_compiled_procedure(label(&&entry689), env);
    goto after_lambda690;
entry689:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[1], nil()), argl, env);
    proc = make_compiled_procedure(label(&&entry691), env);
    goto after_lambda692;
entry691:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[118], nil()), argl, env);
    val = make_compiled_procedure(label(&&entry693), env);
    goto after_lambda694;
entry693:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[111], cons(symbols[112], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[79], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch698;
compiled_branch699:
    cont = label(&&after_call700);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch698:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call700:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch696;
true_branch695:
    proc = lexical_address_lookup(3, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[97], env);
    env = stack_pop();
    val = lexical_address_lookup(2, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch701;
compiled_branch702:
    cont = label(&&after_call703);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch701:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call703:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch704;
compiled_branch705:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch704:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call706:
false_branch696:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[66], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch710;
compiled_branch711:
    cont = label(&&after_call712);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch710:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call712:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(4, 0, env);
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch713;
compiled_branch714:
    cont = label(&&after_call715);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch713:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call715:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch708;
true_branch707:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[108], env);
    env = stack_pop();
    val = lexical_address_lookup(4, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch716;
compiled_branch717:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch716:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call718:
false_branch708:
    proc = lexical_address_lookup(1, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch722;
compiled_branch723:
    cont = label(&&after_call724);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch722:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call724:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch719;
compiled_branch720:
    cont = label(&&after_call721);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch719:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call721:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch725;
compiled_branch726:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch725:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call727:
after_if709:
after_if697:
after_lambda694:
    lexical_address_set(0, 0, val, env);
    val = symbols[27];
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[66], env);
    env = stack_pop();
    stack_push(env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[99], env);
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(1, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch731;
compiled_branch732:
    cont = label(&&after_call733);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch731:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call733:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch729;
true_branch728:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[26], env);
    env = stack_pop();
    val = lexical_address_lookup(3, 0, env);
    argl = cons(val, nil());
    val = string("Unbound variable -- SET!");
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch734;
compiled_branch735:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch734:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call736:
false_branch729:
    proc = make_compiled_procedure(label(&&entry737), env);
    goto after_lambda738;
entry737:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[103], nil()), argl, env);
    proc = lexical_address_lookup(1, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[105], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch742;
compiled_branch743:
    cont = label(&&after_call744);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch742:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call744:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[104], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch739;
compiled_branch740:
    cont = label(&&after_call741);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch739:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call741:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch745;
compiled_branch746:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch745:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call747:
after_lambda738:
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[98], env);
    env = stack_pop();
    val = lexical_address_lookup(1, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch748;
compiled_branch749:
    cont = label(&&after_call750);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch748:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call750:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch751;
compiled_branch752:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch751:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call753:
after_if730:
after_lambda692:
    val = symbols[119];
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch754;
compiled_branch755:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch754:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call756:
after_lambda690:
    lexical_address_set(0, 0, val, env);
    val = symbols[27];
    proc = lexical_address_lookup(0, 0, env);
    val = lexical_address_lookup(1, 2, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch757;
compiled_branch758:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch757:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call759:
after_lambda688:
    val = symbols[119];
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch760;
compiled_branch761:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch760:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call762:
after_lambda686:
    define_variable(symbols[50], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry763), env);
    goto after_lambda764;
entry763:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[106], cons(symbols[107], cons(symbols[1], nil()))), argl, env);
    proc = make_compiled_procedure(label(&&entry765), env);
    goto after_lambda766;
entry765:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[103], nil()), argl, env);
    proc = make_compiled_procedure(label(&&entry767), env);
    goto after_lambda768;
entry767:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[118], nil()), argl, env);
    val = make_compiled_procedure(label(&&entry769), env);
    goto after_lambda770;
entry769:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[111], cons(symbols[112], nil())), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[79], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch774;
compiled_branch775:
    cont = label(&&after_call776);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch774:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call776:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch772;
true_branch771:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[110], env);
    env = stack_pop();
    val = lexical_address_lookup(2, 0, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(3, 1, env);
    argl = cons(val, argl);
    val = lexical_address_lookup(3, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch777;
compiled_branch778:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch777:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call779:
false_branch772:
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[66], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch783;
compiled_branch784:
    cont = label(&&after_call785);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch783:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call785:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(3, 0, env);
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch786;
compiled_branch787:
    cont = label(&&after_call788);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch786:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call788:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch781;
true_branch780:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[108], env);
    env = stack_pop();
    val = lexical_address_lookup(3, 1, env);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch789;
compiled_branch790:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch789:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call791:
false_branch781:
    proc = lexical_address_lookup(1, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch795;
compiled_branch796:
    cont = label(&&after_call797);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch795:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call797:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch792;
compiled_branch793:
    cont = label(&&after_call794);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch792:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call794:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch798;
compiled_branch799:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch798:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call800:
after_if782:
after_if773:
after_lambda770:
    lexical_address_set(0, 0, val, env);
    val = symbols[27];
    proc = lexical_address_lookup(0, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[105], env);
    env = stack_pop();
    val = lexical_address_lookup(1, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch804;
compiled_branch805:
    cont = label(&&after_call806);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch804:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call806:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[104], env);
    env = stack_pop();
    val = lexical_address_lookup(1, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch801;
compiled_branch802:
    cont = label(&&after_call803);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch801:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call803:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch807;
compiled_branch808:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch807:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call809:
after_lambda768:
    val = symbols[119];
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch810;
compiled_branch811:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch810:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call812:
after_lambda766:
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[98], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch813;
compiled_branch814:
    cont = label(&&after_call815);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch813:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call815:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch816;
compiled_branch817:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch816:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call818:
after_lambda764:
    define_variable(symbols[53], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry819), env);
    goto after_lambda820;
entry819:
    env = compiled_procedure_env(proc);
    env = extend_environment(nil(), argl, env);
    proc = make_compiled_procedure(label(&&entry821), env);
    goto after_lambda822;
entry821:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[120], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[53], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    env = get_global_environment();
    val = lookup_variable_value(symbols[57], env);
    argl = cons(val, argl);
    val = symbols[57];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch823;
compiled_branch824:
    cont = label(&&after_call825);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch823:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call825:
    env = stack_pop();
    cont = stack_pop();
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[53], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    env = get_global_environment();
    val = lookup_variable_value(symbols[59], env);
    argl = cons(val, argl);
    val = symbols[59];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch826;
compiled_branch827:
    cont = label(&&after_call828);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch826:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call828:
    env = stack_pop();
    cont = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    goto *c_label(cont);
after_lambda822:
    stack_push(cont);
    stack_push(proc);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[35], env);
    stack_push(proc);
    env = get_global_environment();
    val = lookup_variable_value(symbols[99], env);
    argl = cons(val, nil());
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[123], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[122], env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch832;
compiled_branch833:
    cont = label(&&after_call834);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch832:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call834:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[121], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[122], env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch829;
compiled_branch830:
    cont = label(&&after_call831);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch829:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call831:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch835;
compiled_branch836:
    cont = label(&&after_call837);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch835:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call837:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch838;
compiled_branch839:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch838:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call840:
after_lambda820:
    define_variable(symbols[124], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry841), env);
    goto after_lambda842;
entry841:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[125], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[61], env);
    env = stack_pop();
    val = symbols[126];
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch843;
compiled_branch844:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch843:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call845:
after_lambda842:
    define_variable(symbols[31], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry846), env);
    goto after_lambda847;
entry846:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[125], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch848;
compiled_branch849:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch848:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call850:
after_lambda847:
    define_variable(symbols[127], val, env);
    val = symbols[27];
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    stack_push(proc);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[133], env);
    argl = cons(val, nil());
    val = symbols[133];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch896;
compiled_branch897:
    cont = label(&&after_call898);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch896:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call898:
    argl = cons(val, nil());
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[132], env);
    argl = cons(val, nil());
    val = symbols[132];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch893;
compiled_branch894:
    cont = label(&&after_call895);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch893:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call895:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[131], env);
    argl = cons(val, nil());
    val = symbols[131];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch890;
compiled_branch891:
    cont = label(&&after_call892);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch890:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call892:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[130], env);
    argl = cons(val, nil());
    val = symbols[130];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch887;
compiled_branch888:
    cont = label(&&after_call889);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch887:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call889:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[129], env);
    argl = cons(val, nil());
    val = symbols[129];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch884;
compiled_branch885:
    cont = label(&&after_call886);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch884:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call886:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[116], env);
    argl = cons(val, nil());
    val = symbols[116];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch881;
compiled_branch882:
    cont = label(&&after_call883);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch881:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call883:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[114], env);
    argl = cons(val, nil());
    val = symbols[114];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch878;
compiled_branch879:
    cont = label(&&after_call880);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch878:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call880:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[60], env);
    argl = cons(val, nil());
    val = symbols[60];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch875;
compiled_branch876:
    cont = label(&&after_call877);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch875:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call877:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[65], env);
    argl = cons(val, nil());
    val = symbols[65];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch872;
compiled_branch873:
    cont = label(&&after_call874);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch872:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call874:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[56], env);
    argl = cons(val, nil());
    val = symbols[56];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch869;
compiled_branch870:
    cont = label(&&after_call871);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch869:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call871:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[79], env);
    argl = cons(val, nil());
    val = symbols[79];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch866;
compiled_branch867:
    cont = label(&&after_call868);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch866:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call868:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[128], env);
    argl = cons(val, nil());
    val = symbols[128];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch863;
compiled_branch864:
    cont = label(&&after_call865);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch863:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call865:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[66], env);
    argl = cons(val, nil());
    val = symbols[66];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch860;
compiled_branch861:
    cont = label(&&after_call862);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch860:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call862:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[40], env);
    argl = cons(val, nil());
    val = symbols[40];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch857;
compiled_branch858:
    cont = label(&&after_call859);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch857:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call859:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[88], env);
    argl = cons(val, nil());
    val = symbols[88];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch854;
compiled_branch855:
    cont = label(&&after_call856);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch854:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call856:
    argl = stack_pop();
    argl = cons(val, argl);
    stack_push(argl);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[67], env);
    argl = cons(val, nil());
    val = symbols[67];
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch851;
compiled_branch852:
    cont = label(&&after_call853);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch851:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call853:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch899;
compiled_branch900:
    cont = label(&&after_call901);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch899:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call901:
    env = stack_pop();
    define_variable(symbols[122], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry902), env);
    goto after_lambda903;
entry902:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[134], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[114], env);
    env = stack_pop();
    val = number(2);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch907;
compiled_branch908:
    cont = label(&&after_call909);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch907:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call909:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch905;
true_branch904:
    val = number(3);
    goto *c_label(cont);
false_branch905:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[130], env);
    env = stack_pop();
    val = number(2);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch910;
compiled_branch911:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch910:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call912:
after_if906:
after_lambda903:
    define_variable(symbols[135], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry913), env);
    goto after_lambda914;
entry913:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[122], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[136], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    env = get_global_environment();
    val = lookup_variable_value(symbols[67], env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch915;
compiled_branch916:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch915:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call917:
after_lambda914:
    define_variable(symbols[121], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry918), env);
    goto after_lambda919;
entry918:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[122], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[136], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    val = make_compiled_procedure(label(&&entry920), env);
    goto after_lambda921;
entry920:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[125], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[63], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch922;
compiled_branch923:
    cont = label(&&after_call924);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch922:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call924:
    argl = cons(val, nil());
    val = symbols[126];
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch925;
compiled_branch926:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch925:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call927:
after_lambda921:
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch928;
compiled_branch929:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch928:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call930:
after_lambda919:
    define_variable(symbols[123], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry931), env);
    goto after_lambda932;
entry931:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[125], cons(symbols[137], nil())), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[28], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[127], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch933;
compiled_branch934:
    cont = label(&&after_call935);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch933:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call935:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch936;
compiled_branch937:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch936:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call938:
after_lambda932:
    define_variable(symbols[32], val, env);
    val = symbols[27];
    val = string(";;; M-C-Eval input:");
    define_variable(symbols[138], val, env);
    val = symbols[27];
    val = string(";;; M-C-Eval value:");
    define_variable(symbols[139], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry939), env);
    goto after_lambda940;
entry939:
    env = compiled_procedure_env(proc);
    env = extend_environment(nil(), argl, env);
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[140], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[138], env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch941;
compiled_branch942:
    cont = label(&&after_call943);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch941:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call943:
    env = stack_pop();
    cont = stack_pop();
    stack_push(cont);
    proc = make_compiled_procedure(label(&&entry944), env);
    goto after_lambda945;
entry944:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[141], nil()), argl, env);
    proc = make_compiled_procedure(label(&&entry946), env);
    goto after_lambda947;
entry946:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[142], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[143], env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[139], env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch948;
compiled_branch949:
    cont = label(&&after_call950);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch948:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call950:
    env = stack_pop();
    cont = stack_pop();
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[144], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch951;
compiled_branch952:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch951:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call953:
after_lambda947:
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[22], env);
    env = stack_pop();
    stack_push(env);
    env = get_global_environment();
    val = lookup_variable_value(symbols[145], env);
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch954;
compiled_branch955:
    cont = label(&&after_call956);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch954:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call956:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch957;
compiled_branch958:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch957:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call959:
after_lambda945:
    stack_push(proc);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[146], env);
    argl = nil();
    if(is_primitive_procedure(proc)) goto primitive_branch960;
compiled_branch961:
    cont = label(&&after_call962);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch960:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call962:
    argl = cons(val, nil());
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch963;
compiled_branch964:
    cont = label(&&after_call965);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch963:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call965:
    cont = stack_pop();
    env = get_global_environment();
    proc = lookup_variable_value(symbols[147], env);
    argl = nil();
    if(is_primitive_procedure(proc)) goto primitive_branch966;
compiled_branch967:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch966:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call968:
after_lambda940:
    define_variable(symbols[147], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry969), env);
    goto after_lambda970;
entry969:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[148], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[149], env);
    argl = nil();
    if(is_primitive_procedure(proc)) goto primitive_branch971;
compiled_branch972:
    cont = label(&&after_call973);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch971:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call973:
    env = stack_pop();
    cont = stack_pop();
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[149], env);
    argl = nil();
    if(is_primitive_procedure(proc)) goto primitive_branch974;
compiled_branch975:
    cont = label(&&after_call976);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch974:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call976:
    env = stack_pop();
    cont = stack_pop();
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[150], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch977;
compiled_branch978:
    cont = label(&&after_call979);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch977:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call979:
    cont = stack_pop();
    env = get_global_environment();
    proc = lookup_variable_value(symbols[149], env);
    argl = nil();
    if(is_primitive_procedure(proc)) goto primitive_branch980;
compiled_branch981:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch980:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call982:
after_lambda970:
    define_variable(symbols[140], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry983), env);
    goto after_lambda984;
entry983:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[148], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[150], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch985;
compiled_branch986:
    cont = label(&&after_call987);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch985:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call987:
    cont = stack_pop();
    env = get_global_environment();
    proc = lookup_variable_value(symbols[149], env);
    argl = nil();
    if(is_primitive_procedure(proc)) goto primitive_branch988;
compiled_branch989:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch988:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call990:
after_lambda984:
    define_variable(symbols[143], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry991), env);
    goto after_lambda992;
entry991:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[151], nil()), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[33], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch996;
compiled_branch997:
    cont = label(&&after_call998);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch996:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call998:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch994;
true_branch993:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[150], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[85], env);
    env = stack_pop();
    stack_push(proc);
    val = symbols[153];
    argl = cons(val, nil());
    stack_push(env);
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[34], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch1002;
compiled_branch1003:
    cont = label(&&after_call1004);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1002:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1004:
    argl = stack_pop();
    argl = cons(val, argl);
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[36], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch999;
compiled_branch1000:
    cont = label(&&after_call1001);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch999:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1001:
    argl = stack_pop();
    argl = cons(val, argl);
    val = symbols[152];
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch1005;
compiled_branch1006:
    cont = label(&&after_call1007);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1005:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1007:
    argl = cons(val, nil());
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch1008;
compiled_branch1009:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1008:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call1010:
false_branch994:
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[150], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch1011;
compiled_branch1012:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1011:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call1013:
after_if995:
after_lambda992:
    define_variable(symbols[144], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry1014), env);
    goto after_lambda1015;
entry1014:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[154], cons(symbols[155], cons(symbols[156], nil()))), argl, env);
    stack_push(cont);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[79], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch1019;
compiled_branch1020:
    cont = label(&&after_call1021);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1019:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1021:
    env = stack_pop();
    cont = stack_pop();
    if(is_false(val)) goto false_branch1017;
true_branch1016:
    val = lexical_address_lookup(0, 1, env);
    goto *c_label(cont);
false_branch1017:
    proc = lexical_address_lookup(0, 0, env);
    stack_push(cont);
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[157], env);
    env = stack_pop();
    stack_push(proc);
    stack_push(env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[88], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch1025;
compiled_branch1026:
    cont = label(&&after_call1027);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1025:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1027:
    argl = cons(val, nil());
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, argl);
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, argl);
    proc = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch1028;
compiled_branch1029:
    cont = label(&&after_call1030);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1028:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1030:
    argl = cons(val, nil());
    env = stack_pop();
    stack_push(argl);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[67], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 2, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch1022;
compiled_branch1023:
    cont = label(&&after_call1024);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1022:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1024:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch1031;
compiled_branch1032:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1031:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call1033:
after_if1018:
after_lambda1015:
    define_variable(symbols[157], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry1034), env);
    goto after_lambda1035;
entry1034:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[96], cons(symbols[156], nil())), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[157], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    val = nil();
    argl = cons(val, argl);
    val = make_compiled_procedure(label(&&entry1036), env);
    goto after_lambda1037;
entry1036:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[93], cons(symbols[158], nil())), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[40], env);
    env = stack_pop();
    stack_push(cont);
    stack_push(proc);
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, nil());
    stack_push(argl);
    proc = lexical_address_lookup(1, 0, env);
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    if(is_primitive_procedure(proc)) goto primitive_branch1038;
compiled_branch1039:
    cont = label(&&after_call1040);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1038:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1040:
    argl = stack_pop();
    argl = cons(val, argl);
    proc = stack_pop();
    cont = stack_pop();
    if(is_primitive_procedure(proc)) goto primitive_branch1041;
compiled_branch1042:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1041:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call1043:
after_lambda1037:
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch1044;
compiled_branch1045:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1044:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call1046:
after_lambda1035:
    define_variable(symbols[136], val, env);
    val = symbols[27];
    val = make_compiled_procedure(label(&&entry1047), env);
    goto after_lambda1048;
entry1047:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[156], nil()), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[157], env);
    env = stack_pop();
    val = lexical_address_lookup(0, 0, env);
    argl = cons(val, nil());
    val = number(0);
    argl = cons(val, argl);
    val = make_compiled_procedure(label(&&entry1049), env);
    goto after_lambda1050;
entry1049:
    env = compiled_procedure_env(proc);
    env = extend_environment(cons(symbols[93], cons(symbols[158], nil())), argl, env);
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[130], env);
    env = stack_pop();
    val = number(1);
    argl = cons(val, nil());
    val = lexical_address_lookup(0, 1, env);
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch1051;
compiled_branch1052:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1051:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call1053:
after_lambda1050:
    argl = cons(val, argl);
    if(is_primitive_procedure(proc)) goto primitive_branch1054;
compiled_branch1055:
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1054:
    gc();
    val = apply_primitive_procedure(proc, argl);
    goto *c_label(cont);
after_call1056:
after_lambda1048:
    define_variable(symbols[115], val, env);
    val = symbols[27];
    stack_push(env);
    env = get_global_environment();
    proc = lookup_variable_value(symbols[124], env);
    argl = nil();
    if(is_primitive_procedure(proc)) goto primitive_branch1057;
compiled_branch1058:
    cont = label(&&after_call1059);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1057:
    gc();
    val = apply_primitive_procedure(proc, argl);
after_call1059:
    env = stack_pop();
    define_variable(symbols[145], val, env);
    val = symbols[27];
    env = get_global_environment();
    proc = lookup_variable_value(symbols[147], env);
    argl = nil();
    if(is_primitive_procedure(proc)) goto primitive_branch1060;
compiled_branch1061:
    cont = label(&&after_call1062);
    val = compiled_procedure_entry(proc);
    goto *c_label(val);
primitive_branch1060:
    gc();
    val = apply_primitive_procedure(proc, argl);
    after_call1062:
    // ---- end generated code ----

    free_memory();
    return 0;
}
