
#include "config.h"
#include "memory.h"
#include "object.h"
#include "parser.h"
#include "scheme.h"
#include "stack.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void error(const char* msg) {
    fprintf(stderr, msg);
    exit(EXIT_FAILURE);
}

extern Object stack, g_env;
static Object exp, env, val, proc, argl, unev;
void* cont;

extern Object S_OK, S_FALSE, S_QUOTE, S_SET_VAR, S_DEFINE, S_LAMBDA, S_IF,
              S_PROCEDURE,S_BEGIN,S_COND,S_ELSE,S_LET;


int main() {
    init_memory();
    init_symbols();

repl:
    stack_initialize();
    printf("\nmemory usage: %d\%\n", 100 * memory_usage() / MEMORY_SIZE);
    printf(";;; C-Eval input:\n");
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
    if(memory_usage() > MEMORY_SIZE / 10 * 9) {
        gc_reset_root();

        gc_push_root(exp);
        gc_push_root(proc);
        gc_push_root(argl);
        gc_push_root(unev);
        gc_push_root(stack);
        gc_push_root(env);
        gc_push_root(g_env);
        gc_push_root(val);

        collect_garbage();

        val = gc_pop_root();
        g_env = gc_pop_root();
        env = gc_pop_root();
        stack = gc_pop_root();
        unev = gc_pop_root();
        argl = gc_pop_root();
        proc = gc_pop_root();
        exp = gc_pop_root();
    }
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
    free_memory();
    return 0;
}
