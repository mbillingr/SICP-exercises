#pragma once

#include "object.h"

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

Object append(Object seq1, Object seq2);
size_t list_length(Object list);

Object enclosing_environment(Object env);
Object first_frame(Object env);
Object make_frame(Object variables, Object values);
Object frame_variables(Object frame);
Object frame_values(Object frame);
void add_binding_to_frame(Object var, Object val, Object frame);
Object extend_environment(Object vars, Object vals, Object base_env);
Object lookup_variable_value(Object var, Object env);
void set_variable_value(Object var, Object val, Object env);
void define_variable(Object var, Object val, Object env);

Object setup_environment();

Object get_global_environment();

bool is_tagged_list(Object exp, Object tag);
bool is_self_evaluating(Object exp);
bool is_variable(Object exp);
bool is_quoted(Object exp);
Object text_of_quotation(Object exp);

bool is_assignment(Object exp);
Object assignment_variable(Object exp);
Object assignment_value(Object exp);

bool is_lambda(Object exp);
Object lambda_paramaters(Object exp);
Object lambda_body(Object exp);
Object make_lambda(Object parameters, Object body);

bool is_compound_procedure(Object proc);
Object make_procedure(Object parameters, Object body, Object env);
Object procedure_parameters(Object proc);
Object procedure_body(Object proc);
Object procedure_environment(Object proc);

bool is_definition(Object exp);
Object definition_variable(Object exp);
Object definition_value(Object exp);

bool is_if(Object exp);
Object if_predicate(Object exp);
Object if_consequent(Object exp);
Object if_alternative(Object exp);
Object* if_alternative_ptr(Object exp);
Object make_if(Object predicate, Object consequent, Object alternative);

bool is_begin(Object exp);
Object begin_actions(Object exp);
bool is_last_exp(Object seq);
Object first_exp(Object seq);
Object rest_exps(Object seq);
Object make_begin(Object seq);
Object sequence_to_exp(Object seq);

bool is_cond(Object exp);
Object cond_clauses(Object exp);
Object cond_predicate(Object clause);
Object cond_actions(Object clause);
bool is_cond_else_clause(Object clause);
Object cond_to_if(Object exp);

bool is_let(Object exp);
Object let_spec(Object exp);
Object let_body(Object exp);
Object let_vars(Object spec);
Object let_values(Object spec);
Object let_to_combination(Object exp);

bool is_application(Object exp);
Object operator(Object exp);
Object operands(Object exp);
bool no_operands(Object ops);
Object first_operand(Object ops);
Object rest_operands(Object ops);

Object empty_arglist();
Object adjoin_arg(Object arg, Object arglist);
bool is_last_operand(Object ops);

void user_print(Object obj);
