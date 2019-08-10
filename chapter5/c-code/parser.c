#include "parser.h"

#include "config.h"
#include <stdio.h>

static char input_buffer[INPUT_BUFFER_SIZE];
static char current_token[TOKEN_BUFFER_SIZE];
static char* input_cursor;

extern Object S_QUOTE;

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

Object read() {
    input_cursor = input_buffer;
    fgets(input_buffer, INPUT_BUFFER_SIZE, stdin);
    skip_whitespace();
    next_token();
    return parse_expression();
}
