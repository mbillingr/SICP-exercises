#pragma once

#include "object.h"

void stack_initialize();
void stack_push(Object obj);
Object stack_pop();
void stack_push_label(void* label);
void* stack_pop_label();
