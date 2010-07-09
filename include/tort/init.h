#ifndef _tort_INIT_H
#define _tort_INIT_H

void tort_runtime_initialize_malloc();
void tort_runtime_initialize_error();
void tort_runtime_initialize_mtable();
void tort_runtime_initialize_symbol();
void tort_runtime_initialize_gc();
void tort_runtime_initialize_io();
void tort_runtime_initialize_write();
void tort_runtime_initialize_block();
void tort_runtime_initialize_debug();
void tort_runtime_initialize_symtab();
void tort_runtime_initialize_method();
void tort_runtime_initialize_lisp();

#endif
