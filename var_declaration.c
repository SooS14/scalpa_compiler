
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "scalpa.h"
#include "linked_list.h"
#include "var_declaration.h"

extern struct symbol_table_t symbol_table;

void init_symbol_table() {
    symbol_table.table_size = INIT_TABLE_SIZE;
    symbol_table.last_ident_index = 0;
    symbol_table.symbols = malloc(INIT_TABLE_SIZE * sizeof(struct symbol_t));
    symbol_table.cur_symbol_scope = 0;
}

void free_symbol_table() {
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        free(symbol_table.symbols[i].ident);
        switch (symbol_table.symbols[i].var_func_par) {
        case VAR_T:
            if (symbol_table.symbols[i].type.var.typename.range_list != NULL) {
                free(symbol_table.symbols[i].type.var.typename.range_list);
            }
            if (symbol_table.symbols[i].type.var.typename.range_array != NULL) {
                free(symbol_table.symbols[i].type.var.typename.range_array);
            }
            break;
        case FUNC_T:
            if (symbol_table.symbols[i].type.func.index_param != NULL) {
                free(symbol_table.symbols[i].type.func.index_param);
            }
            break;
        case PARAM_T:
            if (symbol_table.symbols[i].type.param.typename.range_list 
                != NULL) {
                free(symbol_table.symbols[i].type.param.typename.range_list);
            }
            if (symbol_table.symbols[i].type.param.typename.range_array 
                != NULL) {
                free(symbol_table.symbols[i].type.param.typename.range_array);
            }
            break;
        default : break;// ERROR exit (impossible)
        }
    }
    free(symbol_table.symbols);
}

void display_symbol(struct symbol_t *symbol, int index, int n) {
    char atomic_type[5];
    if (symbol->var_func_par == FUNC_T) {
        printf("\n");
    }
    for (int j = 0; j < n; j++) {
        printf(" ");
    }
    switch (symbol->var_func_par) {
    case VAR_T:
        switch (symbol->type.var.typename.atomic_type) {
            case VOID_A : strncpy(atomic_type, "unit", 5); break;
            case BOOL_A : strncpy(atomic_type, "bool", 5); break;
            case INT_A  : strncpy(atomic_type, "int ", 5); break;
            default     : strncpy(atomic_type, "??? ", 5); break;
        }
        printf("| variable  | %s        | %s", atomic_type, symbol->ident);
        if (symbol->type.var.typename.symbol_type == ARRAY_TYPE) {
            printf(" ( rangelist[");
            for (int j = 0; j < symbol->type.var.typename.len_range_list; j++) {
               printf("%i..%i,", symbol->type.var.typename.range_array[j][0],
                   symbol->type.var.typename.range_array[j][1]);
            }
            printf("] )");
        }
        break;
    
    case FUNC_T:
        switch (symbol->type.func.atomic_type) {
            case VOID_A : strncpy(atomic_type, "unit", 5); break;
            case BOOL_A : strncpy(atomic_type, "bool", 5); break;
            case INT_A  : strncpy(atomic_type, "int ", 5); break;
            default     : strncpy(atomic_type, "??? ", 5); break;
        }
        printf("| function  | %s        | %s", atomic_type, symbol->ident);
        printf(" ( parameters[");
        for (int j = 0; j < symbol->type.func.nb_param; j++) {
            printf("%i,", symbol->type.func.index_param[j]);
        }
        printf("] )");
        break;

    case PARAM_T:
        switch (symbol->type.param.typename.atomic_type) {
            case VOID_A : strncpy(atomic_type, "unit", 5); break;
            case BOOL_A : strncpy(atomic_type, "bool", 5); break;
            case INT_A  : strncpy(atomic_type, "int ", 5); break;
            default     : strncpy(atomic_type, "??? ", 5); break;
        }
        if (symbol->type.param.ref) {
            printf("| parameter | ref %s    | %s", atomic_type, symbol->ident);
        }
        else {
            printf("| parameter | %s        | %s", atomic_type, symbol->ident);
        }
        if (symbol->type.param.typename.symbol_type == ARRAY_TYPE) {
            printf(" ( rangelist[");
            for (int j = 0; j < symbol->type.param.typename.len_range_list; 
                 j++) {
               printf("%i..%i,", symbol->type.param.typename.range_array[j][0],
                   symbol->type.param.typename.range_array[j][1]);
            }
            printf("] )");
        }
        break;
    default: break; // ERROR exit (impossible)
    }
        printf("\r| %i\n", index);
}

void display_symbol_table() {
    switch (symbol_table.last_ident_index) {
        case 0: 
            printf("\nTABLE OF SYMBOLS : EMPTY\n\n"); return;
        case 1:
            printf("\nTABLE OF SYMBOLS : (1 symbol in table)\n\n"); break;
        default :
            printf("\nTABLE OF SYMBOLS : (%i symbols in table)\n\n", 
                symbol_table.last_ident_index);
    }
    int n = floor(log10(symbol_table.last_ident_index)) + 1;
    (n + 3 < 8) ? (n = 8) : (n += 3); // for column alignment
    for (int j = 0; j < n; j++) {
        printf(" ");
    }
    printf("| type      | atomic_type | identifier | rangelist/parameters\
 \r| index\n");
    for (int i = 0; i < symbol_table.last_ident_index; i ++) {
        display_symbol(&symbol_table.symbols[i], i, n);
    }
}

int is_symbol_in_table(char *identname, int scope) {
    int size = strlen(identname) + 1;
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        if (scope == symbol_table.symbols[i].scope &&
            size == symbol_table.symbols[i].ident_length &&
            !strncmp(identname, symbol_table.symbols[i].ident, size)) {
                return 1;
        }
    }
    return 0;
}

void realloc_table () {
    if (symbol_table.table_size <= symbol_table.last_ident_index + 1) {
        symbol_table.table_size += INIT_TABLE_SIZE;
        symbol_table.symbols = realloc(symbol_table.symbols,
            symbol_table.table_size * sizeof(struct symbol_t));
    }
}

void copy_typename_table(struct typename_t *dest, struct typename_t origin) {
    dest->atomic_type = origin.atomic_type;
    dest->symbol_type = origin.symbol_type;
    // if new variable is an array then, the rangelist is stored as an array
    // of type (* array) [2]
    dest->range_list = NULL;
    if (origin.symbol_type == ARRAY_TYPE) {
        int len = list_len(origin.range_list)/2;
        dest->len_range_list = len;
        dest->range_array = malloc(len * sizeof(int[2]));
        int i = 0;
        struct node *temp_node =  origin.range_list->first;
        while (temp_node != NULL) {
            dest->range_array[i][0] = *(int*)temp_node->data;
            temp_node = temp_node->next;
            dest->range_array[i][1] = *(int*)temp_node->data;
            temp_node = temp_node->next;
            i++;
        }
    }
    // can't be a function or a parameter
    else {
        dest->len_range_list = 0;
        dest->range_array = NULL;
    }
}

void add_var_symbol_list (struct linked_list *identlist, 
                          struct typename_t type_) {
    while (list_len(identlist) != 0) {
        if (is_symbol_in_table(list_get_first(identlist), 
            symbol_table.cur_symbol_scope)) {
                handle_error("identifier [%s] already declared.", 
                    list_get_first(identlist));
        }
        realloc_table();
        char *new_ident_name = (char *)list_get_first(identlist);
        struct symbol_t *new_symbol = 
            &symbol_table.symbols[symbol_table.last_ident_index];
        new_symbol->scope = symbol_table.cur_symbol_scope;
        new_symbol->var_func_par = VAR_T;
        new_symbol->ident_length = strlen(new_ident_name)+1;
        new_symbol->ident = malloc(new_symbol->ident_length);
        strncpy(new_symbol->ident, new_ident_name, new_symbol->ident_length);
        new_symbol->type.var.initialiazed = 0;
        copy_typename_table(&new_symbol->type.var.typename, type_);
        symbol_table.last_ident_index ++;
        list_pop(identlist);
    }
    list_free(identlist);
    if (type_.symbol_type == ARRAY_TYPE) {
        list_free(type_.range_list);
    }
}