
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
}

void free_symbol_table() {
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        free(symbol_table.symbols[i].ident);
        if (symbol_table.symbols[i].rangelist != NULL) {
            free(symbol_table.symbols[i].rangelist);
        }
    }
    free(symbol_table.symbols);
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
    printf("| atomic_type | identifier | rangelist\r| index\n");
    for (int i = 0; i < symbol_table.last_ident_index; i ++) {
        char atomic_type[5];
        switch (symbol_table.symbols[i].atomic_type) {
            case VOID_A : strncpy(atomic_type, "unit", 5); break;
            case BOOL_A : strncpy(atomic_type, "bool", 5); break;
            case INT_A  : strncpy(atomic_type, "int ", 5); break;
            default     : strncpy(atomic_type, "??? ", 5); break;
        }
        for (int j = 0; j < n; j++) {
            printf(" ");
        }
        printf("| %s        | %s", atomic_type, symbol_table.symbols[i].ident);
        if (symbol_table.symbols[i].symbol_type == ARRAY_TYPE) {
            printf(" ( rangelist[");
            for (int j = 0; j < symbol_table.symbols[i].len_range_list; j++) {
                printf("%i..%i,", symbol_table.symbols[i].rangelist[j][0],
                    symbol_table.symbols[i].rangelist[j][1]);
            }
            printf("] )");
        }
        printf("\r| %i\n", i);
    }
}

int is_symbol_in_table(char *identname) {
    int size = strlen(identname) + 1;
    for (int i = 0; i < symbol_table.last_ident_index; i++) {
        if (size == symbol_table.symbols[i].ident_length &&
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

void add_symbol_list (struct linked_list *identlist, struct typename_t type) {
    while (list_len(identlist) != 0) {
        if (is_symbol_in_table(list_get_first(identlist))) {
            handle_error("identifier [%s] already declared.", 
                list_get_first(identlist));
        }
        realloc_table();
        char *new_ident_name = (char *)list_get_first(identlist);
        struct symbol_t *new_symbol = 
            &symbol_table.symbols[symbol_table.last_ident_index];
        new_symbol->initialiazed = 0;
        new_symbol->ident_length = strlen(new_ident_name)+1;
        new_symbol->ident = malloc(new_symbol->ident_length);
        strncpy(new_symbol->ident, new_ident_name, new_symbol->ident_length);
        new_symbol->atomic_type = type.atomic_type;
        new_symbol->symbol_type = type.symbol_type;
        // if new variable is an array then, the rangelist is stored as an array
        // of type (* array) [2]
        if (new_symbol->symbol_type == ARRAY_TYPE) {
            new_symbol->len_range_list = list_len(type.rangelist)/2;
            new_symbol->rangelist = 
                malloc(new_symbol->len_range_list * sizeof(int[2]));
            int i = 0;
            struct node *temp_node =  type.rangelist->first;
            while (temp_node != NULL) {
                new_symbol->rangelist[i][0] = *(int*)temp_node->data;
                temp_node = temp_node->next;
                new_symbol->rangelist[i][1] = *(int*)temp_node->data;
                temp_node = temp_node->next;
                i++;
            }
        }
        else {
            new_symbol->len_range_list = 0;
            new_symbol->rangelist = NULL;
        }
        symbol_table.last_ident_index ++;
        list_pop(identlist);
    }
    list_free(identlist);
    if (type.symbol_type == ARRAY_TYPE) {
        list_free(type.rangelist);
    }
}