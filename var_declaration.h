#ifndef VAR_DECLARATION_H
#define VAR_DECLARATION_H

#define INIT_TABLE_SIZE 1024

/**
 * @brief initialize the symbol table
 * call free_symbol_table() to free allocated memory later
 */
void init_symbol_table();

/**
 * @brief free all memory allocated for the symbol table and its element
 */
void free_symbol_table();

/**
 * @brief display symbol table
 * symbol table need to be initialized before calling this function
 */
void display_symbol_table();

/**
 * @brief check if the name of the symbol is in table
 * @param char * identname, name of the symbol
 * @result 1 if symbol is in table, else 0
 */
int is_symbol_in_table(char *identname);

/**
 * @brief add INIT_TABLE_SIZE allocated space to the current table 
 * for new symbols. This function is only called by add_symbol_list()
 */
void realloc_table ();

/**
 * @brief add multiple symbols in the symbol table
 * @param struct linked_list *identlist, list of symbols to add to the table
 * @param struct typename_t, type of symbols to add to the table,
 * contain arraytype / atomictype, rangelist if arraytype
 */
void add_symbol_list (struct linked_list *identlist, struct typename_t type);

#endif