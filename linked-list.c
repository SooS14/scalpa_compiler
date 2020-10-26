#include <stdlib.h>
#include <stdio.h>
#include "linked-list.h"
#include "scalpa.h"

struct linked_list * list_init(void) {
    struct linked_list *list = malloc(sizeof(struct linked_list *));
    list->length = 0;
    list->first = NULL;
    return list;
}

void list_free(struct linked_list *list) {
    if (list == NULL) {
        return;
    }
    struct node * temp = list->first;
    while (temp != NULL) {
        temp = temp->next;
        list_pop(list);
    }
    free(list);
}

void list_push(struct linked_list *list, void *x) {
    if (list == NULL) {
        handle_error("list_push() can't push an element to a NULL\
         pointer\n");
    }
    struct node *new_element = malloc(sizeof(struct node));
    list->length ++;
    new_element = malloc(sizeof (struct node));
    new_element->data = x;
    new_element->next = list->first;
    list->first = new_element;
}

void * list_pop(struct linked_list *list) {
    if (list == NULL) {
        handle_error("list_pop() can't pop an element from a NULL\
         pointer\n");
    }
    if (list->length == 0) {
            return NULL;
    }
    struct node *first = list->first;
    list->length --;
    void *x = first->data;
    list->first = first->next;
    free(first);
    return x;
}

int len_list(struct linked_list *list) {
    if (list == NULL) {
        handle_error("len_list() can't get length of a NULL pointer\n");
    }
    return list->length;
}