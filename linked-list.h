#ifndef LINKED_LIST_H
#define LINKED_LIST_H

struct node {
  void *data;
  struct node * next;
};

struct linked_list {
  int length;
  struct node * first;
};

struct linked_list * list_init(void);

void list_free(struct linked_list *list);

void list_push(struct linked_list *list, void *x, int size_of_data);

void * list_get_first(struct linked_list *list);

/*
 * Don't return the data of the popped element, use list_get_first()
 * for that (or list->first->data)
 */
void list_pop(struct linked_list *list);

int list_len(struct linked_list *list);

#endif