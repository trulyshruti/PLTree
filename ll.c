#include "ll.h"
#include <stdio.h>
#include <stdlib.h>


void init_list(struct List *list) {
	list->head = NULL;
	list->tail = NULL;
}

void *get(struct List *list, int n) {
	int i = 0;
	struct node *curr;

	curr = list->head;

	while (i < n && curr != NULL) {
		curr = curr->next;
		i++;
	}

	return curr->data;
}




void traverse_list(struct List *list, void (*f)(void *)) 
{
	//Start at the beginning
	struct node *curr = list->head;

	//Iterate until we get to the end of the list
	while (curr != NULL)
	{   
		f(curr->data);
		    
		//Move to next node
		curr = curr->next;
	}   
}


void free_list(struct List *list) {
	while (list->head != NULL) {
		struct node *curr = list->head;
		struct node *next = curr->next;
		free(curr);
		list->head = next;
	}
	free(list);
}



void append(struct List *list, void *data) {
	struct node *curr;
	struct node *new;

	curr = list->tail;

	if (curr == NULL) {
		curr = malloc(sizeof(struct node));
		curr->next = NULL;
		list->head = curr;
		list->tail = curr;
	} else {
		new = malloc(sizeof(struct node));

		new->data = data;
		
		new->next = NULL;

		curr->next = new;

		list->tail = new;
	}
}
