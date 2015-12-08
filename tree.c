#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "tree.h"


void print(struct tree *str) {
	if (str == NULL)
		return;
	
	switch(str->type) {
		case CHAR:
			putchar(str->data.c);
			break;
		case INT:
			printf("%d", str->data.i);
			break;
		case DOUBLE:
			printf("%f", str->data.d);
			break;
		case TREE:
			print(str->data.t);
			break;
		default:
			break;
	}

	print(get_branch(str, 0));

	print(get_ith_sibling(str, 1));

}

int equal(struct tree *lhs, struct tree *rhs) {

	int retval;
	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = 0;
	} else {	
		switch(lhs->type) {
			case CHAR:
				retval = lhs->data.c == rhs->data.c;
				break;
			case INT:
				retval = lhs->data.i == rhs->data.i;
				break;
			case DOUBLE:
				retval = lhs->data.d == rhs->data.d;
				break;
			default:
				retval = 0;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);
	return retval;
}
int lt(struct tree *lhs, struct tree *rhs) {
	int retval;
	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		fprintf(stderr, "TYPE MISMATCH!\n");
		retval = 0;
	} else {	
		switch(lhs->type) {
			case CHAR:
				retval = lhs->data.c < rhs->data.c;
				break;
			case INT:
				retval = lhs->data.i < rhs->data.i;
				break;
			case DOUBLE:
				retval = lhs->data.d < rhs->data.d;
				break;
			default:
				retval = 0;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);
	return retval;
}
int gt(struct tree *lhs, struct tree *rhs) {
	int retval;
	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = 0;
	} else {	
		switch(lhs->type) {
			case CHAR:
				retval = lhs->data.c > rhs->data.c;
				break;
			case INT:
				retval = lhs->data.i > rhs->data.i;
				break;
			case DOUBLE:
				retval = lhs->data.d > rhs->data.d;
				break;
			default:
				retval = 0;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);
	return retval;
}
int lte(struct tree *lhs, struct tree *rhs) {
	return gt(rhs, lhs);
}
int gte(struct tree *lhs, struct tree *rhs) {
	return lt(rhs, lhs);
}


void free_tree(struct tree *t) {
	struct tree *child;

	if (t == NULL) {
		return;
	}

	while(t->children) {
		child = t->children;
		t->children = child->sibling;
		dec_refcount(child);
	}

	if (t->type == TREE) {
		dec_refcount(t->data.t);
	}

	free(t);
}



struct tree* inc_refcount(struct tree *t) {
	if (t) {
		t->refcount++;
	}

	return t;
}

struct tree* dec_refcount(struct tree *t) {
	if (t) {
		if (--(t->refcount) <= 0) {
			free_tree(t);
			t = NULL;
		}
	}

	return t;
}

struct tree *sub(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c - rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i - rhs->data.i, NULL);
				break;
			case DOUBLE:
				retval =  double_treemake(lhs->data.d - rhs->data.d, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;
}
struct tree *add(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c + rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i + rhs->data.i, NULL);
				break;
			case DOUBLE:
				retval =  double_treemake(lhs->data.d + rhs->data.d, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;
}

struct tree *mul(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c * rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i * rhs->data.i, NULL);
				break;
			case DOUBLE:
				retval =  double_treemake(lhs->data.d * rhs->data.d, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;

}

struct tree *divd(struct tree *lhs, struct tree *rhs) {
	struct tree *retval;

	inc_refcount(lhs);
	inc_refcount(rhs);

	if (lhs->type != rhs->type) {
		retval = NULL;
	} else {
		switch (lhs->type) {
			case CHAR:
				retval = char_treemake(lhs->data.c / rhs->data.c, NULL);
				break;
			case INT:
				retval =  int_treemake(lhs->data.i / rhs->data.i, NULL);
				break;
			case DOUBLE:
				retval =  double_treemake(lhs->data.d / rhs->data.d, NULL);
				break;
			default:	
				retval =  NULL;
		}
	}

	dec_refcount(rhs);
	dec_refcount(lhs);

	return retval;
}

void init_tree(struct tree *root) {
	root->children = NULL;
	root->sibling = NULL;
	root->type = VOID;
	root->width = 0;
}

struct tree *int_treemake(int i_data, struct tree *child, ...) {
	va_list args;
	union data_u data;
	struct tree *root;

	va_start(args, child);
	data.i = i_data;
	root = treemake(INT, data, child, args);
	va_end(args);


	return root;
}
	

struct tree *char_treemake(char c_data, struct tree *child, ...) {
	va_list args;
	union data_u data;
	struct tree *root;

	va_start(args, child);
	data.c = c_data;
	root = treemake(CHAR, data, child, args);
	va_end(args);

	return root;
}

	
struct tree *double_treemake(int d_data, struct tree *child, ...) {
	va_list args;
	union data_u data;
	struct tree *root;

	va_start(args, child);
	data.d = d_data;
	root = treemake(DOUBLE, data, child, args);
	va_end(args);

	return root;
}

struct tree *tree_treemake(struct tree *t_data, struct tree *child, ...) {
	va_list args;
	union data_u data;
	struct tree *root;

	va_start(args, child);
	data.t = t_data;
	root = treemake(TREE, data, child, args);
	va_end(args);

	return root;
}	

struct tree *void_treemake(struct tree *child, ...) {

	va_list args;
	struct tree *root;
	union data_u data;

	data.t = NULL;

	va_start(args, child);
	root = treemake(VOID, data, child, args);
	va_end(args);

	return root;
}

struct tree *treemake(data_type type, union data_u data, struct tree *child,  va_list args) {
	struct tree *root = malloc(sizeof(struct tree));

	init_tree(root);
	root->type = type;
	root->data = data;


	while (child != NULL) {
		add_child(root, child);
		child = va_arg(args, struct tree *);
	}

	return root;
}

int add_sibling (struct tree *root, struct tree *sibling, int n) {
	if (root == NULL)
		return -1;
	
	inc_refcount(sibling);

	if (root->sibling == NULL) {
		root->sibling = sibling;
		return n;
	} else {
		return add_sibling(root->sibling, sibling, n+1);
	}

}

int add_child (struct tree *root, struct tree *child) {
	if (root == NULL) {
		return -1;
	}
	inc_refcount(child);
	if (root->children == NULL) {
		root->children = child;
		root->width = 1;
		return 1;
	} else {
		int n = add_sibling(root->children, child, 2);
		root->width = n;
		return n;
	}
}

void set_type (struct tree *root, data_type t) {
	root->type = t;
}

data_type get_type(struct tree *t) {
	return t->type;
}

struct tree *get_ith_sibling(struct tree *root, int i) {
	if (i == 0) {
		return root;
	}

	if (root == NULL) {
		return NULL;
	}

	if (root->sibling == NULL) {
		return NULL;
	}

	return get_ith_sibling(root->sibling, i-1);
}

struct tree *get_branch(struct tree *root, int branch) {
	return get_ith_sibling(root->children, branch);
}
