#include <stdio.h>

typedef enum {INT, CHAR, DOUBLE, BOOL, VOID, TREE} data_type;

struct tree {
	data_type type;
	void *data;
	struct tree *children;
	struct tree *sibling;
};

void init_tree(struct tree *root) {
	root->data = NULL;
	root->children = NULL;
	root->sibling = NULL;
}


void add_sibling (struct tree *root, struct tree *sibling) {
	if (root->sibling == NULL) {
		root->sibling = sibling;
	} else {
		add_sibling(root->sibling, sibling);
	}
}

void add_child (struct tree *root, struct tree *child) {
	if (root->children == NULL) {
		root->children = child;
	} else {
		add_sibling(root->children, child);
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

	if (root->sibling == NULL) {
		return NULL;
	}

	return get_ith_sibling(root->sibling, i-1);
}

struct tree *get_branch(struct tree *root, int branch) {
	return get_ith_sibling(root->children, branch);
}
