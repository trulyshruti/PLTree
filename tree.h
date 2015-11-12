#ifndef __TREE_H__
#define __TREE_H__

typedef enum {INT, CHAR, DOUBLE, BOOL, VOID, TREE} data_type;

struct tree {
	data_type type;
	void *data;
	struct tree *children;
	struct tree *sibling;
};

void init_tree (struct tree *root);
void add_sibling (struct tree *root, struct tree *sibling);
void add_child (struct tree *root, struct tree *child);
void set_type (struct tree *root, data_type type);
data_type get_type(struct tree *root);
struct tree *get_ith_sibling(struct tree *root, int i);
struct tree *get_branch(struct tree *root, int branch);

#endif
