#ifndef __TREE_H__
#define __TREE_H__

typedef enum {INT, CHAR, DOUBLE, BOOL, VOID, TREE} data_type;

union data_u {
	int i;
	char c;
	double d;
	struct tree *t;
};

struct tree {
	data_type type;
	union data_u data;
	int width;

	struct tree *children;
	struct tree *sibling;
};

void print (struct tree *str);

void init_tree (struct tree *root);

int equal(struct tree *lhs, struct tree *rhs);
int lt(struct tree *lhs, struct tree *rhs);
int gt(struct tree *lhs, struct tree *rhs);
int lte(struct tree *lhs, struct tree *rhs);
int gte(struct tree *lhs, struct tree *rhs);

struct tree *sub(struct tree *lhs, struct tree *rhs);
struct tree *add(struct tree *lhs, struct tree *rhs);
struct tree *mult(struct tree *lhs, struct tree *rhs);



struct tree *int_treemake(int i_data, struct tree *child, ...);
struct tree *char_treemake(char c_data, struct tree *child, ...);
struct tree *double_treemake(int d_data, struct tree *child, ...);
struct tree *tree_treemake(struct tree *t_data, struct tree *child, ...);

struct tree *treemake(data_type type, union data_u data, struct tree *child, va_list args);
struct tree *tree_of_string (char *str);
int add_sibling (struct tree *root, struct tree *sibling, int n);
int add_child (struct tree *root, struct tree *child);
void set_type (struct tree *root, data_type type);
data_type get_type(struct tree *root);
struct tree *get_ith_sibling(struct tree *root, int i);
struct tree *get_branch(struct tree *root, int branch);

#endif
