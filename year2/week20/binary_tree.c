#include <stdlib.h>

// Find the leaks

struct tree_node {
    int datum;
    struct tree_node* left;
    struct tree_node* right;
};

typedef struct tree_node tree_node;

typedef struct {
    tree_node* root;
} tree;

tree* tree_init() {
    tree* t = (tree*)malloc(sizeof(tree));
    t->root = NULL;

    return t;
}

tree_node* node_init(int x) {
    tree_node* n = (tree_node*)malloc(sizeof(tree_node));
    n->left = NULL;
    n->right = NULL;
    n->datum = x;

    return n;
}

void __tree_insert(tree_node* root, tree_node* leaf) {
    if (root->datum == leaf->datum)
        return;

    if (leaf->datum < root->datum) {
        if (root->left == NULL)
            root->left = leaf;
        else
            __tree_insert(root->left, leaf);
    }

    else {
        if (root->right == NULL)
            root->right = leaf;
        else
            __tree_insert(root->right, leaf);
    }
}

void tree_insert(tree* t, int d) {
    if (t->root == NULL)
       t->root = node_init(d);
    else
        __tree_insert(t->root, node_init(d));
}

void __tree_dtor(tree_node* root) {
    if (root == NULL)
        return;
    __tree_dtor(root->left);
    __tree_dtor(root->right);
    free(root);
}

void tree_dtor(tree* t) {
    __tree_dtor(t->root);
}

int main() {
    tree* t = tree_init();

    for (int i = 0; i < 5; ++i)
        tree_insert(t, rand());
    tree_dtor(t);
    return 0;
}
