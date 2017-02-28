/**************************************************************************
* This is an implementation of a simple red-black tree to keep track of
* unused pixel values. The tree holds unique integer values.
*
**************************************************************************/
struct t_red_black_node {
    enum { red, black } color;

    int value;

    struct t_red_black_node *left;
    struct t_red_black_node *right;
    struct t_red_black_node *parent;
};

typedef struct t_red_black_node NODE;

/**************************************************************************
* Returns true if the given value is in or below the given node
**************************************************************************/
int nodeHasValue(NODE* root, int value);

/***************************************************************************
* Add a value to the tree. Note the tree argument is a pointer to a pointer
* to the root node of the tree.
***************************************************************************/
int addValueToNode(NODE** tree, int value);
