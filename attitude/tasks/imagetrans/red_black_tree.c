#include <stdlib.h>
#include <stdio.h>

#include "red_black_tree.h"

/**************************************************************************
*
**************************************************************************/
NODE* createNode() {

    NODE* node = (NODE*)calloc(1, sizeof(NODE));

    return node;
}

/**************************************************************************
*
**************************************************************************/
void destroyNode(NODE* node) {

    free(node);

}

/**************************************************************************
*
**************************************************************************/
void printNode(NODE* node) {

    if(node == NULL) {
        printf("node: NULL\n");
        return;
    }

    printf("node: %d ",node->value);
    if(node->color==red) printf("red ");
    else                 printf("black ");

    if(node->left ==NULL) printf("left: NULL ");
    else                  printf("left: %d ", node->left->value);
    
    if(node->right==NULL) printf("right: NULL");
    else                  printf("right: %d", node->right->value);

    printf("\n");
}

/**************************************************************************
*
**************************************************************************/
void printNodes(NODE* root) {

    printNode(root);

    if(root!= NULL) {
        if(root->left  != NULL) printNodes(root->left);
        if(root->right != NULL) printNodes(root->right);
    }

}

/**************************************************************************
*
**************************************************************************/
int nodeHasValue(NODE* root, int value) {

    if(root==NULL) return 0;
    if(root->value == value) return 1;

    if(value< root->value) return nodeHasValue(root->left,  value);
    else                   return nodeHasValue(root->right, value);


} /* end of nodeHasValue function */

/**************************************************************************
*
**************************************************************************/
int addNode( NODE** tree, NODE* node ) {
    NODE* base;
    base = *tree;

/*    printf("adding ");
    printNode(node);
    printf("to ");
    if(base != NULL) printNode(base);
    else             printf("NULL\n");*/

    /*************************************************
    * if it's null, then we've found a place to add
    * the new node
    *************************************************/
    if ( base == NULL ) {
        *tree = node;
        return 1;
    }

    /************************************************************
    * if the value is the same, then we already have this value *
    ************************************************************/
    if(node->value == base->value) return 0;
    

    if (node->value <  base->value) {
        /**********************
        * it goes on the left *
        **********************/
        if(base->left != NULL) return addNode( &(base->left ), node);
        else {
            base->left = node;
            node->parent = base;
            return 1;
        }
    } else {
        /***********************
        * it goes on the right *
        ***********************/
        if(base->right != NULL) return addNode( &(base->right ), node);
        else {
            base->right = node;
            node->parent = base;
            return 1;
        }
        
    }

} /* end of addNode function */



/***************************************************************************
*
***************************************************************************/
void leftRotateNode(NODE** tree, NODE* x ) {

/*printf("leftRotate: start\n");*/

    /************************************************
    * the right hand child will become the new root *
    ************************************************/
    NODE* y;
    y = x->right;

    /*************************************************
    * Turn y's left sub-tree into x's right sub-tree *
    *************************************************/
    x->right = y->left;
    if ( y->left != NULL ) y->left->parent = x;

    /********************************
    * y's new parent was x's parent *
    ********************************/
    y->parent = x->parent;

    /* Set the parent to point to y instead of x */
    /* First see whether we're at the root */
    if ( x->parent == NULL ) *tree = y;
    else {
        if ( x == (x->parent)->left ) {
            /* x was on the left of its parent */
            x->parent->left = y;
        } else {
            /* x must have been on the right */
            x->parent->right = y;
        }
    }

    /* Finally, put x on y's left */
    y->left = x;
    x->parent = y;

} /* end of leftRotateNode function */

/***************************************************************************
*
***************************************************************************/
void rightRotateNode(NODE** tree, NODE* x ) {

/*printf("rightRotate: start\n");*/

    /************************************************
    * the right hand child will become the new root *
    ************************************************/
    NODE* y;
    y = x->left;

    /*************************************************
    * Turn y's left sub-tree into x's right sub-tree *
    *************************************************/
    x->left = y->right;
    if ( y->right != NULL ) y->right->parent = x;

    /********************************
    * y's new parent was x's parent *
    ********************************/
    y->parent = x->parent;

    /* Set the parent to point to y instead of x */
    /* First see whether we're at the root */
    if ( x->parent == NULL ) *tree = y;
    else {
        if ( x == (x->parent)->right ) {
            /* x was on the left of its parent */
            x->parent->right = y;
        } else {
            /* x must have been on the right */
            x->parent->left = y;
        }
    }

    /* Finally, put x on y's left */
    y->right = x;
    x->parent = y;

} /* end of leftRotateNode function */


/***************************************************************************
*
***************************************************************************/
int addNodeAndBallance( NODE** tree, NODE* x ) {

    NODE* y;
    int was_added;

 /*   printf("addNodeAndBallance: start node@%d *tree@%d\n",(int)x, (int)(*tree));*/



    /**************************************
    * Insert in the tree in the usual way *
    **************************************/
    x->color = red;
    was_added = addNode(tree, x);
    if(! was_added) return 0;
    
/*    printf("added\n");*/
   /* if(*tree != NULL) printNodes(*tree);*/

    /* Now restore the red-black property */

    while ( (x != *tree) && (x->parent->color == red) ) {

       if ( x->parent == x->parent->parent->left ) {
           /***********************************************
           * x's parent is a left, y is x's right 'uncle' *
           ***********************************************/
           y = x->parent->parent->right;
           if ( y!=NULL && y->color == red ) {
               /*****************************
               * case 1 - change the colors *
               *****************************/
               x->parent->color = black;
               y->color = black;
               x->parent->parent->color = red;
               /* Move x up the tree */
               x = x->parent->parent;

           } else {
               /********************
               * y is a black node *
               ********************/
               if ( x == x->parent->right ) {
                   /* and x is to the right */
                   /* case 2 - move x up and rotate */
                   x = x->parent;
                   leftRotateNode(tree, x );
               } else {
               /* case 3 */
                    x->parent->color = black;
                    x->parent->parent->color = red;
                    rightRotateNode(tree, x->parent->parent );
               }
           }
       } else {
           /***********************************************
           * x's parent is a right, y is x's left 'uncle' *
           ***********************************************/
/*printf("here\n");*/
           y = x->parent->parent->left;
           if ( y!=NULL && y->color == red ) {
               /*****************************
               * case 1 - change the colors *
               *****************************/
/*printf("case 1\n");*/
               x->parent->color = black;
               y->color = black;
               x->parent->parent->color = red;
               /* Move x up the tree */
               x = x->parent->parent;

           } else {
               /********************
               * y is a black node *
               ********************/
               if ( x == x->parent->left ) {
                   /********************************
                   * and x is to the left
                   * case 2 - move x up and rotate
                   ********************************/
/*printf("case 2\n");*/
                   x = x->parent;
                   rightRotateNode(tree, x );
/*printf("after right rotate:\n");
printNodes(*tree);*/
               } else {
                   /*********
                   * case 3 *
                   *********/
/*printf("case 3\n");*/
                   x->parent->color = black;
                   x->parent->parent->color = red;
                   leftRotateNode(tree, x->parent->parent );
/*printf("after left rotate:\n");
printNodes(*tree);*/
               }
           }

        }
    }
    /* Colour the root black */
    (*tree)->color = black;
    
    return 1;
}


/***************************************************************************
*
***************************************************************************/
int addValueToNode(NODE** tree, int value) {

    int was_added;

    NODE* node = createNode();
    node->value = value;
    was_added = addNodeAndBallance(tree,node);

/*    printNodes(*tree);
    printf("%d was added %d\n\n", value, was_added);*/

    if(! was_added) destroyNode(node);

    return was_added;

}

