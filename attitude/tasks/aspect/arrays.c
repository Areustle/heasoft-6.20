#include <stdlib.h>
#include "arrays.h"
/********************************************************************
* allocate storage for an array of floats, numerical recipies style *
********************************************************************/
float** allocateArray(int dimenx,int dimeny)
{
float** a;
int i;

a=(float**)malloc(sizeof(float*)*dimeny);

*a=(float*)malloc(sizeof(float)*dimenx*dimeny);

for(i=1;i<dimeny;++i) {
    a[i]=a[i-1]+dimenx;
}

return(a);

} /* end of allocateArray function */

/********************************************************************
* free storage for an array of floats, numerical recipies style 
********************************************************************/
void freeArray(float** a) {

free(*a);
free(a);

}



/*******************************************************************
* set all the elements of a float array as allocated above to zero *
*******************************************************************/
void blankArray(float** a, int dimenx, int dimeny)
{
int i,j;

for(j=0;j<dimeny;++j) {
    for(i=0;i<dimenx;++i) {
        a[j][i]=0.;
    }
}

} /* end of blankArray function */


/*******************************************
* sum over four adjacent cells of an array *
*******************************************/
float boxSum(float** a, int i, int j) {

return(a[j  ][i  ]+
       a[j-1][i  ]+
       a[j  ][i-1]+
       a[j-1][i-1]);

}



