/********************************************************************
* allocate storage for an array of floats, numerical recipies style *
********************************************************************/
float** allocateArray(int dimenx,int dimeny);

/********************************************************************
* free storage for an array of floats, numerical recipies style 
********************************************************************/
void freeArray(float** a);

/*******************************************************************
* set all the elements of a float array as allocated above to zero *
*******************************************************************/
void blankArray(float** a, int dimenx, int dimeny);

/*******************************************
* sum over four adjacent cells of an array *
*******************************************/
float boxSum(float** a, int i, int j);

