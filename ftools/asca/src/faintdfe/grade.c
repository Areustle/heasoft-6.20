/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/faintdfe/grade.c,v 1.4 2001/10/15 19:06:23 zpan Exp $   */
/*                   */
/*
*	grade routine 
*
*	input:	handle[9]  pointer array to pulse height,
*                           0:center, 1-8:surrounding pixels
*               split       split threshold
*	output:	*sumph      total ph
*               *type       grade
*               *above      numbers of pixels summed in *sumph
*
*		version 3.0	91-04-23	by K.Yoshida
*		version 3.1	91-05-01	by K.Yoshida
*
*       Modified to adapt the new grade definition:
*         0  single
*         1  single+
*         2  vertical split
*         3  left split
*         4  right split
*         5  single-sided+
*         6  L-shaped & square
*         7  others
*
*      However in this program, 8 is assigned to L-shaped event to use
*      the old version PH summation algorithm.  The number is changed 
*      in the output stage.
* This program was modified from the original. Now "handle" is an array
* of short ints instead of an array of pointers to short ints.
* also provided function prototypes. Finally changed the syntax of
* the function call to omit the pointer to the grade and added an option
* where the PHA sums will not be calculated if sumph or above are null
* pointers
* - Ed Pier.
*/

#include <stdlib.h>
#include <stdio.h>
#include "grade.h"

static lookup[256] = {  0,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7,
			4,4,8,7,5,5,6,7,7,7,7,7,7,7,7,7,
			1,1,2,5,1,1,5,7,5,7,7,7,5,7,7,7,
			4,4,8,7,5,5,6,7,7,7,7,7,7,7,7,7,
			2,2,7,7,2,2,7,7,8,7,7,7,8,7,7,7,
			8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			5,5,7,7,5,5,7,7,6,7,7,7,6,7,7,7,
			7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			1,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7,
			5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			1,1,2,5,1,1,5,7,5,7,7,7,5,7,7,7,
			5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			5,5,7,7,5,5,7,7,7,7,7,7,7,7,7,7,
			6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7  };


/* normal mode */
int normclassify_event(short int handle[9], int split, int* sumph, int* above) {
	int i, n;
        int type;

	n = 0;
        for (i = 1; i < 9; i++) {

		if( handle[i] >= split ){
			n += 1<<(i-1);
		}
	}

	type = lookup[n];

        if(sumph!=NULL && above!=NULL) {
            *sumph = sum_phas(split, above, handle, type);
        }

	if( type == 8 ) type = 6; /* set the grade of L-shaped event to 6 */
	return type;
}

/**************************************************************
* integer power	- this was replaces by 1<<n in normclassify_event
**************************************************************/
int power(int base, int n) {
	int i, p;

	p = 1;
	for(i = 1; i <= n; ++i){
		p = p*base;
	}
	return p;
}



/*********************************************************************
*	Sum PHs except for corner pixels
*		In the case of grade6, add a corner pixel
*********************************************************************/
int sum_phas(int split, int* above, short int handle[9], int type) {
	int sumph;
	int i;

	*above = 1;
	sumph = handle[0];
	for (i = 2; i < 8; i++)  /* sum ph over split_thres. */
	{
	        if ( handle[i] >= split ){
			 /* judge pixels over split_threshold */
			switch (i) {
				case 2: case 4: case 5: case 7:
					sumph += handle[i];
					(*above) ++;
					break;
				default:
					break;
			}
		}
	}
	if( 6==type ){
		if( handle[1] >= split )
		{
			if( (handle[2]>=split) && (handle[4]>=split) )
			{
				(*above)++;
				sumph += handle[1];
			}
		}
		if( handle[3] >= split )
		{
			if( (handle[2]>=split) && (handle[5]>=split) )
			{
				(*above)++;
				sumph += handle[3];
			}
		}
		if( handle[6] >= split )
		{
			if( (handle[4]>=split) && (handle[7]>=split) )
			{
				(*above)++;
				sumph += handle[6];
			}
		}
		if( handle[8] >= split )
		{
			if( (handle[5]>=split) && (handle[7]>=split) )
			{
				(*above)++;
				sumph += handle[8];
			}
		}
	}

	return sumph;
}

/* fast mode - not used by faintdfe*/
int fastclassify_event(short int handle[3], int split, 
                       int* sumph, int* above_split) {
        int i;
        int type=0;

        *above_split = 1;
        *sumph = handle[0] ;
        for (i = 1; i < 3; i++)
        {
	    if (handle[i] >= split)  /*judge pixels over split_threshold */
	    {    
	        (*above_split)++;
	        *sumph += handle[i];   /* sum ph over split_threshold */
	    }
        }
	if (*above_split <= 1) 
		type = 0;
	else if (*above_split == 2) 
		type = 1;
	else if (*above_split == 3) 
		type = 2;

	return type;
}
