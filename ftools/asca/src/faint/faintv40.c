/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/faint/faintv40.c,v 3.6 1996/04/16 23:26:45 dunfee Exp $   */
/*                   */
/*
*	grade routine 
*
*	input:	*handle[9]  pointer array to pulse height,
*                           0:center, 1-8:surrounding pixels
*               split       split threshold
*	output:	*sumph      total ph
*               *type       grade
*               *above      numbers of pixels summed in *sumph
*
*		version 3.0	91-04-23	by K.Yoshida
*		version 3.1	91-05-01	by K.Yoshida
*		version 4.0	92-12-01	by T.Dotani
*    version4.0
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
*/

#include "faint.h"

static lookup_v40[256] = { 0,1,2,5,1,1,5,7,3,5,8,6,3,5,7,7,
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


/* A simple pre-cfortran wrapper */

void faint_v40(cf_handle,cf_split,cf_sumph,cf_type,cf_above)
int cf_handle[9];
int cf_split,*cf_sumph,*cf_type,*cf_above;
{
 void classify_v40();

 classify_v40(cf_handle,cf_split,cf_sumph,cf_type,cf_above);
}
FCALLSCSUB5(faint_v40,FAINT_V40,faint_v40,INTV,INT,PINT,PINT,PINT)


/* normal mode */
void classify_v40(handle, split, sumph, type, above)
int handle[9];
int split, *sumph, *type, *above;
{
	int i, n;

	n = 0;
        for (i = 1; i < 9; i++)
        {
		/* printf("%d ", handle[i] ); */
		if( handle[i] >= split ){
			n += power_v40(2, i-1);
			/* printf("%d ", power_v40(2, i-1) ); */
		}
	}
	/* printf("%d\n", n); */
	/* *type = toGrade(n); */
	*type = lookup_v40[n];

	*sumph = sum_v40(split, above, handle, type);

	if( *type == 8 ) *type = 6; /* set the grade of L-shaped event to 6 */
/*	return *type; */
}

/*	power_v40	*/
power_v40(base, n)
int base, n;
{
	int i, p;

	p = 1;
	for(i = 1; i <= n; ++i){
		p = p*base;
	}
	return p;
}


/*
*	Sum PHs except for corner pixels
*		In the case of grade6, add a corner pixel
*/
int sum_v40(split, above, handle, type) 
int split, *above;
int handle[9];
int *type;
{
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
	if( 6==*type ){
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
