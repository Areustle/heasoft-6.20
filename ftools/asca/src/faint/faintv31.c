/*  FTOOLs info: $Header: /headas/headas/ftools/asca/src/faint/faintv31.c,v 3.6 1996/04/16 23:26:41 dunfee Exp $   */
/*                   */
/*
*
*	grade routine 
*
*	input:	handle[9]   array of pulse heights,
*                           0:center, 1-8:surrounding pixels
*               split       split threshold
*	output:	*sumph      total ph
*               *type       grade
*               *above      numbers of pixels summed in *sumph
*
*		version 3.0	91-04-23	by K.Yoshida
*		version 3.1	91-05-01	by K.Yoshida
*		slight mods 	91-11		Robin Corbet
*		(to simplfy call from FORTRAN)
*/

#include "faint.h"

static char rcsid[] = "$Id: faintv31.c,v 3.6 1996/04/16 23:26:41 dunfee Exp $";

static lookup_v31[256] = {  0,1,2,4,1,1,4,7,3,4,5,6,3,4,7,7,
			    3,3,5,7,4,4,6,7,7,7,7,7,7,7,7,7,
			    1,1,2,4,1,1,4,7,4,7,7,7,4,7,7,7,
			    3,3,5,7,4,4,6,7,7,7,7,7,7,7,7,7,
			    2,2,7,7,2,2,7,7,5,7,7,7,5,7,7,7,
		    	    5,5,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			    4,4,7,7,4,4,7,7,6,7,7,7,6,7,7,7,
			    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			    1,1,2,4,1,1,4,7,3,4,5,6,3,4,7,7,
			    4,4,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			    1,1,2,4,1,1,4,7,4,7,7,7,4,7,7,7,
			    4,4,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			    4,4,7,7,4,4,7,7,7,7,7,7,7,7,7,7,
			    6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
			    7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7  };


/* A simple pre-cfortran wrapper */

void faint_v31(cf_handle,cf_split,cf_sumph,cf_type,cf_above)
int cf_handle[9];
int *cf_split,*cf_sumph,*cf_type,*cf_above;
{
 void classify_v31();

 classify_v31(cf_handle,cf_split,cf_sumph,cf_type,cf_above);
}
FCALLSCSUB5(faint_v31,FAINT_V31,faint_v31,INTV,PINT,PINT,PINT,PINT)


/* normal mode */
void classify_v31(handle, split, sumph, type, above)
int handle[9];
int *split, *sumph, *type, *above;
{
	int i, i2, n;
	n = 0;
        for (i = 1; i < 9; i++)
        {
		/* printf("handle[%d] = %d \n", i,handle[i] ); */
		if( handle[i] >= *split ){
			n += power_v31(2, i-1);
		/* printf("%d ", power_v31(2, i-1) ); */
		}
	}
	/* printf("n = %d\n", n); */
	/* *type = toGrade(n); */
	*type = lookup_v31[n];

	*sumph = sum_v31(split, above, handle, type);

/*	return *type; */
}

/*	power_v31	*/
power_v31(base, n)
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
int sum_v31(split, above, handle, type) 
int *split, *above;
int handle[9];
int *type;
{
	int sumph;
	int i;

	*above = 1;
	sumph = handle[0];


	for (i = 2; i < 8; i++)  /* sum ph over split_thres. */
	{
	        if ( handle[i] >= *split ){
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
		if( handle[1] >= *split )
		{
			if( (handle[2]>=*split) && (handle[4]>=*split) )
			{
				(*above)++;
				sumph += handle[1];
			}
		}
		if( handle[3] >= *split )
		{
			if( (handle[2]>=*split) && (handle[5]>=*split) )
			{
				(*above)++;
				sumph += handle[3];
			}
		}
		if( handle[6] >= *split )
		{
			if( (handle[4]>=*split) && (handle[7]>=*split) )
			{
				(*above)++;
				sumph += handle[6];
			}
		}
		if( handle[8] >= *split )
		{
			if( (handle[5]>=*split) && (handle[7]>=*split) )
			{
				(*above)++;
				sumph += handle[8];
			}
		}
	}

	return sumph;
}

