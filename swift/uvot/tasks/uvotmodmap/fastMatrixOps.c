/*	Collection of routines to replace some of the standard vecmath 
	procedures with faster ones
*/

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <math.h>
#include "vecmat.h"
#include "uvotmodmap.h"
#include "fastMatrixOps.h"

#define MATRIX_OFFSET(m, x, y) \
  (m->xDimension * (y + m->yAlias) + x + m->xAlias)


/* *************************** timeIncrement *************************** */
/*	Prints the number of seconds since the last time it was called */
void timeIncrement (char *msg)
{static time_t		firstEntryTime = 0,
					lastTime = 0;
time_t				currentTime;

currentTime = time (0);
if ( !lastTime)
	lastTime = currentTime;
if ( !firstEntryTime)
	firstEntryTime = currentTime;
printf ("timing: %s: total=%ld, interval=%ld\n", msg, 
	(long) (currentTime - firstEntryTime), (long) (currentTime - lastTime));
lastTime = currentTime;
}

/* ********************** countAndSumMatrix ********************** */
/*	Count the non-zero elements of the pixMap matrix, and for each such
	element, sum the corresponding element of the observation matrix
*/
int countAndSumMatrix (	/* returns the count of non-zero pixels */
	IntMatrix *pixMap,
	RealMatrix *observation,
	double *sum		/* sum of observations placed here */
	)
{double 		total = 0.0;
int				count = 0;
int				i, iLimit,
				j, jLimit;
int				*mapBase, *currentMap;
int				mapIncrement,
				dataIncrement;
double			*dataBase, *currentData;

iLimit = pixMap->xMax;
jLimit =pixMap->yMax;
mapIncrement = pixMap->xDimension;
dataIncrement = observation->xDimension;
for (j=pixMap->yMin
		, mapBase = BASE_DATA (pixMap, j)
		, dataBase = BASE_DATA (observation, j)
	  ;	j<=jLimit
	  ;	++j
		, dataBase+=dataIncrement
		, mapBase+=mapIncrement
	  )
	{
	for (i=pixMap->xMin
			, currentData=dataBase+i
			, currentMap=mapBase+i
		  ;	i<=iLimit
		  ;	++i
			, ++currentData
			, ++currentMap
		  )
		{if (*currentMap)
			{++count;
			total += *currentData;
			}
		
		}
	}

*sum = total;
return count;
}

/* ********************** countAndSumMatrixSection ********************** */
/*	Operates like countAndSumMatrix () above except:
		1. Only the submatrix of pixMap bounded by the points
			(sectionX, sectionY) and (maxX, maxY) and the
			corresponding submatrix of observation are processed.
		2. Only the elements at x-interval dx and y-interval dy
			are processed.
	Note:  All coordinates are relative to a lower left element
			coordinate of (1, 1) 
*/
int countAndSumMatrixSection (
	IntMatrix *pixMap,
	RealMatrix *observation,
	int sectionX,
	int sectionY,
	int maxX,
	int maxY,
	int dx,
	int dy,
	double *sum
	)
{double 		total = 0.0;
int				count = 0;
int				i, iLimit,
				j, jLimit;
int				*mapBase, *currentMap;
int				mapIncrement,
				dataIncrement;
double			*dataBase, *currentData;

iLimit = maxX;
jLimit =maxY;
mapIncrement = dy * pixMap->xDimension;
dataIncrement = dy * observation->xDimension;
for (j=sectionY
		, mapBase = BASE_DATA (pixMap, j)
		, dataBase = BASE_DATA (observation, j)
	  ;	j<=jLimit
	  ;	j+=dy
		, dataBase+=dataIncrement
		, mapBase+=mapIncrement
	  )
	{
	for (i=sectionX
			, currentData=dataBase+i
			, currentMap=mapBase+i
		  ;	i<=iLimit
		  ;	i+=dx
			, currentData+=dx
			, currentMap+=dx
		  )
		{if (*currentMap)
			{++count;
			total += *currentData;
			}
		
		}
	}

*sum = total;
return count;
}

/* ************************* fastClearOutsiders ************************* */
int fastClearOutsiders (IntMatrix *mask, IterateClearOutsiders *iSSD)
{int			*maskBase;
double			*diffBase;
int				numberCleared = 0;
int				i, iMax,
				j, jMax;
double			mean;
double			test;
double			*pDiff;
int				*pMask;
RealMatrix		*image;

image = iSSD->image;
test = iSSD->sigma * iSSD->nSig;
mean = iSSD->mean;
iMax = mask->xMax;
jMax = mask->yMax;
for (j=mask->yMin
		, i=mask->xMin
		, maskBase = BASE_DATA (mask, j)
		, diffBase = BASE_DATA (image, j)
	  ;	j<=jMax
	  ; ++j
		, maskBase += mask->xDimension
		, diffBase += image->xDimension
	)
	{
	for (i=mask->xMin
			, pDiff = diffBase + i
			, pMask = maskBase + i
		  ;	i<=iMax
		  ;	++i
			, ++pDiff
			, ++pMask
		)
		{if (*pMask)
			{if (fabs(*pDiff - mean) >test)
				{++numberCleared;
				*pMask = 0;
				--mask->count;
				}
			}
		}
	}

return numberCleared;
}

/* ************************* fastCopyIntMatrix ************************** */
void fastCopyIntMatrix (IntMatrix *s, IntMatrix *d)
{int		j,
			jLimit,
			moveSize,
			offset;

moveSize = s->xDimension * sizeof(int);
jLimit = s->yMax;
for (j=s->yMin, offset = 0;	j<=jLimit;	++j, offset+=s->xDimension)
	{memcpy (d->data+offset, s->data+offset, moveSize);
	}
d->count = s->count;
}

/* ****************************** fast_sum ****************************** */
double fast_sum (RealMatrix *r)
{int		i,
			pointCount;
double		*pd;
double		value;

pointCount = r->xDimension * r->yDimension;
for (value=0., pd=r->data, i=0;	i<pointCount;	++i, ++pd)
	value += *pd;
return value;
}

/* *************************** fast_countStep *************************** */
int fast_countStep (IntMatrix *mtrx, int dx, int dy)
{int			count,
				*pi,
				*piBase;
int				i,
				j;

count = 0;
for (j=mtrx->yMin;	j<=mtrx->yMax;	j+=dy)
	{piBase = mtrx->data + (j+mtrx->yAlias)*mtrx->xDimension+mtrx->xAlias;
	for (i=mtrx->xMin, pi=piBase+i;	i<=mtrx->xMax;	i+=dx, pi+=dx)
		{
		if (*pi)
			++count; 
		}
	}
mtrx->count = count;
return count;
}

/* *********************** fast_int_matrix_section ********************** */
/*
 * copies a section of matrix m starting at xMin, yMin to matrix o
 * the amount copied fills o
 */
void
fast_int_matrix_section (IntMatrix * m, IntMatrix * o, int xMin, int yMin)
{
  int y;
int		*mData,
		*oData;
int		bytesToMove;
int		mOffset, oOffset;

bytesToMove = o->xSize * sizeof (int);
mData = m->data;
oData = o->data;
  for (y = 0; y < o->ySize; ++y)
    {
    mOffset = MATRIX_OFFSET(m, xMin, y + yMin);
    oOffset = MATRIX_OFFSET(o, o->xMin, y + o->yMin);
	memcpy (oData+oOffset, mData+mOffset, bytesToMove);

    }
fast_countStep (o, 1,1);
return;
}

/* ************************** fastIterateScale ************************* */
void fastIterateScale (RealMatrix *tile, IterateScale *scale)
{double			*tileBase, *tilePtr,
				*outBase, *outPtr,
				temp;
double			factor;
int				i,
				j;
int				accumulate;
RealMatrix		*output = scale->output;

factor = scale->scale;
accumulate = scale->accumulate;
for (j=tile->yMin
		, tileBase = BASE_DATA (tile, j)
		, outBase = BASE_DATA (output, j + scale->yMin-1) + scale->xMin-1
	  ;	j<=tile->yMax
	  ;	tileBase += tile->xDimension
		, outBase += output->xDimension
	  	, ++j
	  )
	{for (i=tile->xMin
			, tilePtr = tileBase + i
			, outPtr = outBase + i
		  ;i<=tile->xMax	
		  ;++i
			,  ++outPtr
			, ++tilePtr)
		{temp = factor * *tilePtr;
		if (accumulate)
			temp += *outPtr;
		*outPtr = temp;
		}
	}
}

/* ************************** fast_sumMaskStep ************************* */
double fast_sumMaskStep (RealMatrix *rm, IntMatrix *im, int dx, int dy)
{double			sum;
double			*rd;
int				*id;
int				iBase,
				iDeltaX,
				iDeltaY,
				iOffset,
				rBase,
				rDeltaX,
				rDeltaY,
				rOffset;
int				i,
				j;

id = im->data;
rd = rm->data;
iDeltaX = im->xSize;
rDeltaX = rm->xSize;
iDeltaY = dy * im->xSize;
rDeltaY = dy * rm->xSize;
for (sum=0, j=im->yMin
		,iBase = im->xDimension*(j+im->yAlias)+im->xAlias
		,rBase = rm->xDimension*(j+rm->yAlias)+rm->xAlias
	  ;	j<=im->yMax
	  ;	j+=dy, iBase+=iDeltaY, rBase+= rDeltaY)
	{for (i=im->xMin, 
			iOffset=iBase+i,
			rOffset=rBase+i
		  ;	i<=im->xMax
		  ;	i+=dx, iOffset+=dx,
			rOffset+=dx)
		{if ( id[iOffset] )
			sum += rd[rOffset];
		}
	}
return sum;
}


