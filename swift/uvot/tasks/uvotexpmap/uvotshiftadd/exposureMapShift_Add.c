/*	I think that in order for this to work we need a bad pixel map for the 
	entire detector.  Otherwise it is impossible to account for bad pixels
	which are not telemetered, but which impinge on the telemetered
	pixels because of tracking.
*/

/*	Package of procedures which compensate for drift in the satelite
	during an exposure.  Consists of 4 entry points
	
	
	The first is initialization which accepts a pointer to an 
	EXPOSURE_ARGS structure.  This procedure saves the input data
	and initializes the internal data of the package.  The input
	structure and all of its members must remain in scope until the
	third procedure returns to its caller.
		
	The second procedure accepts tracking data one position at a time.
	The tracking data consists of the position of the detector relative
	to the reference celestial position (RCP) and the time spent at the new position.
	Thus, a tracking record of 10 seconds trackingX=-1 and trackingY=-1
	would signify that the satelite attitude had changed so that the
	RCP feature which had been detected by the (0, 0) detector
	pixel is now being detected by the
	detector pixel at the (1,1) position, and that the new position
	was maintained for 10 seconds.  A similar statement applies
	to all the pixels in the detector.  
	Every time the tracking procedure is called the duration and position
	are saved in memory allocated by the procedure.  The min and max
	of the positions are maintained by this procedure.
	
	
	The third procedure is called when all tracking data have been passed
	into the second procedure.  The time durations of points with the
	same offset are summed and the total exposure time is calculated.
	
	On-board software compensates for the tracking except in 2 cases:
		1.  A bad pixel shifts to a location.
		2.  A non-extistant pixel moves to a location.
		QUESTION:  If an image uses a subset of the detector, will the
				on-board SW compensate for pixels which are physically
				present, but not reported?
	
	The program compensates for Case 2 by subtracting the duration of 
	the tracking position from the exposure of the pixel.
	
	The treatment of Case 1 depends on the type of the bad pixel.
	If it is dead, the tracking position duration is subtracted
	from the location of the pixel.  A flickering or hot
	pixel results in the exposure being set to invalid.  Compression 
	errors are ignored.
	
*/

#include <stdlib.h>
#include <stdio.h>
#include "exposureMapShift_Add.h"

typedef struct _TRACKING_AGGREGATE {
	int							totalDuration;
	int							i,
								j;
	struct _TRACKING_AGGREGATE	*nextAggregate;
	} TRACKING_AGGREGATE;

static EXPOSURE_ARGS	*args;

static int		trackingPositionsAllocated,
				trackingPositionsUsed;
int				*exposureMap;
static TRACKING_POSITION	*positionHistory;
static SUBMATRIX	fullDetector, *pfullDetector = &fullDetector;

static int		trackMinX,
				trackMinY,
				trackMaxX,
				trackMaxY,
				trackXSize,
				trackYSize;
static int		*quality;

#define TRACKING_OFFSET(x,y) ((y-trackMinY)*trackXSize + x-trackMinX)
#define CORRUPT_PIXEL (2 | 4 | 8)


static int max (int a, int b)
{
	return a > b ? a : b;
}

static int min (int a, int b)
{
	return a < b ? a : b;
}


/* **************************** calculateSize *************************** */
static void calculateSize (SUBMATRIX *m)
{m->xSize = m->xMaxIndex - m->xMinIndex + 1;
m->ySize = m->yMaxIndex - m->yMinIndex + 1;
return;
}

/* *************************** processBoundary ***************************** */
/*	The input rectangle is assumed to be part of the set of non-telemetered
	pixels which border the telemetered pixels on one side.
	
	For each pixel in the input rectangle (open on the right and on the top), 
	examine the pixels which it overlays because of the tracking.  If an 
	overlayed pixel is within the image which is being telemetered,
	reduce the exposure by the duration of the tracking position.
	
*/

static void processBoundary (int xLow, int xHigh, int yLow, int yHigh, 
		TRACKING_AGGREGATE *aggregate, EXPOSURE_ARGS *ea)
{int				i, j;
int					*pi,
					*pquality;
int					driftI, driftJ;
TRACKING_AGGREGATE	*currentAggregate;

if ( (xLow>=xHigh)  || (yLow>=yHigh) )
	return;
for (j=yLow;	j<yHigh;	++j)	/* FOR each pixel in input rectangle */
	{for (i=xLow;	i<xHigh;	++i)
		{
		for (currentAggregate=aggregate;	currentAggregate
			  ;currentAggregate=currentAggregate->nextAggregate)
			{
			driftI = i + currentAggregate->i;
			driftJ = j + currentAggregate->j;
			if ( !IN_MATRIX (driftI, driftJ, ea->exposure))
				continue;	/* Overlayed pixel not in output array */
			pi = exposureMap + MATRIX_OFFSET(driftI, driftJ, ea->exposure);
			if (*pi == -1)
				continue;
			if ( !IN_MATRIX (i, j, pfullDetector) )
				{*pi -= currentAggregate->totalDuration;
				continue;
				}
			if ( !IN_MATRIX(i, j, ea->quality) )
				{
#if 0
				 *pi = -1;	/*	original pixel not in quality 
								matrix - status unknown */
#endif
				continue;
				}
			pquality = MATRIX_OFFSET (i, j, ea->quality) + ea->quality->data;
			if ( !*pquality)
				continue;
			if (*pquality & CORRUPT_PIXEL)
				*pi = -1;
			else
				*(pi) -= currentAggregate->totalDuration;
			}
		}
	} 

}

/* *********************** exposureMapSA_DestroyMap ********************* */
int exposureMapSA_DestroyMap (EXPOSURE_ARGS *ar)
{
free (ar->exposure->data);
free (ar->exposure);
return 0;
}



/* ************************* exposureMapSA_EndTrack ********************* */
EXPOSURE_ARGS *exposureMapSA_EndTrack (void)
{
int			i, j, k;
int			lowI, highI, lowJ, highJ;
int			*pi, *pi2;
int			driftI, driftJ;
int			totalTime;
int			trackingSize;
int			boundaryXMin, boundaryXMax, boundaryYMin, boundaryYMax;
int			corruptPixel = CORRUPT_PIXEL;
TRACKING_POSITION	*ptp;
TRACKING_AGGREGATE	*aggregatePool,
					*currentAggregate,
					*previousAssigned,
					*firstAssigned;

if ( !trackingPositionsUsed)
	{printf ("No tracking data\n");
	return 0;
	}
trackXSize = trackMaxX - trackMinX + 1;
trackYSize = trackMaxY - trackMinY + 1;
trackingSize = trackXSize * trackYSize;
aggregatePool = calloc(trackingSize , sizeof(TRACKING_AGGREGATE));

/*	Sum the durations of all tracking records with the same position
	and calculate the total exposure time
*/
for (totalTime=0, i=0, ptp=positionHistory;	
	  i<trackingPositionsUsed;
	  ++i, ++ptp)
	{k = TRACKING_OFFSET(ptp->trackingX, ptp->trackingY);
	currentAggregate = aggregatePool + k;
	currentAggregate->totalDuration += ptp->duration;
	currentAggregate->i = ptp->trackingX;
	currentAggregate->j = ptp->trackingY;
	totalTime += ptp->duration;
	}

/*	Collect the TRACKING_AGGREGATE s with a positive duration into a 
	linked list.  This will save some time if the tracking matrix
	is sparse.
*/
for (k=0, previousAssigned= 0, firstAssigned=0, currentAggregate=aggregatePool;
	   k<trackingSize; ++k, ++currentAggregate)
	{if ( !(currentAggregate->totalDuration))
		continue;
	currentAggregate->nextAggregate = 0;
	if ( !firstAssigned)
		{firstAssigned = currentAggregate;
		}
	else
		{previousAssigned->nextAggregate = currentAggregate;
		}
	previousAssigned = currentAggregate;
	}
args->exposure = calloc (sizeof ( SUBMATRIX), 1);
*(args->exposure) = *(args->telemetered);

calculateSize (args->exposure);
args->exposure->data = calloc (args->exposure->xSize*args->exposure->ySize, 
		sizeof (int));
exposureMap = args->exposure->data;

/*	Set the exposure of all telemetered pixels to the
	total exposure time.
	Using the telemetered SUBMATRIX to define the loop limits is left
	over from a time when I thought that the output was supposed to
	include the entire set of pixels that the detector swept over as the
	satellite changed orientation.  It is harmless to leave it this way.
*/
for (j=args->telemetered->yMinIndex;	j<=args->telemetered->yMaxIndex;	++j)
	{for (i=args->telemetered->xMinIndex, 
			pi=args->exposure->data + MATRIX_OFFSET(i,j,args->exposure)
		  ; i<=args->telemetered->xMaxIndex;	++i, ++pi)
		*pi = totalTime;
	}

/*	To understand the following processing think of the exposure
	matrix as being imbedded in a larger matrix which includes the 
	off-image pixels which have drifted onto pixel positions
	which have been telemetered.  I refer to these pixels as `interfering
	pixels'.  The so-called `boundaries' in the following comments 
	refer to the interfering pixels which surround the telemetered 
	image.  
	Recall that the tracking records record how far a detector pixel
	has moved.  Therefore, in order to determine which pixels have 
	overlayed a given pixel you must subtract the tracking offset.
	
	Start with the left boundary.
*/
boundaryXMin = args->exposure->xMinIndex - trackMaxX;
boundaryXMax = args->exposure->xMinIndex;
boundaryYMin = args->exposure->yMinIndex - trackMaxY;
boundaryYMax = args->exposure->yMaxIndex + 1 - trackMinY;
processBoundary (boundaryXMin, boundaryXMax, boundaryYMin, boundaryYMax, 
		firstAssigned, args);
/*	Next do the right boundary */
boundaryXMin = args->exposure->xMaxIndex + 1;
boundaryXMax = args->exposure->xMaxIndex + 1 - trackMinX;
processBoundary (boundaryXMin, boundaryXMax, boundaryYMin, boundaryYMax, 
		firstAssigned, args);

/*	Now the upper and lower boundary points.  
*/
boundaryXMin = args->exposure->xMinIndex;
boundaryXMax = args->exposure->xMaxIndex + 1;

boundaryYMax = args->exposure->yMinIndex;
boundaryYMin = args->exposure->yMinIndex - trackMaxY;
processBoundary (boundaryXMin, boundaryXMax, boundaryYMin, boundaryYMax, 
		firstAssigned, args);
boundaryYMax = args->exposure->yMaxIndex + 1 -trackMinY;
boundaryYMin = args->exposure->yMaxIndex + 1;
processBoundary (boundaryXMin, boundaryXMax, boundaryYMin, boundaryYMax, 
		firstAssigned, args);

/*	Next account for the effect of bad pixels which are contained
	in the transmitted image 
*/
lowI = args->exposure->xMinIndex;
if (trackMinX < 0)
	lowI += trackMinX;
lowI = max (lowI, args->quality->xMinIndex);
lowJ = args->exposure->yMinIndex;
if (trackMinY < 0)
	lowJ += trackMinY;
lowJ = max (lowJ, args->quality->yMinIndex);
highI = args->exposure->xMaxIndex;
if (trackMaxX > 0)
	highI += trackMaxX;
highI = min (highI, args->quality->xMaxIndex);
highJ = args->exposure->yMaxIndex;
if (trackMaxY > 0)
	highJ += trackMaxY;
highJ = min (highJ, args->quality->yMaxIndex);
for (j=lowJ;	j<=highJ;	++j)
	{for (i=lowI,
			pi=args->quality->data + MATRIX_OFFSET(i, j, args->quality)
	  ;	i<=highI
	  ;	++i, ++pi)
		{if ( !*pi)
			continue;
		pi2 = args->exposure->data + MATRIX_OFFSET (i, j, args->exposure);
		*pi2 = -1;
		if (*pi & corruptPixel)
			{for (currentAggregate=firstAssigned;	currentAggregate;	
				  currentAggregate = currentAggregate->nextAggregate  )
				{
				driftI = i + currentAggregate->i;
				driftJ = j + currentAggregate->j;
				if ( !IN_MATRIX (driftI, driftJ, args->telemetered) )
					continue;
				pi2 = args->exposure->data + 
					MATRIX_OFFSET(driftI, driftJ, args->exposure);
				*pi2 = -1;
				}
			}
		else if (*pi & 1)	/* pixel is dead */
			{for (currentAggregate=firstAssigned;	currentAggregate;	
				  currentAggregate = currentAggregate->nextAggregate  )
				{
				driftI = i + currentAggregate->i;
				driftJ = j + currentAggregate->j;
				pi2 = args->exposure->data + 
					MATRIX_OFFSET(driftI, driftJ, args->exposure);
				if (*pi2 == -1)
					continue;
				if ( IN_MATRIX (driftI, driftJ, args->telemetered) )
					*pi2 -= currentAggregate->totalDuration;
				else
					*pi2 += currentAggregate->totalDuration;
				}
			}
		else	/* compression error */
			continue;
		}
	}

free (aggregatePool);
free (positionHistory);
return args;
}

/* ************************** exposureMapSA_Init ************************ */
int exposureMapSA_Init (EXPOSURE_ARGS *arguments)
{

args = arguments;
calculateSize (args->quality);
calculateSize (args->telemetered);
trackMinX = 0;
trackMinY = 0;
trackMaxX = 0;
trackMaxY = 0;
quality = arguments->quality->data;
trackingPositionsUsed = 0;
positionHistory = 0;
trackingPositionsAllocated = 0;
fullDetector.xMinIndex = 0;
fullDetector.yMinIndex = 0;
fullDetector.xMaxIndex = arguments->detectorXSize - 1;
fullDetector.yMaxIndex = arguments->detectorYSize - 1;

return 0;
}

/* ************************** exposureMapSA_Track *********************** */
int exposureMapSA_Track (TRACKING_POSITION *tp)
{TRACKING_POSITION		*tp1;
if (trackingPositionsUsed >= trackingPositionsAllocated)
	{trackingPositionsAllocated += 1000;
	positionHistory = realloc (positionHistory, 
			sizeof (TRACKING_POSITION) * trackingPositionsAllocated);
	}
tp1 = positionHistory + trackingPositionsUsed;
*tp1 = *tp;
if (trackMinX > tp1->trackingX)
	trackMinX = tp1->trackingX;
if (trackMaxX < tp1->trackingX)
	trackMaxX = tp1->trackingX;
if (trackMinY > tp1->trackingY)
	trackMinY = tp1->trackingY;
if (trackMaxY < tp1->trackingY)
	trackMaxY = tp1->trackingY;
++trackingPositionsUsed;

return 0;
}
