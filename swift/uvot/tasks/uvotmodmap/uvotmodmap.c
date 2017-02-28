/*
 * $Source: /headas/headas/swift/uvot/tasks/uvotmodmap/uvotmodmap.c,v $
 * $Revision: 1.28 $
 * $Date: 2007/10/25 21:08:41 $
 *
 *
 * $Log: uvotmodmap.c,v $
 * Revision 1.28  2007/10/25 21:08:41  rwiegand
 * Squash negative counts in input image.
 *
 * Revision 1.27  2007/09/14 15:04:24  rwiegand
 * Modified logging of timing information.
 *
 * Revision 1.26  2007/08/02 21:36:32  rwiegand
 * Allow operating on SKY input files and badpixfile=NONE.  Warning: have
 * experienced exceedingly long run times on SKY data (1.5 hours for 1 image).
 *
 * Revision 1.25  2007/06/21 15:49:16  rwiegand
 * Write MOD8CORR keyword.
 *
 * Revision 1.24  2005/02/04 19:06:21  rwiegand
 * Only prompt for PPS file name if PPS output has been requested.
 *
 * Revision 1.23  2005/02/01 00:27:21  rwiegand
 * The NAXISn keywords of the PPS file were not being updated when the user
 * specified certain extensions.
 *
 * Revision 1.22  2005/01/31 17:28:29  rwiegand
 * Write UVOT window keywords if input did not include them.  Fixed
 * case when user specified quality extension by name.
 *
 * Revision 1.21  2005/01/31 16:21:17  rwiegand
 * Committing Bob O'Brien's changes:
 * Support processing of image in primary header.
 * Use load_raw_window to get image parameters.
 *
 * Revision 1.21  2005/1/26  10:30:00  robrien
 * Support processing of image in primary header.
 * Use load_raw_window to get image parameters.
 *
 * Revision 1.20  2004/11/02 15:04:25  rwiegand
 * Updated subimaging parameter names.
 *
 * Revision 1.19  2004/10/27 19:42:40  rwiegand
 * Removed offensive FILENAME keyword.
 *
 * Revision 1.18  2004/10/18 15:45:28  rwiegand
 * Declare scratch variables static.
 *
 * Revision 1.17  2004/10/14 20:07:11  rwiegand
 * Removed unused variables.  Updated dimensions of PPS output for sub-imaging.
 *
 * Revision 1.16  2004/10/14 19:13:54  rwiegand
 * Use absolute RAW coordinates (world).  Pruned conditionally compiled code.
 *
 * Revision 1.15  2004/10/13 21:13:27  rwiegand
 * Committing Bob O'Brien's optimizations and sub-windowing support.
 *
 * Revision 1.12  2004/03/04 16:09:34  rwiegand
 * Made parameters lower-case for consistency.
 *
 * Revision 1.11  2003/11/26 15:19:47  rwiegand
 * Introduced structures for passing parameters and task state.  Added
 * function for copying header keywords.  Use report_xxx for logging.
 *
 * Revision 1.10      2003/10/24 14:35:44  valett
 * initialized output image to input image to assure the intire output
 * image has meaningful data.  This corrected an error where areas lying
 * outside the input image size mod 4 where not being set.
 *
 * Revision 1.9       2003/08/08 18:32:57  valett
 * updated new binning keywords and values.
 *
 * Revision 1.8       2003/06/18 14:54:48  valett
 * Removed BINBPE keyword processing.  Updated definition of binning
 * keywords.
 *
 * Revision 1.7       2003/06/11 21:14:48  miket
 * global name change from umodmap to uvotmodmap
 *
 * Revision 1.6       2003/05/23 19:28:10  valett
 * initialize mean and sigma to avaid bogus warnings.
 *
 * Revision 1.5       2003/05/13 17:10:36  valett
 * updated keywords and keyword values written.
 * UVOT file structure change (quality file is separate).
 * removed "check for too few counts in image to remove mod 8 noise" code.
 *
 * Revision 1.4       2003/03/03 05:00:53  valett
 * Update to umodmap to work with new file formats.
 *
 * Revision 1.3       2003/01/23 16:21:30  valett
 * changed output file comparison to tfdiff.
 *
 * Revision 1.2       2002/08/07 16:35:26  valett
 * updated keywords in FITs output files.
 *
 * Revision 1.1       2002/08/02 18:54:35  valett
 * Initial version.
 */

/* TO DO  (ROB)
	Check the quality matrix to see if it
		is consistent with the observation window
*/


#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <memory.h>

#include "fitsio.h"
#include "pil.h"
#include "headas.h"
#include "report.h"

#define WRITE_TIMING_INFO 0
#define WRITE_DETAILS 0
#define MAX_BINS 10000
static int		countBins [MAX_BINS];


#define TOOLSUB uvotmodmap
/* headas_main() requires that TOOLSUB be defined first */
#include "headas_main.c"

#define TOOLNAME "uvotmodmap"
#define VERSION "v2.1"
#define CREATOR TOOLNAME " " VERSION    /* CREATOR keyword value */

#define MAX_DIMENSION 4000
#include "vecmat.h"
#include "uvotmodmap.h"
#include "uvotfile.h"
#include "uvottool.h"
#include "fastMatrixOps.h"


enum
{
  TARGET_NONE,
  TARGET_PRODUCT,
  TARGET_PPSFILE
};


typedef struct
{
  int code;

  char infile[PIL_LINESIZE];   /* Input file name */
  char qfile[PIL_LINESIZE];    /* Quality file name */
  char outfile[PIL_LINESIZE];  /* Output file name */
  char ppsfile[PIL_LINESIZE];  /* Mod8Map file name */

  int ppsproduct;              /* output Mod8Map file */
  int nSig;                    /* Significance level for sigma clipping */
  int nBox;                    /* Size of sliding box in units of 8 pixels */
  int clobber;                 /* overwrite existing output file? */
  int history;                 /* write HISTORY keywords? */
  int chatter;                 /* verbosity level */
  int processSubimage;
  int subimageXMin;
  int subimageYMin;
  int subimageXMax;
  int subimageYMax;
  int imageExtension;
  int qualityExtension;

} Parameters;

typedef struct {
	int				binX,
					binY;
	int				windowX0,
					windowY0;	/* Origin of image - World Co-ordinates */
	int				xMin, xMax, /* Limits of image in FITS co-ordinates */
					yMin, yMax;
	int				nSig;
	int				nBox;
	IntMatrix		*quality;
	RealMatrix		*inOSW,
					*outOSW,
					*mod8Map;
	} MOD8_PAR;


typedef struct
{
  fitsfile * infptr;
  fitsfile * outfptr;
  fitsfile * ppsfptr;
  fitsfile * qfptr;

  int processingImages;
  int width, height;

  Parameters * par;

} Task;


/* Function Prototypes */

int uvotmodmap_getpar (Parameters * par);
int uvotmodmap_work (Parameters * par);

int max2 (int a, int b)
{
   /* return the greater value */
   if (a > b)
      return a;
   else
      return b;
}

int min2 (int a, int b)
{
   /* return the lesser value */
   if (a < b)
      return a;
   else
      return b;
}


/* uvotmodmap iteration specialization */

void
iterate_set_false_where_true (IntMatrix * m, void *raw, int x, int y)
{
  IntMatrix * o = (IntMatrix *) raw;
  if (get_int_matrix(m, x, y))
    set_int_matrix(o, x, y, 0);
}

/*	if the element m(x,y) is non-zero add (raw->data(x,y)-raw->mean)**2 to raw->result */
void
iterate_sum_square_diff (IntMatrix * m, void *raw, int x, int y)
{
  IterateSumSquareDifference *info = (IterateSumSquareDifference *) raw;
  if (get_int_matrix(m, x, y))
    {
      double in, d2;
      in = get_real_matrix(info->input, x, y);
      d2 = in - info->mean;	/* ROB replace pow () */
      info->result += d2*d2;
    }
}

void
iterate_clear_outsiders (IntMatrix * m, void *raw, int x, int y)
{
  IterateClearOutsiders *info = (IterateClearOutsiders *) raw;
  double q;
  q = get_real_matrix(info->image, x, y);
/*	ROB don't divide by standard deviation - move to other side
	of inequality and multiply
*/
  if (fabs(q - info->mean)  > info->nSig * info->sigma)
    set_int_matrix(m, x, y, 0);
}


void
iterate_fix_zeros (RealMatrix * m, void *info, int x, int y)
{
  double q;
  q = get_real_matrix(m, x, y);
  if (q == 0.0)
    set_real_matrix(m, x, y, 1.0);
}

/* *************************** getFileHDU ************************ */
int getFileHDU (const char *file, FileSpec *spec, fitsfile *fptr)
{
int		code = 0;

if (file_parse_path (file, spec))
	{code = TASK_INPUT_ERROR;
	report_error ("Invalid input '%s' => %d\n", file);
	}
else if (spec->hdu1)
	{if (file_resolve_hdu (spec, fptr))
		{code = TASK_INPUT_ERROR;
		report_error ("Invalid input HDU specified '%s' => %d\n", file);
		}
	else
		report_verbose ("User specified extension '%s' => %d\n",
			spec->ext, spec->hdu1);
	}
return code;
}

static IntMatrix scratch;

/* ********************** initializeMatrixSection *********************** */
void initializeMatrixSection (IntMatrix *m, int xMin, int xMax, int yMin, int yMax)
{

  m->xMin = xMin;
  m->xMax = xMax;
  m->yMin = yMin;
  m->yMax = yMax;

  m->xSize = xMax - xMin + 1;
  m->ySize = yMax - yMin + 1;
  m->xDimension = m->xSize;
  m->yDimension = m->ySize;
  m->xAlias = -xMin;
  m->yAlias = -yMin;

  m->alias = &scratch;
  m->count = -(m->xSize * m->ySize + 1);
  return;
}

/* ************************* calculateMod8Map *********************** */
int calculateMod8Map (MOD8_PAR	*mp)
{
/* variables contained in the MOD8_PAR structure */
int				binX, binY;
int				windowX0, windowY0;
int				xMin, xMax, yMin, yMax;
IntMatrix		*quality;
IntMatrix		*pixMap;
RealMatrix		*inOSW;
int				nSig;
int				nBox;
RealMatrix		*outOSW;

/* Locally defined variables */
int				i, j, x, y;
int				n;
double			tmp;
long			numPixels;
long			nBoxBymod8TileDX, nBoxBymod8TileDY;
long			mod8TileDX, mod8TileDY;
int				mod8xmin, mod8ymin;
int				mod8xmax, mod8ymax;
long			minDX = 0, minDY = 0;
long			maxDX = 0, maxDY = 0;
long			boxDX, boxDY;
double			sigma;
double			mean;
double 			tol;       /* epsilon for doubeling point calculations */
IntMatrix		*pixMask,
				*tempPixMask1;
IterateScale	iScale;
RealMatrix		*mod8Map, *mod8Tile;
RealMatrix		*meanTile;
RealMatrix 		*StartImTile = 0;  /* starting image tile. */
RealMatrix 		*InterImTile = 0;  /* intermediate image tile */
RealVector *TotalInMod8Col;   /* Sum over mod 8 cell columns of mod-8 map */
RealVector *TotalInOSWCol;    /* Sum over mod 8 cell columns of input OSW */
RealVector *TotalInMod8Row;   /* Sum over mod 8 cell rows of mod-8 map */
RealVector *TotalInOSWRow;    /* Sum over mod 8 cell rows of input OSW */
double TotalInMod8Cols;       /* Total of all Mod 8 Cols */
double TotalInMod8Rows;       /* Total of all Mod 8 Rows */
double doubleMod8TileDX;     /* holds double (Mod8TileDX) */
double doubleMod8TileDY;     /* holds double (Mod8TileDY) */
long RunningPixel;           /* Running Pixel index */
double RunningMod8Total;     /*Running total of mod-8 values */
double RunningPosition;      /* Current position of virtual pixel boundary */
double OldRunningPosition;   /* Previous virtual pixel boundary position */
int				retVal = 0;

int				debugCount1, debugCount2;

binX = mp->binX;
binY = mp->binY;
windowX0 = mp->windowX0;
windowY0 = mp->windowY0;
xMin = mp->xMin;
xMax = mp->xMax;
yMin = mp->yMin;
yMax = mp->yMax;
quality = mp->quality;
inOSW = mp->inOSW;
nSig = mp->nSig;
nBox = mp->nBox;
outOSW = mp->outOSW;

mod8TileDX = 8/binX;
mod8TileDY = 8/binY;
doubleMod8TileDX = (double) mod8TileDX;
doubleMod8TileDY = (double) mod8TileDY;


/* Deal with the window sizes so that the correct pixels are
     processed for the mod 8 noise. */

n = (windowX0 / binX) + xMin - 1;
if ((n % mod8TileDX) == 0)
	mod8xmin = xMin;
else
	{mod8xmin = mod8TileDX * ((n / mod8TileDX) + 1) - (windowX0 / binX) + 1;
	mp->xMin = mod8xmin;
	}

n = (windowX0 / binX) + xMax;
if ((n % mod8TileDX) == 0)
	mod8xmax = xMax;
else
	{mod8xmax = mod8TileDX * (n / mod8TileDX) - (windowX0 / binX);
	mp->xMax = mod8xmax;
	}

n = (windowY0 / binY) + yMin - 1;
if ((n % mod8TileDY) == 0)
	mod8ymin = yMin;
else
	{mod8ymin = mod8TileDY * ((n / mod8TileDY) + 1) - (windowY0 / binY) + 1;
	mp->yMin = mod8ymin;
	}

n = (windowY0 / binY) + yMax;
if ((n % mod8TileDY) == 0)
	mod8ymax = yMax;
else
	{mod8ymax = mod8TileDY * (n / mod8TileDY) - (windowY0 / binY);
	mp->yMax = mod8ymax;
	}

report_debug("mod8xmin, mod8ymin = %d %d\n", mod8xmin, mod8ymin);
report_debug("mod8xmax, mod8ymax = %d %d\n", mod8xmax, mod8ymax);
#if WRITE_TIMING_INFO
timeIncrement("calculateMod8Map");
#endif

/* Allocate space for the pixel map. */

pixMap = create_int_matrix(xMin, xMax, yMin, yMax);
if (pixMap == NULL)
	{
	report_error("Memory allocation error - pixMap\n");
	return (1);
	}

iterate_int_matrix(pixMap, iterate_set_true, 0);
iterate_int_matrix(quality, iterate_set_false_where_true, pixMap);

DESTROY_INT_MATRIX(quality);

/* check that the image has enough counts to be corrected for mod8 noise */

/* Is the mean countrate big enough?
	 mod8 noise up to 10- 20% effect, assume that must have ~3%
	 accuracy to improve image this corresponds approx to
	 mean * nBox^2 > 1000 */

  /* allocate space for pixel mask. */

pixMask = create_int_matrix(xMin, xMax, yMin, yMax);
if (pixMask == NULL)
	{
	report_error("Memory allocation error - pixMask\n");
	return (1);
	}

mod8Map = create_real_matrix(xMin, xMax, yMin, yMax);
if (mod8Map == NULL)
	{
	report_error("Memory allocation error - mod8Map\n");
    return (1);
	}
mp->mod8Map = mod8Map;

  /* calculate size of box */
assert(nBox >= 2);
boxDX = (long) (mod8TileDX * floor(nBox / 2.0));
boxDY = (long) (mod8TileDY * floor(nBox / 2.0));

  /* calculate good pixels to be used in producing the mod 8
	 Tile Map using a sigma clipping method.
	 Go through each pixel 1 by 1 to find out if is is a
	 fluctuation with a doubling box. */

iterate_int_matrix(pixMask, iterate_set_true, 0);

nBoxBymod8TileDX = nBox * mod8TileDX;
nBoxBymod8TileDY = nBox * mod8TileDY;

tempPixMask1 = create_int_matrix(xMin, xMax, yMin, yMax);
if (tempPixMask1 == NULL)
	{
	report_error("Memory allocation error - tempPixMask1\n");
	return (1);
	}

for (x = mod8xmin; x <= mod8xmax; x += boxDX)
  for (y = mod8ymin; y <= mod8ymax; y += boxDY)
	{
	int		*mapBase,
			*currentMap;
	double	*observationBase,
			*currentObservation;
	double	value, value1;
	int		iLimit,
			jLimit;
	int		pixelsCleared;
	double	observationSum;
			
	int		binCount,
			currentCount,
			*pBin,
			maxBinUsed,
			minBinUsed,
			sectionSize;
#if WRITE_DETAILS
report_verbose("clearing outsiders x=%d, y=%d\n", x, y);
#endif
			
	minDX = max2(mod8xmin, x - nBoxBymod8TileDX);
	minDY = max2(mod8ymin, y - nBoxBymod8TileDY);
	maxDX = min2(mod8xmax, x + nBoxBymod8TileDX);
	maxDY = min2(mod8ymax, y + nBoxBymod8TileDY);

	sectionSize = (maxDX-minDX+1) * (maxDY-minDY+1);
	memset (countBins, 0, MAX_BINS * sizeof (int));
	maxBinUsed = 0;
	for (j = minDY
		, mapBase = BASE_DATA (pixMap, j)
		, observationBase = BASE_DATA (inOSW, j)
	  ; j <= maxDY
	  ; ++j
		, mapBase += pixMap->xDimension
		, observationBase += inOSW->xDimension
		)
		{
		for (i = minDX
			, currentMap = mapBase + i
			, currentObservation = observationBase + i
	  	; i <= maxDX
	  	; ++i
			, ++currentMap, ++currentObservation
			)
			{if (*currentMap)
				{currentCount = *currentObservation;
				if (currentCount >= MAX_BINS)
					currentCount = MAX_BINS - 1;
				else if (currentCount < 0) {
					*currentObservation = 0;
					currentCount = 0;
				}
				++countBins[currentCount];
				if (currentCount>maxBinUsed)
					maxBinUsed = currentCount;
				}
			}
		}  /* i<=maxDX */
	minBinUsed = 0;
	pixelsCleared = 1;
debugCount1= 0 ;
#if 1
	debugCount2 = 0;
	for (binCount=0, i=0;	i<=maxBinUsed;	++i)
		{binCount += countBins[i];
		}
#endif
#define CALCULATE_WITH_BINS 1
#if CALCULATE_WITH_BINS
	while (pixelsCleared)
		{
		for (numPixels=0, observationSum=0., i=minBinUsed
				, pBin=countBins+i;
			i<=maxBinUsed;
			++i, ++pBin)
			{observationSum += i * *pBin;
			numPixels += *pBin;
			}
		mean = observationSum/numPixels;
		for (value=0., i=minBinUsed, pBin=countBins+i
			;i<=maxBinUsed
			;++i, ++pBin)
			{value1 = (i-mean);
			value += *pBin * value1 * value1;
			}
		sigma = sqrt(value/(numPixels-1));
		value = sigma * nSig;
		pixelsCleared = 0;
		for (pBin=countBins+maxBinUsed, i=maxBinUsed
			;	pBin >= countBins
			;	--pBin, --i
			)
			{if ( (i - mean) > value)
				{--maxBinUsed;
				pixelsCleared += *pBin;
				}
			else
				break;
			}
		for (i=minBinUsed, pBin=countBins+i
			;	i<maxBinUsed
			;++i, ++pBin
			)
			{if ( (mean - i) > value)
				{++minBinUsed;
				pixelsCleared += *pBin;
				}
			else
				break;
			}
		debugCount1 += pixelsCleared;
				
		}	/* while (pixelsCleared) */
debugCount2= 0 ;
#else		/*  !CALCULATE_WITH_BINS */
			
	initializeMatrixSection (tempPixMask1, minDX, maxDX, minDY, maxDY);
	fast_int_matrix_section (pixMap, tempPixMask1, minDX, minDY);
	/* Calculate the mean and standard deviation of the current rectangle */
	pixelsCleared = 1;
	while (pixelsCleared)
		{
		IterateSumSquareDifference iSSD;
		IterateClearOutsiders iOutsiders;
				
#define COMBINE_COUNT_AND_SUM 1
#if COMBINE_COUNT_AND_SUM
		numPixels = countAndSumMatrix (tempPixMask1, inOSW, &observationSum);
		mean = observationSum/numPixels;
#else
		numPixels = count(tempPixMask1);

		assert(numPixels);
		mean = sumMask(inOSW, tempPixMask1) / numPixels;
#endif
		iSSD.result = 0;
		iSSD.mean = mean;
		iSSD.input = inOSW;
		iterate_int_matrix(tempPixMask1, iterate_sum_square_diff,
						   &iSSD);

		assert(numPixels > 1);
		sigma = sqrt(iSSD.result / (numPixels - 1));
		iOutsiders.image = inOSW;
		iOutsiders.mean = mean;
		iOutsiders.sigma = sigma;
		iOutsiders.nSig = par->nSig;
		pixelsCleared = fastClearOutsiders (tempPixMask1, 
								&iOutsiders);
		debugCount2 += pixelsCleared;
		}	/* while (pixelsCleared) */
			
#endif		/* CALCULATE_WITH_BINS */

	for (j = 1
			, iLimit=min2 (boxDX, mod8xmax+1-x)
			, jLimit=min2 (boxDY, mod8ymax+1-y)
			, mapBase = BASE_DATA (pixMask, j+y-1) + x - 1
			, observationBase = BASE_DATA (inOSW, j+y-1) + x - 1
		  ; j <= jLimit
		  ; ++j
			, mapBase += pixMask->xDimension
			, observationBase += inOSW->xDimension
			)
		{
		for (i = 1
				, currentMap = mapBase + i
				, currentObservation = observationBase + i
		  	; i <= iLimit
		  	; ++i
				, ++currentMap, ++currentObservation
				)
			{
			value = *currentObservation;
			/*value1 = get_real_matrix (inOSW, x + i-1, y + j -1);
			if (value != value1) printf("Values differ\n");*/
			/* 	ROB Don't divide by sigma */
			if (fabs(value - mean)  > nSig*sigma)
				{
				*currentMap = 0;
				}
				/*set_int_matrix (pixMask, x+i-1, y+j-1, 0);*/
			}
		}	/* for (j=1 TO jLimit) */

	}	/* for y=mod8TileDY */

DESTROY_INT_MATRIX(tempPixMask1);
#if WRITE_TIMING_INFO
timeIncrement("cleared outsiders");
#endif

/* Then exclude everything else in a mod8 tile with a fluctuation */
for (x = mod8xmin; x <= mod8xmax; x += mod8TileDX)
  for (y = mod8ymin; y <= mod8ymax; y += mod8TileDY)
	{
			
	int allTrue;
	IntMatrix alias;
	IntFunctor *functor;

	alias_int_matrix(&alias, pixMask,
					x, x + mod8TileDX - 1,
					y, y + mod8TileDY - 1);

	allTrue = all(&alias);
	functor = allTrue ? iterate_set_true : iterate_set_false;
	iterate_int_matrix(&alias, functor, 0);
	}
iterate_int_matrix(pixMask, iterate_set_same, pixMap);

DESTROY_INT_MATRIX(pixMask);
#if WRITE_TIMING_INFO
timeIncrement("excluded others");
#endif

mod8Tile = create_real_matrix(1, mod8TileDX, 1, mod8TileDY);
if (mod8Tile == NULL)
	{
	report_error("Memory allocation error - mod8Tile\n");
	return (1);
	}

meanTile = create_real_matrix(1, mod8TileDX, 1, mod8TileDY);
if (meanTile == NULL)
	{
	report_error("Memory allocation error - meanTile\n");
	return (1);
	}

 /* calculate the modulo-8 map */
#if WRITE_TIMING_INFO
timeIncrement("Entering module-8 map calculation");
#endif
		
debugCount1= 0 ;
pixMask = create_int_matrix( minDX, maxDX, minDY, maxDY);
for (x = mod8xmin; x <= mod8xmax; x += mod8TileDX)	/* for each tile in the image */
  for (y = mod8ymin; y <= mod8ymax; y += mod8TileDY)
	{
#if WRITE_DETAILS
char buffer[64];
sprintf(buffer, "processing tile x=%d, y=%d", x, y);
timeIncrement(buffer);
#endif
	for (i = 1; i <= mod8TileDX; ++i)	/* for each pixel in the tile */
	  for (j = 1; j <= mod8TileDY; ++j)
		{double		observationSum;
				
		numPixels = 0;
		n = 0;
		while ((numPixels == 0) &&
					 (((boxDX + (n * mod8TileDX))
					   * (boxDY + (n * mod8TileDY))) <= xMax * yMax))
			{
			minDX = max2(mod8xmin + i - 1,
							   x + i - 1 - boxDX - (n * mod8TileDX));
			minDY = max2(mod8ymin + j - 1,
							   y + j - 1 - boxDY - (n * mod8TileDY));
			maxDX = min2(mod8xmax,
							   x + i - 1 + boxDX + (n * mod8TileDX));
			maxDY = min2(mod8ymax,
							   y + j - 1 + boxDY + (n * mod8TileDY));

#define VIRTUAL_SECTION 1
#if VIRTUAL_SECTION
			numPixels = countAndSumMatrixSection (pixMap, inOSW, minDX,
							minDY, maxDX, maxDY,
							mod8TileDX, mod8TileDY, &observationSum);
#else
			initializeMatrixSection (pixMask,
					  minDX, maxDX, minDY, maxDY);

			fast_int_matrix_section(pixMap, pixMask, minDX, minDY);
#endif

			++n;
			}

		if (n>debugCount1)
			debugCount1 = n;
		if (numPixels > 0)
			{
#if VIRTUAL_SECTION
			tmp = observationSum/numPixels;

#else
			tmp = fast_sumMaskStep(inOSW, pixMask,
					   mod8TileDX, mod8TileDY) / numPixels;
#endif
			set_real_matrix(meanTile, i, j, tmp);
				}
		else
			set_real_matrix(meanTile, i, j, 0);

		}	/* for j, i */

	mean = fast_sum(meanTile) / (meanTile->xSize * meanTile->ySize);
	if (mean == 0.0)
		mean = 1.0;
	iScale.scale = 1 / mean;
	iScale.output = mod8Tile;
	iScale.xMin = 1;
	iScale.yMin = 1;
	iScale.accumulate = 0;
#define SINGLE_SCALE_STEP 0
#if SINGLE_SCALE_STEP == 0
	fastIterateScale (meanTile, &iScale);

	iScale.scale = 1;
#endif
	iScale.output = mod8Map;
	iScale.xMin = x;
	iScale.yMin = y;
#if SINGLE_SCALE_STEP
	fastIterateScale (meanTile, &iScale);
#else
	fastIterateScale (mod8Tile, &iScale);
#endif
	/*iterate_real_matrix(mod8Tile, iterate_scale, &iScale);*/
	}	/* for x, y */
DESTROY_INT_MATRIX(pixMask);

#if WRITE_TIMING_INFO
timeIncrement ("Exiting module-8 map calculation");
#endif
/* if there are any gaps in the modulo-8 map, fill them with 1.0 */
iterate_real_matrix(mod8Map, iterate_fix_zeros, 0);
  

/* Set tolerance at machine precision */
tol = 1e-12;

/* Make sure the image begins and ends at a physical CCD
	pixel boundary */

  /* We must force the code below to work even for windows
	 that are not of size modulo 8 by excluding part of the
	 window */

  /* Allocate space for the starting and intermediate image tiles. */

StartImTile = create_real_matrix(1, mod8TileDX, 1, mod8TileDY);
if (StartImTile == NULL)
	{
	report_error("Memory allocation error - StartImTile\n");
	return (1);
	}

InterImTile = create_real_matrix(1, mod8TileDX, 1, mod8TileDY);
if (InterImTile == NULL)
	{
	report_error("Memory allocation error - InterImTile\n");
	return (1);
	}

  /* Allocate space for the sums over rows and columns */

TotalInMod8Col = create_real_vector(1, mod8TileDX);
if (TotalInMod8Col == NULL)
	{
	report_error("Memory allocation error - TotalInMod8Col\n");
	return (1);
	}

TotalInOSWCol = create_real_vector(1, mod8TileDX);
if (TotalInOSWCol == NULL)
	{
	report_error("Memory allocation error - TotalInOSWCol\n");
	return (1);
	}

TotalInMod8Row = create_real_vector(1, mod8TileDY);
if (TotalInMod8Row == NULL)
	{
	report_error("Memory allocation error - TotalInMod8Row\n");
	return (1);
	}

TotalInOSWRow = create_real_vector(1, mod8TileDY);
if (TotalInOSWRow == NULL)
	{
	report_error("Memory allocation error - TotalInOSWRow\n");
	return (1);
	}

for (x = mod8xmin; x <= mod8xmax; x += mod8TileDX)
  for (y = mod8ymin; y <= mod8ymax; y += mod8TileDY)
	{
#if WRITE_DETAILS
report_verbose("processing section x=%d, y=%d\n", x, y);
#endif
	real_matrix_section(inOSW, StartImTile, x, y);

	real_matrix_section(mod8Map, mod8Tile, x, y);


	/* Now dealing with a single mod8Tile
		First redistribute in y (before x) */

	for (i = 1; i <= mod8TileDX; ++i)
		{
		double mod8Sum = 0.0;
		double oswSum = 0.0;

		for (j = 1; j <= mod8TileDY; ++j)
			{
			/* Set the intermediate image tile to zeros */
				set_real_matrix(InterImTile, i, j, 0.0);

			/* Sum up flux in each of the pixels in Col i
				for the mod8Map and real image */

			mod8Sum += get_real_matrix(mod8Tile, i, j);
			oswSum += get_real_matrix(StartImTile, i, j);
			}

		set_real_vector(TotalInMod8Col, i, mod8Sum);
		set_real_vector(TotalInOSWCol, i, oswSum);
/*headas_chat(3, "TotalInMod8Col(%d) = %e\n", i, mod8Sum);
headas_chat(3, "TotalInOSWCol(%d) = %e\n", i, oswSum);
*/
		/* Now find the boundaries and correct image */

		RunningPixel = 1;
		RunningMod8Total = 0.0;
		RunningPosition = 0.0;
		OldRunningPosition = 0.0;

		for (j = 1; j <= mod8TileDY - 1; ++j)
		 while (RunningMod8Total <
				get_real_vector(TotalInMod8Col, i)
				* (j - tol) / doubleMod8TileDY)
		  if (RunningMod8Total
				+ (RunningPixel - RunningPosition)
				* get_real_matrix(mod8Tile, i, RunningPixel)
				<= get_real_vector(TotalInMod8Col, i)
				* j / doubleMod8TileDY)
			{
			/* Add in the rest of the counts from RunningPixel and move on to
				the next pixel */

			RunningMod8Total +=
						(RunningPixel - RunningPosition)
						* get_real_matrix(mod8Tile, i, RunningPixel);

			tmp = get_real_matrix(InterImTile, i, j)
						+ (RunningPixel - RunningPosition)
						* get_real_matrix(StartImTile, i, RunningPixel);
			set_real_matrix(InterImTile, i, j, tmp);
/*headas_chat(4, "set InterInTile(%d, %d) = %e\n", i, j, tmp);
*/
			RunningPosition = RunningPixel;
			++RunningPixel;
			}
		else
			{
			/* Just add in the neccessary counts */

			OldRunningPosition = RunningPosition;

			RunningPosition = OldRunningPosition
						+ (get_real_vector(TotalInMod8Col, i)
						* j / doubleMod8TileDY - RunningMod8Total)
						/ get_real_matrix(mod8Tile, i, RunningPixel);

			tmp = get_real_matrix(InterImTile, i, j)
						+ (RunningPosition - OldRunningPosition)
						* get_real_matrix(StartImTile, i, RunningPixel);
			set_real_matrix(InterImTile, i, j, tmp);
/*headas_chat(4, "set InterInTile.else(%d, %d) = %e\n", i, j, tmp);
*/
			RunningMod8Total =
						get_real_vector(TotalInMod8Col, i)
						* j / doubleMod8TileDY;
			}	 /* end if while and for */
		/* Now the last pixel contains whatever is left over */

		tmp = get_real_vector(TotalInOSWCol, i)
				- sum_range(InterImTile, i, i, 1, mod8TileDY - 1);
		set_real_matrix(InterImTile, i, mod8TileDY, tmp);
		}			 /* end for i */

	/* Now redistribute in x */

	copy_real_matrix(InterImTile, StartImTile);
	for (j = 1; j <= mod8TileDY; ++j)
		{
		tmp = 0;
		for (i = 1; i <= mod8TileDX; ++i)
			{
			set_real_matrix(InterImTile, i, j, 0.0);
			tmp += get_real_matrix(StartImTile, i, j);
			}
		set_real_vector(TotalInOSWRow, j, tmp);
		}

	TotalInMod8Cols = 0.0;
	for (i = 1; i <= mod8TileDX; ++i)
		TotalInMod8Cols += get_real_vector(TotalInMod8Col, i);

	RunningPixel = 1;
	RunningMod8Total = 0.0;
	RunningPosition = 0.0;
	OldRunningPosition = 0.0;

	for (i = 1; i <= mod8TileDX - 1; ++i)
	 while (RunningMod8Total
			< TotalInMod8Cols * (i - tol) / doubleMod8TileDX)
	  if (RunningMod8Total + (RunningPixel - RunningPosition)
				* get_real_vector(TotalInMod8Col, RunningPixel)
				<= TotalInMod8Cols * i / doubleMod8TileDX)
		{
		RunningMod8Total += (RunningPixel - RunningPosition)
						* get_real_vector(TotalInMod8Col, RunningPixel);
		for (j = 1; j <= mod8TileDY; ++j)
			{
			tmp = get_real_matrix(InterImTile, i, j)
					+ (RunningPixel - RunningPosition)
					* get_real_matrix(StartImTile, RunningPixel, j);
			set_real_matrix(InterImTile, i, j, tmp);
			}
		RunningPosition = RunningPixel;
		RunningPixel++;
		}
	else
		{
		OldRunningPosition = RunningPosition;
		RunningPosition = OldRunningPosition
					+ (TotalInMod8Cols * i / doubleMod8TileDX
					- RunningMod8Total)
					/ get_real_vector(TotalInMod8Col, RunningPixel);
		for (j = 1; j <= mod8TileDY; ++j)
			{
			tmp = get_real_matrix(InterImTile, i, j)
					+ (RunningPosition - OldRunningPosition)
					* get_real_matrix(StartImTile, RunningPixel, j);
					set_real_matrix(InterImTile, i, j, tmp);
			}
		RunningMod8Total = TotalInMod8Cols * i / doubleMod8TileDX;
		}		 /*end if, while, for */

	for (j = 1; j <= mod8TileDY; ++j)
		{
		tmp = get_real_vector(TotalInOSWRow, j)
				- sum_range(InterImTile, 1, mod8TileDX - 1, j, j);
		set_real_matrix(InterImTile, mod8TileDX, j, tmp);
		}

	/* Write redistributed image to output file */
	iScale.output = outOSW;
	iScale.scale = 1;
	iScale.xMin = x;
	iScale.yMin = y;
	iScale.accumulate = 0;
	iterate_real_matrix(InterImTile, iterate_scale, &iScale);
	if (headas_chatpar > 3)
		iterate_real_matrix(InterImTile, iterate_print, 0);


	/* Now repeat whole redistribution but with x first then y */

	real_matrix_section(inOSW, StartImTile, x, y);

	for (j = 1; j <= mod8TileDY; ++j)
		{
		double mod8Sum = 0.0;
		double oswSum = 0.0;
		for (i = 1; i <= mod8TileDX; ++i)
			{
			set_real_matrix(InterImTile, i, j, 0.0);
			mod8Sum += get_real_matrix(mod8Tile, i, j);
            oswSum += get_real_matrix(StartImTile, i, j);
 			}
		set_real_vector(TotalInMod8Row, j, mod8Sum);
		set_real_vector(TotalInOSWRow, j, oswSum);
		RunningPixel = 1;
		RunningMod8Total = 0.0;
		RunningPosition = 0.0;
		OldRunningPosition = 0.0;

		for (i = 1; i <= mod8TileDX - 1; ++i)
			{
			while (RunningMod8Total
					< get_real_vector(TotalInMod8Row, j)
					* (i - tol) / doubleMod8TileDX)
			  if (RunningMod8Total
					+ (RunningPixel - RunningPosition)
					* get_real_matrix(mod8Tile, RunningPixel, j)
					<= get_real_vector(TotalInMod8Row, j)
					* i / doubleMod8TileDX)
				{
				RunningMod8Total +=
						(RunningPixel - RunningPosition)
						* get_real_matrix(mod8Tile, RunningPixel, j);
				tmp = get_real_matrix(InterImTile, i, j)
						+ (RunningPixel - RunningPosition)
						* get_real_matrix(StartImTile, RunningPixel, j);
				set_real_matrix(InterImTile, i, j, tmp);
				RunningPosition = (double) RunningPixel;
				RunningPixel++;
				}
			else
				{
				OldRunningPosition = RunningPosition;
				RunningPosition = OldRunningPosition
							+ (get_real_vector(TotalInMod8Row, j) * i /
							doubleMod8TileDX - RunningMod8Total)
							/ get_real_matrix(mod8Tile, RunningPixel, j);
				tmp = get_real_matrix(InterImTile, i, j)
							+ (RunningPosition - OldRunningPosition)
							* get_real_matrix(StartImTile, RunningPixel, j);
				set_real_matrix(InterImTile, i, j, tmp);

				RunningMod8Total =
							get_real_vector(TotalInMod8Row, j)
							* i / doubleMod8TileDX;
				} /* end if, end while */
			} /* end for i */

		tmp = get_real_vector(TotalInOSWRow, j)
				- sum_range(InterImTile, 1, mod8TileDX - 1, j, j);
		set_real_matrix(InterImTile, mod8TileDX, j, tmp);
		} /* end for j */

	/* Now redistribute in y */

	copy_real_matrix(InterImTile, StartImTile);
	for (i = 1; i <= mod8TileDX; ++i)
		{
		tmp = 0.0;
		for (j = 1; j <= mod8TileDY; ++j)
			{
			set_real_matrix(InterImTile, i, j, 0.0);
			tmp += get_real_matrix(StartImTile, i, j);
			}
		set_real_vector(TotalInOSWCol, i, tmp);
		}

	TotalInMod8Rows = 0.0;
	for (j = 1; j <= mod8TileDY; ++j)
		TotalInMod8Rows += get_real_vector(TotalInMod8Row, j);

	RunningPixel = 1;
	RunningMod8Total = 0.0;
	RunningPosition = 0.0;
	OldRunningPosition = 0.0;

	for (j = 1; j <= mod8TileDY - 1; ++j)
	 while (RunningMod8Total
			< TotalInMod8Rows * (j - tol) / doubleMod8TileDY)
	  if (RunningMod8Total
				+ (RunningPixel - RunningPosition)
				* get_real_vector(TotalInMod8Row, RunningPixel)
				<= TotalInMod8Rows * j / doubleMod8TileDY)
		{
		RunningMod8Total += (RunningPixel - RunningPosition)
							* get_real_vector(TotalInMod8Row, RunningPixel);
		for (i = 1; i <= mod8TileDX; ++i)
			{
			tmp = get_real_matrix(InterImTile, i, j)
							+ (RunningPixel - RunningPosition)
							* get_real_matrix(StartImTile, i, RunningPixel);
			set_real_matrix(InterImTile, i, j, tmp);
			}
		RunningPosition = (double) RunningPixel;
		++RunningPixel;
		}
	else
		{
		OldRunningPosition = RunningPosition;
		RunningPosition = OldRunningPosition
					+ (TotalInMod8Rows * j / doubleMod8TileDY
					- RunningMod8Total)
					/ get_real_vector(TotalInMod8Row, RunningPixel);

		for (i = 1; i <= mod8TileDX; ++i)
			{
			tmp = get_real_matrix(InterImTile, i, j)
						+ (RunningPosition - OldRunningPosition)
						* get_real_matrix(StartImTile, i, RunningPixel);
			set_real_matrix(InterImTile, i, j, tmp);
			}
		RunningMod8Total = TotalInMod8Rows * j / doubleMod8TileDY;
 		}

	for (i = 1; i <= mod8TileDX; ++i)
		{
		tmp = get_real_vector(TotalInOSWCol, i)
				- sum_range(InterImTile, i, i, 1, mod8TileDY - 1);
		set_real_matrix(InterImTile, i, mod8TileDY, tmp);
		}

	/* Write redistributed image to output file */
	iScale.output = outOSW;
	iScale.scale = 1;
	iScale.xMin = x;
	iScale.yMin = y;
	iScale.accumulate = 1;
	iterate_real_matrix(InterImTile, iterate_scale, &iScale);

/*headas_chat(4, "x first then y\n");*/
	if (headas_chatpar > 3)
		iterate_real_matrix(InterImTile, iterate_print, 0);
	}	/* FOR y <= mod8ymax */

/* Finally divide ouput file by 2 so that we have an average */

iScale.output = outOSW;
iScale.scale = 0.5;
iScale.xMin = 1;
iScale.yMin = 1;
iScale.accumulate = 0;
iterate_real_matrix(outOSW, iterate_scale, &iScale);

#if WRITE_TIMING_INFO
timeIncrement("applied correction");
#endif

/* Free up memory. */
DESTROY_REAL_VECTOR(TotalInMod8Col);
DESTROY_REAL_VECTOR(TotalInOSWCol);
DESTROY_REAL_VECTOR(TotalInMod8Row);
DESTROY_REAL_VECTOR(TotalInOSWRow);
DESTROY_REAL_MATRIX(InterImTile);
DESTROY_REAL_MATRIX(StartImTile);
DESTROY_REAL_MATRIX(mod8Tile);
DESTROY_REAL_MATRIX(meanTile);

/* deallocate bad pixel map */
DESTROY_INT_MATRIX(pixMap);
return retVal;
}

/**************************************************************************/
int
uvotmodmap (void)
{
  Parameters par = { 0 };
  int status = 0;

  set_toolname(TOOLNAME);
  set_toolversion(VERSION);

  add_report_function(&report_headas);

  /*  get input parameters */
  status = uvotmodmap_getpar(&par);

  /* call work function to simulate a UVOT image */
  if (!status)
    status = uvotmodmap_work(&par);

  remove_report_function(&report_headas);

  return (status);
}


/**************************************************************************/
int
uvotmodmap_getpar (Parameters * par)
/*  read input parameters for the uvotmodmap tasks from the .par file */
{
  int status = 0;

  if (!status)
    status = PILGetFname("infile", par->infile);

  if (!status)
    status = PILGetFname("badpixfile", par->qfile);

  if (!status)
    status = PILGetString("outfile", par->outfile);

  if (!status)
    status = PILGetBool("mod8prod", &par->ppsproduct);

  if (!status && par->ppsproduct)
    status = PILGetString("mod8file", par->ppsfile);

  if (!status)
    status = PILGetInt("nsig", &par->nSig);

  if (!status)
    status = PILGetInt("ncell", &par->nBox);

  if (!status)
    status = PILGetBool("clobber", &par->clobber);

  if (!status)
    status = PILGetBool("history", &par->history);

  if (!status)
    status = PILGetBool("subimage", &par->processSubimage);

  if (!status && par->processSubimage)
    {
      status = PILGetInt("xmin", &par->subimageXMin);

      if (!status)
        status = PILGetInt("xmax", &par->subimageXMax);

      if (!status)
        status = PILGetInt("ymin", &par->subimageYMin);

      if (!status)
        status = PILGetInt("ymax", &par->subimageYMax);
    }

  if (status)
    fprintf(stderr, "unable to load parameters [%d]\n", status);

  return (status);
}


int
copy_header (Task * task, int target, int * pstatus)
{
  fitsfile * from = task->infptr;
  fitsfile * to = (target == TARGET_PPSFILE)
                ? task->ppsfptr : task->outfptr;

  fits_copy_header(from, to, pstatus);

  if (*pstatus)
    report_error("unable to copy header [%d]\n", *pstatus);
  else
    {
      fits_write_date(to, pstatus);

      fits_update_key_str(to, "ORIGIN", "Swift Science Center",
                 "Processing Site", pstatus);
      fits_update_key_lng(to, "BITPIX", FLOAT_IMG,
                  "number of pix per data pixel", pstatus);
      fits_update_key_log(to, "MOD8CORR", 1,
                  "Modulo 8 noise pattern corrected", pstatus);

      if (target == TARGET_PPSFILE)
        {
          if (task->processingImages)
            {
              fits_update_key_lng(to, "NAXIS1", task->width,
                      "Image width", pstatus);
              fits_update_key_lng(to, "NAXIS2", task->height,
                      "Image height", pstatus);
            }
          /* fits_set_hdustruc(to, pstatus); */

          fits_update_key_str(to, "CONTENT",
                 "UVOT OSW MODULO-8 Tile", "Content of file", pstatus);
          fits_update_key_str(to, "HDUCLASS", "OGIP",
                "Format conforms to OGIP/GSFC conventions", pstatus);
          fits_update_key_str(to, "HDUCLAS1", "IMAGE",
                "File contains an image", pstatus);
          fits_update_key_str(to, "HDUCLAS2", "GENERIC",
                "Detector map", pstatus);
          fits_update_key_str(to, "CREATOR", CREATOR,
                    "Modulo-8 correction tool", pstatus);
        }

      if (*pstatus)
        report_error("unable to update keywords [%d]\n", *pstatus);
    }

  if (!*pstatus)
    {
      /* ensure BLANK does not appear */
      int tmp = 0;
      if (fits_delete_key(to, "BLANK", &tmp))
        fits_clear_errmsg();
    }

  return *pstatus;
}

/*	the following are used by an experimental method of determining
	which points to use in the background value calculation
*/
/**************************************************************************/
int
process_extension (Task * task, int imageExtension, int qualityExtension)
{
  int status = 0;
  Parameters * par = task->par;
  int hduType;
  int anynul;                   /* were there any null values returned */
  int xMax, xMin;               /* Minimum and Maximum x value for window */
  int yMax, yMin;               /* Minimum and Maximum y value for window */
  int mod8xmax, mod8xmin;       /* Minimum and Maximum x value for window */
  int mod8ymax, mod8ymin;       /* Minimum and Maximum y value for window */

  long n;                       /* looping variable */
  long binX, binY;              /* binning values */
  long windowX0, windowY0;      /* WindowX0 and WindowY0 */
  long windowDX, windowDY;      /* WindowDX and WindowDY */
  long mod8TileDX;
  long mod8TileDY;              /* size of modul0-8 tile */

  double doubleMod8TileDX;     /* holds double (Mod8TileDX) */
  double doubleMod8TileDY;     /* holds double (Mod8TileDY) */

  RealMatrix *inOSW = 0;        /* input science observation window */
  RealMatrix *outOSW = 0;       /* output science observation window */
  RealMatrix *mod8Map = 0;

  IntMatrix *quality = 0;       /* quality matrix */
  IntMatrix *pixMap = 0;
  RawWindow window;
  char ctype1[64];
  long naxis1, naxis2;
  int corrected = 0;

  /* move to the next HDU */
  fits_movabs_hdu(task->infptr, imageExtension, &hduType, &status);
  if (status)
    {
      report_error("unable to move to image HDU %d [%d]\n",
              imageExtension, status);
      return status;
    }

  fits_read_key_log(task->infptr, "MOD8CORR", &corrected, 0, &status);
  if (status)
    {
      /* MOD8CORR is a new keyword, so assume false if not found */
      report_warning("unable to read MOD8CORR, assuming false [%d]\n", status);
      status = 0;
    }
  else if (corrected)
    {
      report_verbose("HDU %d is already corrected, copying to output\n");
      if (fits_copy_hdu(task->infptr, task->outfptr, 0, &status))
        report_error("unable to transfer HDU to output [%d]\n", status);
      return status;
    }

  if (task->qfptr)
    if (fits_movabs_hdu(task->qfptr, qualityExtension, &hduType, &status))
      {
        report_error("unable to move to quality HDU %d [%d]\n",
              qualityExtension, status);
        return status;
      }

  status = load_raw_window(task->infptr, &window);
  if (status)
    {
      report_error("unable to read binning parameters [%d]\n", status);
      return status;
    }
  else
    {
      binX = window.binx;
      binY = window.biny;
      windowX0 = window.x0;
      windowY0 = window.y0;
      windowDX = window.dx;
      windowDY = window.dy;
    }

  if (fits_read_key_str(task->infptr, "CTYPE1", ctype1, 0, &status))
    report_warning("unable to read CTYPE1 [%d]\n", status);

  if (!strcmp(ctype1, "RA---TAN"))
    {
      int tmp;
      report_status("CTYPEn [%s] indicates SKY, initializing to full image\n", ctype1);
      windowX0 = 0;
      windowY0 = 0;
      
      tmp = 0;
      if (fits_read_key_lng(task->infptr, "NAXIS1", &naxis1, 0, &tmp))
        report_warning("unable to read NAXIS1 [%d]\n", status=tmp);

      tmp = 0;
      if (fits_read_key_lng(task->infptr, "NAXIS2", &naxis2, 0, &tmp))
        report_warning("unable to read NAXIS2 [%d]\n", status=tmp);

      windowDX = naxis1 * binX;
      windowDY = naxis2 * binY;
      if (status)
        {
          report_error("unable to read NAXISn [%d]\n", status);
          return status;
        }
    }

  report_verbose("hduType = %d, binX = %d, binY = %d\n", hduType, binX, binY);
  if ((binX == 0) || (binY == 0))
  {
      report_warning("WARNING:  The input file has a binning factor of 0.\n");
      return (1);
  }

  /* calculate size of modulo-8 tile */
  mod8TileDX = 8 / binX;
  mod8TileDY = 8 / binY;

  report_verbose("mod8TileDX and mod8TileDY = %d and %d\n", 
        mod8TileDX, mod8TileDY);
  
  doubleMod8TileDX = (double) mod8TileDX;
  doubleMod8TileDY = (double) mod8TileDY;

  xMin = yMin = 1;
  xMax = windowDX / binX;
  yMax = windowDY / binY;

  report_verbose("xMin yMin xMax yMax = %d %d %d %d\n", 
        xMin,yMin,xMax, yMax);
   
  /* allocate memory for quality array */
  quality = create_int_matrix(xMin, xMax, yMin, yMax);
  if (quality == NULL)
    {
      report_error("Memory allocation error - quality array\n");
      return (1);
    }

  if (task->qfptr)
    {
      long size = quality->xSize * quality->ySize;
      fits_read_img_int(task->qfptr, 1, 1, size, 0,
            quality->data, &anynul, &status);
      report_verbose("quality size = %ld\n", size);
    }
  else
    {
      int i, size;
      size = quality->xSize * quality->ySize;
      for (i = 0; i < size; ++i)
        quality->data[i] = 0;
    }

  if (status)
    {
      report_error("unable to read quality image [%d]\n", status);
      return status;
    }

  /* allocate space for the input and output observation science window */
  /* output observation science window should be double the size
     because of /2 later */

  inOSW = create_real_matrix(xMin, xMax, yMin, yMax);
  if (inOSW == NULL)
    {
      report_error("Memory allocation error - input science window\n");
      return (1);
    }

  outOSW = create_real_matrix(xMin, xMax, yMin, yMax);
  if (outOSW == NULL)
    {
      report_error("Memory allocation error - output science window\n");
      return (1);
    }

  {
    long s;
    long size = inOSW->xSize * inOSW->ySize;
    fits_read_img_dbl(task->infptr, 1, 1, size, 0, inOSW->data, &anynul, &status);
    
    /* initialize output to input, to deal with cases where the modulo 8
       correction is smaller than the actual image size.  That is when
       mod8(x|y)(min|max) is less then (x|y)(min|max) */
    for (s = 0; s < size; ++s)
       outOSW->data[s] = inOSW->data[s]; 
  }


  /* Deal with the window sizes so that the correct pixels are
     processed for the mod 8 noise. */

  n = (windowX0 / binX) + xMin - 1;
  if ((n % mod8TileDX) == 0)
    mod8xmin = xMin;
  else
    mod8xmin = mod8TileDX * ((n / mod8TileDX) + 1) - (windowX0 / binX) + 1;

  n = (windowX0 / binX) + xMax;
  if ((n % mod8TileDX) == 0)
    mod8xmax = xMax;
  else
    mod8xmax = mod8TileDX * (n / mod8TileDX) - (windowX0 / binX);

  n = (windowY0 / binY) + yMin - 1;
  if ((n % mod8TileDY) == 0)
    mod8ymin = yMin;
  else
    mod8ymin = mod8TileDY * ((n / mod8TileDY) + 1) - (windowY0 / binY) + 1;

  n = (windowY0 / binY) + yMax;
  if ((n % mod8TileDY) == 0)
    mod8ymax = yMax;
  else
    mod8ymax = mod8TileDY * (n / mod8TileDY) - (windowY0 / binY);

  report_debug("mod8xmin, mod8ymin = %d %d\n", mod8xmin, mod8ymin);
  report_debug("mod8xmax, mod8ymax = %d %d\n", mod8xmax, mod8ymax);
#if WRITE_TIMING_INFO
timeIncrement ("Operator input is complete");
#endif

/* Check to see if modulo-8 noise is lost in binning */

/*headas_chat(2, "Check to see if modulo-8 noise is lost in binning\n");*/
  if (!((binX >= 8) && (binY >= 8)))
    {
      /* Allocate space for the pixel map. */

      pixMap = create_int_matrix(xMin, xMax, yMin, yMax);
      if (pixMap == NULL)
        {
          report_error("Memory allocation error - pixMap\n");
          return (1);
        }

      iterate_int_matrix(pixMap, iterate_set_true, 0);
      iterate_int_matrix(quality, iterate_set_false_where_true, pixMap);


      /* Is the mean countrate big enough?
         mod8 noise up to 10- 20% effect, assume that must have ~3%
         accuracy to improve image this corresponds approx to
         mean * nBox^2 > 1000 */

  /*    if (mean * nBox * nBox > 1000.0)  */
		{MOD8_PAR		m8Pars;
		
		m8Pars.binX = binX;
		m8Pars.binY = binY;
		m8Pars.windowX0 = windowX0;
		m8Pars.windowY0 = windowY0;
		m8Pars.nSig = par->nSig;
		m8Pars.nBox = par->nBox;
		if (par->processSubimage)
			{IntMatrix		*qualitySub;
			RealMatrix		*inOSWSub,
							*outOSWSub;
			int				mnX = par->subimageXMin,
							mxX = par->subimageXMax,
							mnY = par->subimageYMin,
							mxY = par->subimageYMax;
			int				*pi1,
							*pi2, *pi2Base;
			int				i,
							j;
			double			*pd1, *pd1Base,
							*pd2, *pd2Base;
			
			/*	Convert the input subwindow definition
				to FITS co-ordinates
			*/
			mnX -= (windowX0-1);
			mnY -= (windowY0-1);
			mxX -= (windowX0-1);
			mxY -= (windowY0-1);
			if ( (mnX>=mxX) || (mnY>=mxY) )
				{printf ("Improper sub-image definition\n");
				return 1;
				}
			if (mnX<xMin) 
				mnX = xMin;
			if (mnY<yMin) 
				mnY = yMin;
			if (mxX>xMax) 
				mxX = xMax;
			if (mxY>yMax) 
				mxY = yMax;
			qualitySub = create_int_matrix(mnX, mxX, mnY, mxY);
			for (j=mnY
					, pi1 = qualitySub->data
					, pi2Base = BASE_DATA (quality, j)
				; j<=mxY
				; ++j, pi2Base+= quality->xDimension
				)
				{for (i=mnX, pi2=pi2Base+i
					; i<= mxX
					; ++i, ++pi2, ++pi1
					)
					*pi1 = *pi2;
				}
			
			DESTROY_INT_MATRIX(quality);
			inOSWSub = create_real_matrix(mnX, mxX, mnY, mxY);
			for (j=mnY
					, pd1 = inOSWSub->data
					, pd2Base = BASE_DATA (inOSW, j)
				; j<=mxY
				; ++j, pd2Base+= inOSW->xDimension
				)
				{for (i=mnX, pd2=pd2Base+i
					; i<= mxX
					; ++i, ++pd2, ++pd1
					)
					*pd1 = *pd2;
				}
			
			outOSWSub = create_real_matrix(mnX, mxX, mnY, mxY); 
			
			m8Pars.xMin = mnX;
			m8Pars.xMax = mxX;
			m8Pars.yMin = mnY;
			m8Pars.yMax = mxY;
			m8Pars.quality = qualitySub;
			m8Pars.inOSW = inOSWSub;
			m8Pars.outOSW = outOSWSub;
			
			if (calculateMod8Map (&m8Pars) )
				return 1;
			mnX = m8Pars.xMin;
			mxX = m8Pars.xMax;
			mnY = m8Pars.yMin;
			mxY = m8Pars.yMax;
			mod8Map = m8Pars.mod8Map;
			for (j=mnY
					, pd1Base = BASE_DATA (outOSWSub, j)
					, pd2Base = BASE_DATA (outOSW, j)
				; j<=mxY
				; ++j
					, pd1Base += outOSWSub->xDimension
					, pd2Base += outOSW->xDimension
				)
				{for (i=mnX
						, pd1=pd1Base+i
						, pd2=pd2Base+i
					; i<= mxX
					; ++i, ++pd2, ++pd1
					)
					*pd2 = *pd1;
				}
			DESTROY_REAL_MATRIX (outOSWSub);
			}
		else
			{
			m8Pars.xMin = xMin;
			m8Pars.xMax = xMax;
			m8Pars.yMin = yMin;
			m8Pars.yMax = yMax;
			m8Pars.quality = quality;
			m8Pars.inOSW = inOSW;
			m8Pars.outOSW = outOSW;
			if (calculateMod8Map (&m8Pars) )
				return 1;
			mod8Map = m8Pars.mod8Map;
			}
		}
    /*  else
        { */
          /* Mean was too small to make good enough mod8 map */

    /*      headas_chat(1, "Too few counts in image to remove mod8 noise.\n");
            call warning("LowMod8Noise")
            copy_real_matrix(inOSW, outOSW);
        }  */

      /* deallocate bad pixel map */
      DESTROY_INT_MATRIX(pixMap);
    }
  else
    {
      /* No Modulo-8 noise in image */

      report_status("No Modulo-8 noise in image.\n");
      /* call warning('"noMod8Noise") */
      copy_real_matrix(inOSW, outOSW);
    }


  /* if PPS product required */
  if (par->ppsproduct)
    {
      task->width = mod8Map->xSize;
      task->height = mod8Map->ySize;
      copy_header(task, TARGET_PPSFILE, &status);

      fits_write_img_dbl(task->ppsfptr, 1, 1,
            mod8Map->xSize * mod8Map->ySize, mod8Map->data, &status);
      if (status)
        {
          report_error("unable to write PPS mod8Map [%d]\n", status);
          return status;
        }
    }
  DESTROY_REAL_MATRIX(mod8Map);
  /* write OSW data */
    {
      long size = outOSW->xSize * outOSW->ySize;
      copy_header(task, TARGET_PRODUCT, &status);
      fits_write_img_dbl(task->outfptr, 1, 1, size, outOSW->data, &status);

      if (window.wcs)
        if (save_raw_window(task->outfptr, &window))
          {
            report_error("unable to write UVOT window keywords\n");
            return status;
          }
    }

  DESTROY_REAL_MATRIX(inOSW);
  DESTROY_REAL_MATRIX(outOSW);

#if WRITE_TIMING_INFO
timeIncrement ("Leaving process_extension");
#endif

  return status;

}  /* process_extension (...) */

/**************************************************************************/

int
uvotmodmap_work (Parameters * par)
{
  Task task = { 0 };
  FileSpec		inSpec = 	{0},
				qualitySpec = 	{0};

  int status = 0;
  int numberOfHDU;              /* number of HDU's in infile */
  int hduType;

  task.par = par;

  /* Open the input image file */
  if (!status)
    {
      fits_open_file(&task.infptr, par->infile, READONLY, &status);
      if (status)
        report_error("unable to open input file %s [%d]\n",
                par->infile, status);
      else
        report_status("Opened the input file:\n %s\n", par->infile);
    }
  if (!status)
    {
      status = getFileHDU (par->infile, &inSpec, task.infptr);
      par->imageExtension = inSpec.hdu1;
    }

  if (!status)
    fits_get_num_hdus(task.infptr, &numberOfHDU, &status);

  if (!status)
    if (strcasecmp(par->qfile, "NONE"))
      {
        /* Open the quality file */
        fits_open_file(&task.qfptr, par->qfile, READONLY, &status);
        if (status)
          report_error("Unable to open the quality file [%d]\n", status);
        else
          report_status("Opened the quality file:\n %s\n", par->qfile);
      }
    else
      report_status("Not using any quality data\n");

  /* Create the output file */
  if (!status)
    {
      /* clobber existing output file */
      if (!status && par->clobber)
        remove(par->outfile);

      fits_create_file(&task.outfptr, par->outfile, &status);
      if (status)
        report_error("Unable to create output file '%s' [%d]\n",
            par->outfile, status);
      else
        report_status("Created the output file '%s'\n", par->outfile);
    }

  /* move to the primary HDU */
  if (!status)
    {
      fits_movabs_hdu(task.infptr, 1, &hduType, &status);
      if (status)
        report_error("Unable to move to primary HDU [%d]\n", status);
    }

  /* copy over primary header */
  if (!status)
    {
      file_create_primary(task.infptr, task.outfptr, &inSpec);
      if (status)
        report_error("Unable to copy primary header to output\n", status);
    }

  /* if PPS product required */
  if (!status && par->ppsproduct)
    {
      /* Create the PPS Modulo-8 Map file. */

      /* clobber existing ppsfile? */
      if (par->clobber)
         remove(par->ppsfile);

      fits_create_file(&task.ppsfptr, par->ppsfile, &status);
      if (status)
        report_error("Unable to create the PPS product [%d]\n", status);
      else
        report_status("Created the PPS file '%s'\n", par->ppsfile);
    }

  if (!status && par->ppsproduct)
    {
      file_create_primary(task.infptr, task.ppsfptr, &inSpec);
      if (status)
        report_error("Unable to copy primary header to PPS file\n", status);
    }

  if (!status && task.qfptr)
    {
      status = getFileHDU(par->qfile, &qualitySpec, task.qfptr);
      /* If no quality extension is given, default to the input extension */
      if (!qualitySpec.hdu1)
        qualitySpec.hdu1 = inSpec.hdu1;
      par->qualityExtension = qualitySpec.hdu1;
    }

  if (!status && task.qfptr)
    {
      fits_movabs_hdu(task.qfptr, 1, &hduType, &status);
      if (status)
        report_error("Unable to move to first HDU of quality file [%d]\n",
                   status);
    }

  if (!status)
    {
      /*  at this point, we are at the primary header in all files */

      int extension;

      task.processingImages = 1;

      if (par->imageExtension)
        {
          status = process_extension (&task, par->imageExtension, 
                    par->qualityExtension);
        }
      else
        {
          for (extension = 2; !status && extension <= numberOfHDU; ++extension)
            {
              report_status("extension %d\n", extension);
              status = process_extension(&task, extension, extension);
            }
        }
    }

  if (!status && par->history)
    {
      HDpar_stamp(task.outfptr, 1, &status);

      if (task.ppsfptr)
        HDpar_stamp(task.ppsfptr, 1, &status);

      if (status)
        report_error("unable to write parameter history [%d]\n", status);
    }

  if (task.outfptr)
    {
      int tmp = 0;
      if (status)
        fits_delete_file(task.outfptr, &tmp);
      else
        fits_close_file(task.outfptr, &tmp);
      if (tmp)
        report_error("unable to close output [%d]\n", tmp);
    }

  if (task.infptr)
    {
      int tmp = 0;
      fits_close_file(task.infptr, &tmp);
    }

  if (task.ppsfptr)
    {
      int tmp = 0;
      if (status)
        fits_delete_file(task.ppsfptr, &tmp);
      else
        fits_close_file(task.ppsfptr, &tmp);
      if (tmp)
        report_error("unable to close PPS output [%d]\n", tmp);
    }

  if (task.qfptr)
    {
      int tmp = 0;
      fits_close_file(task.qfptr, &tmp);
    }

  return (status);
}

