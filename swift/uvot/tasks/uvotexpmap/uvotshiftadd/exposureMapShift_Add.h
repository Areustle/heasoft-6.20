
typedef struct
	{int		duration,
				trackingX,
				trackingY;
	} TRACKING_POSITION;

typedef struct {
	/* indices of lower left element */
	int			xMinIndex, yMinIndex;
	/* indices of upper right element */
	int			xMaxIndex, yMaxIndex;
	/*	The size members are here for convenience */
	int			xSize, ySize;
	int			*data;
	} SUBMATRIX;

typedef struct {
	int			detectorXSize, detectorYSize;
	int null;
	/* definition of the telemetered image */
	SUBMATRIX	*quality;
	/*	The telemetered member is defined as a SUBMATRIX as a convenience.
		The data member is not referenced by the shift and add procedure.
	*/
	SUBMATRIX	*telemetered;
	/*	the exposure map allocates the exposure SUBMATRIX and stores the 
		results there
	*/
	SUBMATRIX	*exposure;
	} EXPOSURE_ARGS;

#define IN_MATRIX(x,y,m) (((m->xMinIndex<=x)&&(x<=m->xMaxIndex)\
		&&(m->yMinIndex<=y)&&(y<=m->yMaxIndex))? 1: 0)
#define MATRIX_OFFSET(i,j, m) ((j-m->yMinIndex)*m->xSize + i - m->xMinIndex)

int exposureMapSA_Init (EXPOSURE_ARGS *arguments);
int exposureMapSA_Track (TRACKING_POSITION *tp);
int exposureMapSA_DestroyMap (EXPOSURE_ARGS *ar);
EXPOSURE_ARGS *exposureMapSA_EndTrack (void);

