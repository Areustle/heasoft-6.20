#ifndef GTIS_INCLUDED

typedef struct {

int n;
double* start;
double* stop;

int current; /* special value -1 indicates gtifile=NONE */

} GTIS;

/******************************************************************************
*
******************************************************************************/
GTIS* allocateGTIS(int n);

/******************************************************************************
* read a FITS file[ext]
******************************************************************************/
GTIS* readFITSGTIS(char* filename);

void restrictGTIs( GTIS * gtis, double start, double stop);

#define GTIS_INCLUDED
#endif /* GTIS_INCLUDED */
