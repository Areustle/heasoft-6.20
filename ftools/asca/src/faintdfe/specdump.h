#include "fitsio.h"
#include "longnam.h"

typedef struct {

fitsfile* fp;

int time_col;
int ccd_col;
int npixels_col;
int dfe_col;
int spectrum_col;

long nbins;
long nrows;

} SPECDUMP;


/****************************************************************************
*****************************************************************************
* create a new specdump file
****************************************************************************/
SPECDUMP* openSpecDump(char* filename, int min_pha, int nbins);

/************************************************************************
*************************************************************************
* add a single row to the specdump file
************************************************************************/
void addSpecDumpRow(SPECDUMP* specdump, 
               double time, int ccd, int npixels, double dfe, 
               double* spectrum);

/****************************************************************************
*****************************************************************************
* close a specdump file and destroy the corresponding structure
****************************************************************************/
void closeSpecDump(SPECDUMP* specdump);
