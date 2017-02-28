/* bodfluxhis.h */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"

#define BIT16 0x00010000
#define BIT18 0x00040000


   void bodfluxhis_plot_(int *nptf, float *xf, float *yf, float *zf, 
        int *npt, float *x, float *y, float *z, float *tjdsta1, 
	float *tjdstp1, float *tjdsta2, float *tjdstp2, float *tjdb,
        float *tjde, int *nvp);
   void writeLog(char *fitsFname, char *outfil_dir);
   void getFname(float *tjdsta, float *tjdstp, char *ss, char *data_dir);
   int  getTjd(float *tjdsta, float *tjdstp, float *tjdFitsta, 
        float *tjdFitstp, float *tjdsta1, float *tjdstp1, float *tjdsta2, 
        float *tjdstp2, float **tjdb, float **tjde, int *nvp, char *data_dir);
   void getnrows(long *nrows, float *startTime, float *stopTime,long *noutrows,
        float *tjdsta,float *tjdstp, long *firstrow, long *lastrow);
   void getBindata(int *binsz,long *noutrows,float **bstartTime,float **bFlux,
        float **bFluxErr, float *startTime,float *Flux,float *FluxErr);
   void getSelectData(long *noutrows, long *firstrow, long *lastrow, 
        float **startTime, float **Flux,float **FluxErr,float **FluxFits, 
        float **FluxErrFits, float **startTimeFits);
   void checkBinsz(int *nbin, long *noutrows, int *binsz);
   
   void getfitsdata( char *bodfil, long *nrows, float **startTime, 
        float **stopTime, float **FluxFits, float **FluxErrFits, 
        float **startTimeFits);
   void getFitsData(char **listfn, long *nrows, int *nfile, float **startTime, 
        float **stopTime, float **FluxFits, float **FluxErrFits, 
        float **startTimeFits);
   void writefits(int *nrows, float *StartTime, float *Flux, 
        float *FluxErr, char *filename, int *binsz);
   void getlist(char *listfn[], int *nfile, char *data_dir); 
