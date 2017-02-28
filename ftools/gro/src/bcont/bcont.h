/* bcont.h */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"
#include <ctype.h>
#include <math.h>
#include "general.h"

   void getBcontFn(char *listfn[], int *nfile, char *data_dir, int *nchan
        , long *ncol);
   void getChan(int *chansta, int *chanstp, int *nchan); 
   void getDet(int *detsta, int *detstp);   
   void getTjd(double *tjdsta, double *tjdstp, double *tjdFitsta, 
        double *tjdFitstp);
   void getFitsData(char **listfn,long *nrows, int *nfile, double **midTime, 
        long ***cts, double ***ctsErr, long *ncol, float **timeRes);
   void getBinData(long *nrows, double **midTime, long ***Cts, 
        double ***CtsErr, long *binrows, double **binTime, double ***binCts,
	double ***binCtsErr, int *binsz,long *ncol, float **timeRes,
        float **binTimeRes);
   void getSelectData(long *binrows, double *tjdsta, double *tjdstp,
        double **binTime, double **sbinTime, int *chansta, int *chanstp,
        double ***binCts, double ***sbinCts, long *snrows, double ***binCtsErr,
        double ***sbinCtsErr,int *nchan, long *ncol, float **binTimeRes,
        float **sTimeRes);
   void getSumData(long *snrows, double **sbinTime, double ***sbinCts,
        double ***sbinCtsErr,double **sumCts, double **sumCtsErr,
        int *ndet, int **det);
   void getRates(long *snrows, double **sumCts, double **sumCtsErr,
	double **Rate, double **RateErr, float **sTimeRes);
   void getwtFlg(char *wtFlg, char *rcFlg);
   void getFname(char *ss, char *data_dir);
   void writeAsiic(long *npt, double **x, double **y, double **z, 
        char *data_dir, int **det, int *ndet, float **timeRes, int *binsz,
		   char *flg, char *outFn);
   void writeFits(long *snrows, double *sbinTime, double *sumCts, 
        double *sumCtsErr, char *data_dir, int *det, int *ndet, 
        float *timeRes, char *rcFlg, char **infname, int *nfile,
	char *filename, int *chansta, int *chanstp, int *binsz);
