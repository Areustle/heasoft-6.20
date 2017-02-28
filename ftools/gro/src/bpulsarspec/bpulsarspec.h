/* bpulsarspec.h */
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "cfortran.h"
#include "fitsio.h"
#include "ftools.h"
#include "xpi.h"
#include <ctype.h>
#include <math.h>

   void pulsplot_(long *nptf, double *xf, double *yf, double *zf, long *nPhas, 
        double *yp, double *zp, int *phsta, int *phstp, int *decnum,
        int *chani, int *chanj, int *chanm, int *chann,
	double *tjdsta, double *tjdstp, double *fluxphs, double *fluxphsErr,
        int *chansta, int *chanstp, int *nphs);
   void getFname(char *ss, char *data_dir);
   void getTjd(double *tjdsta, double *tjdstp, double *tjdFitsta, 
        double *tjdFitstp);
   void getPhase(int *phsta, int *phstp); 
   void getFitsData(char *fn, long *nrows, double **startTime, 
        double **stopTime, int ***Cts, double ***CtsErr);
   void writefits(char *infname, char *bpulsfn, char *filename, 
        long *snrows, double *sumCts, 
        double *sumCtsErr, double *texpos, int *decnum);
   void getpulsfn(char *infname, char *data_dir, char *bpulsfn); 
   void saveHtjd(double **x, double **y, double **z, long *npt, 
        char *data_dir, char *infile, double *texpos, int *binsz,
        double *tjdsta, double *tjdstp);
   void savePhs(int **x, double **y, double **z, long *npt, 
        char *data_dir, char *infile, double *texpos, int *binsz,
        double *tjdsta, double *tjdstp);
   void getSelectData(long *nrows, double *tjdsta, double *tjdstp, 
        double **startTime, double **stopTime, int *phsta, int *phstp,
        int ***binCts, double **stime, double ***sbinCts, long *snrows,
        double ***binCtsErr, double ***sbinCtsErr, double **sdt);
   void getHardn(long *npt, double ***sbinCts, double **hardn, 
        double ***sbinCtsErr, double **hardnErr, int *chani,
        int *chanj, int *chanm, int *chann);
   void getSelectPhas(long *nrows, double *tjdsta, double *tjdstp, 
        double **startTime, double **stopTime, int *phsta, int *phstp,
        int ***binCts, double ***sbinCtPhas,long *nPhas,
        double ***binCtsErr, double ***sbinCtsErrPhas);
   void getHardnPhas(long *npt, double ***sbinCtPhas, double **hardnPhas, 
        double ***sbinCtsErr, double **hardnErrPhas, 
        int *chani, int *chanj, int *chanm, int *chann);
   void getSumData(long *snrows, double ***sbinCts, double ***sbinCtsErr, 
        double **sumCts, double **sumCtsErr, double **sdt, double **sumRate,
        double **sumRateErr);
   void getChan(int *chani, int *chanj, int *chanm, int *chann,int *chansta,
              int *chanstp);     
   void writeRates(char *infname, char *bpulsfn, char *filename, 
        long *snrows, double *sumRates, double *ratesErr,int *decnum,
        double *texpos);
   void getBinData(long *nrows, double **startTime, double **stopTime, 
           int ***Cts, double ***CtsErr, long *bnrows, 
           double **bstartTime, double **bstopTime, int ***binCts,
           double ***binCtsErr, int *binsz);
   void getExpos(long *nrows, double *tjdsta, double *tjdstp,
        double **startTime, double **stopTime, double *texpos);
   void getFlux(double ***binPhas, double ***binPhasErr,
		double **fluxphs, double **fluxphsErr, int *chansta, 
                int *chanstp, int *nphs);
   void getPhasData(long *nrows, double *tjdsta, double *tjdstp, 
       double **startTime, double **stopTime, int *phsta, int *phstp,
       int ***binCts, double ***binPhas,double ***binCtsErr, 
       double ***binPhasErr);
