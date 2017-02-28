/* ----------------------------------------------------------------- */
/* Constants used in this program */ 

/* File data types */
#define LC  1      /* Light curve */
#define PHA 2      /* Pulse height spectra - Type II */
#define DPI 3      /* Detector image */
#define DPITAB 4   /* Table of detector images */
#define DPH 5      /* Table of detector histograms - multi-slice */
#define EVENTS 10  /* INPUT: EVENTS */

#define NGAUSSCELLP 3  /* Number of cell parameters for Gaussian stats */

#define FLOAT_INDEF (1e307)  /* Floating point "undefined" value */

/* ----------------------------------------------------------------- */
/* bbevts.c */

extern int evt2cells(double *t, int ntimes, 
		     int **cellsizes, int **cellpops, int *ncells, 
		     double tstart, double tstop, double timedel, int nspill);


extern int *evtbayes(int *cellsizes, int *cellpops, int ncells, 
		     double ncp_prior, int *ncparray, 
		     double **bestlogprob, int **lastcellstart, int nlag);

extern int rebinevts(double *t, int ntimes, 
		     int **cellsizes, int **cellpops,
		     int *cps, int ncps, double timedel);

extern double *evtcumsum(int ntimes);


/* bblc.c */

extern int lc2cells(double *t, double *counts, double *dt, int ntimes, 
		    int **cellsizes, int **cellpops, int *ncells, 
		    double tstart, double tstop, double timedel);

extern int *lcbayes(int *cellsizes, int *cellpops, int ncells, 
		    double ncp_prior, int *ncparray, 
		    double **bestlogprob, int **lastcellstart, int nlag);

extern int rebinlc(double *dt, double *counts,
		   int **cellsizes, int **cellpops,
		   int *cps, int ncps, double timedel);

double *lccumsum(double *counts, int ntimes);


/* bblcgauss.c */
extern int lcgauss2cells(double *t, double *rate, double *rerror, int ntimes, 
			 double **cellparams, int *ncells, double *data_range, 
			 double tstart, double tstop, double timedel);

extern int *lcgaussbayes(double *cellparams, int ncells, double data_range, 
			 double ncp_prior, int *ncparray, 
			 double **bestlogprob, int **lastcellstart, int nlag);

extern int rebinlcgauss(double *counts, double *error,
			double **cellparams, double *data_range, 
			int *cps, int ncps, double timedel);

double *lcgausscumsum(double *dt, double *rate, double *error, 
		      int ntimes, char *hduclas3);


/* burstdur.c */

#ifdef GTI_AND
extern int burstdur(double *t, double *dt, double *cumsum, int nrows,
		    double *txxlist, int ntxx, double tpeak,
		    struct gti_struct *txxgti,
		    struct gti_struct *peak1sgti,
		    double *txx_uncertainty, double rms, int durerrmeth);

extern int burstspan(double *t, double *dt, int nrows,
		     double tstart, double tstop,
		     long int *istart0, long int *istop0);

#endif
