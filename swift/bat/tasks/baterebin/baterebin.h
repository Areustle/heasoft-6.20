#include "bat_gswdev.h"
#define NMAX_ROWS 512
#define NMAX_EBINS 300

struct det_qual_struct {
  /* flag for good gain values for a particular detector */
  int flag[DAP_CELLS];                
};

#define LOW_FLT_GAIN 8000      /* Lowest value of flight gain allowed */
#define HIGH_FLT_GAIN 32000     /* Highest value of flight gain allowed */
#define LOW_FLT_OFFSET 12000      /* Lowest value of flight offset allowed */
#define HIGH_FLT_OFFSET 30000     /* Highest value of flight offset allowed */
#define LOW_PUL_GAIN 5.0     /* Lowest value of pulser gain allowed */
#define HIGH_PUL_GAIN 25.0     /* Highest value of pulser gain allowed */
#define LOW_PUL_OFFSET -80.0     /* Lowest value of pulser offset allowed (not restrictive currently) */
#define HIGH_PUL_OFFSET 30.0     /* Highest value of pulser offset allowed (not restrictive currently) */
#define LOW_RESID_GAIN2 0.0001     /* Lowest value of residual quadratic term allowed (not restrictive currently) */
#define HIGH_RESID_GAIN2 0.0018     /* Highest value of residual quadratic term allowed */
#define LOW_RESID_GAIN1 -12.0     /* Lowest value of pulser gain allowed */
#define HIGH_RESID_GAIN1 2.0     /* Highest value of pulser gain allowed */
#define LOW_RESID_OFFSET 1000.0     /* Lowest value of residual offset term allowed (not restrictive currently) */
#define HIGH_RESID_OFFSET 9500.0     /* Highest value of residual offset term allowed */

#define LOW_RESID_CUBIC -0.000002     /* Lowest value of residual cubic term allowed  */
#define HIGH_RESID_CUBIC 0.000002    /* Highest value of residual cubic term allowed (restricts ~ 3) */
#define LOW_RESID_GAIN2_CU -0.002     /* Lowest value of residual quadratic term allowed (restricts ~ 3) */
#define HIGH_RESID_GAIN2_CU 0.008     /* Highest value of residual quadratic term allowed */
#define LOW_RESID_GAIN1_CU -15.0     /* Lowest value of pulser gain allowed */
#define HIGH_RESID_GAIN1_CU -0.2     /* Highest value of pulser gain allowed (restricts ~ 35) */
#define LOW_RESID_OFFSET_CU 1000.0     /* Lowest value of residual offset term allowed (restricts ~ 5) */
#define HIGH_RESID_OFFSET_CU 13000.0     /* Highest value of residual offset term allowed (restricts ~ 1) */

#define BIGGEST_CORR 50.0     /* Largest correction (in keV) allowed at low end */

/* Flag values for gain_offset structure */
#define NO_QUAD_FIT 3        /* Residuals are zeros */
#define GAINOFF_OUTOFRNG 2   /* Some gain or offset out of above ranges */
#define TOO_BIG_CORR 1       /* A couple detectors, 36734 and 46257, have a comparatively large correction at the low end */
#define GOOD_GAINS 0          /* All the gains and offsets are okey-dokey */
#define GAP 4
#define NO_GAIN_REC 5
#define MASKED_OUT 7
#define N_GAIN_STATES 100

/* Biggest item other than the DPH column itself in the DPH extension */
#define LEN_STR 15      /* LPDname is 240A15 */
#define MAX_REP 16      /* LPDname is 240A15 */

#define QUAD_METH 2     /* Flag for using quadratic correction */
#define CUBIC_METH 3    /* Flag for using cubic correction */
/*  for passing parameters to the main routine     */
struct parm_struct 
{
	char *taskname;
	char *taskver;
	char infile[PIL_LINESIZE];    /* A DPH file with units of counts; flight data */
	char outfile[PIL_PATH_MAX];   /* A DPH-like file with units of counts/keV */
	char calfile[PIL_PATH_MAX];   /* Pseudo-linear ADU to pulser DAC conversion in flight data */
	char residfile[PIL_PATH_MAX]; /* Residuals between pseudo-linear and quadratic ADU to DAC relation;
				         a ground file */
	char pulserfile[PIL_PATH_MAX];  /* Best values known for pulser DAC to energy conversion */ 
	char fltpulserfile[PIL_PATH_MAX]; /* Values for pulser DAC to energy conversion used onboard */
	char detmask[PIL_PATH_MAX];    /* Mask to apply to input image */
	char outmap[PIL_PATH_MAX];    /* Map of imperfect conversions */
	double lowecare;       /* Lowest energy where it matters if integral bin overlaps (for outmap flag) */
	double highecare;      /* Highest energy where it matters if integral bin overlaps (for outmap flag) */
	int residext, pulserext, fltpulserext;
	int goodval;                  /* Value of "good" for detmask (from keyword in detmask) */
	char outefile[PIL_PATH_MAX];  /* Desired output binning */
	int wholecount;        /* whole or fractional binning */ 
        char rowstr[PIL_LINESIZE];
        char outdatatype[PIL_LINESIZE]; /* Should be SHORT, INT, FLOAT or DOUBLE */
	char ebins[PIL_LINESIZE];
};

/*************** Global variables recording characteristics of input ***********************/
int nrowsin = 0;
int nrowsout = 0;
long int timestartin = 999999999;
long int timestartout = 999999999;
long int timestopin = 0;
long int timestopout = 0;
long int totalexp = 0;
double least_E = 0.0;
double greatest_E = 6553.6;

/*************** function prototypes ***********************/

int prepfile(fitsfile *infile, fitsfile *outfile, long int nrows, long int nchannels,
	       	float * outemin, float * outemax, struct parm_struct * parms, int calmode);
int baterebin(void);
int calc_true_e(
		        float *cmine, /* Nominal values of the low edge of the input energy bins */
			float *out_min_e, /* Values of the low edge of the output energy bins */
			float *out_max_e, /* Values of the max edge of the output energy bins */
			long int nchannels, /* Number of input energy bins */
			long int out_nchannels, /* Number of output energy bins */
			bat_ecal_data *caldata, 
			struct det_qual_struct *detqual
);
int baterebin_work(
		        float *cmine, /* Nominal values of the low edge of the input energy bins */
			float *out_min_e, /* Values of the low edge of the output energy bins */
			float *out_max_e, /* Values of the max edge of the output energy bins */
			float *spectra, /* Numbers in the input energy bins */
			float *respectra, /* Numbers in the output energy bins */
			long int nchannels, /* Number of input energy bins */
			long int out_nchannels, /* Number of output energy bins */
			int  whole, /* Move only whole counts? */
			bat_ecal_data *caldata, 
			struct det_qual_struct *detqual,
			struct image_struct *outmap
);
int baterebin_getpar(struct parm_struct *parms, char *taskname, char *version); 
int baterebin_printsummary (struct parm_struct *parms, char *taskname, char *version);
int cal_value_check(bat_ecal_data *caldata,
		    struct det_qual_struct *detqual,
	       	 float *highestlowcorrebin, 
		 float *lowesthighcorrebin,
	       	 float lowebinout, 
		 float highebinout,
		 struct parm_struct *parms,
		 int *nlow,
		 int *nhigh,
		 int *lowdet,
		 int *highdet,
		 struct image_struct *detmask,
		 struct image_struct *outmap);
#if 0
int parse_frange(char *crange, float *fmin, float *fmax);
int parse_franges(char *crange, float *fmin, float *fmax, 
		  int nranges, float defmin, float defmax);
int read_ebounds(fitsfile *ebinfile, long int *nbins, int nmaxbins,
		 float *fmin, float *fmax, int *status);
int read_ebins(char *ebinlist, float *fmin, float *fmax, int nmaxbins);
#endif

/***********************************************************/
