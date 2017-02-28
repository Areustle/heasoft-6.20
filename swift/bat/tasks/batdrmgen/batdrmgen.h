/* -----------------batdrmgen.h - header file----------------------- */
/* ----------------------------------------------------------------- */


/* Define constants */

#define MTFUNC_PARMS 3
#define N_MT   36
#define DET_THICKNESS  0.2
#define N_COEFFS 20
#define N_PEAKS 3
#define N_DEPTHS 1000
#define CD_EDGE 26.72
#define TE_EDGE 31.82
#define EK1_CD 23.172
#define EK1_TE 27.471
#define SUB_BINS 10


/* Define structures */
struct parm_struct  /* tool parameters */
{
  char *taskname;
  char *taskver;
  char infile[PIL_PATH_MAX];     /* Input file string */
  char outfile[PIL_PATH_MAX];    /* Output file name */
  char hkfile[PIL_PATH_MAX];     /* DAP housekeeping file name */
  char calfile[PIL_PATH_MAX];    /* Calibration file name: MTFUNC_PARMS */
  char mtvalfile[PIL_PATH_MAX];  /* Calibration file name: MT_VALUES */
  char depthfile[PIL_PATH_MAX];  /* depth distribution file name */
  char efile[PIL_PATH_MAX];      /* User incident energy file name (or NONE)*/
  char detmask[PIL_PATH_MAX];    /* Detector quality mask file name */
  char coltype[PIL_PATH_MAX];    /* Column name with energy (default = "PI") */
  char escale[PIL_PATH_MAX];     /* File or description of energy scale */ 
  char method[PIL_LINESIZE];     /* Method to use for computing response
                                    (default = INDEF) */
  char fudge[PIL_LINESIZE];      /* fudge factor to apply to matrix 
                                    (default = 1) */
  int flight_data;               /* Does the PHA file contain flight data?
				    (1=yes 0=no)
                                    (default: 1) */
  int Nphoton_bins;              /* Number of incident photon energy bins */
  double Elimit_lo,Elimit_hi ;   /* Energy bin edge range*/
  double hv_def;                 /* Bias voltage default (if no hk file) */
  double vthr_def;               /* XA1 threshold default (if no hk file) */
  int row;                       /* row number of the spectrum of interest */
  int use_mean;                  /* 1: use mean values; 0: use table values */
 
};

struct multi_mt_struct { /* spectral model parameters */
  float mutau_e[N_MT];
  float mutau_h[N_MT];
  float fraction[N_MT];
  float voltage;
  float sigma;
  float gain_coeff;
  float gain_index;
  float exp_lambda;
  float exp_coeff; 
  float exp_index; 
  float norm; 
  int batfilev;
  int use_gain_adj;      /* 1: apply gain correction using "gain_adj" values
                            0: apply gain correction using gain_coeff 
                               and gain_index */
  float *gain_adj;       /* values used to determine gain correction */
  int use_sigma;         /* 1: calculate sigma using "sigma_coeffs" values
                            0: use sigma determined from table or 
			       SIGMA keyword */
  float *sigma_coeffs;   /* values used to calculate sigma */

  /* parameters used to calculate the transmission of photons through the 
   * source packaging, air, passive materials, and edges of the lead tiles
   */

  float src_density;
  float src_low_coeff;
  float src_low_index;
  float src_high_coeff;
  float src_high_index;
  float src_smooth;
  float src_thickness;

  float air_density;
  float air_low_coeff;
  float air_low_index;
  float air_high_coeff;
  float air_high_index;
  float air_smooth;
  float *psv;
  int   cfunncof;
  float cfunemin;
  float cfunemax;
  float *cfunp;

  float pb_density;
  float pb_low_coeff;
  float pb_low_index;
  float pb_mid_coeff;
  float pb_mid_index;
  float pb_high_coeff;
  float pb_high_index;
  float pb_smooth;
  float pb_edge_norm;  /* Lead tile edge effect normalization */

  /* housekeeping parameters */

  float hv[128];
  float vthr[256];
  int san_ngood[256];
}; 


typedef struct {  
    float gamma;
    float voltage;
    float sigma;
    float gain_coeff;
    float gain_index;
    float exp_lambda;
    float exp_coeff;
    float exp_index;
    float norm; 
 } Parm_record;


struct resp_struct /* Response matrix data */
{
  double mskwtsqf;           /* = sum[f*(2f-1+C)] */
  double mjdref;             /* Reference date for observation time (MJD) */
  double time, time_stop;    /* Observation start and stop times (MET) */
  double timezero;           /* Offset time (MET seconds) */
  double srcpos[3];          /* Source position (BAT_X/Y/Z) */
  char gainmeth[FLEN_CARD];  /* Type of gain correction applied */
  int gainapp;               /* Was gain correction applied?
				(1=yes, 0=no) */
/*int pcodeapp;  */          /* Was partial coding correction applied?
				(1=yes, 0=no) */
/*double pcodefr;  */        /* partial coding fraction
				(a number between 0 and 1) */ 
/*int ffapp;   */            /* Was flat-fielding correction applied?
				(1=yes, 0=no) */
  int nfapp;                 /* Was Tueller/Hullinger nearfielding applied?
				(1=yes, 0=no) */
  int ngpixapp;              /* Was normalized by number of good pixels?
				(1=yes, 0=no) */
  double ngoodpix;           /* Number of good pixels */
  int fluxmeth;              /* Flux extraction method for PHA file
				0: unknown;
				1: raw;
				2: weighted;
				3: fitted (eventually)
			     */

  struct caldbparms_struct caldb; /* CALDB keyword parameters */

  /* Pre-Gaussian Convolution pulse height binning description */
  int nphabins_pre;            /* Number of pre-convolution pulse height bins */
  double *emin_pre, *emax_pre; /* Lower and upper bounds of pre-convolution */
			       /* pulse height bins, expressed in keV units */

  /* Pulse height binning description */
  int nphabins;         /* Number of pulse height bins */
  double *emin, *emax;  /* Lower and upper bounds of pulse height bins,
			   expressed in fiducial keV units */

  /* Incident photon binning description */
  int nebins;                   /* Number of incident photon energy bins */
  double *energ_lo, *energ_hi;  /* Energy bin edge range*/

  /* Response matrix data */
  double *matrix;    /* Pointer to a redistribution matrix and effective area
                        array of the form 
                              matrix[nebins][nphabins]
                        where the PHA bins are the most rapidly varying index.
                        Units of cm^2.  The content of one array cell is
                        the probability that a photon with energy E will
                        register with pulse-height P, multiplied by the 
                        effective area at energy E. */
			
};

/* ----------------------------------------------------------------- */
/* Function prototypes */

extern int prepfile(fitsfile *infile, fitsfile *outfile, 
		    int ncols, int nrows, 
		    char *colnames[], char *colforms[], char *colunits[], 
		    char *comments[],
		    char *extname, int append);

extern int writeresp(fitsfile *infile, fitsfile *outfile, 
		     struct parm_struct *parms,
		     struct resp_struct *resp);

extern int writeebounds(fitsfile *infile, fitsfile *outfile, 
			struct parm_struct *parms, 
			struct resp_struct *resp);

extern int writeallresp(struct parm_struct *parms,
			struct resp_struct *resp);

extern int readphainfo(struct parm_struct *parms,
	      	       struct resp_struct *resp);

extern int read_col_or_keyword(fitsfile *fptr, int datatype, char *colname,
                               int row, void *nulval, void *value, 
			       int *anynul, int *status);

extern int update_respfile(char *pha_name, char *resp_name, int row);

extern int internal_escale(struct parm_struct *parms, 
			   struct resp_struct *resp);

extern int calc_response(struct parm_struct *parms, 
			 struct resp_struct *resp);

extern float hecht (float lambda_e, float lambda_h, float depth);

extern int exp_tail (
    struct resp_struct *resp, struct multi_mt_struct *cal_parms,
    float photon_energy, float dist_eff_area, double *result);

extern float trans (
    struct multi_mt_struct *p, int flight_data, float energy, 
    double *srcpos);

extern int mutau_model (
    float mutaue, float mutauh, float voltage, float gain_adjust, int n_depths,
    int n_bins, float energy, float norm, double *emax, float *dist, 
    double *result);

extern double erf_fast (float x);

extern int gauss_convolve (
    struct resp_struct *resp, float sigma,
    double *input_spec, double *output_spec);

extern int multi_mutau_func (
    struct resp_struct *resp, struct multi_mt_struct *cal_parms,
    int n_Evectors, int n_depths, float *dist, float energy,
    int flight_data, double *result);

extern int expand_dist(int n_coeffs, int n_vectors, int n_x,
    float *coeffs, float *x, float *dist);

extern int get_correction(int n_coeffs, int n_x, float low_x, float high_x,
    float *coeffs, float *x, float *corr);

extern int apply_correction (struct resp_struct *resp,
    struct multi_mt_struct *cal_parms);

extern int get_distinterp(float energy, float tanx, float tany, 
    fitsfile *fits_file, float *eninterp, float *xinterp, float *yinterp,
    float *dist_interp);

extern int lookup_params(float energy, float tanx, float tany, 
    fitsfile *fits_file, float **param_vector, int *n_elements, 
			 int *n_vectors);

extern int get_mutau(char *fits_name, long extno, 
    struct multi_mt_struct *cal_parms);

extern int get_cal_parms(char *fits_name, int use_mean, long extno, 
    struct multi_mt_struct *cal_parms,struct resp_struct *resp); 

extern int record_compare(void const * e1, void const * e2);

extern int interp_params(float *eninterp, float *xinterp, float *yinterp,
    float *parinterp, int n_elements, int n_vectors, 
			 float *param_vector); 

extern int batdrmgen_getpar(struct parm_struct *parms);

extern int *read_detmask(char *filename, long axes[2], int goodval, 
		         int *status);

extern void banner(struct parm_struct *parms);

extern void summary(struct parm_struct *parms);

extern int batdrmgen_work(struct parm_struct *parms);

extern int get_hk (char *hk_name, struct parm_struct *parms, 
    struct multi_mt_struct *cal_parms, int *dm_enable, double time);

extern int sum_detmask (int *detmask, int *san_ngood);

extern int sum_dm_enable (int *dm_enable, int *san_ngood);

extern int search_array (double *array, long array_size, double value);

/* ----------------------------------------------------------------- */
