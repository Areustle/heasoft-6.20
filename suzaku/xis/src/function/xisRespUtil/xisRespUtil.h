/*************************************************
      xisRespUtil.h

   Version 0.0   2005.11.30    H. Nakajima

   Version 1.1   2006.10.16    Y. ISHISAKI
   	xisrsp_seek_nearest() -> xisrsp_seek_valid_row()
	add int rebin to several functions

****************************************************/

#define Si_Kedge 1.839		/* Si-K edge  [keV] */
#define Si_Ka    1.7398		/* Si-Ka line [keV] */
#define O_Kedge  0.5431		/* O-K edge   [keV] */
#define PI       3.1415926535897932385
#define SQRT_2PI 2.5066282746310002

#define PHA_CHAN	4096
#define BIN_ENE		3.65E-3 /* PI channel step    [keV/chan] */
#define DEFAULT_LOWEST	0.2	/* Lowest RMF energy  [keV] */
#define DEFAULT_HIGHEST	16.0	/* Highest RMF energy [keV] */

/* Structure of output RMF file */
#define RESP_EXTENSION_NUM	3
#define PRIMARY_HDU_ID		0
#define MATRIX_EXTENSION_ID	1
#define EBOUND_EXTENSION_ID	2
#define MATRIX_EXTENSION_NAME	"MATRIX"
#define EBOUND_EXTENSION_NAME	"EBOUNDS"
#define MATRIX_EXTENSION_COLNUM	6
#define EBOUND_EXTENSION_COLNUM	3

#define EFF_CCD_EXTENSION_ID	1
#define EFF_OBF_EXTENSION_ID	2
#define EDG_CCD_EXTENSION_ID	3
#define EDG_OBF_EXTENSION_ID	4

#define RMF_NORM_EXTENSION_ID	1
#define RMF_PSUM_EXTENSION_ID	2

#define SPTH_EXTENSION_ID	4
#define GAIN_NORM_EXTENSION_ID	6
#define GAIN_PSUM_EXTENSION_ID	7

#define RESP_BASE		0
#define RESP_PREC		"E"
#define EBND_PREC		"E"

#define XIStotalPosNo		4
                                                /*****************************/
                                                /* energy - channel relation */
                                                /*****************************/
typedef struct energy_bin {
  long   size;
  double *energ_lo;
  double *energ_hi;
} energyBin;
                                                         /********************/
                                                         /* efficiency table */
                                                         /********************/
typedef struct eff_table {
  long    size;
  double *energy;
  double *value;
  long    edgenum;
  double *edge;
} effTable;
                                                       /**********************/
                                                       /* response parameter */
                                                       /**********************/
typedef struct resp_param {
  long size;
  double *value;
  int bi_si_edge_mode;
  int fi_si_edge_mode;
} respParam;
                                                          /*******************/
                                                          /* response matrix */
                                                          /*******************/
typedef struct resp_slice {
  double* array;
  int startIndex;   /* base = 0 */
  int size;
} respSlice;

typedef struct xis_response {
  int size;
  double* response;
  int peakIndex;
  int eventTh;
  double* edge;
  int edgenum;
} xisResponse;
                                                 /****************************/
                                                 /* gain and spth parameters */
                                                 /****************************/
typedef struct gain_param {
  long size;
  double *offl;
  double *linrl;
  double *quadl;
  double *offh;
  double *linrh;
  double *quadh;
} gainParam;

typedef struct spth_param {
  long size;
  double *offset;
  double *slope;
  double *minimum;
} spthParam;

char *xisrsp_pname(void);
char *xisrsp_version(void);

char* xisrspGetRespExtensionName(int extnum);
int xisrspPutMatrixHeader(fitsfile *fp, char *fn, energyBin *ebin, int rebin);
int xisrspPutEboundHeader(fitsfile *fp, char *fn, energyBin *ebin, int rebin);
int xisrspWriteOGIPHeader(fitsfile *fp, char *fn, int rebin);

energyBin* xisrsp_ebin_init();
int xisrsp_ebin_init_file(energyBin* eb, char *filename);
int xisrsp_ebin_init_const(energyBin* eb, double lo, double hi, double w);
double xisrsp_ebin_lo(energyBin* eb, int iebin);
double xisrsp_ebin_hi(energyBin* eb, int iebin);
void xisrsp_ebin_term(energyBin* eb);
void xisrsp_ebin_free(energyBin* eb);

effTable* xisrsp_efficiency_init();
double xisrsp_efficiency_value(effTable* t, double energy);
int xisrsp_efficiency_edgeNum(effTable* t);
const double* xisrsp_efficiency_edgeArray(effTable* t);
void xisrsp_efficiency_term(effTable* t);
void xisrsp_efficiency_free(effTable* t);

respParam* xisrsp_respparam_init(int bi_si_edge_mode, int fi_si_edge_mode);
void xisrsp_respparam_free(respParam* t);
void xisrsp_respparam_term(respParam* t);

spthParam* xisrsp_spthParam_init();
gainParam* xisrsp_gainParam_init();
void xisrsp_spthParam_free();
void xisrsp_gainParam_free();

xisResponse* xisrsp_init();
void xisrsp_term(xisResponse* r);
respSlice xisrsp_resp(xisResponse* r, double E, double factor, int sensor, respParam* p, double spth);
int xisrsp_edgeNum(xisResponse* r);

int xisrsp_read_format_version(fitsfile *fp, char *extname, int *format_version);
long xisrsp_seek_valid_row(fitsfile *fp, double obstime);
void xisrsp_calc_weighted_qe(effTable* qe, effTable* weightqe, double* weight);
void xisrsp_calc_weighted_param(respParam* param, respParam* weightparam, double* weight);
void xisrsp_calc_weighted_spthp(spthParam* spthp, spthParam* weightspthp, double* weight);
void xisrsp_calc_weighted_gainp(gainParam* gainp, gainParam* weightgainp, double* weight);

void xirsp_bnkdef_matrix(void);
void xirsp_bnkdef_ebound(void);
void xisrsp_bnkput_matrix(double energ_lo, double energ_hi, respSlice *sp);
void xisrsp_bnkput_ebound(int channel, double Emin, double Emax);
int xisrsp_fitswrite_matrix(fitsfile *ofp, long irow, int rebin);
int xisrsp_fitswrite_ebound(fitsfile *ofp, long irow, int rebin);
int xisrsp_create_matrix_ext(fitsfile *ofp, long nrow, int rebin);
int xisrsp_create_ebound_ext(fitsfile *ofp, long nrow, int rebin);

char *xisrsp_get_caldb_file(char *instrume, char *codename, char *o_filename);
