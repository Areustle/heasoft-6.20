#ifndef _HXD_RSP_UTIL_H_
#define _HXD_RSP_UTIL_H_
/*
 * hxdrspUtil.c
 *      create arf/rmf fits file
 *         version 0.0.1 2003-10-15,  created by Y.Terada
 *         version 0.0.2 2003-10-18, modefied by Y.Terada
 *                 for NeXT SGD dummy response
 *         version 0.0.3 2003-11-23, modefied by Y.Terada
 *                 for NeXT SGD dummy response, (E-reso)
 *         version 0.0.4 2004-08-06, by Y.Terada
 *                 for HXD team, included into astroe_dir (hxd home)
 *         version 0.1.0 2005-04-12, by Y.Terada
 *                 included into hxdrspUtil
 *         version 0.1.1 2005-05-10, by Y.Terada
 *                 change header file
 *         version 0.1.2 2005-06-06, by Y.Terada
 *         version 0.1.3 2005-06-13, by Y.Terada
 *         version 0.1.4 2005-06-22, by Y.Terada
 *         version 0.2.0 2005-11-05, by Y.Terada
 */

#define HXDRSPUTIL_OK 1
#define HXDRSPUTIL_NG 0

/*
 * --- Arf ---
 */
#define MKDMYARF_MAX_NAXIS 512
#define ARF_FITS_PRIMARY_HDU  1
#define ARF_FITS_BINTABLE_HDU 2


typedef struct {
  /*** header keywords ***/
  char telescop[128];
  char instrume[128];
  char detnam[128];
} HxdArfInfo;

typedef struct {
  /*** data body ***/
  int   row_num[MKDMYARF_MAX_NAXIS];
  double energy_low[MKDMYARF_MAX_NAXIS];
  double energy_high[MKDMYARF_MAX_NAXIS];
  double specresp[MKDMYARF_MAX_NAXIS];
  /*** valid number ***/
  long irow;
} HxdArfData;

int hxdrspUtil_arf_init( HxdArfInfo* com, HxdArfData *data, 
			 char *telescop, char *instrume, char *detnam);
int hxdrspUtil_arf_read_ascii ( char* asciiname, HxdArfData* data);
int hxdrspUtil_arf_dump_data  ( HxdArfData* data);
int hxdrspUtil_arf_check_data ( HxdArfData* data);
int hxdrspUtil_arf_create_fits( char* arfname, HxdArfInfo* com);
int hxdrspUtil_arf_write_fits ( HxdArfData* data );
int hxdrspUtil_arf_fits_add_comment( char *comments);
int hxdrspUtil_arf_close_fits ( void );

/*
 * --- Rmf ---
 */
#define MKDMYRMF_MAX_NAXIS   1024
#define MKDMYRMF_MAX_CHANNEL 1024
#define RMF_FITS_PRIMARY_HDU      1
#define RMF_FITS_MATRIX_EXTENSION 2
#define RMF_FITS_EBOUND_EXTENSION 3

typedef struct {
  /*** header keywords ***/
  char telescop[128];
  char instrume[128];
  char detnam[128];
} HxdRmfInfo;

typedef struct {
  int   row_num[MKDMYRMF_MAX_NAXIS];
  float energy_low[MKDMYRMF_MAX_NAXIS];
  float energy_high[MKDMYRMF_MAX_NAXIS];
  float pi[MKDMYRMF_MAX_CHANNEL][MKDMYRMF_MAX_NAXIS];
  /*** valid number ***/
  int detchans;  /** channel number **/
  long irow;
} HxdRmfData;

typedef struct {
  int   row_num[MKDMYRMF_MAX_CHANNEL];
  float energy_min[MKDMYRMF_MAX_CHANNEL];
  float energy_max[MKDMYRMF_MAX_CHANNEL];
  int detchans;  /** channel number **/
  int logscale;  /** channel scale  0:Lin 1:Log **/
  float energy_range_min;
  float energy_range_max;
} HxdRmfPIDef;

int hxdrspUtil_rmf_init( HxdRmfInfo* com, HxdRmfData* data_rsp, 
			 HxdRmfPIDef* data_ch, 
			 char *telescop, char *instrume, char *detnam,
			 int detchans,  int logscale, 
			 float energy_range_min, float energy_range_max);

int hxdrspUtil_rmf_define_pi  ( HxdRmfPIDef* data_ch);
int hxdrspUtil_rmf_mkdmy_data ( HxdRmfData*  data_rsp, HxdRmfPIDef *data_ch);
int hxdrspUtil_rmf_read_ascii ( char* asciiname, HxdRmfData* data_rsp);
int hxdrspUtil_rmf_dump_data  ( HxdRmfData* data_rsp);
int hxdrspUtil_rmf_check_data ( HxdRmfData* data_rsp, HxdRmfPIDef *data_ch);

int hxdrspUtil_rmf_create_fits( char* rmfname, HxdRmfInfo* com,
				HxdRmfData* data_rsp, HxdRmfPIDef *data_ch);
int hxdrspUtil_rmf_write_fits ( HxdRmfData* data_rsp,  HxdRmfPIDef *data_ch);
int hxdrspUtil_rmf_close_fits ( void );

/******************** EOF ********************/
#endif
