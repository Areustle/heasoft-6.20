/*
 SimASTE_XISarfgen.c
   SimASTE module : Photon Generator & ARF builder for xissimarfgen

  2006-04-09 version 1.0	Y.ISHISAKI

  2006-04-24 version 1.1	Y.ISHISAKI
  	rename parameter: source_image_file -> source_image
  	rename parameter: energy_step_file -> estepfile
  	add parameters: pointing, ref_alpha, ref_delta, ref_roll

  2006-05-28 version 1.2	Y.ISHISAKI
	add parameters: detmask, gtifile, date_obs, attitude, contamifile
	accept region_mode=DETREG,SKYREG,DETEXPR,SKYEXPR
	accept limit_mode=MIXED
	write MASK_RATIO_CCD, MASK_RATIO_REG keywords, if 'detmask' is specified

  2006-06-13 version 1.3	Y.ISHISAKI
	check if 0.0 == detect in calculating avgwei in write_arf_val()

  2006-07-24 version 2.0	Y.ISHISAKI
	support for CALDB
	BnkGet SimASTE:TELESCOP:PTR, SimASTE:INSTRUME:PTR for CALDB

  2006-08-02 version 2.1	Y.ISHISAKI
	bug fix in setting com.pointing (com.source_mode had been set) in _com()
	use aste_gti_xxx()
	read ea1, ea2, ea3 if attitude=none, which is moved from SimASTE_Root
	read RA_NOM, DEC_NOM from attitude for skyref when pointint=auto
	add aberration parameter
	convert ACT -> DET in make_regmap(), read_detmask(), read_source_image()

  2006-08-02 version 2.2	Y.ISHISAKI
	fix printf format for RA (%6.3f -> %06.3f), DEC (%5.2f -> %05.2f)
	add write_history(), BnkGet/Put SimASTE:WRITE_HISTORY:FUNC in _init()
	add write_random_number_info()
	ignore error in writing history
	write LEAPFILE keyword
	use aste_att_ea() in astetool-1.80 instead of aste_att_euler()
	use acos() in generating random photon for SOURCE_MODE_UNIFORM
	add aperture_cosine parameter, EVS SimASTE:APERTURE_COSINE
	BnkPut SimASTE:N_PHOTON, SimASTE:N_DETECT, SimASTE:N_WEISUM
	unit of RESPRERR changed "percent" -> ""

  2006-08-10 version 2.3	Y.ISHISAKI
	remove unused energy bins in decide_energy_step()
	add/rename ARF column names, XRT_EFFAREA, SHIELD_TRANSMIS, TOTAL->INPUT
	treat shield & contamination transmissions separately in calc_cw_sums()
	BnkGet SimASTE:XRT:SHIELD_TRANSMIS:FUNC in _ini()
	BnkGet SimASTE:XRT:SHIELD_TRANSMIS in _ana()
	fill -1.0 to wmap for out-of-bounds pixels in make_regmap()
	write SOURCE_RATIO_REG keyword in create_arf_file()
	count up com.sum_source_reg in SimASTE_XISarfPhotonGen_ana()
	support "full", "dense", "medium", "sparse" in read_estepfile()
	recognize emin/max=-1.0 as energ_lo/hi, in read_estepfile()
	add 0.5*ebin to calculate n in case of digit-loss in decide_energy_step()
	automatically add emin_min, emax_max if needed, in decide_energy_step()
	ignore energy difference less than 1e-6 keV, in decide_energy_step()

  2006-10-17 version 2.4	Y.ISHISAKI
	add NULL argument for xis_contami() version 1.2

  2006-11-25 version 2.5	Y.ISHISAKI
	bug fix in reading RA_NOM, DEC_NOM from attitude file in read_attitude()
	print error status in cfitsio error
	fill -1 to wmap for detmask when region_mode=DETxx in make_regmap()
	write WCS keyword to handle fk5 region file in make_ds9reg_image()
	write WCS keyword to WMAP header in create_wmap()

  2007-05-28 version 2.6	Y.ISHISAKI
	add parameters: 'enable_pixq','hotpixfiles','badcolumfile','calmaskfile'
					'pixq_min,'pixq_max','pixq_and','pixq_eql'
	call xisPixqStatWriteFits() in write_history()
	call xisSciWriteKeys() in create_arf_file()
	add eval_pixq(), called in _init()
	accept rmffile='CALDB'

  2007-07-16 version 2.7	Y.ISHISAKI
	not to use ANL_ENDLOOP in _ana() for aste_anl-1.80 bug (fixed in 1.81)

  2007-09-22 version 2.8	Y.ISHISAKI
	bug fix in read_source_image() for source_mode=DETFITS
	remove unused CALDB_INFO caldb in _init()

  2008-03-08 version 2.9	Y.ISHISAKI
	add hduclas1, hduclas2, wmrebin, x_offset, y_offset to FITS_IMAGE_KEY
	WMAP image is usable for source_image when source_mode=DETFITS
	WMAP image is usable for regfileN when region_mode=DETFITS or SKYFITS
	fix history when rmffile='CALDB'
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <unistd.h>
#include "fitsio.h"
#include "anl.h"
#include "bnk.h"
#include "evs.h"
#include "cli.h"
#include "pil.h"
#include "atFunctions.h"
#include "aste_coord.h"
#include "aste_rand.h"
#include "aste_time.h"
#include "aste_att.h"
#include "aste_gti.h"
#include "aste_caldb.h"
#include "aeFitsHeaderUtil.h"
#include "xis_contami.h"
#include "xisTelemFormat.h"
#include "xisSciUtil.h"
#include "xisPixelQuality.h"
#include "SimASTE.h"

#define FAST_CALC_CW_SUM

static char pname[] = "xissimarfgen";
static char pname1[] = "SimASTE_XISarfPhotonGen";
static char pname2[] = "SimASTE_XISarfBuild";
char SimASTE_XISarfPhotonGen_version[] = "version 2.9";
char SimASTE_XISarfBuild_version[] = "version 2.9";

#define unless(a) if(!(a))

typedef struct {
	char telescop[FLEN_KEYWORD];
	char instrume[FLEN_KEYWORD];
	double equinox;
	char radecsys[FLEN_KEYWORD];
	char hduclas1[FLEN_KEYWORD], hduclas2[FLEN_KEYWORD];
	char mtype1[FLEN_KEYWORD], mform1[FLEN_KEYWORD];
	char ctype1[FLEN_KEYWORD], ctype2[FLEN_KEYWORD];
	double crpix1, crpix2;
	double cdelt1, cdelt2;
	double crval1, crval2, crota2;		/* SKYREF */
	int wmrebin, x_offset, y_offset;	/* WMAP keywords */
} FITS_IMAGE_KEY;

typedef struct {
	double t0, t1;
	AtEulerAng e0, e1;
	AtEulerAng *ea;
} ATT_DATA;

typedef struct {
	double photon_time;
	double weight;
	int ipos;
	short idetx, idety;
} PHOTON_MEMORY;

enum skyref_mode {
	SKYREF_MODE_AUTO,
	SKYREF_MODE_USER
};

enum source_mode {
	SOURCE_MODE_SKYFITS,
	SOURCE_MODE_DETFITS,
	SOURCE_MODE_J2000,
	SOURCE_MODE_SKYXY,
	SOURCE_MODE_DETXY,
	SOURCE_MODE_UNIFORM
};

enum region_mode {
	REGION_MODE_SKYFITS,
	REGION_MODE_DETFITS,
	REGION_MODE_SKYCIRC,
	REGION_MODE_DETCIRC,
	REGION_MODE_SKYREG,
	REGION_MODE_DETREG,
	REGION_MODE_SKYEXPR,
	REGION_MODE_DETEXPR
};

enum limit_mode {
	LIMIT_MODE_MIXED,
	LIMIT_MODE_NUM_PHOTON,
	LIMIT_MODE_ACCURACY
};

#define MAX_POINT			100
#define MAX_REGION			200
#define MAX_ENERGY			10000
#define MAX_TIME_STEP		1000000
#define MAX_PHOTON_MEMORY	1000000

static struct estep_tables {
	struct {
		double mi, ma, bi;
	} e[12];
}  estep_full = { {
	{ -1.0, -1.0, 0.0 },
	{ -999, -999, -999 } }
}, estep_dense = { {
	{	-1.000,	-1.000,	0.010 },
	{	 2.101,	 2.399,	0.002 },
	{	 2.501,	 2.799,	0.002 },
	{	 3.101,	 3.299,	0.002 },
	{	11.801,	11.999,	0.002 },
	{	13.601,	13.799,	0.002 },
	{	14.201,	14.499,	0.002 },
	{ -999, -999, -999 } }
}, estep_medium = { {
	{	-1.000,	 4.000,	0.100 },
	{	 4.000,	 7.000,	0.200 },
	{	 7.000,	11.000,	0.500 },
	{	11.000,	-1.000,	1.000 },
	{	 0.210,	 0.300,	0.010 },
	{	 2.100,	 2.400,	0.010 },
	{	 2.500,	 2.800,	0.010 },
	{	 3.100,	 3.300,	0.010 },
	{	11.800,	12.000,	0.100 },
	{	13.600,	13.800,	0.100 },
	{	14.200,	14.500,	0.100 },
	{ -999, -999, -999 } }
}, estep_sparse = { {
	{	-1.000,	 1.000,	0.200 },
	{	 1.000,	 7.000,	0.500 },
	{	 7.000,	 8.000,	1.000 },
	{	 8.000,	-1.000,	2.000 },
	{	 0.250,	 0.300,	0.050 },
	{	 2.100,	 2.400,	0.050 },
	{	 2.200,	 2.250,	0.010 },
	{	 2.300,	 2.350,	0.010 },
	{	 2.600,	 2.800,	0.020 },
	{	 3.100,	 3.300,	0.100 },
	{	11.800,	12.000,	0.100 },
	{ -999, -999, -999 } }
};

static PIXQ_INFO pixq;
static PIXQ_STAT statistics;

static struct {

	char *telescop;
	char *instrume;
	int clobber;		/* overwrite arf files or not */
	TELDEF *teldef;
	char *leapfile;

	enum skyref_mode pointing;	/* auto/user */
	SKYREF skyref;

	int aperture_cosine;
	double minRadius, maxRadius;
	double minAngle, maxAngle;
	double r2min, r2max, geomarea;

	enum source_mode source_mode;
	char source_image[PIL_LINESIZE];
	FITS_IMAGE_KEY source_kp;
	struct Hrndm2 *hm2;
	double source_x, source_y;
	double source_ra, source_dec;
	double source_rmin, source_rmax;
	double cos_source_rmin, cos_source_mami;
	AtRotMat sky_rm;

	int num_region;
	enum region_mode region_mode;
	double region_x[MAX_REGION], region_y[MAX_REGION];
	double region_rmin[MAX_REGION], region_rmax[MAX_REGION];
	char regfile[MAX_REGION][PIL_LINESIZE];
	char arffile[MAX_REGION][PIL_LINESIZE];
	fitsfile *arffp[MAX_REGION];
	int region_nx, region_ny;
	float *regmap[MAX_REGION];
	double *wmap[MAX_REGION];
	FITS_IMAGE_KEY region_kp[MAX_REGION];
	double sum_source_reg[MAX_REGION];
	double sum_detmask_reg[MAX_REGION];
	double tot_ccd_pix_reg[MAX_REGION];

	char detmask[PIL_LINESIZE];
	FITS_IMAGE_KEY detmask_kp;
	int ndetx, ndety;
	float *detmaskmap;
	double sum_detmask;
	double tot_ccd_pix;

	enum limit_mode limit_mode;
	int num_photon;
	double accuracy;

	char phafile[PIL_LINESIZE];

	char gtifile[PIL_LINESIZE];
	GTI_DATA gti;
	int gti_nt;
	double *gti_t;
	char date_obs[PIL_LINESIZE];
	double photon_time;

	char attitude[PIL_LINESIZE];
	ATT_DATA att;
	AtEulerAng ea_deg, ea;
	double ra_nom, dec_nom;	/* read from attitude & used when pointing=auto */
	int aberration;
	int mjdrefi;
	double mjdreff;

	char *rmffile, o_rmffile[PIL_LINESIZE];
	int num_energ;
	double *energ_lo;			/* ENERG_LO in RMF/ARF */
	double *energ_hi;			/* ENERG_HI in RMF/ARF */
	double *energ_cen;			/* (ENERG_HI+ENERG_HI)/2.0 */

	char *contamifile, o_contamifile[PIL_LINESIZE];
	XIS_CONTAMI *xcp;

	char estepfile[PIL_LINESIZE];
	int num_ebin;
	double emin[MAX_ENERGY];	/* min. energy of user specification */
	double emax[MAX_ENERGY];	/* max. energy of user specification */
	double ebin[MAX_ENERGY];	/* energy step of user specification */

	int num_calc;
	struct calc_energy {
		double Ecen;			/* energies of simulated photons */
		double Ewid;			/* width of simulating energy bin */
	} *calc;

	int calc_index;
	struct calc_values_per_energy {
		double input;			/* number of input photons being generated */
		int iphoton;
		PHOTON_MEMORY *pmem;	/* com.pmem_strage[0] or com.pmem_storage[1] */
		struct calc_values_per_region {
			double detect;		/* detected number of photons */
			double relerr;		/* relative error in result */
			double weisum;		/* sum of weight without contami */
#ifndef FAST_CALC_CW_SUM
			double cw_sum;		/* sum of weight with contami */
#endif
		} *reg;
	} *ene;

	double n_photon, n_detect, n_weisum;

	int pmem_size;				/* com.num_photon or MAX_PHOTON_MEMORY */
	PHOTON_MEMORY *pmem_storage[2];

#ifdef FAST_CALC_CW_SUM
	double *cw_enes;				/* used in calc_cw_sums() */
	double *cw_transmis;			/* used in calc_cw_sums() */
	double **cw_sums1, **cw_sums2;	/* used in calc_cw_sums() */
#endif

	int (*prev_write_history)(fitsfile *);
	double (*shield_transmis_func)(double);

	int enable_pixq;
	char hotpixfiles[PIL_LINESIZE];
	char *badcolumfile, o_badcolumfile[PIL_LINESIZE];
	char *calmaskfile, o_calmaskfile[PIL_LINESIZE];
	unsigned int pixq_min;
	unsigned int pixq_max;
	unsigned int pixq_and;
	unsigned int pixq_eql;

} com;

static double
dummy_shield_transmis_func(double energy)
{
	return 1.0;
}

static int
write_history(fitsfile *fp)
{
	int i, istat;
	char history[PIL_LINESIZE + FLEN_VALUE];
	AtRightAscension ra;
	AtDeclination dec;
	HOTPIXFILE_INFO *hp;
	PIXQ_INFO *p = &pixq;

	if ( NULL != com.prev_write_history &&
		 write_history != com.prev_write_history ) {
		istat = (*com.prev_write_history)(fp);
		if ( istat ) {
			return istat;
		}
	}

	istat = SimASTE_write_history_pname(fp, pname1);
	if ( 0 == istat ) {
		istat = SimASTE_write_history_pname(fp, pname2);
	}

	sprintf(history, "\
  pointing=%s", (SKYREF_MODE_AUTO == com.pointing) ? "AUTO" : "USER");
	fits_write_history(fp, history, &istat);

	atDegToRA(com.skyref.alpha, &ra);
	sprintf(history, "\
  ref_alpha=%.4f (%02dh%02dm%06.3fs)", com.skyref.alpha,
		ra.hour, ra.min, ra.sec);
	fits_write_history(fp, history, &istat);

	atDegToDec(com.skyref.delta, &dec);
	sprintf(history, "\
  ref_delta=%.4f (%s%02dd%02dm%05.2fs)", com.skyref.delta,
		(dec.sign < 0) ? "-" : "+", dec.deg, dec.min, dec.sec);
	fits_write_history(fp, history, &istat);

	sprintf(history, "\
  ref_roll=%.4f", com.skyref.roll);
	fits_write_history(fp, history, &istat);

	if ( com.aberration ) {
		sprintf(history, "\
  aberration=yes  mjdrefi=%d  mjdreff=%.17f", com.mjdrefi, com.mjdreff);
	} else {
		sprintf(history, "\
  aberration=no");
	}
	fits_write_history(fp, history, &istat);

	sprintf(history, "\
  aperture_cosine=%s", com.aperture_cosine ? "yes" : "no");
	fits_write_history(fp, history, &istat);

	sprintf(history, "\
  minangle=%.1f  maxangle=%.1f  minradius=%.5f  maxradius=%.5f",
		com.minRadius, com.maxRadius, com.minAngle, com.maxAngle);
	fits_write_history(fp, history, &istat);

	switch ( com.source_mode ) {
	case SOURCE_MODE_SKYFITS:
		sprintf(history, "\
  source_mode=SKYFITS");
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  source_image='%s'", com.source_image);
		fits_write_history(fp, history, &istat);
		break;
	case SOURCE_MODE_DETFITS:
		sprintf(history, "\
  source_mode=DETFITS");
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  source_image='%s'", com.source_image);
		fits_write_history(fp, history, &istat);
		break;
	case SOURCE_MODE_J2000:
		sprintf(history, "\
  source_mode=J2000  source_ra=%.6f  source_dec=%.6f",
			com.source_ra, com.source_dec);
		fits_write_history(fp, history, &istat);
		break;
	case SOURCE_MODE_SKYXY:
		sprintf(history, "\
  source_mode=SKYXY  source_x=%.1f  source_y=%.1f",
			com.source_x, com.source_y);
		fits_write_history(fp, history, &istat);
		break;
	case SOURCE_MODE_DETXY:
		sprintf(history, "\
  source_mode=DETXY  source_x=%.1f  source_y=%.1f",
			com.source_x, com.source_y);
		fits_write_history(fp, history, &istat);
		break;
	case SOURCE_MODE_UNIFORM:
		sprintf(history, "\
  source_mode=UNIFORM  source_rmin=%.3f  source_rmax=%.3f",
			com.source_rmin, com.source_rmax);
		fits_write_history(fp, history, &istat);
		break;
	default:
		;
	}

	switch ( com.region_mode ) {
	case REGION_MODE_SKYFITS:
		sprintf(history, "\
  region_mode=SKYFITS  num_region=%d", com.num_region);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < com.num_region; i++) {
			sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
			fits_write_history(fp, history, &istat);
		}
		break;
	case REGION_MODE_DETFITS:
		sprintf(history, "\
  region_mode=DETFITS  num_region=%d", com.num_region);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < com.num_region; i++) {
			sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
			fits_write_history(fp, history, &istat);
		}
		break;
	case REGION_MODE_SKYCIRC:
		sprintf(history, "\
  region_mode=SKYCIRC  num_region=%d", com.num_region);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < com.num_region; i++) {
			sprintf(history, "\
  %d: ( %.1f , %.1f )  %.1f - %.1f", i+1,
				com.region_x[i], com.region_y[i],
				com.region_rmin[i], com.region_rmax[i]);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
			fits_write_history(fp, history, &istat);
		}
		break;
	case REGION_MODE_DETCIRC:
		sprintf(history, "\
  region_mode=DETCIRC  num_region=%d", com.num_region);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < com.num_region; i++) {
			sprintf(history, "\
  %d: ( %.1f , %.1f )  %.1f - %.1f", i+1,
				com.region_x[i], com.region_y[i],
				com.region_rmin[i], com.region_rmax[i]);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
			fits_write_history(fp, history, &istat);
		}
		break;
	case REGION_MODE_SKYREG:
		sprintf(history, "\
  region_mode=SKYREG  num_region=%d", com.num_region);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < com.num_region; i++) {
			sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
			fits_write_history(fp, history, &istat);
		}
		break;
	case REGION_MODE_DETREG:
		sprintf(history, "\
  region_mode=DETREG  num_region=%d", com.num_region);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < com.num_region; i++) {
			sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
			fits_write_history(fp, history, &istat);
		}
		break;
	case REGION_MODE_SKYEXPR:
		sprintf(history, "\
  region_mode=SKYEXPR  num_region=%d", com.num_region);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < com.num_region; i++) {
			sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
			fits_write_history(fp, history, &istat);
		}
		break;
	case REGION_MODE_DETEXPR:
		sprintf(history, "\
  region_mode=DETEXPR  num_region=%d", com.num_region);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < com.num_region; i++) {
			sprintf(history, "\
  regfile%d='%s'", i+1, com.regfile[i]);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
  arffile%d='%s'", i+1, com.arffile[i]);
			fits_write_history(fp, history, &istat);
		}
		break;
	default:
		;
	}

	switch ( com.limit_mode ) {
	case LIMIT_MODE_MIXED:
		sprintf(history, "\
  limit_mode=MIXED  num_photon=%d  accuracy=%.2e",
			com.num_photon, com.accuracy);
		fits_write_history(fp, history, &istat);
		break;
	case LIMIT_MODE_NUM_PHOTON:
		sprintf(history, "\
  limit_mode=NUM_PHOTON  num_photon=%d", com.num_photon);
		fits_write_history(fp, history, &istat);
		break;
	case LIMIT_MODE_ACCURACY:
		sprintf(history, "\
  limit_mode=ACCURACY  accuracy=%.2e", com.accuracy);
		fits_write_history(fp, history, &istat);
		break;
	default:
		;
	}

	sprintf(history, "\
  phafile='%s'", com.phafile);
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  detmask='%s'", com.detmask);
	fits_write_history(fp, history, &istat);
	if ( 0 != CLstricmp("none", com.detmask) ) {
		sprintf(history, "\
    MASK_RATIO_CCD = %.1f / %.0f = %.6f",
			com.sum_detmask, com.tot_ccd_pix, com.sum_detmask/com.tot_ccd_pix);
		fits_write_history(fp, history, &istat);
	}
	if ( 0 != CLstricmp("none", com.gtifile) ) {
		sprintf(history, "\
  gtifile='%s'", com.gtifile);
		fits_write_history(fp, history, &istat); sprintf(history, "\
    TSTART=%.1f  TSTOP=%.1f  ONTIME=%.1f  NGTI=%d",
			com.gti.tstart, com.gti.tstop, com.gti.ontime, com.gti.ngti);
	} else {
		sprintf(history, "\
  gtifile='%s'  date_obs='%s' (t=%.1f)",
			com.gtifile, com.date_obs, com.photon_time);
	}
	fits_write_history(fp, history, &istat);
	if ( 0 != CLstricmp("none", com.attitude) ) {
		AtEulerAng *e0 = &com.att.e0, *e1 = &com.att.e1;
		sprintf(history, "\
  attitude='%s'", com.attitude);
		fits_write_history(fp, history, &istat); sprintf(history, "\
    EA=( %.4f %.4f %.4f ) at t=%.1f",
			e0->phi*RAD2DEG, e0->theta*RAD2DEG, e0->psi*RAD2DEG, com.att.t0);
		fits_write_history(fp, history, &istat); sprintf(history, "\
    EA=( %.4f %.4f %.4f ) at t=%.1f",
			e1->phi*RAD2DEG, e1->theta*RAD2DEG, e1->psi*RAD2DEG, com.att.t1);
	} else {
		sprintf(history, "\
  attitude='%s'  ea1=%.4f  ea2=%.4f  ea3=%.4f", com.attitude,
			com.ea.phi*RAD2DEG, com.ea.theta*RAD2DEG, com.ea.psi*RAD2DEG);
	}
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  rmffile='%s'%s", com.rmffile,
		(com.rmffile == com.o_rmffile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  contamifile='%s'%s", com.contamifile,
		(com.contamifile == com.o_contamifile) ? "" : " (CALDB)");
	fits_write_history(fp, history, &istat);
	sprintf(history, "\
  estepfile='%s'", com.estepfile);
	fits_write_history(fp, history, &istat);
	for (i = 0; i < com.num_ebin; i++) {
		if ( 0.0 == com.ebin[i] ) {
			sprintf(history, "\
  %3d: %6.3f - %6.3f keV : Randomized within RMF bin",
				i+1, com.emin[i], com.emax[i]);
		} else if ( com.ebin[i] < 0.0 ) {
			sprintf(history, "\
  %3d: %6.3f - %6.3f keV : Randomized in Each %.3f keV",
				i+1, com.emin[i], com.emax[i], - com.ebin[i]);
		} else {
			sprintf(history, "\
  %3d: %6.3f - %6.3f keV : Fixed Energy in Each %.3f keV",
				i+1, com.emin[i], com.emax[i], com.ebin[i]);
		}
		fits_write_history(fp, history, &istat);
	}

	sprintf(history, "\
  enable_pixq=%s  clobber=%s",
		com.enable_pixq ? "yes" : "no", com.clobber ? "yes" : "no");
	fits_write_history(fp, history, &istat);
	if ( com.enable_pixq ) {
		sprintf(history, "\
  hotpixfiles='%s'", com.hotpixfiles);
		fits_write_history(fp, history, &istat);
		for (i = 0; i < p->num_hotpixfile; i++) {
			hp = &p->hotpixfile_list[i];
			sprintf(history, "\
    %d: '%s'", i+1, hp->filename);
			fits_write_history(fp, history, &istat);
			sprintf(history, "\
       num_hotpix=%ld  num_added=%ld  num_dupli=%ld",
				hp->num_hotpix, hp->num_added, hp->num_dupli);
			fits_write_history(fp, history, &istat);
		}
		sprintf(history, "\
  badcolumfile='%s'%s", com.badcolumfile,
			(com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  calmaskfile='%s'%s", com.calmaskfile,
			(com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  pixq_min=%u (0x%08x)  pixq_max=%u (0x%08x)",
			com.pixq_min, com.pixq_min, com.pixq_max, com.pixq_max);
		fits_write_history(fp, history, &istat);
		sprintf(history, "\
  pixq_and=%u (0x%08x)  pixq_eql=%u (0x%08x)",
			com.pixq_and, com.pixq_and, com.pixq_eql, com.pixq_eql);
		fits_write_history(fp, history, &istat);
	}

	if ( istat ) {
		anl_msg_error("\
%s: fits_write_history() failed (%d)\n", pname, istat);
		return istat;
	}

	if ( com.enable_pixq ) {
		istat = xisPixqStatWriteFits(&statistics, fp);
	}

	return istat;
}

static void
show_parameter(char *title)
{
	int i;
	char buf[80];
	AtRightAscension ra;
	AtDeclination dec;

	anl_msg_always("\n");
	anl_msg_always(title, pname);
	anl_msg_always("\n\n");

	switch ( com.pointing ) {
	case SKYREF_MODE_AUTO:
		anl_msg_always("\
%4s%-20s%s\n", "", "POINTING", "AUTO");
		break;
	case SKYREF_MODE_USER:
		anl_msg_always("\
%4s%-20s%s\n", "", "POINTING", "USER");
		break;
	default:
		anl_msg_always("\
%4s%-20s%s\n", "", "POINTING", "UNKNOWN");
	}

	atDegToRA(com.skyref.alpha, &ra);
	sprintf(buf, "%.4f (%02dh%02dm%06.3fs)", com.skyref.alpha,
		ra.hour, ra.min, ra.sec);
	anl_msg_always("\
  %4s%-20s%s\n", "", "REF_ALPHA (J2000)", buf);
	atDegToDec(com.skyref.delta, &dec);
	sprintf(buf, "%.4f (%s%02dd%02dm%05.2fs)", com.skyref.delta,
		(dec.sign < 0) ? "-" : "+", dec.deg, dec.min, dec.sec);
	anl_msg_always("\
  %4s%-20s%s\n", "", "REF_DELTA (J2000)", buf);
	anl_msg_always("\
  %4s%-20s%.4f\n", "", "REF_ROLL (deg)", com.skyref.roll);

	anl_msg_always("\
%4s%-20s%s\n", "", "ABERRATION", com.aberration ? "YES" : "NO");
	anl_msg_always("\
%4s%-20s%s\n", "", "APERTURE_COSINE", com.aperture_cosine ? "YES" : "NO");
	anl_msg_always("\
%4s%-20s%.3f (deg)\n", "", "MINANGLE", com.minAngle);
	anl_msg_always("\
%4s%-20s%.3f (deg)\n", "", "MAXANGLE", com.maxAngle);
	anl_msg_always("\
%4s%-20s%.5f (mm)\n", "", "MINRADIUS", com.minRadius);
	anl_msg_always("\
%4s%-20s%.5f (mm)\n", "", "MAXRADIUS", com.maxRadius);
	anl_msg_always("\
%4s%-20s%.4f (cm2)\n", "", "GEOMAREA", com.geomarea);

	switch ( com.source_mode ) {
	case SOURCE_MODE_SKYFITS:
		anl_msg_always("\
%4s%-20s%s\n", "", "SOURCE_MODE", "SKYFITS");
		anl_msg_always("\
  %4s%-20s%s\n", "", "SOURCE_IMAGE", com.source_image);
		break;
	case SOURCE_MODE_DETFITS:
		anl_msg_always("\
%4s%-20s%s\n", "", "SOURCE_MODE", "DETFITS");
		anl_msg_always("\
  %4s%-20s%s\n", "", "SOURCE_IMAGE", com.source_image);
		break;
	case SOURCE_MODE_J2000:
		anl_msg_always("\
%4s%-20s%s\n", "", "SOURCE_MODE", "J2000");
		anl_msg_always("\
  %4s%-20s( %9.4f , %9.4f ) (deg)\n", "", "SOURCE_RA,DEC",
			com.source_ra, com.source_dec);
		break;
	case SOURCE_MODE_SKYXY:
		anl_msg_always("\
%4s%-20s%s\n", "", "SOURCE_MODE", "SKYXY");
		anl_msg_always("\
  %4s%-20s( %6.1f , %6.1f )\n", "", "SOURCE_X,Y",
			com.source_x, com.source_y);
		break;
	case SOURCE_MODE_DETXY:
		anl_msg_always("\
%4s%-20s%s\n", "", "SOURCE_MODE", "DETXY");
		anl_msg_always("\
  %4s%-20s( %6.1f , %6.1f )\n", "", "SOURCE_DETX,Y",
			com.source_x, com.source_y);
		break;
	case SOURCE_MODE_UNIFORM:
		anl_msg_always("\
%4s%-20s%s\n", "", "SOURCE_MODE", "UNIFORM");
		anl_msg_always("\
  %4s%-20s%9.4f - %9.4f (arcmin)\n", "", "SOURCE_RMIN,RMAX",
			com.source_rmin, com.source_rmax);
		break;
	default:
		anl_msg_always("\
%4s%-20s%s\n", "", "SOURCE_MODE", "UNKNOWN");
	}

	switch ( com.region_mode ) {
	case REGION_MODE_SKYFITS:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "SKYFITS");
		for (i = 0; i < com.num_region; i++) {
			anl_msg_always("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
			anl_msg_always("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
		}
		break;
	case REGION_MODE_DETFITS:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "DETFITS");
		for (i = 0; i < com.num_region; i++) {
			anl_msg_always("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
			anl_msg_always("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
		}
		break;
	case REGION_MODE_SKYCIRC:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "SKYCIRC");
		for (i = 0; i < com.num_region; i++) {
			anl_msg_always("\
%4s%3d%-19s( %6.1f , %6.1f ) %6.1f - %6.1f\n",
				"", i+1, ": X,Y,RMIN,RMAX",
				com.region_x[i], com.region_y[i],
				com.region_rmin[i], com.region_rmax[i]);
			anl_msg_always("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
		}
		break;
	case REGION_MODE_DETCIRC:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "DETCIRC");
		for (i = 0; i < com.num_region; i++) {
			anl_msg_always("\
%4s%3d%-19s( %6.1f , %6.1f ) %6.1f - %6.1f\n",
				"", i+1, ": X,Y,RMIN,RMAX",
				com.region_x[i], com.region_y[i],
				com.region_rmin[i], com.region_rmax[i]);
			anl_msg_always("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
		}
		break;
	case REGION_MODE_SKYREG:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "SKYREG");
		for (i = 0; i < com.num_region; i++) {
			anl_msg_always("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
			anl_msg_always("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
		}
		break;
	case REGION_MODE_DETREG:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "DETREG");
		for (i = 0; i < com.num_region; i++) {
			anl_msg_always("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
			anl_msg_always("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
		}
		break;
	case REGION_MODE_SKYEXPR:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "SKYEXPR");
		for (i = 0; i < com.num_region; i++) {
			anl_msg_always("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
			anl_msg_always("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
		}
		break;
	case REGION_MODE_DETEXPR:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "DETEXPR");
		for (i = 0; i < com.num_region; i++) {
			anl_msg_always("\
%4s%3d: REGFILE %s\n", "", i+1, com.regfile[i]);
			anl_msg_always("\
%4s     ARFFILE %s\n", "", com.arffile[i]);
		}
		break;
	default:
		anl_msg_always("\
%4s%-20s%s\n", "", "REGION_MODE", "UNKNOWN");
	}

	switch ( com.limit_mode ) {
	case LIMIT_MODE_MIXED:
		anl_msg_always("\
%4s%-20s%s\n", "", "LIMIT_MODE", "MIXED");
		anl_msg_always("\
  %4s%-20s%d (photons)\n","","NUM_PHOTON", com.num_photon);
		anl_msg_always("\
  %4s%-20s%.6f\n", "", "  ACCURACY", com.accuracy);
		break;
	case LIMIT_MODE_NUM_PHOTON:
		anl_msg_always("\
%4s%-20s%s\n", "", "LIMIT_MODE", "NUM_PHOTON");
		anl_msg_always("\
  %4s%-20s%d (photons)\n","","NUM_PHOTON", com.num_photon);
		break;
	case LIMIT_MODE_ACCURACY:
		anl_msg_always("\
%4s%-20s%s\n", "", "LIMIT_MODE", "ACCURACY");
		anl_msg_always("\
  %4s%-20s%.6f\n", "", "  ACCURACY", com.accuracy);
		break;
	default:
		anl_msg_always("\
%4s%-20s%s\n", "", "LIMIT_MODE", "UNKNOWN");
	}

	anl_msg_always("\
%4s%-20s%s\n", "", "PHAFILE", com.phafile);
	anl_msg_always("\
%4s%-20s%s\n", "", "DETMASK", com.detmask);
	anl_msg_always("\
%4s%-20s%s\n", "", "GTIFILE", com.gtifile);
	if ( 0 == CLstricmp("none", com.gtifile) ) {
		anl_msg_always("\
  %4s%-20s%s (t=%.1f)\n", "", "DATE_OBS", com.date_obs, com.photon_time);
	}
	anl_msg_always("\
%4s%-20s%s\n", "", "ATTITUDE", com.attitude);
	if ( 0 == CLstricmp("none", com.attitude) ) {
		anl_msg_always("\
  %4s%-20s%.4f %.4f %.4f\n", "", "EA1/2/3",
			com.ea.phi*RAD2DEG, com.ea.theta*RAD2DEG, com.ea.psi*RAD2DEG);
	}
	anl_msg_always("\
%4s%-20s%s%s\n", "", "RMFFILE", com.rmffile,
		(com.rmffile == com.o_rmffile) ? "" : " (CALDB)");
	anl_msg_always("\
%4s%-20s%s%s\n", "", "CONTAMIFILE", com.contamifile,
		(com.contamifile == com.o_contamifile) ? "" : " (CALDB)");
	anl_msg_always("\
%4s%-20s%s\n", "", "ESTEPFILE", com.estepfile);
	for (i = 0; i < com.num_ebin; i++) {
		if ( 0.0 == com.ebin[i] ) {
			anl_msg_always("\
%6s%3d: %6.3f - %6.3f keV : Randomized within RMF bin\n",
				"", i+1, com.emin[i], com.emax[i]);
		} else if ( com.ebin[i] < 0.0 ) {
			anl_msg_always("\
%6s%3d: %6.3f - %6.3f keV : Randomized in Each %.3f keV\n",
				"", i+1, com.emin[i], com.emax[i], - com.ebin[i]);
		} else {
			anl_msg_always("\
%6s%3d: %6.3f - %6.3f keV : Fixed Energy in Each %.3f keV\n",
				"", i+1, com.emin[i], com.emax[i], com.ebin[i]);
		}
	}
	anl_msg_always("\
%4s%-20s%s\n", "", "ENABLE_PIXQ", com.enable_pixq ? "YES" : "NO");
	if ( com.enable_pixq ) {
		anl_msg_always("\
  %4s%-20s%s\n", "", "HOTPIXFILES", com.hotpixfiles);
		anl_msg_always("\
  %4s%-20s%s%s\n", "", "BADCOLUMFILE", com.badcolumfile,
		(com.badcolumfile == com.o_badcolumfile) ? "" : " (CALDB)");
		anl_msg_always("\
  %4s%-20s%s%s\n", "", "CALMASKFILE", com.calmaskfile,
		(com.calmaskfile == com.o_calmaskfile) ? "" : " (CALDB)");
		anl_msg_always("\
  %4s%-20s%u (0x%08x)\n", "", "PIXQ_MIN", com.pixq_min, com.pixq_min);
		anl_msg_always("\
  %4s%-20s%u (0x%08x)\n", "", "PIXQ_MAX", com.pixq_max, com.pixq_max);
		anl_msg_always("\
  %4s%-20s%u (0x%08x)\n", "", "PIXQ_AND", com.pixq_and, com.pixq_and);
		anl_msg_always("\
  %4s%-20s%u (0x%08x)\n", "", "PIXQ_EQL", com.pixq_eql, com.pixq_eql);
	}
	anl_msg_always("\
%4s%-20s%s\n", "", "CLOBBER", com.clobber ? "YES" : "NO");
}

static float *
make_ds9reg_image(char *regfile, char *regexpr, int nx, int ny)
{
	static char exptmpl[] = "regfilter(\"%s\")";
	static char tmpfile[] = "mem://tmp.fits";
	static char extname[] = "DUMP_IMAGE";
	static char *ttype[2] = { "X",  "Y" };
	static char *tform[2] = { "1I", "1I"};
	static char *tunit[2] = { "",   ""  };

	char *k;
	int ix, iy, ipos;
	float *regmap;
	long irow, nrows, n_good_rows;
	double tcdltx, tcdlty;

	fitsfile *fp = NULL;
	char *regstat = NULL;
	int istat = 0, ncol = 2;
	TELDEF_ASTROE *aste = com.teldef->mission.aste;

	nrows = nx * ny;
	regmap = malloc( nrows * sizeof(*regmap) );
	if ( NULL == regmap ) {
		anl_msg_error("\
%s: regmap[%d x %d] malloc() failed\n", pname, nx, ny);
		goto error;
	}

fits_create_file(&fp, tmpfile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: could not open tmpfil; may already exist?\n", pname);
		goto error;
    }

/* construct binary table header */
fits_create_tbl(fp, BINARY_TBL, 0, ncol, ttype, tform, tunit, extname, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_create_tbl(tmpfil) failed (%d)\n", pname, istat);
		goto error;
	}

	if ( nx == aste->sky.xsiz && ny == aste->sky.ysiz ) {
		tcdltx = - (aste->sky.xscl / aste->focallen) * RAD2DEG;
		tcdlty = + (aste->sky.yscl / aste->focallen) * RAD2DEG;
		if (
fits_write_key_str(fp, k="RADECSYS", "FK5",
	"celestial coord system", &istat) ||
fits_write_key_fixdbl(fp, k="EQUINOX", 2000.0, 1,
	"Equinox of celestial coord system", &istat) ||
fits_write_key_fixdbl(fp, k="TCRPX1", aste->sky.xcen, 1,
	"X reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="TCRVL1", com.skyref.alpha, 5,
	"X reference pixel value (deg)", &istat) ||
fits_write_key_fixdbl(fp, k="TCDLT1", tcdltx, 7,
	"X pixel scale (deg/pixel)", &istat) ||
fits_write_key_fixdbl(fp, k="TCROT1", com.skyref.roll, 5,
	"X pixel rotation (deg)", &istat) ||
fits_write_key_str(fp, k="TCTYP1", "RA---TAN",
	"X coordinate projection method", &istat) ||
fits_write_key_fixdbl(fp, k="TCRPX2", aste->sky.ycen, 1,
	"Y reference pixel", &istat) ||
fits_write_key_fixdbl(fp, k="TCRVL2", com.skyref.delta, 5,
	"Y reference pixel value (deg)", &istat) ||
fits_write_key_fixdbl(fp, k="TCDLT2", tcdlty, 7,
	"Y pixel scale (deg/pixel)", &istat) ||
fits_write_key_fixdbl(fp, k="TCROT2", com.skyref.roll, 5,
	"Y pixel rotation (deg)", &istat) ||
fits_write_key_str(fp, k="TCTYP2", "DEC--TAN",
	"Y coordinate projection method", &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_write_key('%s') failed (%d)\n", pname, k, istat);
			goto error;
		}
	}

	irow = 1L;
	for (iy = 1; iy <= ny; iy++) {
		for (ix = 1; ix <= nx; ix++) {
fits_write_col_int(fp, 1, irow, 1, 1, &ix, &istat);
fits_write_col_int(fp, 2, irow, 1, 1, &iy, &istat);
			irow++;
		}
	}
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_col() failed (%d)\n", pname, istat);
		goto error;
	}

fits_flush_file(fp, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_flush_file() failed (%d)\n", pname, istat);
		goto error;
	}

	if ( NULL == regexpr ) {
		regstat = malloc( nrows + strlen(regfile) + sizeof(exptmpl));
		if ( NULL != regstat ) {
			regexpr = &regstat[nrows];
			sprintf(regexpr, exptmpl, regfile);
		}
	} else {
		regstat = malloc( nrows );
	}

	if ( NULL == regstat ) {
		anl_msg_error("\
%s: regstat[%ld] malloc() failed\n", pname, nrows);
		goto error;
	}

fits_find_rows(fp, regexpr, 1, nrows, &n_good_rows, regstat, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_find_rows('%s') failed (%d)\n", pname, regexpr, istat);
		goto error;
	}
	anl_msg_info("\
   regexpr=%s, n_good_rows=%ld\n", regexpr, n_good_rows);

	for (ipos = 0; ipos < nrows; ipos++) {
		regmap[ipos] = regstat[ipos] ? 1.0 : 0.0;
	}

	free(regstat);
fits_close_file(fp, &istat);

	return regmap;

 error:
	if ( NULL != regstat ) {
		free(regstat);
	}

	if ( NULL != fp ) {
		istat = 0;
fits_close_file(fp, &istat);
	}

	if ( NULL != regmap ) {
		free(regmap);
	}

	return NULL;
}

static int
pixel_to_ecs(FITS_IMAGE_KEY *kp, double x, double y, double *ra, double *dec)
{
	double mjd0, mjd1;
	AtPolarVect pv0, pv1;
	AtVect v0, v1;
	AtEulerAng ea;
	AtRotMat rm, rm_inv;
	AtVect vec_ecs;				/* 天球座標系での目標天体の位置ベクトル */
	AtVect vec_sky;				/* SKY 座標系での目標天体の位置ベクトル */
	double alpha, delta, dummy;

	if ( NULL == kp ) {
		return -1;
	}

	/* alpha, delta, roll をオイラー角に変換する */
	ea.phi = kp->crval1 * DEG2RAD;
	ea.theta = M_PI_2 - kp->crval2 * DEG2RAD;
	ea.psi = M_PI_2 + kp->crota2 * DEG2RAD;

	/* オイラー角を回転行列に変換した上でその逆行列を求める */
	atEulerToRM(&ea, rm);
	atInvRotMat(rm, rm_inv);

	/* SKY 座標系での方向ベクトルに変換。*/
	vec_sky[0] = kp->cdelt1 * (x - kp->crpix1) * DEG2RAD;
	vec_sky[1] = kp->cdelt2 * (y - kp->crpix2) * DEG2RAD;
	vec_sky[2] = 1.0;

	/* 衛星の姿勢を使って 天球座標系での方向ベクトルに逆変換 */
	atRotVect(rm_inv, vec_sky, vec_ecs);

	/* 天球座標に変換 */
	atVectToPolDeg(vec_ecs, &dummy, &alpha, &delta);

	if ( 1950.0 == kp->equinox ) {
		atB1950toJ2000(alpha, delta, &alpha, &delta);
	} else if ( 2000.0 != kp->equinox ) {
		mjd0 = 51544.0 - (2000.0 - kp->equinox) / 0.0027379093;
		mjd1 = MJD_J2000;
		pv0.lon = alpha * DEG2RAD;
		pv0.lat = delta * DEG2RAD;
		pv0.r = 1.0;
		atPolToVect(&pv0, v0);
		atPrecession(mjd0, v0, mjd1, v1);
		atVectToPol(v1, &pv1);
		alpha = pv1.lon * RAD2DEG;
		delta = pv1.lat * RAD2DEG;
	}

	*ra = alpha;
	*dec = delta;

	return 0;
}

static int
check_keyword(fitsfile *fp, int flag_sky, int nx, int ny, FITS_IMAGE_KEY *kp)
{
	struct fits_key_list {
		char *key;
		int req, datatype;
		void *ptr;
	};

	static struct {
		struct fits_key_list
			telescop,
			instrume,
			object,
			exposure,
			equinox,
			radecsys,
			hduclas1,
			hduclas2,
			mtype1,
			mform1,
			ctype1,
			ctype2,
			cdelt1,
			cdelt2,
			crpix1,
			crpix2,
			crval1,
			crval2,
			crota2,
			wmrebin,
			x_offset,
			y_offset,
			end;
	} list = {
		{ "TELESCOP",	0, TSTRING, NULL },
		{ "INSTRUME",	0, TSTRING, NULL },
		{ "OBJECT",		0, TSTRING, NULL },
		{ "EXPOSURE",	0, TSTRING, NULL },
		{ "EQUINOX",	1, TDOUBLE, NULL },
		{ "RADECSYS",	1, TSTRING, NULL },
		{ "HDUCLAS1",	0, TSTRING, NULL },
		{ "HDUCLAS2",	0, TSTRING, NULL },
		{ "MTYPE1",		0, TSTRING, NULL },
		{ "MFORM1",		0, TSTRING, NULL },
		{ "CTYPE1",		0, TSTRING, NULL },
		{ "CTYPE2",		0, TSTRING, NULL },
		{ "CDELT1",		1, TDOUBLE, NULL },
		{ "CDELT2",		1, TDOUBLE, NULL },
		{ "CRPIX1",		0, TDOUBLE, NULL },
		{ "CRPIX2",		0, TDOUBLE, NULL },
		{ "CRVAL1",		1, TDOUBLE, NULL },
		{ "CRVAL2",		1, TDOUBLE, NULL },
		{ "CROTA2",		0, TDOUBLE, NULL },
		{ "WMREBIN",	0, TINT, NULL },
		{ "X-OFFSET",	0, TINT, NULL },
		{ "Y-OFFSET",	0, TINT, NULL },
		{ NULL, 0, 0, NULL }
	};

	int i, nfound;
	char card[FLEN_CARD];

	struct fits_key_list *lp = &list.telescop;
	int istat = 0;

	if ( NULL == kp ) {
		return 0;
	}

	list.telescop.ptr = &kp->telescop;
	list.instrume.ptr = &kp->instrume;
	list.equinox.ptr  = &kp->equinox;
	list.radecsys.ptr = kp->radecsys;
	list.hduclas1.ptr = kp->hduclas1;
	list.hduclas2.ptr = kp->hduclas2;
	list.mtype1.ptr   = kp->mtype1;
	list.mform1.ptr   = kp->mform1;
	list.ctype1.ptr   = kp->ctype1;
	list.ctype2.ptr   = kp->ctype2;
	list.crpix1.ptr   = &kp->crpix1;
	list.crpix2.ptr   = &kp->crpix2;
	list.cdelt1.ptr   = &kp->cdelt1;
	list.cdelt2.ptr   = &kp->cdelt2;
	list.crval1.ptr   = &kp->crval1;
	list.crval2.ptr   = &kp->crval2;
	list.crota2.ptr   = &kp->crota2;
	list.wmrebin.ptr  = &kp->wmrebin;
	list.x_offset.ptr = &kp->x_offset;
	list.y_offset.ptr = &kp->y_offset;

/* set initial values */
	kp->telescop[0] = '\0';
	kp->instrume[0] = '\0';
	kp->equinox     = 0.0;
	kp->radecsys[0] = '\0';
	kp->hduclas1[0] = '\0';
	kp->hduclas2[0] = '\0';
	kp->mtype1[0]   = '\0';
	kp->mform1[0]   = '\0';
	if ( flag_sky ) {
		strcpy(kp->ctype1, "RA---TAN");
		strcpy(kp->ctype2, "DEC--TAN");
	} else {
		kp->ctype1[0] = '\0';
		kp->ctype2[0] = '\0';
	}
	kp->crpix1      = (nx + 1) / 2.0;
	kp->crpix2      = (ny + 1) / 2.0;
	kp->cdelt1      = 0.0;
	kp->cdelt2      = 0.0;
	kp->crval1      = 0.0;
	kp->crval2      = 0.0;
	kp->crota2      = 0.0;
	kp->wmrebin     = 1;
	kp->x_offset    = 1;
	kp->y_offset    = 1;

	for (i = nfound = 0; NULL != lp[i].key; i++) {
		istat = 0;
		fits_read_card(fp, lp[i].key, card, &istat);
		if ( 0 == istat ) {
			nfound++;
			anl_msg_info("%s\n", card);
			if ( NULL != lp[i].ptr ) {
				if ( TDOUBLE == lp[i].datatype ) {
fits_read_key_dbl(fp, lp[i].key, lp[i].ptr, NULL, &istat);
				} else if ( TSTRING == lp[i].datatype ) {
fits_read_key_str(fp, lp[i].key, lp[i].ptr, NULL, &istat);
				} else if ( TINT == lp[i].datatype ) {
fits_read_key(fp, TINT, lp[i].key, lp[i].ptr, NULL, &istat);
				}
			}
		}
		if ( istat && flag_sky && 1 == lp[i].req ) {
			anl_msg_error("\
%s: required keyword '%s' not found\n", pname, lp[i].key);
			return -1;
		}
	}

	if ( flag_sky && 0 != strcmp("RA---TAN", kp->ctype1) ) {
		anl_msg_error("\
%s: CTYPE1 = '%s' not supported\n", pname, kp->ctype1);
		return -1;
	}

	if ( flag_sky && 0 != strcmp("DEC--TAN", kp->ctype2) ) {
		anl_msg_error("\
%s: CTYPE2 = '%s' not supported\n", pname, kp->ctype1);
		return -1;
	}

	if ( nfound ) {
		anl_msg_info("\
   %d keywords detected\n", nfound);
	}

	return 0;
}

static float *
read_fits_image(char *fn, int flag_sky, int *nx, int *ny, FITS_IMAGE_KEY *kp)
{
	fitsfile *fp;
	float *image;
	int simple, bitpix, naxis, extd, anynul, istat, nelem;
	long naxes[2], pc, gc;

	istat = 0;
	fits_open_file(&fp, fn, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: FITS image '%s' open failed (%d)\n", pname, fn, istat);
		return NULL;
	}

	fits_read_imghdr(fp,2,&simple,&bitpix,&naxis,naxes,&pc,&gc,&extd,&istat);
	*nx = naxes[0];
	*ny = naxes[1];
	if ( naxis < 2 ) {
		*ny = 1;
	}
	unless ( 0 == istat && naxis && nx && ny ) return NULL;

	nelem = (*nx) * (*ny);
	image = malloc( nelem * sizeof(*image) );
	if ( NULL == image ) {
		anl_msg_error("\
%s: image[%dx%d] malloc() failed\n", pname, *nx, *ny);
		return NULL;
	}

	if ( NULL != kp ) {
		istat = check_keyword(fp, flag_sky, *nx, *ny, kp);
		if ( istat ) {
			free(image);
			return NULL;
		}
	}

	fits_read_img_flt(fp, 1, 1, nelem, 0.0, image, &anynul, &istat);
	fits_close_file(fp, &istat);
	if ( istat ) {
		anl_msg_info("\
%s: FITS image '%s' read error\n", pname, fn);
		free(image);
		return NULL;
	}

	return image;
}

static int
convert_act_to_det(TELDEF *teldef, int nx, int ny, float *act_image)
{
	int ix, iy, iactx, iacty, idetx, idety, iactpos, idetpos;
	float *det_image;
	TELDEF_ASTROE *aste = teldef->mission.aste;

	anl_msg_warning("\
%s: WARNING: image type is ACT, converting to DET\n", pname);

	if ( aste->det.xsiz != aste->act.xsiz ||
		 aste->det.ysiz != aste->act.ysiz ) {
		anl_msg_error("\
%s: invalid teldef file, ACT_X|YSIZ != DET_X|YSIZ\n", pname);
		return -1;
	}

	if ( nx != aste->det.xsiz || ny != aste->det.ysiz ) {
		anl_msg_error("\
%s: invalid image size %dx%d\n", pname, nx, ny);
		return -1;
	}

	det_image = malloc( sizeof(*det_image) * nx * ny );
	if ( NULL == det_image ) {
		anl_msg_error("\
%s: det_image[%dx%d] malloc() failed\n", pname, nx, ny);
		return -1;
	}

	for (iy = 0; iy < ny; iy++) {
		iacty = iy + aste->act.xpix1;
		for (ix = 0; ix < nx; ix++) {
			iactx = ix + aste->act.ypix1;
			xis_act2det(teldef, iactx, iacty, &idetx, &idety);
			iactpos = iy * nx + ix;
			idetpos = (idety - aste->det.ypix1) * nx + idetx - aste->det.xpix1;
			det_image[idetpos] = act_image[iactpos];
		}
	}

	memcpy(act_image, det_image, nx * ny * sizeof(*det_image));
	free(det_image);
	return 0;
}

static float *
read_detfits(TELDEF *teldef, char *regfile, int i, FITS_IMAGE_KEY *kp)
{
	float *image, *image2;
	int nx, ny, nx2, ny2;
	int ix, iy, ipos, ix2, iy2, ipos2;

	nx = teldef->mission.aste->det.xsiz;
	ny = teldef->mission.aste->det.ysiz;

	image = read_fits_image(regfile, 0, &nx2, &ny2, kp);
	if ( NULL == image ) {
		return NULL;
	}

	if ( 0 == strcmp(kp->hduclas1, "IMAGE") &&
		 0 == strcmp(kp->hduclas2, "WMAP") ) {
		if ( 0 != strcmp(kp->mtype1, "DET") ||
			 0 != strcmp(kp->mform1, "DETX,DETY") ) {
			anl_msg_error("\
%s: source_image is WMAP, but not in DET coordinates\n", pname);
			free(image);
			return NULL;
		}
		image2 = image;
		image = malloc( nx * ny * sizeof(*image) );
		if ( NULL == image ) {
			anl_msg_error("\
%s: image[%dx%d] malloc() failed\n", pname, nx, ny);
			free(image2);
			return NULL;
		}
		anl_msg_info("\
   converting WMAP [%dx%d*%d+%d+%d] into DET image\n",
			nx2, ny2, kp->wmrebin, kp->x_offset, kp->y_offset);
		for (ipos = 0, iy = 1; iy <= ny; iy++) {
			iy2 = ((iy - 1) / kp->wmrebin + 1) - kp->y_offset;
			for (ix = 1; ix <= nx; ix++, ipos++) {
				ix2 = ((ix - 1) / kp->wmrebin + 1) - kp->x_offset;
				if ( 0 <= iy2 && iy2 < ny2 && 0 <= ix2 && ix2 < nx2 ) {
					ipos2 = iy2 * nx2 + ix2;
					image[ipos] = (image2[ipos2] < 0.0) ? 0.0 : 1.0;
				} else {
					image[ipos] = 0.0;
				}
			}
		}
		free(image2);

	} else if ( nx != nx2 || ny != ny2 ) {
		anl_msg_error("\
%s: regfile%d size mismatch (nx=%d, ny=%d)\n", pname, i, nx2, ny2);
		free(image);
		return NULL;
	} else if ( '\0' == *kp->ctype1 && '\0' == *kp->ctype2 ) {
		anl_msg_warning("\
%s: WARNING: CTYPE1, CTYPE2 is not specified, assuming DETX, DETY\n", pname);
	} else if ( 0 == CLstricmp(kp->ctype1, "DETX") &&
			    0 == CLstricmp(kp->ctype2, "DETY") ) {
		;
	} else if ( 0 == CLstricmp(kp->ctype1, "ACTX") &&
			    0 == CLstricmp(kp->ctype2, "ACTY") ) {
		if ( convert_act_to_det(teldef, nx, ny, image) ) {
			free(image);
			return NULL;
		}
	} else {
		anl_msg_error("\
%s: invalid CTYPE1='%s', CTYPE2='%s'\n", pname, kp->ctype1, kp->ctype2);
		free(image);
		return NULL;
	}

	if ( *kp->telescop && 0 != strcmp(kp->telescop, teldef->telescop) ) {
		anl_msg_warning("\
%s: WARNING: TELESCOP keyword mismatch\n", pname);
	}
	if ( *kp->instrume && 0 != strcmp(kp->instrume, teldef->instrume) ) {
		anl_msg_warning("\
%s: WARNING: INSTRUME keyword mismatch\n", pname);
	}

	return image;
}

static float *
read_skyfits(TELDEF *teldef, char *regfile, int i, FITS_IMAGE_KEY *kp)
{
	float *image, *image2;
	int nx, ny, nx2, ny2;
	int ix, iy, ipos, ix2, iy2, ipos2;

	nx = teldef->mission.aste->sky.xsiz;
	ny = teldef->mission.aste->sky.ysiz;

	image = read_fits_image(regfile, 0, &nx2, &ny2, kp);
	if ( NULL == image ) {
		return NULL;
	}

	if ( 0 == strcmp(kp->hduclas1, "IMAGE") &&
		 0 == strcmp(kp->hduclas2, "WMAP") ) {
		if ( 0 != strcmp(kp->mtype1, "SKY") ||
			 0 != strcmp(kp->mform1, "X,Y") ) {
			anl_msg_error("\
%s: source_image is WMAP, but not in SKY coordinates\n", pname);
			free(image);
			return NULL;
		}
		image2 = image;
		image = malloc( nx * ny * sizeof(*image) );
		if ( NULL == image ) {
			anl_msg_error("\
%s: image[%dx%d] malloc() failed\n", pname, nx, ny);
			free(image2);
			return NULL;
		}
		anl_msg_info("\
   converting WMAP [%dx%d*%d+%d+%d] into SKY image\n",
			nx2, ny2, kp->wmrebin, kp->x_offset, kp->y_offset);
		for (ipos = 0, iy = 1; iy <= ny; iy++) {
			iy2 = ((iy - 1) / kp->wmrebin + 1) - kp->y_offset;
			for (ix = 1; ix <= nx; ix++, ipos++) {
				ix2 = ((ix - 1) / kp->wmrebin + 1) - kp->x_offset;
				if ( 0 <= iy2 && iy2 < ny2 && 0 <= ix2 && ix2 < nx2 ) {
					ipos2 = iy2 * nx2 + ix2;
					image[ipos] = (image2[ipos2] < 0.0) ? 0.0 : 1.0;
				} else {
					image[ipos] = 0.0;
				}
			}
		}
		free(image2);

	} else if ( nx != nx2 || ny != ny2 ) {
		anl_msg_error("\
%s: regfile%d size mismatch (nx=%d, ny=%d)\n", pname, i, nx2, ny2);
		free(image);
		return NULL;
	} else if ( SKYREF_MODE_USER == com.pointing ) {
		if ( com.skyref.alpha != com.region_kp[i].crval1 ||
			 com.skyref.delta != com.region_kp[i].crval2 ||
			com.skyref.roll  != com.region_kp[i].crota2 ) {
			anl_msg_warning("\
%s: WARNING: SKYREF is different from the user specification\n", pname);
		}
	}

	if ( *kp->telescop && 0 != strcmp(kp->telescop, teldef->telescop) ) {
		anl_msg_warning("\
%s: WARNING: TELESCOP keyword mismatch\n", pname);
	}
	if ( *kp->instrume && 0 != strcmp(kp->instrume, teldef->instrume) ) {
		anl_msg_warning("\
%s: WARNING: INSTRUME keyword mismatch\n", pname);
	}

	return image;
}

static int
read_source_image(char *source_image)
{
	int nx, ny, ix, iy, ipos, nx2, ny2, ix2, iy2, ipos2;
	float *image, *image2;
	FITS_IMAGE_KEY *kp = &com.source_kp;

	anl_msg_info("\
%s: reading source_image '%s'\n", pname, source_image);

	if ( SOURCE_MODE_SKYFITS == com.source_mode ) {
		image = read_fits_image(source_image, 1, &nx, &ny, kp);
		if ( NULL == image ) {
			return -1;
		}
	} else {	/* SOURCE_MODE_DETFITS */
		image = read_fits_image(source_image, 0, &nx, &ny, kp);
		if ( NULL == image ) {
			return -1;
		}

		if ( *kp->telescop && 0 != strcmp(kp->telescop, com.telescop) ) {
			anl_msg_warning("\
%s: WARNING: TELESCOP keyword mismatch\n", pname);
		}
		if ( *kp->instrume && 0 != strcmp(kp->instrume, com.instrume) ) {
			anl_msg_warning("\
%s: WARNING: INSTRUME keyword mismatch\n", pname);
		}
		if ( 0 == strcmp(kp->hduclas1, "IMAGE") &&
			 0 == strcmp(kp->hduclas2, "WMAP") ) {
			if ( 0 != strcmp(kp->mtype1, "DET") ||
				 0 != strcmp(kp->mform1, "DETX,DETY") ) {
				anl_msg_error("\
%s: source_image is WMAP, but not in DET coordinates\n", pname);
				free(image);
				return -1;
			}
			image2 = image;
			nx2 = nx;
			ny2 = ny;
			nx = com.teldef->mission.aste->det.xsiz;
			ny = com.teldef->mission.aste->det.ysiz;
			image = malloc( nx * ny * sizeof(*image) );
			if ( NULL == image ) {
				anl_msg_error("\
%s: image[%dx%d] malloc() failed\n", pname, nx, ny);
				free(image2);
				return -1;
			}
			anl_msg_info("\
   converting WMAP [%dx%d*%d+%d+%d] into DET image\n",
				nx2, ny2, kp->wmrebin, kp->x_offset, kp->y_offset);
			for (ipos = 0, iy = 1; iy <= ny; iy++) {
				iy2 = ((iy - 1) / kp->wmrebin + 1) - kp->y_offset;
				for (ix = 1; ix <= nx; ix++, ipos++) {
					ix2 = ((ix - 1) / kp->wmrebin + 1) - kp->x_offset;
					if ( 0 <= iy2 && iy2 < ny2 && 0 <= ix2 && ix2 < nx2 ) {
						ipos2 = iy2 * nx2 + ix2;
						image[ipos] = image2[ipos2];
					} else {
						image[ipos] = -1.0;
					}
				}
			}
			free(image2);

		} else if ( com.teldef->mission.aste->det.xsiz != nx ||
				    com.teldef->mission.aste->det.ysiz != ny ) {
			anl_msg_error("\
%s: source_image size mismatch (nx=%d, ny=%d)\n", pname, nx, ny);
			free(image);
			return -1;
		} else if ( '\0' == *kp->ctype1 && '\0' == *kp->ctype2 ) {
			anl_msg_warning("\
%s: WARNING: CTYPE1, CTYPE2 is not specified, assuming DETX, DETY\n", pname);
		} else if ( 0 == CLstricmp(kp->ctype1, "DETX") &&
				    0 == CLstricmp(kp->ctype2, "DETY") ) {
			;
		} else if ( 0 == CLstricmp(kp->ctype1, "ACTX") &&
					0 == CLstricmp(kp->ctype2, "ACTY") ) {
			if ( convert_act_to_det(com.teldef, nx, ny, image) ) {
				free(image);
				return -1;
			}
		} else {
			anl_msg_error("\
%s: invalid CTYPE1='%s', CTYPE2='%s'\n", pname, kp->ctype1, kp->ctype2);
			free(image);
			return -1;
		}
	}

	if ( nx < 2 || ny < 2 ) {
		anl_msg_error("\
%s: too small energy bins (%dx%d) in FITS image '%s'\n",
			pname, nx, ny, source_image);
		free(image);
		return -1;
	}

	anl_msg_info("\
   %dx%d image read\n", nx, ny);

	com.hm2 = Hrndm2_init(image, nx, 0.5, nx+0.5, ny, 0.5, ny+0.5);
	free(image);

	if ( NULL == com.hm2 ) {
		anl_msg_error("\
%s: error in Hrndm2_init\n", pname);
		return -1;
	}

	return 0;
}

static int
read_detmask(char *detmask)
{
	int nx, ny, ipos, nxny;
	float *image;
	FITS_IMAGE_KEY *kp = &com.detmask_kp;

	com.ndetx = com.teldef->mission.aste->det.xsiz;
	com.ndety = com.teldef->mission.aste->det.ysiz;

	if ( 0 == CLstricmp("none", detmask) ) {
		com.detmaskmap = NULL;
		return 0;
	}

	anl_msg_info("\
%s: reading detmask '%s'\n", pname, detmask);

	image = read_fits_image(detmask, 0, &nx, &ny, kp);
	if ( NULL == image ) {
		return -1;
	} else if ( com.ndetx != nx || com.ndety != ny ) {
		anl_msg_error("\
%s: detmask size mismatch (nx=%d, ny=%d)\n", pname, nx, ny);
		free(image);
		return -1;
	} else if ( '\0' == *kp->ctype1 && '\0' == *kp->ctype2 ) {
		anl_msg_warning("\
%s: WARNING: CTYPE1, CTYPE2 is not specified, assuming DETX, DETY\n", pname);
	} else if ( 0 == CLstricmp(kp->ctype1, "DETX") &&
			    0 == CLstricmp(kp->ctype2, "DETY") ) {
		;
	} else if ( 0 == CLstricmp(kp->ctype1, "ACTX") &&
			    0 == CLstricmp(kp->ctype2, "ACTY") ) {
		if ( convert_act_to_det(com.teldef, nx, ny, image) ) {
			free(image);
			return -1;
		}
	} else {
		anl_msg_error("\
%s: invalid CTYPE1='%s', CTYPE2='%s'\n", pname, kp->ctype1, kp->ctype2);
		free(image);
		return -1;
	}
	if ( *kp->telescop && 0 != strcmp(kp->telescop, com.telescop) ) {
		anl_msg_warning("\
%s: WARNING: TELESCOP keyword mismatch\n", pname);
	}
	if ( *kp->instrume && 0 != strcmp(kp->instrume, com.instrume) ) {
		anl_msg_warning("\
%s: WARNING: INSTRUME keyword mismatch\n", pname);
	}

	anl_msg_info("\
   %dx%d image read\n", nx, ny);

	nxny = nx * ny;
	com.tot_ccd_pix = nxny;
	com.sum_detmask = 0.0;
	for (ipos = 0; ipos < nxny; ipos++) {
		com.sum_detmask += image[ipos];
	}
	com.detmaskmap = image;

	anl_msg_info("\
   mask_ratio_ccd = %.1f / %.0f = %.6f\n",
		com.sum_detmask, com.tot_ccd_pix, com.sum_detmask/com.tot_ccd_pix);

	return 0;
}

static int
read_gtifile(char *gtifile)
{
	int i, j, nt, iuse, *use, itmp, istat;
	AtTimeD attime;
	double t, t0, t1;

	if ( 0 == CLstricmp("none", gtifile) ) {
		istat = aefits_datestr2attimeD(com.date_obs, &attime);
		if ( istat ) {
			anl_msg_error("\
%s: invalid format of date_obs='%s'\n", pname, com.date_obs);
			return istat;
		}
		com.photon_time = attimeD2aste(&attime);
		com.gti.tstart = com.photon_time;
		com.gti.tstop = com.photon_time;
		com.gti.telapse = 0.0;
		com.gti.ontime = 0.0;
		com.gti.ngti = 1;
		com.gti.start = com.gti.stop = &com.gti.tstart;

	} else {

		anl_msg_info("\
%s: reading gtifile '%s'\n", pname, gtifile);
		istat = aste_gti_read(&com.gti, gtifile);
		if ( istat ) {
			return istat;
		}
		if ( 1 == com.gti.ngti && 0.0 == com.gti.ontime ) {	/* fixed time */
			com.photon_time = com.gti.tstart;
		}

	}

/* determine number of time step */
	nt = com.num_photon;
	if ( 0.0 == com.gti.ontime ) {	/* fixed time or using TIME column */
		if ( nt <= 0 || com.gti.ngti < nt ) {
			nt = com.gti.ngti;
		}
	} else {
		if ( nt <= 0 || com.gti.ontime + 1 < nt ) {
			nt = (int)ceil(com.gti.ontime + 1);
		}
	}
	if ( MAX_TIME_STEP < nt ) {
		nt = MAX_TIME_STEP;
	}
	com.gti_nt = nt;

	if ( 0 != CLstricmp("none", gtifile) ) {
		anl_msg_info("\
   tstart=%.1f, tstop=%.1f, ontime=%.1f, ngti=%d, nt=%d\n",
			com.gti.tstart, com.gti.tstop, com.gti.ontime, com.gti.ngti, nt);
	}

	if ( 1 == com.gti.ngti && 0.0 == com.gti.ontime ) {	/* fixed time */
		com.gti_t = NULL;
		return 0;
	}

	com.gti_t = malloc( sizeof(*com.gti_t) * nt );
	if ( NULL == com.gti_t ) {
		anl_msg_error("\
%s: com.gti_t[%d] malloc() failed\n", pname, nt);
		return NGP_NO_MEMORY;
	}

/* stepping time */
	if ( 0.0 < com.gti.ontime ) {
		for (i = 0; i < nt; i++) {
			if ( 1 == nt ) {
				t = com.gti.ontime / 2;
			} else {
				t = com.gti.ontime * i / (nt - 1);
			}
			for (j = 0; j < com.gti.ngti; j++) {
				t0 = com.gti.start[j];
				t1 = com.gti.stop[j];
				if ( t0 + t <= t1 ) {
					com.gti_t[i] = t0 + t;
					break;
				}
				t = t - (t1 - t0);
			}
			if ( j == com.gti.ngti ) {
				com.gti_t[i] = com.gti.tstop;
			}
		}
	} else if ( 0.0 == com.gti.ontime ) { /* fixed time or using TIME column */
		if ( nt == com.gti.ngti ) {
			memcpy(com.gti_t, com.gti.start, nt * sizeof(*com.gti_t));
		} else if ( nt < com.gti.ngti ) {
			use = (int *)(com.gti_t + nt) - nt;	/* use as work area */
			for (i = 0; i < nt; i++) {
				iuse = (int)( (com.gti.ngti - i) * aste_drndts() );
				if ( com.gti.ngti - i - 1 < iuse ) {
					iuse = com.gti.ngti - i - 1;
				}
				for (j = 0; j < i; j++) {
					if ( use[j] <= iuse ) {
						iuse++;
					} else {
						while ( j < i ) {	/* insert iuse */
							itmp = use[j];
							use[j] = iuse;
							iuse = itmp;
							j++;
						}
						break;
					}
				}
				use[i] = iuse;
			}
			for (i = 0; i < nt; i++) {
				iuse = use[i];
				com.gti_t[i] = com.gti.start[iuse];
			}
		} else {	/* com.gti.ngti < nt: should not happen, just for safety */
			anl_msg_error("\
%s: nt=%d exceeds ngti=%d, something is wrong\n", pname, nt, com.gti.ngti);
			free(com.gti_t);
			return -1;
		}
	} else {	/* com.gti.ontime < 0, should not happen, just for safety */
		anl_msg_error("\
%s: com.gti.ontime=%.1f < 0.0, something is wrong\n", pname, com.gti.ontime);
		free(com.gti_t);
		return -1;
	}

	if ( 1 == nt ) {	/* fixed time */
		com.photon_time = com.gti_t[0];
		free(com.gti_t);
		com.gti_t = NULL;
		return 0;
	}

	return 0;
}

static int
read_attitude(char *attitude)
{
	int i, istat;
	char date[80];
	AtTimeD attime;
	double t, t0, t1;
	AtEulerAng *e0, *e1;
	ATTFILE *ap;
	SKYREF skyref;

	com.att.ea = NULL;
	t0 = com.att.t0 = com.gti.tstart;
	t1 = com.att.t1 = com.gti.tstop;
	e0 = &com.att.e0;
	e1 = &com.att.e1;

	if ( 0 == CLstricmp("none", attitude) ) {
		*e0 = *e1 = com.ea;
		aste_euler2skyref(com.teldef, &com.ea, &skyref);
		com.ra_nom = skyref.alpha;
		com.dec_nom = skyref.delta;
		return 0;
	}

	anl_msg_info("\
%s: reading attitude '%s'\n", pname, attitude);
	ap = aste_att_init(attitude);
	if ( NULL == ap ) {
		return -1;
	}

	if ( aste_att_ea(ap, t=t1, e1) ||
		 aste_att_ea(ap, t=t0, e0) ) goto error;
	aste2attimeD(t0, &attime); attime.ss = 0.0;
	aefits_attimeD2datestr(&attime, date);
	anl_msg_info("\
   EA=( %.4f %.4f %.4f ) at t=%.1f [%s]\n",
		e0->phi*RAD2DEG, e0->theta*RAD2DEG, e0->psi*RAD2DEG, t0, date);
	aste2attimeD(t1, &attime); attime.ss = 0.0;
	aefits_attimeD2datestr(&attime, date);
	anl_msg_info("\
   EA=( %.4f %.4f %.4f ) at t=%.1f [%s]\n",
		e1->phi*RAD2DEG, e1->theta*RAD2DEG, e1->psi*RAD2DEG, t1, date);

	istat = 0;
	if ( NULL != ap->fp ) {
		if ( fits_read_key_dbl(ap->fp, "RA_NOM", &com.ra_nom, NULL, &istat) ||
			 fits_read_key_dbl(ap->fp, "DEC_NOM",&com.dec_nom,NULL, &istat) ||
			 0 ) {
			anl_msg_warning("\
%s: WARNING: reading RA_NOM, DEC_NOM failed (%d), calculate from EULER\n",
				pname, istat);
			t = (t0 + t1) / 2;
			if ( aste_att_ea(ap, t, &com.ea) ) goto error;
			aste_euler2skyref(com.teldef, &com.ea, &skyref);
			com.ra_nom = skyref.alpha;
			com.dec_nom = skyref.delta;
		}
		anl_msg_info("\
   RA_NOM=%.4f, DEC_NOM=%.4f\n", com.ra_nom, com.dec_nom);
	}

	if ( 1 == com.gti_nt ) {	/* fixed time */
		t = com.photon_time;
		if ( aste_att_ea(ap, t, &com.ea) ) goto error;
		return 0;
	}

	com.att.ea = malloc( com.gti_nt * sizeof(*com.att.ea) );
	if ( NULL == com.att.ea ) {
		anl_msg_error("\
%s: com.att.ea[%d] malloc() failed\n", pname, com.gti_nt);
		aste_att_close(ap);
		return -1;
	}

	for (i = 0; i < com.gti_nt; i++) {
		t = com.gti_t[i];
		if ( aste_att_euler(t, &com.att.ea[i]) ) {
		error:
			aste2attimeD(t, &attime); attime.ss = 0.0;
			aefits_attimeD2datestr(&attime, date);
			anl_msg_error("\
%s: could not find attitude at t=%.1f [%s]\n", pname, t, date);
			if ( NULL != com.att.ea ) free(com.att.ea);
			aste_att_close(ap);
			return -1;
		}
	}

	aste_att_close(ap);
	return 0;
}

static int
randomize_time(void)
{
	int i, iuse;
	double ttmp;
	AtEulerAng etmp;

	for (i = 0; i < com.gti_nt - 1; i++) {
		iuse = i + (int)( (com.gti_nt - i) * aste_drndts() );
		if ( com.gti_nt - 1 < iuse ) {
			iuse = com.gti_nt - 1;
		}
		if ( i != iuse ) {
			ttmp = com.gti_t[i];
			com.gti_t[i] = com.gti_t[iuse];
			com.gti_t[iuse] = ttmp;
			if ( NULL != com.att.ea ) {
				etmp = com.att.ea[i];
				com.att.ea[i] = com.att.ea[iuse];
				com.att.ea[iuse] = etmp;
			}
		}
	}

	return 0;
}

static int
read_energ_lo_hi(char *rmffile)
{
	fitsfile *fp;
	char comment[80];
	int istat, istat2, hdunum;
	int ie, ne, col_lo, col_hi, anul;
	int exact = 1;

	istat = istat2 = 0;
	fits_open_file(&fp, rmffile, READONLY, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: rmffile '%s' open failed (%d)\n", pname, rmffile, istat);
		return istat;
	}

	fits_get_hdu_num(fp, &hdunum);
	if ( 1 == hdunum ) {	/* primary HDU */
		fits_movnam_hdu(fp, BINARY_TBL, "MATRIX", 0, &istat);
		if ( istat ) {
			istat = 0;
			fits_movnam_hdu(fp, BINARY_TBL, "SPECRESP MATRIX", 0, &istat);
			if ( istat ) {
				fits_close_file(fp, &istat2);
				anl_msg_error("\
%s: no MATRIX extension in '%s'\n", pname, rmffile);
				return istat;
			}
		}
	}

	fits_read_key(fp, TINT, "NAXIS2", &ne, comment, &istat);
	fits_get_colnum(fp, exact, "ENERG_LO", &col_lo, &istat);
	fits_get_colnum(fp, exact, "ENERG_HI", &col_hi, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: ENERG_LO/HI column not found in '%s'\n", pname, rmffile);
		fits_close_file(fp, &istat2);
		return istat;
	}

	com.num_energ = ne;
	com.energ_lo = malloc(3 * sizeof(double) * ne);
	com.energ_hi = com.energ_lo + ne;
	com.energ_cen = com.energ_hi + ne;
	if ( NULL == com.energ_lo ) {
		anl_msg_error("\
%s: ENERG_LO/HI malloc() failed for '%s'\n", pname, rmffile);
		fits_close_file(fp, &istat2);
		return istat;
	}

	fits_read_col_dbl(fp, col_lo, 1, 1, ne, 0.0, com.energ_lo, &anul, &istat);
	fits_read_col_dbl(fp, col_hi, 1, 1, ne, 0.0, com.energ_hi, &anul, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: ENERG_LO/HI column read error for '%s'\n", pname, rmffile);
		fits_close_file(fp, &istat2);
		free(com.energ_lo);
		com.energ_lo = NULL;
		return istat;
	}

	for (ie = 0; ie < ne; ie++) {
		com.energ_cen[ie] = (com.energ_lo[ie] + com.energ_hi[ie]) / 2.0;
	}

	fits_close_file(fp, &istat);

	return 0;
}

static int
read_contamifile(char *contamifile)
{
	int istat;

	if ( 0 == CLstricmp("NONE", contamifile) ) {
		com.xcp = NULL;
		return 0;
	}
	istat = xis_contami_init(&com.xcp, contamifile);

	return istat;
}

static int
read_estepfile(char *estepfile)
{
	int i, n;
	FILE *fp;
	char *p, line[PIL_LINESIZE];
	double energ_lo, energ_hi;
	double emin, emax, ebin, emin_min, emax_max;
	struct estep_tables *estep_table_p;

	energ_lo = com.energ_lo[0];
	energ_hi = com.energ_hi[com.num_energ-1];
	estep_table_p = NULL;

	if ( 0 == CLstricmp("none", estepfile) ||
		 0 == CLstricmp("dense", estepfile) ||
		 0 == CLstricmp("default", estepfile) ) {
		estep_table_p = &estep_dense;
	} else if ( 0 == CLstricmp("full", estepfile) ) {
		estep_table_p = &estep_full;
	} else if ( 0 == CLstricmp("medium", estepfile) ) {
		estep_table_p = &estep_medium;
	} else if ( 0 == CLstricmp("sparse", estepfile) ) {
		estep_table_p = &estep_sparse;
	}

	if ( NULL != estep_table_p ) {
		for (i = 0; i < sizeof(estep_full.e) / sizeof(*estep_full.e); i++) {
			if ( -999 == estep_table_p->e[i].mi ) {
				break;
			}
			com.emin[i] = estep_table_p->e[i].mi;
			if ( com.emin[i] < 0.0 ) {
				com.emin[i] = energ_lo;
			}
			com.emax[i] = estep_table_p->e[i].ma;
			if ( com.emax[i] <= 0.0 ) {
				com.emax[i] = energ_hi;
			}
			com.ebin[i] = estep_table_p->e[i].bi;
		}
		com.num_ebin = i;
		return 0;
	}

	anl_msg_info("\
%s: reading estepfile '%s'\n", pname, estepfile);

	fp = fopen(estepfile, "r");
	if ( NULL == fp ) {
		anl_msg_error("\
%s: estepfile '%s' open failed\n", pname, estepfile);
		return -1;
	}

	i = 0;
	emin_min = emax_max = -1.0;
	while ( NULL != fgets(line, sizeof(line), fp) ) {
		p = strchr(line, '#');
		if ( NULL != p ) *p = '\0';
		p = strchr(line, '!');
		if ( NULL != p ) *p = '\0';
		p = strchr(line, ';');
		if ( NULL != p ) *p = '\0';
		p = strchr(line, '%');
		if ( NULL != p ) *p = '\0';
		emin = emax = ebin = 0.0;
		n = sscanf(line, "%lf%lf%lf", &emin, &emax, &ebin);
		if ( n <= 0 ) {
			continue;
		} else if ( 3 != n ) {
			anl_msg_warning("\
%s: incomplete line '%s' ignored (n=%d)\n", pname, line, n);
			continue;
		}
		if ( -1.0 == emin ) {
			emin = energ_lo;
		}
		if ( -1.0 == emax ) {
			emax = energ_hi;
		}
		if ( i < MAX_ENERGY ) {
			com.emin[i] = emin;
			com.emax[i] = emax;
			com.ebin[i] = ebin;
			i++;
		} else {
			anl_msg_error("\
%s: too many energy steps (%d)\n", pname, i);
			fclose(fp);
			return -1;
		}
		if ( emin_min < 0.0 || emin < emin_min ) {
			emin_min = emin;
		}
		if ( emax_max < 0.0 || emax_max < emax ) {
			emax_max = emax;
		}
	}

	fclose(fp);

	com.num_ebin = i;
	if ( com.num_ebin <= 0 ) {
		anl_msg_error("\
%s: no energy steps (%d)\n", pname, i);
		return -1;
	}

	anl_msg_info("\
   num_ebin=%d, emin_min=%.3f, emax_max=%.3f\n",
		com.num_ebin, emin_min, emax_max);

	return 0;
}

static int
compare_calc_energy(const void *v1, const void *v2)
{
	struct calc_energy *p1 = (struct calc_energy *)v1;
	struct calc_energy *p2 = (struct calc_energy *)v2;
	double E1 = p1->Ecen;
	double E2 = p2->Ecen;

	if ( E1 < E2 ) {
		return -1;
	} else if ( E1 > E2 ) {
		return +1;
	}

	if ( p1->Ewid < p2->Ewid ) {
		return -1;
	} else if ( p1->Ewid > p2->Ewid ) {
		return +1;
	}

	return 0;
}

static int
decide_energy_step(void)
{
	int i, j, n, ireg, ie, ne, ne_max, iebin;
	int num_calc, lower_used, upper_used;
	double emin, emax, ebin, ecen;
	double emin_min, emax_max, energy_min, energy_max;
	struct calc_energy *p;

/* decide num_calc first */
	num_calc = 0;
	emin_min = -1.0;
	emax_max = -1.0;
	for (iebin = 0; iebin < com.num_ebin; iebin++) {
		emin = com.emin[iebin];
		emax = com.emax[iebin];
		ebin = com.ebin[iebin];
		if ( 0.0 == ebin ) {
			for (i = 0; i < com.num_energ; i++) {
				if ( emin <= com.energ_lo[i] && com.energ_hi[i] <= emax ) {
					if ( emin_min < 0.0 || com.energ_cen[i] < emin_min ) {
						emin_min = com.energ_cen[i];
					}
					if ( emax_max < 0.0 || emax_max < com.energ_cen[i] ) {
						emax_max = com.energ_cen[i];
					}
					num_calc++;
				}
			}

		} else if ( 0.0 < ebin ) {
			n = (int)floor((emax - emin + 0.5*ebin)/ebin) + 1;
			if ( emin_min < 0.0 || emin < emin_min ) {
				emin_min = emin;
			}
			if ( emax_max < 0.0 || emax_max < emin + n*ebin ) {
				emax_max = emin + n*ebin;
			}
			num_calc += n;

		} else {
			ebin = - ebin;
			n = (int)floor((emax - emin + 0.5*ebin)/ebin);
			if ( emin_min < 0.0 || emin + 0.5*ebin < emin_min ) {
				emin_min = emin + 0.5*ebin;
			}
			if ( emax_max < 0.0 || emax_max < emin + (n-0.5)*ebin ) {
				emax_max = emin + (n-0.5)*ebin;
			}
			num_calc += n;

		}

	}

	energy_min = com.energ_cen[0];
	if ( energy_min < emin_min ) {
		num_calc++;
	}

	energy_max = com.energ_cen[com.num_energ-1];
	if ( emax_max < energy_max ) {
		num_calc++;
	}

/* allocate com.calc */
	com.calc = malloc( sizeof(com.calc[0]) * num_calc );
	if ( NULL == com.calc ) {
		anl_msg_error("\
%s: com.calc[%d] malloc() failed\n", pname, num_calc);
		return -1;
	}

/* set com.calc */
	num_calc = 0;
	for (iebin = 0; iebin < com.num_ebin; iebin++) {
		emin = com.emin[iebin];
		emax = com.emax[iebin];
		ebin = com.ebin[iebin];
		if ( 0.0 == ebin ) {
			for (i = 0; i < com.num_energ; i++) {
				if ( emin <= com.energ_lo[i] && com.energ_hi[i] <= emax ) {
					p = &com.calc[num_calc];
					p->Ecen = com.energ_cen[i];
					p->Ewid = com.energ_hi[i] - com.energ_lo[i];
					num_calc++;
				}
			}

		} else if ( 0.0 < ebin ) {
			n = (int)floor((emax - emin + 0.5*ebin)/ebin) + 1;
			for (i = 0; i < n; i++) {
				p = &com.calc[num_calc];
				p->Ecen = emin + i*ebin;
				p->Ewid = 0.0;
				num_calc++;
			}

		} else {
			ebin = - ebin;
			n = (int)floor((emax - emin + 0.5*ebin)/ebin);
			for (i = 0; i < n; i++) {
				p = &com.calc[num_calc];
				p->Ecen = emin + (i+0.5)*ebin;
				p->Ewid = ebin;
				num_calc++;
			}

		}

	}

	if ( energy_min < emin_min ) {
		anl_msg_warning("\
%s: WARNING: emin_min=%.3f > energy_min=%.3f keV, adding it\n",
			pname, emin_min, energy_min);
		p = &com.calc[num_calc];
		p->Ecen = energy_min;
		p->Ewid = 0.0;
		num_calc++;
	}

	if ( emax_max < energy_max ) {
		anl_msg_warning("\
%s: WARNING: emax_max=%.3f < energy_max=%.3f keV, adding it\n",
			pname, emax_max, energy_max);
		p = &com.calc[num_calc];
		p->Ecen = energy_max;
		p->Ewid = 0.0;
		num_calc++;
	}

/* sort & uniq energy bins */
	qsort(com.calc, num_calc, sizeof(com.calc[0]), compare_calc_energy);

	for (i = 0; i < num_calc - 1; i++) {
		ecen = com.calc[i].Ecen;
		/* checking for i is needed because num_calc changes in the loop */
		while ( i < num_calc - 1 && fabs(ecen - com.calc[i+1].Ecen) < 1e-6 ) {
			num_calc--;
			for (j = i; j < num_calc; j++) {
				com.calc[j] = com.calc[j+1];
			}
			ecen = com.calc[i].Ecen;
		}
	}

/* remove unused energy bins */
	i = 0;
	lower_used = 0;
	ne_max = 0;
	while ( i < num_calc - 1 ) {
		upper_used = 0;
		ne = 0;
		for (ie = 0; ie < com.num_energ; ie++) {
			ecen = com.energ_cen[ie];
			if ( ecen < com.calc[i].Ecen ) {
				continue;
			} else if ( com.calc[i+1].Ecen < ecen ) {
				break;
			}
			upper_used = 1;
			ne++;
		}
		if ( 0 == lower_used && 0 == upper_used ) {
			anl_msg_warning("\
%s: WARNING: E=%.6f keV not used for ARF calculation, removed\n",
				pname, com.calc[i].Ecen);
			num_calc--;
			for (j = i; j < num_calc; j++) {
				com.calc[j] = com.calc[j+1];
			}
		} else {
			lower_used = upper_used;
			if ( ne_max < ne ) {
				ne_max = ne;
			}
			i++;
		}
	}

	if ( 0 == lower_used ) {
		anl_msg_warning("\
%s: WARNING: E=%.6f keV not used for ARF calculation, removed\n",
			pname, com.calc[i].Ecen);
		num_calc--;
	}
	com.num_calc = num_calc;

	if ( 0 == ne_max ) {
		anl_msg_error("\
%s: no calculation energy, bad estepfile\n", pname);
		return -1;
	}
	anl_msg_info("\
   num_calc=%d, ne_max=%d\n", num_calc, ne_max);

#ifdef FAST_CALC_CW_SUM
/* allocate com.cw_enes, com.cw_transmis, com.cw_sums1, com.cw_sums2 */
	com.cw_sums1 = malloc( sizeof(*com.cw_sums1) * 2 * com.num_region );
	com.cw_sums2 = &com.cw_sums1[com.num_region];
	if ( NULL == com.cw_sums1 ) {
		anl_msg_error("\
%s: com.cw_sums1[2x%d] malloc() failed\n", pname, com.num_region);
		return -1;
	}
	com.cw_enes = malloc( sizeof(double) * 2 * ne_max +
						  sizeof(double) * 2 * com.num_region * ne_max );
	com.cw_transmis = &com.cw_enes[ne_max];
	com.cw_sums1[0] = &com.cw_transmis[ne_max];
	com.cw_sums2[0] = &com.cw_sums1[0][com.num_region * ne_max];
	if ( NULL == com.cw_enes ) {
		anl_msg_error("\
%s: com.cw_enes[2x%d], com.cw_sums1[0][2x%dx%d] malloc() failed\n",
			pname, ne_max, com.num_region, ne_max);
		return -1;
	}
	for (ireg = 1; ireg < com.num_region; ireg++) {
		com.cw_sums1[ireg] = &com.cw_sums1[ireg-1][ne_max];
		com.cw_sums2[ireg] = &com.cw_sums2[ireg-1][ne_max];
	}
#endif

	return 0;
}

static int
allocate_ene(void)
{
	int i, num_all;

	com.pmem_size = com.num_photon;
	if ( 0 == com.pmem_size ) {
		com.pmem_size = MAX_PHOTON_MEMORY;
	}
	com.pmem_storage[0] = malloc( sizeof(PHOTON_MEMORY) * com.pmem_size * 2 );
	com.pmem_storage[1] = &com.pmem_storage[0][com.pmem_size];
	if ( NULL == com.pmem_storage[0] ) {
		anl_msg_error("\
%s: com.pmem_storage[2][%d] malloc() failed\n", pname, com.pmem_size);
		return -1;
	}

	num_all = com.num_calc * com.num_region;
	com.ene = malloc( sizeof(com.ene[0]) * com.num_calc +
					  sizeof(com.ene[0].reg[0]) * num_all );
	if ( NULL == com.ene ) {
		anl_msg_error("\
%s: com.ene[%d].reg[%d] malloc() failed\n",
			pname, com.num_calc, com.num_region);
		return -1;
	}

	com.ene[0].reg = (struct calc_values_per_region *)&com.ene[com.num_calc];
	for (i = 1; i < com.num_calc; i++) {
		com.ene[i].reg = &com.ene[i-1].reg[com.num_region];
	}

	com.calc_index = 0;
	for (i = 0; i < com.num_calc; i++) {
		com.ene[i].input = 0.0;
		com.ene[i].iphoton = 0;
		com.ene[i].pmem = com.pmem_storage[i%2];
	}
	for (i = 0; i < num_all; i++) {
		struct calc_values_per_region *p = &com.ene[0].reg[i];
		p->weisum = 0.0;
		p->detect = 0.0;
		p->relerr = 1.0;
	}

	return 0;
}

static int
make_regmap(void)
{
	int i, ix, iy, ipos, nx, ny, npos;
	double x2, y2, r2, r2min, r2max, region_x, region_y;
	float *image, *rp, *regmap;
	double *wmap;
	FITS_IMAGE_KEY *kp;

	anl_msg_info("\
%s: making region maps\n", pname);
	image = NULL;	/* initialize for gcc warning */

	switch ( com.region_mode ) {
	case REGION_MODE_SKYFITS:
	case REGION_MODE_SKYCIRC:
	case REGION_MODE_SKYREG:
	case REGION_MODE_SKYEXPR:
		nx = com.region_nx = com.teldef->mission.aste->sky.xsiz;
		ny = com.region_ny = com.teldef->mission.aste->sky.ysiz;
		npos = nx * ny;
		break;

	case REGION_MODE_DETFITS:
	case REGION_MODE_DETCIRC:
	case REGION_MODE_DETREG:
	case REGION_MODE_DETEXPR:
		nx = com.region_nx = com.teldef->mission.aste->det.xsiz;
		ny = com.region_ny = com.teldef->mission.aste->det.ysiz;
		npos = nx * ny;
		break;

	default:
		anl_msg_error("\
%s: unknonw region_mode in make_regmap()\n", pname);
		return -1;
	}

	for (i = 0; i < com.num_region; i++) {
		kp = &com.region_kp[i];

		switch ( com.region_mode ) {
		case REGION_MODE_SKYFITS:
			anl_msg_info("\
   reading regfile%d '%s'\n", i+1, com.regfile[i]);
			image = read_skyfits(com.teldef, com.regfile[i], i, kp);
			if ( NULL == image ) return -1;
			break;

		case REGION_MODE_DETFITS:
			anl_msg_info("\
   reading regfile%d '%s'\n", i+1, com.regfile[i]);
			image = read_detfits(com.teldef, com.regfile[i], i, kp);
			if ( NULL == image ) return -1;
			break;

		case REGION_MODE_SKYCIRC:
		case REGION_MODE_DETCIRC:
			region_x = com.region_x[i];
			region_y = com.region_y[i];
			r2min = com.region_rmin[i] * com.region_rmin[i];
			r2max = com.region_rmax[i] * com.region_rmax[i];
			image = malloc( npos * sizeof(*image) );
			if ( NULL == image ) {
				anl_msg_error("\
%s: com.regmap[%d][%dx%d] malloc() failed\n", pname, i, nx, ny);
				return -1;
			}
			for (iy = 0; iy < ny; iy++) {
				rp = &image[iy*nx];
				y2 = (iy+1) - region_y;
				y2 *= y2;
				for (ix = 0; ix < nx; ix++) {
					x2 = (ix+1) - region_x;
					x2 *= x2;
					r2 = x2 + y2;
					rp[ix] = ( r2min <= r2 && r2 <= r2max ) ? 1.0 : 0.0;
				}
			}
			break;

		case REGION_MODE_SKYREG:
		case REGION_MODE_DETREG:
			image = make_ds9reg_image(com.regfile[i], NULL, nx, ny);
			if ( NULL == image ) {
				return -1;
			}
			break;

		case REGION_MODE_SKYEXPR:
		case REGION_MODE_DETEXPR:
			image = make_ds9reg_image(NULL, com.regfile[i], nx, ny);
			if ( NULL == image ) {
				return -1;
			}
			break;

		default:
			;
		}

		com.regmap[i] = image;
	}

	if ( REGION_MODE_SKYFITS == com.region_mode &&
		 SKYREF_MODE_AUTO == com.pointing ) {
		com.skyref.alpha = com.region_kp[0].crval1;
		com.skyref.delta = com.region_kp[0].crval2;
		com.skyref.roll  = com.region_kp[0].crota2;
		for (i = 1; i < com.num_region; i++) {
			if ( com.skyref.alpha != com.region_kp[i].crval1 ||
				 com.skyref.delta != com.region_kp[i].crval2 ||
				 com.skyref.roll  != com.region_kp[i].crota2 ) {
				anl_msg_error("\
%s: SKYREF different for regfile%d\n", pname, i+1);
				return -1;
			}
		}
	}

/* initialize wmap, sum_source_reg, sum_detmask_reg, tot_ccd_pix_reg */
	for (i = 0; i < com.num_region; i++) {
		wmap = com.wmap[i] = malloc( npos * sizeof(*wmap) );
		if ( NULL == wmap ) {
			anl_msg_error("\
%s: com.wmap[%d][%dx%d] malloc() failed\n", pname, i, nx, ny);
			return -1;
		}
		regmap = com.regmap[i];
		if ( NULL != com.detmaskmap && (
				REGION_MODE_DETFITS == com.region_mode ||
				REGION_MODE_DETCIRC == com.region_mode ||
				REGION_MODE_DETREG  == com.region_mode ||
				REGION_MODE_DETEXPR == com.region_mode ||
				0 ) ) {
			for (ipos = 0; ipos < npos; ipos++) {
				wmap[ipos] = ( 0.0 < regmap[ipos] &&
							   0.0 < com.detmaskmap[ipos] ) ? 0.0 : -1.0;
			}
		} else {
			for (ipos = 0; ipos < npos; ipos++) {
				wmap[ipos] = ( 0.0 < regmap[ipos] ) ? 0.0 : -1.0;
			}
		}

		com.sum_source_reg[i]  = 0.0;
		com.sum_detmask_reg[i] = 0.0;
		com.tot_ccd_pix_reg[i] = 0.0;
	}

	return 0;
}

static int
eval_pixq(void)
{
	char *k;
	int istat, ipos, nxny, actx, acty, detx, dety;
	fitsfile *ifp;
	float *image;
	unsigned char *actexpo;

	if ( 0 == com.enable_pixq ) {
		return 0;
	}

	anl_msg_info("\
%s: evaluating pixel quality selection\n", pname);

/* initialize pixel quality function */
	pixq.hotpix_filenames = com.hotpixfiles;
	pixq.badcol_filename  = com.badcolumfile;
	pixq.calmask_filename = com.calmaskfile;
	if ( 0 == CLstricmp("none", com.phafile) ) {
		istat = xisSciReadKeys(NULL, &pixq.sci);
		if ( istat ) goto quit;
	} else {
		istat = 0;
		fits_open_file(&ifp, k=com.phafile, READONLY, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_open_file('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
		istat = xisSciReadKeys(ifp, &pixq.sci);
		if ( istat ) {
			int istat2 = 0;
			fits_close_file(ifp, &istat2); /* ignore error */
			goto quit;
		}
		fits_close_file(ifp, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: fits_close_file('%s') failed (%d)\n", pname, k, istat);
			goto quit;
		}
	}
	istat = xisPixelQualityInit(com.instrume, &pixq);
	if ( istat ) goto quit;

/* initialize pixel quality statistics */
	xisPixqStatInit(&statistics);
	statistics.flag_sel = 1;
	statistics.pixq_min = com.pixq_min;
	statistics.pixq_max = com.pixq_max;
	statistics.pixq_and = com.pixq_and;
	statistics.pixq_eql = com.pixq_eql;

/* generate exposure map in ACT coordinates */
	istat = xisPixqExpMapGenACT(&pixq, &statistics, com.gti.tstart);
	if ( istat ) goto quit;

/* free memory for badcol_cache & calmask */
	if ( NULL != pixq.badcol_cache ) {
		free(pixq.badcol_cache);
		pixq.badcol_cache = NULL;
	}
	if ( NULL != pixq.calmask ) {
		free(pixq.calmask);
		pixq.calmask = NULL;
	}

	anl_msg_info("\
%s: merging with detmask\n", pname);

	nxny = com.ndetx * com.ndety;
	image = com.detmaskmap;
	if ( NULL == image ) {
		image = malloc(sizeof(*image) * nxny);
		if ( NULL == image ) {
			anl_msg_error("\
%s: image[%dx%d] malloc() failed\n", pname, com.ndetx, com.ndety);
			istat = -1;
			goto quit;
		}
		for (ipos = 0; ipos < nxny; ipos++) {
			image[ipos] = 1.0;
		}
	}

	actexpo = pixq.actexpo;
	for (acty = 0; acty < com.ndety; acty++) {
		for (actx = 0; actx < com.ndetx; actx++) {
			xis_act2det(com.teldef, actx, acty, &detx, &dety);
			ipos = (dety - 1) * com.ndetx + (detx - 1);
			image[ipos] *= (*actexpo) * pixq.bscale;
			actexpo++;
		}
	}

/* free memory for actexpo */
	free(pixq.actexpo);
	pixq.actexpo = NULL;

	com.tot_ccd_pix = nxny;
	com.sum_detmask = 0.0;
	for (ipos = 0; ipos < nxny; ipos++) {
		com.sum_detmask += image[ipos];
	}
	com.detmaskmap = image;

	anl_msg_info("\
   mask_ratio_ccd = %.1f / %.0f = %.6f\n",
		com.sum_detmask, com.tot_ccd_pix, com.sum_detmask/com.tot_ccd_pix);

	return 0;

 quit:
	return istat;
}

static void
show_energy_step(void)
{
	int i;
	struct calc_energy *p;

	anl_msg_always("\n\
%s: *** energy step information ***\n\n", pname);
	for (i = 0; i < com.num_calc; i++) {
		p = &com.calc[i];
		anl_msg_always("\
%5d: %6.3f %6.3f\n", i+1, p->Ecen, p->Ewid);
	}
}

static int
create_wmap(fitsfile *fp, TELDEF *teldef)
{
	static struct {
		char *key, *value, *comment;
	} *p, keys[] = {
 { "TELESCOP",	"SUZAKU",	"mission/satellite name" },
 { "INSTRUME",	"XIS",		"instrument/detector name" },
 { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
 { "HDUCLAS1",	"IMAGE",	"dataset relates to spectral response" },
 { "HDUVERS1",	"1.0.0",	"Version of family of formats" },
 { "HDUCLAS2",	"WMAP",		"dataset contains Weighted Map Image" },
 { "HDUVERS2",	"1.0.0",	"Version of format" },
 { "WCSNAMEP",	"PHYSICAL",	"" },
 { "WCSTY1P",	"PHYSICAL",	"" },
 { "WCSTY2P",	"PHYSICAL",	"" },
 { "CTYPE1P",	"",	"Source of X-axis" },
 { "CTYPE2P",	"",	"Source of Y-axis" },
};
	int istat = 0;
	int naxis = 2;
	long naxes[2];
	int ikey, nkey = sizeof(keys) / sizeof(*keys);
	TELDEF_ASTROE *aste = com.teldef->mission.aste;
	double cdelt1 = - (aste->sky.xscl / aste->focallen) * RAD2DEG;
	double cdelt2 = + (aste->sky.yscl / aste->focallen) * RAD2DEG;

	keys[0].value = teldef->telescop;
	keys[1].value = teldef->instrume;

	switch ( com.region_mode ) {
	case REGION_MODE_SKYFITS:
	case REGION_MODE_SKYCIRC:
	case REGION_MODE_SKYREG:
	case REGION_MODE_SKYEXPR:
		keys[nkey-2].value = "X";		/* CTYPE1P */
		keys[nkey-1].value = "Y";		/* CTYPE2P */
		naxes[0] = aste->sky.xsiz;
		naxes[1] = aste->sky.ysiz;
		break;
	case REGION_MODE_DETFITS:
	case REGION_MODE_DETCIRC:
	case REGION_MODE_DETREG:
	case REGION_MODE_DETEXPR:
		keys[nkey-2].value = "DETX";	/* CTYPE1P */
		keys[nkey-1].value = "DETY";	/* CTYPE2P */
		naxes[0] = aste->det.xsiz;
		naxes[1] = aste->det.ysiz;
		break;
	}

fits_create_img(fp, FLOAT_IMG, naxis, naxes, &istat);

	for (ikey = 0; ikey < nkey; ikey++) {
		p = &keys[ikey];
fits_write_key_str(fp, p->key, p->value, p->comment, &istat);
	}

fits_write_key_lng(fp, "CRPIX1P", 1,
	"X axis ref pixel of physical coord", &istat);
fits_write_key_lng(fp, "CRVAL1P", 1,
	"physical coord of X ref pixel", &istat);
fits_write_key_lng(fp, "CDELT1P", 1,
	"X axis increment of physical coord", &istat);
fits_write_key_lng(fp, "CRPIX2P", 1,
	"Y axis ref pixel of physical coord", &istat);
fits_write_key_lng(fp, "CRVAL2P", 1,
	"physical coord of Y ref pixel", &istat);
fits_write_key_lng(fp, "CDELT2P", 1,
	"Y axis increment of physical coord", &istat);

	switch ( com.region_mode ) {
	case REGION_MODE_SKYFITS:
	case REGION_MODE_SKYCIRC:
	case REGION_MODE_SKYREG:
	case REGION_MODE_SKYEXPR:
fits_write_key_str(fp, "RADECSYS", "FK5",
	"celestial coord system", &istat);
fits_write_key_fixdbl(fp, "EQUINOX", 2000.0, 1,
	"Equinox of celestial coord system", &istat);
fits_write_key_str(fp, "MTYPE1", "SKY",
	"DM Keyword: Descriptor name", &istat);
fits_write_key_str(fp, "MFORM1", "X,Y",
	"DM Keyword: Descriptor value", &istat);
fits_write_key_str(fp, "CTYPE1", "RA---TAN",
	"X coordinate projection", &istat);
fits_write_key_fixdbl(fp, "CRPIX1", aste->sky.xcen, 1,
	"X reference pixel", &istat);
fits_write_key_fixdbl(fp, "CRVAL1", com.skyref.alpha, 5,
	"X reference pixel value (deg)", &istat);
fits_write_key_fixdbl(fp, "CDELT1", cdelt1, 7,
	"X pixel scale (deg/pixel)", &istat);
fits_write_key_str(fp, "CTYPE2", "DEC--TAN",
	"Y coordinate projection", &istat);
fits_write_key_fixdbl(fp, "CRPIX2", aste->sky.ycen, 1,
	"Y reference pixel", &istat);
fits_write_key_fixdbl(fp, "CRVAL2", com.skyref.delta, 5,
	"Y reference pixel value (deg)", &istat);
fits_write_key_fixdbl(fp, "CDELT2", cdelt2, 7,
	"Y pixel scale (deg/pixel)", &istat);
fits_write_key_fixdbl(fp, "CROTA2", com.skyref.roll, 5,
	"Sky coord rotation angle (deg)", &istat);
		break;
	case REGION_MODE_DETFITS:
	case REGION_MODE_DETCIRC:
	case REGION_MODE_DETREG:
	case REGION_MODE_DETEXPR:
fits_write_key_str(fp, "MTYPE1", "DET",
	"DM Keyword: Descriptor name", &istat);
fits_write_key_str(fp, "MFORM1", "DETX,DETY",
	"DM Keyword: Descriptor value", &istat);
		break;
	}

	return istat;
}

static fitsfile *
create_arf_file(char *arffile)
{
#define NCOLS	15
	static int tfields = NCOLS;
	static char extname[] = "SPECRESP";
	static char *ttype[NCOLS] = {
		"ENERG_LO", "ENERG_HI",
		"SPECRESP", "RESPERR", "RESPRERR",
		"XRT_EFFAREA", "SHIELD_TRANSMIS", "CONTAMI_TRANSMIS",
		"INDEX", "S", "T", "INPUT", "DETECT", "WEISUM", "RELERR"
	};
	static char *tform[NCOLS] = {
		"1E", "1E",
		"1E", "1E", "1E",
		"1E", "1E", "1E",
		"1J", "1E", "1E", "1E", "1E", "1E", "1E"
    };
	static char *tunit[NCOLS] = {
		"keV", "keV",
		"cm**2", "cm**2", "",
		"cm**2", "", "",
		"", "", "", "count", "count", "count", ""
    };
#undef NCOLS
	static struct {
		char *key, *value, *comment;
	} *p, keys[] = {
 { "TELESCOP",	"SUZAKU",	"mission/satellite name" },
 { "INSTRUME",	"XIS",		"instrument/detector name" },
 { "ARFVERSN",	"1992a",	"OGIP classification of FITS format" },
 { "HDUCLASS",	"OGIP",		"format conforms to OGIP standard" },
 { "HDUCLAS1",	"RESPONSE",	"dataset relates to spectral response" },
 { "HDUVERS1",	"1.0.0",	"Version of family of formats" },
 { "HDUCLAS2",	"SPECRESP",	"dataset contains spectral response" },
 { "HDUVERS2",	"1.1.0",	"Version of format (OGIP memo CAL/GEN/92-002a)" }
};
	int ikey, nkey = sizeof(keys) / sizeof(*keys);

	fitsfile *fp;
	char *k, creator[PIL_LINESIZE];
	int istat;

	istat = 0;
	fits_create_file(&fp, arffile, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: creating ARF file '%s' failed (%d)\n", pname, arffile, istat);
		return NULL;
	}

	istat = create_wmap(fp, com.teldef);
	if ( istat ) {
		anl_msg_error("\
%s: creating WMAP failed for '%s' (%d)\n", pname, arffile, istat);
		return NULL;
	}

	fits_create_tbl(fp, BINARY_TBL,
		com.num_energ, tfields, ttype, tform, tunit, extname, &istat);

	keys[0].value = com.teldef->telescop;
	keys[1].value = com.teldef->instrume;

	for (ikey = 0; ikey < nkey; ikey++) {
		p = &keys[ikey];
		fits_write_key_str(fp, p->key, p->value, p->comment, &istat);
	}
	if ( istat ) {
		anl_msg_error("\
%s: header write error for '%s'\n", pname, arffile);
		return NULL;
	}

	if ( com.enable_pixq ) {
		istat = xisSciWriteKeys(fp, &pixq.sci);
		if ( istat ) return NULL;
	}

	fits_write_key_fixdbl(fp, "GEOMAREA", com.geomarea, 4,
		"geometrical area of XRT (cm2)", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: failed to write ARF key GEOMAREA (%d)\n", pname, istat);
		return NULL;
	}

	fits_write_key_str(fp, "TELDEF", com.teldef->filename,
		"name of the telescope definition file", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: failed to write ARF key TELDEF (%d)\n", pname, istat);
		return NULL;
	}

	fits_write_key_str(fp, k="LEAPFILE", aefits_basename(com.leapfile),
		"name of the leap second file", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: fits_write_key_str('%s') failed (%d)\n", pname, k, istat);
		return NULL;
	}

	sprintf(creator, "%s version %s", anl_task_name(), anl_task_version());
	fits_write_key_str(fp, "CREATOR", creator,
		"software that created this file", &istat);
	if ( istat ) {
		anl_msg_error("\
%s: failed to write ARF key CREATOR (%d)\n", pname, istat);
		return NULL;
	}

	istat = aefits_write_module_history(fp, pname);
	if ( istat ) return NULL;

	istat = write_history(fp);
	if ( istat ) return NULL;

	if (
fits_write_key_null(fp, k="SOURCE_RATIO_REG", NULL, &istat) ||
		 0 ) {
		anl_msg_error("\
%s: fits_write_key_null('%s') failed (%d)\n", pname, k, istat);
		return NULL;
	}

	if ( NULL != com.detmaskmap ) {
		if (
fits_write_key_null(fp, k="MASK_RATIO_CCD", NULL, &istat) ||
fits_write_key_null(fp, k="MASK_RATIO_REG", NULL, &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_write_key_null('%s') failed (%d)\n", pname, k, istat);
			return NULL;
		}
	}

	return fp;
}

#ifdef FAST_CALC_CW_SUM

/* fast version */

static int
calc_cw_sums(int n, struct calc_values_per_energy *enep, double **cw_sums)
{
	PHOTON_MEMORY *pmp;
	double weight, detx, dety, detmask_weight, region_weight, final_weight;
	double *cwp;
	int i, ip, np, ireg, ipos, idetpos;
	struct calc_values_per_region *regp;

	if ( NULL == com.xcp ) {
		for (ireg = 0; ireg < com.num_region; ireg++) {
			regp = &enep->reg[ireg];
			for (i = 0; i < n; i++) {
				cw_sums[ireg][i] = regp->weisum;
			}
		}
		return 0;
	}

	for (ireg = 0; ireg < com.num_region; ireg++) {
		cwp = cw_sums[ireg];
		for (i = 0; i < n; i++) {
			cwp[i] = 0.0;
		}
	}

	np = enep->iphoton;

	for (ip = 0; ip < np; ip++) {

		pmp = &enep->pmem[ip];
		weight = pmp->weight;
		ipos = pmp->ipos;
		idetpos = com.ndetx * pmp->idety + pmp->idetx;
		detx = pmp->idetx;
		dety = pmp->idety;

		for (i = 0; i < n; i++) {
			com.cw_transmis[i] = xis_contami(com.xcp, com.cw_enes[i],
				pmp->photon_time, detx, dety, NULL, NULL);
		}

		if ( NULL != com.detmaskmap ) {
			detmask_weight = com.detmaskmap[idetpos];
		} else {
			detmask_weight = 1.0;
		}

		for (ireg = 0; ireg < com.num_region; ireg++) {
			region_weight = com.regmap[ireg][ipos];
			if ( 0.0 < region_weight ) {
				region_weight *= weight;
				/* skip testing detmask here,
				   because already considered in _ana() */
				region_weight *= detmask_weight;
				cwp = cw_sums[ireg];
				for (i = 0; i < n; i++) {
					final_weight = region_weight * com.cw_transmis[i];
					cwp[i] += final_weight;
				}
			}
		}
	}

	return 0;
}

static int
write_arf_col(int idx)
{
	long irow;
	fitsfile *fp;
	int i, n, ie, ie_base, ireg, istat, icol;
	struct calc_values_per_energy *e1, *e2;
	struct calc_values_per_region *r1, *r2;
	double Ecen1, Ecen2, Ecen2_1;
	double input1, input2;
	double cw_sum1, cw_sum2;
	double cw_frac, cw_frac1, cw_frac2;
	double weifrac, weifrac1, weifrac2;
	double energy, energ_lo, energ_hi, specresp, resperr, resprerr;
	double gs, shield_transmis, contami_transmis, xrt_effarea;
	double s, t, input, detect, weisum, relerr;

	if ( idx <= 0 || com.num_calc <= idx ) {
		return 0;
	}

	idx--;		/* decrement idx for backward compatibility */
	e1 = com.ene + idx;
	e2 = com.ene + idx + 1;
	input1 = e1->input;
	input2 = e2->input;
	Ecen1 = com.calc[idx].Ecen;
	Ecen2 = com.calc[idx+1].Ecen;
	Ecen2_1 = Ecen2 - Ecen1;

	istat = 0;

	for (ie = ie_base = n = 0; ie < com.num_energ; ie++) {
		energy = com.energ_cen[ie];
		if ( energy < Ecen1 ) {
			ie_base = ie + 1;
			continue;
		} else if ( Ecen2 < energy ) {
			break;
		}
		com.cw_enes[n] = energy;
		n++;
	}

	calc_cw_sums(n, e1, com.cw_sums1);
	calc_cw_sums(n, e2, com.cw_sums2);

	for (i = 0; i < n; i++) {

		ie = ie_base + i;
		energy = com.energ_cen[ie];
		energ_lo = com.energ_lo[ie];
		energ_hi = com.energ_hi[ie];
		shield_transmis = (*com.shield_transmis_func)(energy);
		gs = com.geomarea * shield_transmis;
		s = (Ecen2 - energy) / Ecen2_1;
		t = 1.0 - s;

		for (ireg = 0; ireg < com.num_region; ireg++) {
			fp = com.arffp[ireg];
			r1 = &e1->reg[ireg];
			r2 = &e2->reg[ireg];
			cw_sum1 = com.cw_sums1[ireg][i];
			cw_sum2 = com.cw_sums2[ireg][i];

			cw_frac1 = cw_sum1 / input1;
			cw_frac2 = cw_sum2 / input2;
			cw_frac  = s * cw_frac1 + t * cw_frac2;
			if ( cw_frac < 0.0 ) cw_frac = 0.0;
			specresp = gs * cw_frac;

			weifrac1 = r1->weisum / input1;
			weifrac2 = r2->weisum / input2;
			weifrac  = s * weifrac1 + t * weifrac2;
			if ( weifrac < 0.0 ) weifrac = 0.0;
			xrt_effarea = com.geomarea * weifrac;

			if ( 0.0 < weifrac ) {
				contami_transmis = cw_frac / weifrac;
			} else {
				contami_transmis = 0.0;
			}

			resperr = s * cw_frac1 * r1->relerr + t * cw_frac2 * r2->relerr;
			resperr *= com.geomarea;
			if ( 0 < specresp ) {
				resprerr = resperr / specresp;
			} else {
				resprerr = 0.0;
			}

			input  = s * input1 + t * input2;
			detect = s * r1->detect + t * r2->detect;
			weisum = s * r1->weisum + t * r2->weisum;
			relerr = s * r1->relerr + t * r2->relerr;

/* output ARF */
			icol = 0;
			irow = ie + 1;
			if (
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &energ_lo, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &energ_hi, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &specresp, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &resperr, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &resprerr, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &xrt_effarea, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &shield_transmis, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &contami_transmis, &istat) ||
fits_write_col_int(fp, ++icol, irow, 1, 1, &idx, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &s, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &t, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &input, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &detect, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &weisum, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &relerr, &istat) ||
				 0 ) {
				anl_msg_error("\
%s: fits_write_col() failed at icol=%d, irow=%ld, for '%s' (%d)\n",
					pname, icol, irow, com.arffile[ireg], istat);
				return istat;
			}
		}
	}

	return istat;
}

#else	/* ! defined(FAST_CALC_CW_SUM) */

/* slow version */

static int
calc_cw_sum(double energy, struct calc_values_per_energy *enep)
{
	PHOTON_MEMORY *pmp;
	double weight, detmask_weight, region_weight, contami_transmis;
	int ip, np, ireg, ipos, idetpos;
	struct calc_values_per_region *regp;

	if ( NULL == com.xcp ) {
		for (ireg = 0; ireg < com.num_region; ireg++) {
			regp = &enep->reg[ireg];
			regp->cw_sum = regp->weisum;
		}
		return 0;
	}

	for (ireg = 0; ireg < com.num_region; ireg++) {
		regp = &enep->reg[ireg];
		regp->cw_sum = 0.0;
	}

	np = enep->iphoton;

	for (ip = 0; ip < np; ip++) {

		pmp = &enep->pmem[ip];
		weight = pmp->weight;
		ipos = pmp->ipos;
		idetpos = com.ndetx * pmp->idety + pmp->idetx;
		contami_transmis = xis_contami(com.xcp, energy,
			pmp->photon_time, pmp->idetx, pmp->idety, NULL, NULL);

		if ( NULL != com.detmaskmap ) {
			detmask_weight = com.detmaskmap[idetpos];
		} else {
			detmask_weight = 1.0;
		}

		for (ireg = 0; ireg < com.num_region; ireg++) {
			regp = &enep->reg[ireg];
			region_weight = com.regmap[ireg][ipos];
			if ( 0.0 < region_weight ) {
				region_weight *= weight;
				/* skip testing detmask here,
				   because already considered in _ana() */
				region_weight *= detmask_weight;
				region_weight *= contami_transmis;
				regp->cw_sum += region_weight;
			}
		}
	}

	return 0;
}

static int
write_arf_col(int idx)
{
	long irow;
	fitsfile *fp;
	int ie, ireg, istat, icol;
	struct calc_energy *calc = com.calc;
	struct calc_values_per_energy *e1, *e2;
	struct calc_values_per_region *r1, *r2;
	double Ecen1, Ecen2, Ecen2_1;
	double input1, input2;
	double cw_frac, cw_frac1, cw_frac2;
	double weifrac, weifrac1, weifrac2;
	double energy, energ_lo, energ_hi, specresp, resperr, resprerr;
	double gs, shield_transmis, contami_transmis, xrt_effarea;
	double s, t, input, detect, cw_sum, weisum, relerr;

	if ( idx <= 0 || com.num_calc <= idx ) {
		return 0;
	}

	idx--;		/* decrement idx for backward compatibility */
	e1 = com.ene + idx;
	e2 = com.ene + idx + 1;
	input1 = e1->input;
	input2 = e2->input;
	Ecen1 = com.calc[idx].Ecen;
	Ecen2 = com.calc[idx+1].Ecen;
	Ecen2_1 = Ecen2 - Ecen1;

	istat = 0;

	for (ie = 0; ie < com.num_energ; ie++) {

		energy = com.energ_cen[ie];
		if ( energy < Ecen1 ) {
			continue;
		} else if ( Ecen2 < energy ) {
			break;
		}

		energ_lo = com.energ_lo[ie];
		energ_hi = com.energ_hi[ie];
		shield_transmis = (*com.shield_transmis_func)(energy);
		gs = com.geomarea * shield_transmis;
		s = (Ecen2 - energy) / Ecen2_1;
		t = 1.0 - s;

		calc_cw_sum(energy, e1);
		calc_cw_sum(energy, e2);

		for (ireg = 0; ireg < com.num_region; ireg++) {
			fp = com.arffp[ireg];
			r1 = &e1->reg[ireg];
			r2 = &e2->reg[ireg];

			cw_frac1 = r1->cw_sum / e1->input;
			cw_frac2 = r2->cw_sum / e2->input;
			cw_frac  = s * cw_frac1 + t * cw_frac2;
			if ( cw_frac < 0.0 ) cw_frac = 0.0;
			specresp = gs * cw_frac;

			weifrac1 = r1->weisum / input1;
			weifrac2 = r2->weisum / input2;
			weifrac  = s * weifrac1 + t * weifrac2;
			if ( weifrac < 0.0 ) weifrac = 0.0;
			xrt_effarea = com.geomarea * weifrac;

			if ( 0.0 < weifrac ) {
				contami_transmis = cw_frac / weifrac;
			} else {
				contami_transmis = 0.0;
			}

			resperr = s * cw_frac1 * r1->relerr + t * cw_frac2 * r2->relerr;
			resperr *= com.geomarea;
			if ( 0 < specresp ) {
				resprerr = resperr / specresp;
			} else {
				resprerr = 0.0;
			}

			input  = s * input1 + t * input2;
			detect = s * r1->detect + t * r2->detect;
			weisum = s * r1->weisum + t * r2->weisum;
			relerr = s * r1->relerr + t * r2->relerr;

/* output ARF */
			icol = 0;
			irow = ie + 1;
			if (
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &energ_lo, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &energ_hi, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &specresp, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &resperr, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &resprerr, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &xrt_effarea, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &shield_transmis, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &contami_transmis, &istat) ||
fits_write_col_int(fp, ++icol, irow, 1, 1, &idx, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &s, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &t, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &input, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &detect, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &weisum, &istat) ||
fits_write_col_dbl(fp, ++icol, irow, 1, 1, &relerr, &istat) ||
				 0 ) {
				anl_msg_error("\
%s: fits_write_col() failed at icol=%d, irow=%ld, for '%s' (%d)\n",
					pname, icol, irow, com.arffile[ireg], istat);
				return istat;
			}
		}
	}

	return istat;
}

#endif	/* FAST_CALC_CW_SUM */

static int
write_arf_wmap(fitsfile *fp, int ireg)
{
	char *k;
	double *wmap;
	int istat, hdutype;
	double source_ratio_reg, mask_ratio_ccd, mask_ratio_reg;

	istat = 0;

	fits_movabs_hdu(fp, 1, &hdutype, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: move to primary array failed (%d)\n", pname, istat);
		return istat;
	}
	wmap = com.wmap[ireg];
	fits_write_img_dbl(fp, 1, 1, com.region_nx * com.region_ny, wmap, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: writing wmap failed (%d)\n", pname, istat);
		return istat;
	}

	fits_movabs_hdu(fp, 2, &hdutype, &istat);
	if ( istat ) {
		anl_msg_error("\
%s: move to 1st extension failed (%d)\n", pname, istat);
		return istat;
	}

	source_ratio_reg = com.sum_source_reg[ireg] / com.n_photon;
	if ( 0.1 <= source_ratio_reg ) {
fits_modify_key_fixdbl(fp, k="SOURCE_RATIO_REG", source_ratio_reg, 6, "\
source image ratio inside selected region", &istat);
	} else {
fits_modify_key_dbl(fp, k="SOURCE_RATIO_REG", source_ratio_reg, 6, "\
source image ratio inside selected region", &istat);
	}
	if ( istat ) {
		anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, k, istat);
		return istat;
	}

	if ( NULL != com.detmaskmap ) {
		mask_ratio_ccd = com.sum_detmask / com.tot_ccd_pix;
		mask_ratio_reg = com.sum_detmask_reg[ireg] / com.tot_ccd_pix_reg[ireg];
		if (
fits_modify_key_fixdbl(fp, k="MASK_RATIO_CCD", mask_ratio_ccd, 6, "\
detmask ratio in whole CCD area", &istat) ||
fits_modify_key_fixdbl(fp, k="MASK_RATIO_REG", mask_ratio_reg, 6, "\
detmask ratio inside selected region", &istat) ||
			 0 ) {
			anl_msg_error("\
%s: fits_modify_key('%s') failed (%d)\n", pname, k, istat);
			return istat;
		}
	}

	return istat;
}

void
SimASTE_XISarfPhotonGen_startup(int *status)
{
	com.rmffile = com.o_rmffile;
	com.contamifile  = com.o_contamifile;
	com.badcolumfile = com.o_badcolumfile;
	com.calmaskfile  = com.o_calmaskfile;
	com.mjdrefi = aste_mjdrefi();
	com.mjdreff = aste_mjdreff();

	*status = ANL_OK;
}

void
SimASTE_XISarfPhotonGen_com(int *status)
{
	int i;
	char *k, buf[PIL_LINESIZE];

	unsigned int uint_min = 0;
	unsigned int uint_max = 4294967295u;
	double pixq_min = 0.0;
	double pixq_max = 524287.0;
	double pixq_and = 0.0;
	double pixq_eql = 0.0;

	if ( *status ) {			/* ftools */

		if ( PILGetBool(k="clobber", &com.clobber) ) {
			goto quit;
		}

		if ( PILGetString(k="pointing", buf) ) goto quit;
		if ( 0 == CLstricmp("AUTO", buf) ) {
			com.pointing = SKYREF_MODE_AUTO;
		} else if ( 0 == CLstricmp("USER", buf) ) {
			com.pointing = SKYREF_MODE_USER;
			if ( PILGetReal(k="ref_alpha", &com.skyref.alpha) ||
				 PILGetReal(k="ref_delta", &com.skyref.delta) ||
				 PILGetReal(k="ref_roll",  &com.skyref.roll) ) {
				goto quit;
			}
		}

		if ( PILGetBool(k="aberration", &com.aberration) ||
			 PILGetBool(k="aperture_cosine", &com.aperture_cosine) ||
			 PILGetReal(k="minangle", &com.minAngle) ||
			 PILGetReal(k="maxangle", &com.maxAngle) ||
			 PILGetReal(k="minradius", &com.minRadius) ||
			 PILGetReal(k="maxradius", &com.maxRadius) ) {
			goto quit;
		}

		if ( PILGetString(k="source_mode", buf) ) goto quit;
		if ( 0 == CLstricmp("SKYFITS", buf) ) {
			com.source_mode = SOURCE_MODE_SKYFITS;
			if ( PILGetFname(k="source_image", com.source_image) ) {
				goto quit;
			}
		} else if ( 0 == CLstricmp("DETFITS", buf) ) {
			com.source_mode = SOURCE_MODE_DETFITS;
			if ( PILGetFname(k="source_image", com.source_image) ) {
				goto quit;
			}
		} else if ( 0 == CLstricmp("J2000", buf) ) {
			com.source_mode = SOURCE_MODE_J2000;
			if ( PILGetReal(k="source_ra", &com.source_ra) ||
				 PILGetReal(k="source_dec", &com.source_dec) ) {
				goto quit;
			}
		} else if ( 0 == CLstricmp("SKYXY", buf) ) {
			com.source_mode = SOURCE_MODE_SKYXY;
			if ( PILGetReal(k="source_x", &com.source_x) ||
				 PILGetReal(k="source_y", &com.source_y) ) {
				goto quit;
			}
		} else if ( 0 == CLstricmp("DETXY", buf) ) {
			com.source_mode = SOURCE_MODE_DETXY;
			if ( PILGetReal(k="source_x", &com.source_x) ||
				 PILGetReal(k="source_y", &com.source_y) ) {
				goto quit;
			}
		} else if ( 0 == CLstricmp("UNIFORM", buf) ) {
			com.source_mode = SOURCE_MODE_UNIFORM;
			if ( PILGetReal(k="source_rmin", &com.source_rmin) ||
				 PILGetReal(k="source_rmax", &com.source_rmax) ) {
				goto quit;
			}
		} else {
			anl_msg_error("\
%s: unknown source_mode=%s\n", pname, buf);
			goto quit;
		}

		if ( PILGetInt(k="num_region", &com.num_region) ) goto quit;
		if ( MAX_REGION < com.num_region ) {
			anl_msg_error("\
%s: too many num_region=%d\n", pname, com.num_region);
			goto quit;
		}

		if ( PILGetString(k="region_mode", buf) ) goto quit;
		if ( 0 == CLstricmp("SKYFITS", buf) ) {
			com.region_mode = REGION_MODE_SKYFITS;
			for (i = 0; i < com.num_region; i++) {
				sprintf(buf, "regfile%d", i+1);
				if ( PILGetFname(k=buf, com.regfile[i]) ) goto quit;
				sprintf(buf, "arffile%d", i+1);
				if ( PILGetFname(k=buf, com.arffile[i]) ) goto quit;
			}
		} else if ( 0 == CLstricmp("DETFITS", buf) ) {
			com.region_mode = REGION_MODE_DETFITS;
			for (i = 0; i < com.num_region; i++) {
				sprintf(buf, "regfile%d", i+1);
				if ( PILGetFname(k=buf, com.regfile[i]) ) goto quit;
				sprintf(buf, "arffile%d", i+1);
				if ( PILGetFname(k=buf, com.arffile[i]) ) goto quit;
			}
		} else if ( 0 == CLstricmp("SKYCIRC", buf) ) {
			com.region_mode = REGION_MODE_SKYCIRC;
			for (i = 0; i < com.num_region; i++) {
				sprintf(buf, "region_x%d", i+1);
				if ( PILGetReal(k=buf, &com.region_x[i]) ) goto quit;
				sprintf(buf, "region_y%d", i+1);
				if ( PILGetReal(k=buf, &com.region_y[i]) ) goto quit;
				sprintf(buf, "region_rmin%d", i+1);
				if ( PILGetReal(k=buf, &com.region_rmin[i]) ) goto quit;
				sprintf(buf, "region_rmax%d", i+1);
				if ( PILGetReal(k=buf, &com.region_rmax[i]) ) goto quit;
				sprintf(buf, "arffile%d", i+1);
				if ( PILGetFname(k=buf, com.arffile[i]) ) goto quit;
			}
		} else if ( 0 == CLstricmp("DETCIRC", buf) ) {
			com.region_mode = REGION_MODE_DETCIRC;
			for (i = 0; i < com.num_region; i++) {
				sprintf(buf, "region_x%d", i+1);
				if ( PILGetReal(k=buf, &com.region_x[i]) ) goto quit;
				sprintf(buf, "region_y%d", i+1);
				if ( PILGetReal(k=buf, &com.region_y[i]) ) goto quit;
				sprintf(buf, "region_rmin%d", i+1);
				if ( PILGetReal(k=buf, &com.region_rmin[i]) ) goto quit;
				sprintf(buf, "region_rmax%d", i+1);
				if ( PILGetReal(k=buf, &com.region_rmax[i]) ) goto quit;
				sprintf(buf, "arffile%d", i+1);
				if ( PILGetFname(k=buf, com.arffile[i]) ) goto quit;
			}
		} else if ( 0 == CLstricmp("SKYREG", buf) ) {
			com.region_mode = REGION_MODE_SKYREG;
			for (i = 0; i < com.num_region; i++) {
				sprintf(buf, "regfile%d", i+1);
				if ( PILGetFname(k=buf, com.regfile[i]) ) goto quit;
				sprintf(buf, "arffile%d", i+1);
				if ( PILGetFname(k=buf, com.arffile[i]) ) goto quit;
			}
		} else if ( 0 == CLstricmp("DETREG", buf) ) {
			com.region_mode = REGION_MODE_DETREG;
			for (i = 0; i < com.num_region; i++) {
				sprintf(buf, "regfile%d", i+1);
				if ( PILGetFname(k=buf, com.regfile[i]) ) goto quit;
				sprintf(buf, "arffile%d", i+1);
				if ( PILGetFname(k=buf, com.arffile[i]) ) goto quit;
			}
		} else if ( 0 == CLstricmp("SKYEXPR", buf) ) {
			com.region_mode = REGION_MODE_SKYEXPR;
			for (i = 0; i < com.num_region; i++) {
				sprintf(buf, "regfile%d", i+1);
				if ( PILGetFname(k=buf, com.regfile[i]) ) goto quit;
				sprintf(buf, "arffile%d", i+1);
				if ( PILGetFname(k=buf, com.arffile[i]) ) goto quit;
			}
		} else if ( 0 == CLstricmp("DETEXPR", buf) ) {
			com.region_mode = REGION_MODE_DETEXPR;
			for (i = 0; i < com.num_region; i++) {
				sprintf(buf, "regfile%d", i+1);
				if ( PILGetFname(k=buf, com.regfile[i]) ) goto quit;
				sprintf(buf, "arffile%d", i+1);
				if ( PILGetFname(k=buf, com.arffile[i]) ) goto quit;
			}
		} else {
			anl_msg_error("\
%s: unknown region_mode=%s\n", pname, buf);
			goto quit;
		}

		com.num_photon = 0;
		com.accuracy = 0.0;
		if ( PILGetString(k="limit_mode", buf) ) goto quit;
		if ( 0 == CLstricmp("MIX", buf) || 0 == CLstricmp("MIXED", buf) ) {
			com.limit_mode = LIMIT_MODE_MIXED;
			if ( PILGetInt(k="num_photon", &com.num_photon) ) goto quit;
			if ( PILGetReal(k="accuracy", &com.accuracy) ) goto quit;
		} else if ( 0 == CLstricmp("NUM_PHOTON", buf) ) {
			com.limit_mode = LIMIT_MODE_NUM_PHOTON;
			if ( PILGetInt(k="num_photon", &com.num_photon) ) goto quit;
		} else if ( 0 == CLstricmp("ACCURACY", buf) ) {
			com.limit_mode = LIMIT_MODE_ACCURACY;
			if ( PILGetReal(k="accuracy", &com.accuracy) ) goto quit;
		} else {
			anl_msg_error("\
%s: unknown limit_mode=%s\n", pname, buf);
			goto quit;
		}

		if ( PILGetFname(k="phafile", com.phafile) ) goto quit;
		if ( PILGetFname(k="detmask", com.detmask) ) goto quit;
		if ( PILGetFname(k="gtifile", com.gtifile) ) goto quit;
		if ( 0 == CLstricmp("none", com.gtifile) ) {
			if ( PILGetString(k="date_obs", com.date_obs) ) goto quit;
		}
		if ( PILGetFname(k="attitude", com.attitude) ) goto quit;
		if ( 0 == CLstricmp("none", com.attitude) ) {
			if ( PILGetReal(k="ea1", &com.ea_deg.phi) ||
				 PILGetReal(k="ea2", &com.ea_deg.theta) ||
				 PILGetReal(k="ea3", &com.ea_deg.psi) ) {
				goto quit;
			}
			com.ea.phi = com.ea_deg.phi * DEG2RAD;
			com.ea.theta = com.ea_deg.theta * DEG2RAD;
			com.ea.psi = com.ea_deg.psi * DEG2RAD;
		}
		if ( PILGetFname(k="rmffile", com.rmffile) ) goto quit;
		if ( PILGetFname(k="contamifile", com.contamifile) ) goto quit;
		if ( PILGetFname(k="estepfile", com.estepfile) ) goto quit;

		if ( PILGetBool(k="enable_pixq", &com.enable_pixq) ) goto quit;
		if ( com.enable_pixq ) {
			if ( PILGetFname(k="hotpixfiles", com.hotpixfiles) ||
				 PILGetFname(k="badcolumfile", com.badcolumfile) ||
				 PILGetFname(k="calmaskfile", com.calmaskfile) ||
				 PILGetReal (k="pixq_min", &pixq_min) ||
				 PILGetReal (k="pixq_max", &pixq_max) ||
				 PILGetReal (k="pixq_and", &pixq_and) ||
				 PILGetReal (k="pixq_eql", &pixq_eql) ||
				 0 ) {
				goto quit;
			}
			if ( pixq_min < uint_min || uint_max < pixq_min ||
				 pixq_max < uint_min || uint_max < pixq_max ||
				 pixq_and < uint_min || uint_max < pixq_and ||
				 pixq_eql < uint_min || uint_max < pixq_eql ||
				 0 ) {
				anl_msg_error("\
%s: 'pixq_***' parameter out of range\n", pname);
				*status = ANL_QUIT;
				return;
			}

			com.pixq_min = (unsigned int)pixq_min;
			com.pixq_max = (unsigned int)pixq_max;
			com.pixq_and = (unsigned int)pixq_and;
			com.pixq_eql = (unsigned int)pixq_eql;
		}

		*status = ANL_OK;;
		return;

	quit:
		anl_msg_error("\
%s: PILGet('%s') failed\n", pname, k);
		*status = ANL_QUIT;
		return;
	}

	*status = ANL_OK;
}

void
SimASTE_XISarfPhotonGen_init(int *status)
{
	int used;

	EvsDef("SimASTE:APERTURE_COSINE");

	BnkGet("SimASTE:TELESCOP:PTR", sizeof(com.telescop), &used, &com.telescop);
	BnkGet("SimASTE:INSTRUME:PTR", sizeof(com.instrume), &used, &com.instrume);

#define get_caldb_file(CODE,O_FILE)	aste_caldb_find(com.instrume, CODE, O_FILE)
	com.contamifile = get_caldb_file("CONTAMI_TRANS", com.o_contamifile);
	if ( NULL == com.contamifile ) goto quit;
	com.rmffile = get_caldb_file("SPECRESP MATRIX", com.o_rmffile);
	if ( NULL == com.rmffile ) goto quit;
	if ( com.enable_pixq ) {
		com.badcolumfile = get_caldb_file("BADPIX", com.o_badcolumfile);
		if ( NULL == com.badcolumfile ) goto quit;
		com.calmaskfile = get_caldb_file("CALMASK", com.o_calmaskfile);
		if ( NULL == com.calmaskfile ) goto quit;
	}
#undef get_caldb_file

	BnkGet("SimASTE:TELDEF", sizeof(com.teldef), &used, &com.teldef);
	BnkGet("SimASTE:LEAPFILE:PTR", sizeof(com.leapfile), &used, &com.leapfile);

	com.r2min = com.minRadius * com.minRadius;
	com.r2max = com.maxRadius * com.maxRadius;
	com.geomarea = M_PI * (com.r2max - com.r2min) / 100;
	com.geomarea *= (com.maxAngle - com.minAngle) / 360;
	if ( SOURCE_MODE_UNIFORM == com.source_mode ) {
		com.cos_source_rmin = cos(com.source_rmin * ARCMIN2RAD);
		com.cos_source_mami = cos(com.source_rmax * ARCMIN2RAD)
							- com.cos_source_rmin;
	}

	if ( SOURCE_MODE_SKYFITS == com.source_mode ||
		 SOURCE_MODE_DETFITS == com.source_mode ) {
		if ( read_source_image(com.source_image) ) {
			goto quit;
		}
	}

	if ( 0 != read_detmask(com.detmask) ) {
		goto quit;
	}

	if ( 0 != read_gtifile(com.gtifile) ) {
		goto quit;
	}

	if ( 0 != read_attitude(com.attitude) ) {	/* read attitude after gti */
		goto quit;
	}

	if ( SKYREF_MODE_USER == com.pointing ) {
		/* use ref_alpha, ref_delta, ref_roll */
	} else if ( SKYREF_MODE_AUTO == com.pointing ) {
		/* use RA_NOM, DEC_NOM in attitude file */
		com.skyref.alpha = com.ra_nom;
		com.skyref.delta = com.dec_nom;
		com.skyref.roll = 0.0;
	} else {
		anl_msg_error("\
%s: pointing mode not supported\n", pname);
		goto quit;
	}
	BnkPut("SimASTE:SKYREF", sizeof(com.skyref), &com.skyref);

	if ( 0 != randomize_time() ) {	/* randomize time after reading attitude */
		goto quit;
	}

	if ( 0 != read_energ_lo_hi(com.rmffile) ) {
		goto quit;
	}

	if ( 0 != read_contamifile(com.contamifile) ) {
		goto quit;
	}

	if ( 0 != read_estepfile(com.estepfile) ) {
		goto quit;
	}

	if ( 0 != decide_energy_step() ) {
		goto quit;
	}

	if ( 0 != allocate_ene() ) {
		goto quit;
	}

	if ( 0 != eval_pixq() ) {
		goto quit;
	}

	if ( 0 != make_regmap() ) {
		goto quit;
	}

	show_parameter("%s:  *** show parameter ***");

	if ( ANL_MSG_DEBUG <= anl_msg_chatter() ) {
		show_energy_step();
	}

	if ( 1 == com.gti_nt ) {			/* fixed time */
		BnkPut("SimASTE:PHOTON_TIME", sizeof(double), &com.photon_time);
		BnkPut("SimASTE:EULER", sizeof(AtEulerAng), &com.ea);
	} else if ( NULL == com.att.ea ) {	/* fixed attitude */
		BnkPut("SimASTE:EULER", sizeof(AtEulerAng), &com.ea);
	}

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
SimASTE_XISarfPhotonGen_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfPhotonGen_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfPhotonGen_ana(int nevent, int eventid, int *status)
{
#define teldef			com.teldef
#define skyref			com.skyref
#define source_ra		com.source_ra
#define source_dec		com.source_dec
#define source_x		com.source_x
#define source_y		com.source_y
#define mjdrefi			com.mjdrefi
#define mjdreff			com.mjdreff

	int i;
	unsigned int ix, iy, ipos;	/* non negative */
	struct calc_values_per_energy *enep;
	struct calc_values_per_region *regp;
	double Ecen, Ewid, input, detect, region_weight;
	double photon_time, detx, dety, xrtx, xrty, xy[2], alpha, delta, x, y;
	double radius, angle, x_in, y_in, theta, phi;
	AtEulerAng *ea;

	if ( com.num_calc <= com.calc_index ) {
		goto quit;
	}

	enep = &com.ene[com.calc_index];
	input = enep->input;

/* check number of photons */
	if ( 0 < com.num_photon && com.num_photon <= input ) {
		for (i = 0; i < com.num_region; i++) {
			regp = &enep->reg[i];
			detect = regp->detect;
			if ( 0.0 == detect || input == detect ) {
				regp->relerr = 1.0;
			} else {
				regp->relerr = sqrt( (input - detect) / input / detect );
			}
		}
		write_arf_col(com.calc_index);
		com.calc_index++;
		if ( com.num_calc <= com.calc_index ) {
			goto quit;
		}
		enep = &com.ene[com.calc_index];
		input = enep->input;
	}

/* set energy */
	Ecen = com.calc[com.calc_index].Ecen;
	Ewid = com.calc[com.calc_index].Ewid;

/* print simulating energy */
	if ( 0.0 == input ) {
		if ( 0.0 < Ewid ) {
			anl_msg_info("\
   ...simulating in %f - %f keV (%d/%d)\n",
				Ecen - Ewid/2, Ecen + Ewid/2, com.calc_index+1, com.num_calc);
		} else {
			anl_msg_info("\
   ...simulating at %f keV (%d/%d)\n", Ecen, com.calc_index+1, com.num_calc);
		}
	}

/* randomize energy, if needed */
	if ( 0.0 < Ewid ) {
		Ecen += Ewid * (aste_drndts() - 0.5);
	}

/* set time */
	if ( 1 == com.gti_nt ) {	/* fixed time */
		photon_time = com.photon_time;
		ea = &com.ea;
	} else {
		i = (int)fmod(input, (double)com.gti_nt);
		photon_time = com.gti_t[i];
		BnkfPutM("SimASTE:PHOTON_TIME", sizeof(double), &photon_time);
		if ( NULL == com.att.ea ) {	/* fixed attitude */
			ea = &com.ea;
		} else {
			ea = &com.att.ea[i];
			BnkfPutM("SimASTE:EULER", sizeof(AtEulerAng), ea);
		}
	}

/* convert coodinates */
	switch ( com.source_mode ) {
	case SOURCE_MODE_SKYFITS:
		aste_drndtsn(2, xy);
		Hrndm2(xy, com.hm2);
		pixel_to_ecs(&com.source_kp, xy[0], xy[1], &alpha, &delta);
		if ( com.aberration ) {
			aste_inv_aberration(photon_time, mjdrefi, mjdreff, &alpha, &delta);
		}
		aste_ecs2det(teldef, ea, alpha, delta, &detx, &dety);
		aste_det2xrt(teldef, detx, dety, &xrtx, &xrty);
		aste_xrt_rec2pol(teldef, xrtx, xrty, &theta, &phi);
		break;

	case SOURCE_MODE_DETFITS:
		aste_drndtsn(2, xy);
		Hrndm2(xy, com.hm2);
		detx = xy[0];
		dety = xy[1];
		aste_det2xrt(teldef, detx, dety, &xrtx, &xrty);
		aste_xrt_rec2pol(teldef, xrtx, xrty, &theta, &phi);
		break;

	case SOURCE_MODE_J2000:
		if ( com.aberration ) {
			alpha = source_ra;
			delta = source_dec;
			aste_inv_aberration(photon_time, mjdrefi, mjdreff, &alpha, &delta);
			aste_ecs2det(teldef, ea, alpha, delta, &detx, &dety);
		} else {
			aste_ecs2det(teldef, ea, source_ra, source_dec, &detx, &dety);
		}
		aste_det2xrt(teldef, detx, dety, &xrtx, &xrty);
		aste_xrt_rec2pol(teldef, xrtx, xrty, &theta, &phi);
		break;

	case SOURCE_MODE_SKYXY:
		aste_sky2ecs(teldef, &skyref, source_x, source_y, &alpha, &delta);
		if ( com.aberration ) {
			aste_inv_aberration(photon_time, mjdrefi, mjdreff, &alpha, &delta);
		}
		aste_ecs2det(teldef, ea, alpha, delta, &detx, &dety);
		aste_det2xrt(teldef, detx, dety, &xrtx, &xrty);
		aste_xrt_rec2pol(teldef, xrtx, xrty, &theta, &phi);
		break;

	case SOURCE_MODE_DETXY:
		detx = source_x;
		dety = source_y;
		aste_det2xrt(teldef, detx, dety, &xrtx, &xrty);
		aste_xrt_rec2pol(teldef, xrtx, xrty, &theta, &phi);
		break;

	case SOURCE_MODE_UNIFORM:
/*
   2006-08-06, Y.ISHISAKI
   To generate a photon which follows f(t) = sin(t) (t0 <= t <= t1),
   (cos(t0) - cos(t1)) * y = F(t) = \int_t0^t sin(t') dt' = cos(t0) - cos(t),
   t = F^{-1}(y) = acos( cos(t0) + y (cos(t1) - cos(t0)) )
   We need to consider aperture cosine factor, elsewhere.
*/
		theta = acos(com.cos_source_rmin + aste_drndts()*com.cos_source_mami);
		theta *= RAD2ARCMIN;
		phi = 360.0 * aste_drndts();
		aste_xrt_pol2rec(teldef, theta, phi, &xrtx, &xrty);
		aste_xrt2det(teldef, xrtx, xrty, &detx, &dety);
		break;

	default:
		anl_msg_error("\
%s: Illegal source mode\n", pname);
		*status = ANL_ERROR;
		return;
	}

/* count up number of input photons */
	enep->input++;
	com.n_photon += 1.0;
	BnkfPutM("SimASTE:N_PHOTON", sizeof(com.n_photon), &com.n_photon);

/* convert coodinates to find location in the integration region */
	switch ( com.region_mode ) {
	case REGION_MODE_SKYFITS:
	case REGION_MODE_SKYCIRC:
	case REGION_MODE_SKYREG:
	case REGION_MODE_SKYEXPR:
		if ( com.aberration ) {
			aste_det2ecs(teldef, ea, detx, dety, &alpha, &delta);
			aste_cor_aberration(photon_time, mjdrefi, mjdreff, &alpha, &delta);
			aste_ecs2sky(teldef, &skyref, alpha, delta, &x, &y);
		} else {
			aste_det2sky(teldef, ea, &skyref, detx, dety, &x, &y);
		}
		break;
	case REGION_MODE_DETFITS:
	case REGION_MODE_DETCIRC:
	case REGION_MODE_DETREG:
	case REGION_MODE_DETEXPR:
		x = detx;
		y = dety;
		break;
	default:
		;
	}
	if ( x < 0.5 || com.region_nx + 0.5 <= x ) {
		ix = com.region_nx;		/* invalid number */
	} else {
		ix = (unsigned int)(x + 0.5) - 1;
	}
	if ( y < 0.5 || com.region_ny + 0.5 <= y ) {
		iy = com.region_ny;		/* invalid number */
	} else {
		iy = (unsigned int)(y + 0.5) - 1;
	}
	if ( ix < com.region_nx && iy < com.region_ny ) {
		ipos = com.region_nx * iy + ix;
		for (i = 0; i < com.num_region; i++) {
			region_weight = com.regmap[i][ipos];
			if ( 0.0 < region_weight ) {
				com.sum_source_reg[i] += region_weight;
			}
		}
	}

/* consider aperture decrease by cosine factor */
	if ( com.aperture_cosine ) {
		if ( cos(theta*ARCMIN2RAD) < aste_drndts() ) {
			EvsfSetM("SimASTE:APERTURE_COSINE");
			*status = ANL_SKIP;
			return;
		}
	}

/* randomize x_in, y_in */
	radius = sqrt( aste_drndts() * ( com.r2max - com.r2min ) + com.r2min );
	angle = com.minAngle + (com.maxAngle - com.minAngle) * aste_drndts();
	angle = angle * DEG2RAD;
	x_in = radius * cos(angle);
	y_in = radius * sin(angle);

/* BNKPUT data */
	BnkfPutM("SimASTE:PHOTON_ENERGY", sizeof(Ecen), &Ecen);
	BnkfPutM("SimASTE:XRTINtheta", sizeof(theta), &theta);
	BnkfPutM("SimASTE:XRTINphi", sizeof(phi), &phi);
	BnkfPutM("SimASTE:XRTINX", sizeof(x_in), &x_in);
	BnkfPutM("SimASTE:XRTINY", sizeof(y_in), &y_in);

#undef teldef
#undef skyref
#undef source_ra
#undef source_dec
#undef source_x
#undef source_y
#undef mjdrefi
#undef mjdreff

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
SimASTE_XISarfPhotonGen_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfPhotonGen_exit(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfBuild_startup(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfBuild_com(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfBuild_init(int *status)
{
	static int (*func)(fitsfile *fp) = write_history;
	int i, used;
	char *arffile;
	fitsfile *arffp;

	com.n_photon = com.n_detect = com.n_weisum = 0.0;

	com.prev_write_history = NULL;
	BnkGet("SimASTE:WRITE_HISTORY:FUNC", sizeof(com.prev_write_history),
		&used, &com.prev_write_history);
	BnkPut("SimASTE:WRITE_HISTORY:FUNC", sizeof(func), &func);

	com.shield_transmis_func = NULL;
	BnkGet("SimASTE:XRT:SHIELD_TRANSMIS:FUNC",
		sizeof(com.shield_transmis_func), &used, &com.shield_transmis_func);
	if ( NULL == com.shield_transmis_func ) {
		com.shield_transmis_func = dummy_shield_transmis_func;
	}

	if ( com.num_calc < 2 ) {	/* impossible to interpolate */
		anl_msg_error("\
%s: Energy steps too few (num_calc=%d).\n", pname, com.num_calc);
		goto quit;
	}

	anl_msg_info("\n\
%s: creating ARF files ...\n", pname);
	for (i = 0; i < com.num_region; i++) {
		arffile = com.arffile[i];
		if ( com.clobber ) unlink(arffile);
		anl_msg_info("\
   Creating %s\n", arffile);
		arffp = create_arf_file(arffile);
		if ( NULL == arffp ) {
			goto quit;
		}
		com.arffp[i] = arffp;
	}
	anl_msg_info("\n");

	*status = ANL_OK;
	return;

 quit:
	*status = ANL_QUIT;
	return;
}

void
SimASTE_XISarfBuild_his(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfBuild_bgnrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfBuild_ana(int nevent, int eventid, int *status)
{
#define teldef			com.teldef
#define skyref			com.skyref
#define mjdrefi			com.mjdrefi
#define mjdreff			com.mjdreff

	int i, used;
	unsigned int idetx, idety, idetpos, ix, iy, ipos;		/* non negative */
	int inside_ccd, inside_map, accuracy_ok, photon_used;	/* logical */
	double photon_time;
	double weight, detmask_weight, region_weight;
	double shield_transmis;
	double detx, dety, x, y, ra, de, input, detect;
	struct calc_values_per_energy *enep;
	struct calc_values_per_region *regp;
	PHOTON_MEMORY *pmp;
	AtEulerAng ea;

	enep = &com.ene[com.calc_index];
	input = enep->input;

/* BNKGET */
	BnkfGetM("SimASTE:XISX", sizeof(detx), &used, &detx);
	BnkfGetM("SimASTE:XISY", sizeof(dety), &used, &dety);
	BnkfGetM("SimASTE:PHOTON_TIME", sizeof(photon_time), &used, &photon_time);

/* convert coodinates */
	if ( detx < 0.5 || com.ndetx + 0.5 <= detx ) {
		idetx = com.ndetx;	/* invalid number */
	} else {
		idetx = (unsigned int)(detx + 0.5) - 1;
	}
	if ( dety < 0.5 || com.ndety + 0.5 <= dety ) {
		idety = com.ndety;	/* invalid number */
	} else {
		idety = (unsigned int)(dety + 0.5) - 1;
	}
	idetpos = com.ndetx * idety + idetx;
	inside_ccd = ( idetx < com.ndetx && idety < com.ndety );
	if ( inside_ccd ) {
		if ( NULL == com.detmaskmap ) {
			detmask_weight = 1.0;
		} else {
			detmask_weight = com.detmaskmap[idetpos];
		}
	} else {
		detmask_weight = 0.0;
	}

	switch ( com.region_mode ) {
	case REGION_MODE_SKYFITS:
	case REGION_MODE_SKYCIRC:
	case REGION_MODE_SKYREG:
	case REGION_MODE_SKYEXPR:
		BnkfGetM("SimASTE:EULER", sizeof(ea), &used, &ea);
		if ( com.aberration ) {
			aste_det2ecs(teldef, &ea, detx, dety, &ra, &de);
			aste_cor_aberration(photon_time, mjdrefi, mjdreff, &ra, &de);
			aste_ecs2sky(teldef, &skyref, ra, de, &x, &y);
		} else {
			aste_det2sky(teldef, &ea, &skyref, detx, dety, &x, &y);
		}
		if ( x < 0.5 || com.region_nx + 0.5 <= x ) {
			ix = com.region_nx;		/* invalid number */
		} else {
			ix = (unsigned int)(x + 0.5) - 1;
		}
		if ( y < 0.5 || com.region_ny + 0.5 <= y ) {
			iy = com.region_ny;		/* invalid number */
		} else {
			iy = (unsigned int)(y + 0.5) - 1;
		}
		ipos = com.region_nx * iy + ix;
		inside_map = ( ix < com.region_nx && iy < com.region_ny );
		break;
	case REGION_MODE_DETFITS:
	case REGION_MODE_DETCIRC:
	case REGION_MODE_DETREG:
	case REGION_MODE_DETEXPR:
		ix = idetx;
		iy = idety;
		ipos = idetpos;
		inside_map = inside_ccd;
		break;
	default:
		ix = iy = ipos = 0;	/* initialize for gcc warning */
		inside_map = 1;
	}

	if ( inside_map ) {
		BnkfGetM("SimASTE:WEIGHT", sizeof(weight), &used, &weight);
		BnkfGetM("SimASTE:XRT:SHIELD_TRANSMIS",
			sizeof(shield_transmis), &used, &shield_transmis);
		if ( 0.0 != shield_transmis ) {
			weight /= shield_transmis;
		}
	}

	accuracy_ok = 1;
	photon_used = 0;

	for (i = 0; i < com.num_region; i++) {
		regp = &enep->reg[i];

		if ( inside_map ) {
			region_weight = com.regmap[i][ipos];
			if ( 0.0 < region_weight ) {
				com.tot_ccd_pix_reg[i] += region_weight;
				if ( 0.0 < detmask_weight ) {
					region_weight *= detmask_weight;
					com.sum_detmask_reg[i] += region_weight;
					region_weight *= weight;

					/* increment weighted number of photons */
					regp->weisum += region_weight;

					if ( 0.0 < region_weight ) {
						/* increment number of detected photons */
						regp->detect++;
						com.wmap[i][ipos] += region_weight;
						photon_used = 1;
					}

					if ( 1.0 < region_weight ) {
						anl_msg_warning("\
%s: WARNING: WEIGHT (=%.6f) exceeds 1.0 !!!\n", pname, region_weight);
					}
				}
			}
		}

		/* calculate accepted ratio and its relative error */
		detect = regp->detect;
		if ( 0.0 == detect || input == detect ) {
			regp->relerr = 1.0;
		} else {
			regp->relerr = sqrt( (input - detect) / input / detect );
		}

		/* check termination condition */
		if ( com.accuracy < regp->relerr ) {
			accuracy_ok = 0;
		}
	}

	if ( photon_used ) {
		com.n_detect += 1.0;
		com.n_weisum += weight;
		BnkfPutM("SimASTE:N_DETECT", sizeof(com.n_detect), &com.n_detect);
		BnkfPutM("SimASTE:N_WEISUM", sizeof(com.n_weisum), &com.n_weisum);

		if ( com.pmem_size <= enep->iphoton ) {
			anl_msg_warning("\
%s: WARNING: PHOTON_MEMORY overflow (pmem_size=%d),\n\
%s: WARNING: go to the next energy step.\n", pname, com.pmem_size, pname);
			goto accuracy_ok;
		}

		pmp = &enep->pmem[enep->iphoton];
		pmp->photon_time = photon_time;
		pmp->weight = weight;
		pmp->ipos = ipos;
		pmp->idetx = (short)idetx;
		pmp->idety = (short)idety;
		enep->iphoton++;
	}

	if ( 0.0 < com.accuracy && accuracy_ok ) {
	accuracy_ok:
		write_arf_col(com.calc_index);
		com.calc_index++;
	}

#undef teldef
#undef skyref
#undef mjdrefi
#undef mjdreff

	*status = ANL_OK;
}

void
SimASTE_XISarfBuild_endrun(int *status)
{
	*status = ANL_OK;
}

void
SimASTE_XISarfBuild_exit(int *status)
{
	int i, istat, istat_final;
	fitsfile *fp;

	istat = istat_final = 0;

/* write output arf file */
	anl_msg_info("\n\
%s: closing ARF files ...\n", pname);
	for (i = 0; i < com.num_region; i++) {

		fp = com.arffp[i];

		if ( (istat = write_arf_wmap(fp, i)) ||
			 (istat = SimASTE_write_timestamp(fp)) ||
			 (istat = SimASTE_write_photon_detect_info(fp)) ||
			 (istat = SimASTE_write_random_number_info(fp)) ||
			 0 ) {
			istat_final = istat;
			istat = 0;	/* ignore error */
		}

/* close output file */
		fits_close_file(fp, &istat);
		if ( istat ) {
			anl_msg_error("\
%s: Can not close ARF file %s (%d)\n", pname, com.arffile[i], istat);
			istat_final = istat;
			istat = 0;	/* ignore error */
		} else {
			anl_msg_info("\
   Closed %s\n", com.arffile[i]);
		}

	}

	if ( istat_final ) {
		*status = ANL_QUIT;
		return;
	}

	anl_msg_info("\
done.\n");

/* free XIS_CONTAMI */
	if ( NULL != com.xcp ) {
		xis_contami_free(com.xcp);
	}

	*status = ANL_OK;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/
