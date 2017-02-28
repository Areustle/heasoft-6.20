/*
 *	numkarf.h: definitions and declarations for numkarf
 *
 *	AUTHOR:
 *            ASDC - ASI Science Data Center
 *  
 *     
 */
#ifndef NUMKARF_H
#define NUMKARF_H

/******************************
 *        header files        *
 ******************************/
#include <stdio.h>	/* Note: for IRAF compatibility,
			standard I/O calls should NOT be used. */
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <unistd.h> /* for getpid */
#include <sys/stat.h>   /* needed by 'mkdir' */
#include <sys/types.h>  /* needed by 'mkdir' */

/* headas headers */
#include <fitsio.h>	 /*Required to use c_fcerr, c_fcecho, etc. */
#include <pil.h>
#include <headas_gti.h>
#include <atFunctions.h> /* "At" library */

/* nustar local headers */
#include "nu_highfits.h"
#include "nu_termio.h"
#include "nu_defs.h"
#include "nu_misc.h"
#include "nu_caldb.h"
#include "nustardasversion.h"



#ifndef errno
extern int errno;
#endif

/*******************************
  Macros to put in nu_highfits.h file
********************************/

#define VECVECACCESS_PT(type, a, b, c)	\
                        ((type *)(a).cols[c]) + (b*(a).Multiplicity[c])

#define VECVEC_ARRAY_READ(type, var, dim, a, b, c) \
                          memcpy(var,VECVECACCESS_PT(type,a,b,c), sizeof(type)*dim)
#define VECVEC_ARRAY_WRITE(type, var, dim, a, b, c) \
                          memcpy(VECVECACCESS_PT(type,a,b,c), var, sizeof(type)*dim)

#define EVECVEC_ARRAY_READ(var, dim, a, b, c) VECVEC_ARRAY_READ(ETYPE, var, dim, a, b, c)
#define EVECVEC_ARRAY_WRITE(var, dim, a, b, c) VECVEC_ARRAY_WRITE(ETYPE, var, dim, a, b, c)

#define DVECVEC_ARRAY_READ(var, dim, a, b, c) VECVEC_ARRAY_READ(DTYPE, var, dim, a, b, c)


/*******************************
*      defines / typedefs      *
********************************/
/* input parameter names */

#define PAR_PHAFILE		 "phafile"
#define PAR_OUTFILE		 "outfile"
#define PAR_OPTAXISFILE		 "optaxisfile"
#define PAR_OFFAXISFILE		 "offaxisfile"
#define PAR_OFFAXISHISTO	 "offaxishisto"
#define PAR_APSTOPHISTO 	 "apstophisto"
#define PAR_GRHISTO     	 "grhisto"
#define PAR_INFILE		 "infile"
#define PAR_PIXPOSFILE		 "pixposfile"
#define PAR_ALIGNFILE   	 "alignfile"
#define PAR_MASTASPECTFILE   	 "mastaspectfile"
#define PAR_ATTFILE		 "attfile"
#define PAR_TELDEF		 "teldef"
#define PAR_INSTRPROBMAPFILE	 "instrprobmapfile"
#define PAR_INEXPOMAPFILE	 "inexpomapfile"
#define PAR_ABERRATION		 "aberration"
#define PAR_DET1REFFILE		 "det1reffile"
#define PAR_PIXBIN		 "pixbin"
#define PAR_PHIBIN		 "phibin"
#define PAR_APSTOPPHIBIN	 "apstopphibin"
#define PAR_GRPHIBIN		 "grphibin"
#define PAR_INARFFILE		 "inarffile"
#define PAR_GRPPSFFILE		 "grppsffile"
#define PAR_PSFDIR		 "psfdir"
#define PAR_VIGNFILE		 "vignfile"
#define PAR_APSTOPCORRFILE	 "apstopcorrfile"
#define PAR_GRCORRFILE  	 "grcorrfile"
#define PAR_DETABSCORRFILE  	 "detabscorrfile"
#define PAR_SRCREGIONFILE  	 "srcregionfile"
#define PAR_PSFFLAG		 "psfflag"
#define PAR_VIGNFLAG		 "vignflag"
#define PAR_APSTOPFLAG		 "apstopflag"
#define PAR_GRFLAG		 "grflag"
#define PAR_DETABSFLAG		 "detabsflag"
#define PAR_EXTENDED		 "extended"
#define PAR_CUTMAPS		 "cutmaps"
#define PAR_BOXSIZE		 "boxsize"
#define PAR_PILOWARF		 "pilowarf"
#define PAR_PIHIGHARF		 "pihigharf"
#define PAR_FLATFLAG		 "flatflag"
#define PAR_PERCENT		 "percent"
#define PAR_INITSEED     	 "initseed"
#define PAR_CLEANUP		 "cleanup"

/* miscellaneous */
#define BINTAB_ROWS              1000
#define BINTAB_ROWS_SMALL        2

#define DOUBLE_SENS              0.00000000000000001

#define ASH_OFFBIN_SCALE         5  /* aperture stop histogram offset bin scale factor (integer) */
#define GRH_OFFBIN_SCALE         5  /* ghost rays histogram offset bin scale factor (integer) */

#define PSF_ROWS                 325
#define PSF_PIXS                 325

#define PSF_CENTERX              162
#define PSF_CENTERY              162

#define EXPO_ROWS                1000
#define EXPO_PIXS                1000

#define CLNM_2DPSFFILE     "2DPSFFILE"

#define KWNM_CRPIX1P       "CRPIX1P"
#define KWNM_CRVAL1P       "CRVAL1P"
#define KWNM_CRPIX2P       "CRPIX2P"
#define KWNM_CRVAL2P       "CRVAL2P"


typedef struct {
  char   telescop[FLEN_VALUE];
  char	 telescop_comm[FLEN_COMMENT];
  char   instrume[FLEN_VALUE];
  char	 instrume_comm[FLEN_COMMENT];
  SPTYPE dateobs;
  SPTYPE timeobs;
  SPTYPE dateend;
  SPTYPE timeend;
  DTYPE  tstart;
  DTYPE  tstop;
  double mjdref;
  DTYPE  pa_pnt;
} ObsInfo_t;

typedef struct {
  /* Vignetting file info */
  int    energ_lo_dim;
  int    theta_dim;
  int    vignet_dim;
  double thetabin;
  /* Psf file info */
  int    psfbin_dim;
  double psfbin;
  /* offaxishisto file info */
  int    offbin_dim;
  double offbin;
  int    phibin_dim;
  double phibin;
} FilesInfo_t;


typedef struct {
  double ontime;
  double livetime;
  double deadc;
  int    xwidth;
  int    ywidth;
  int    crpix1p;
  int    crpix2p;
  int    crval1p;
  int    crval2p;
} ExposureInfo_t;

typedef struct {
  unsigned X;
  unsigned Y;
  unsigned SHAPE;
  unsigned R;
  unsigned ROTANG;
  unsigned COMPONENT;
} RegionExtCol_t;

typedef struct {
  char     shape[17];
  double   X[2];
  double   Y[2];
  double   R[2];
  double   R1[2];
  double   rotang[2];
  int      component[2];
} RegionExtInfo_t;


typedef struct {
  unsigned TIME;
  unsigned X_OA;
  unsigned Y_OA;

  unsigned DET2X_APSTOP;
  unsigned DET2Y_APSTOP;
/*   unsigned X_APSTOP; */
/*   unsigned Y_APSTOP; */

} OptAxisCol_t;

typedef struct {
  double time;
  double x_oa;
  double y_oa;

  double det2x_apstop;
  double det2y_apstop;
/*   double x_apstop; */
/*   double y_apstop; */

} OptAxisInfo_t;

typedef struct {
  char instrume[FLEN_VALUE];
  DTYPE  tstart;
  char	 tstart_comm[FLEN_COMMENT];
  DTYPE  tstop;
  char	 tstop_comm[FLEN_COMMENT];
} OptAxisKeys_t;


typedef struct {
  unsigned TIME;
  unsigned OFF_AXIS;
  unsigned PHI;
  unsigned DELTAX_CEN;
  unsigned DELTAY_CEN;
}OffAxisCol_t;


typedef struct {
  unsigned AZIMUTH;
  unsigned ENERG_LO;
  unsigned THETA;
  unsigned VIGNET;
} VignCol_t;

typedef struct {
  float azimuth;
  float *energ_lo;
  float *theta;
  float *vignet;
} VignInfo_t;


/* Aperture Stop Correction file columns */
typedef struct {
  unsigned AZIMUTH;
  unsigned ENERG_LO;
  unsigned ENERG_HI;
  unsigned THETA;
  unsigned DELTAX_CEN;
  unsigned DELTAY_CEN;
  unsigned APERTURE;
} ApStopCorrCol_t;

/* Aperture Stop Correction file data */
typedef struct {
  float azimuth;
/*   float *energ_lo; */
/*   float *energ_hi; */
/*   float *theta; */
/*   float *deltax_cen; */
/*   float *deltay_cen; */
  float *aperture;
} ApStopCorrData_t;

/* Aperture Stop Correction file info */
typedef struct {

  int    as_energ_lo_dim;
  double as_energ_lo_bin;
  double as_energ_lo_min;

  int    as_energ_hi_dim;
  double as_energ_hi_bin;
  double as_energ_hi_min;

  int    as_theta_dim;
  double as_theta_bin;
  double as_theta_min;

  int    as_deltax_cen_dim;
  double as_deltax_cen_bin;
  double as_deltax_cen_min;

  int    as_deltay_cen_dim;
  double as_deltay_cen_bin;
  double as_deltay_cen_min;

  int    as_aperture_dim;

} ApStopCorrInfo_t;


typedef struct {
  double *en;
  double *corr;
  int n_energy;
} ApStopCorrVal_t;


typedef struct {
  unsigned OFF_AXIS;
  unsigned PHI;
  unsigned DURATION;
} OffAxisHistoCol_t;

typedef struct {
  double off_axis;
  double phi;
  double duration;
} OffAxisHistoInfo_t;


/* Aperture Stop Histogram file columns */
typedef struct {
  unsigned OFF_AXIS;
  unsigned PHI;
  unsigned DELTAX;
  unsigned DELTAY;
  unsigned DURATION;
} ApStopOffAxisHistoCol_t;

/* Aperture Stop Histogram file data */
typedef struct {
  double off_axis;
  double phi;
  double deltax;
  double deltay;
  double duration;
} ApStopOffAxisHistoData_t;

/* Aperture Stop Histogram file info */
typedef struct {

  int    ash_offbin_dim;
  double ash_offbin;
  double ash_offbin_min;

  int    ash_phibin_dim;
  double ash_phibin;

  int    ash_deltax_dim;
  double ash_deltaxbin;
  double ash_deltaxbin_min;

  int    ash_deltay_dim;
  double ash_deltaybin;
  double ash_deltaybin_min;

} ApStopOffAxisHistoInfo_t;


/* Ghost Rays Correction file columns */
typedef struct {
  unsigned AZIMUTH;
  unsigned ENERG_LO;
  unsigned ENERG_HI;
  unsigned THETA;
  unsigned DELTAX_CEN;
  unsigned DELTAY_CEN;
  unsigned RADIUS;
  unsigned GHOSTRAYS;
} GhostRaysCorrCol_t;

/* Ghost Rays Correction file data */
typedef struct {
  float azimuth;
/*   float *energ_lo; */
/*   float *energ_hi; */
/*   float *theta; */
/*   float *deltax_cen; */
/*   float *deltay_cen; */
  float *ghostrays;
} GhostRaysCorrData_t;

/* Ghost Rays Correction file info */
typedef struct {

  int    energ_lo_dim;
  double energ_lo_bin;
  double energ_lo_min;

  int    energ_hi_dim;
  double energ_hi_bin;
  double energ_hi_min;

  int    theta_dim;
  double theta_bin;
  double theta_min;

  int    deltax_cen_dim;
  double deltax_cen_bin;
  double deltax_cen_min;

  int    deltay_cen_dim;
  double deltay_cen_bin;
  double deltay_cen_min;

  int    radius_dim;
  double radius_bin;
  double radius_min;

  int    ghostrays_dim;

} GhostRaysCorrInfo_t;


/* Ghost Rays Histogram file columns */
typedef struct {
  unsigned OFF_AXIS;
  unsigned PHI;
  unsigned DELTAX;
  unsigned DELTAY;
  unsigned DURATION;
} GhostRaysOffAxisHistoCol_t;

/* Ghost Rays Histogram file data */
typedef struct {
  double off_axis;
  double phi;
  double deltax;
  double deltay;
  double duration;
} GhostRaysOffAxisHistoData_t;

/* Ghost Rays Histogram file info */
typedef struct {

  int    offbin_dim;
  double offbin;
  double offbin_min;

  int    phibin_dim;
  double phibin;

  int    deltax_dim;
  double deltaxbin;
  double deltaxbin_min;

  int    deltay_dim;
  double deltaybin;
  double deltaybin_min;

} GhostRaysOffAxisHistoInfo_t;


typedef struct {
  double *en;
  double *corr;
  int n_energy;
} GhostRaysCorrVal_t;


typedef struct {
  /* SYSTEM_ALIGNMENT ext */
  unsigned X_DET2A;
  unsigned Y_DET2A;
  unsigned X_DET2B;
  unsigned Y_DET2B;
  /* OPTICAL_AXIS ext */
  unsigned Q_DET2A_OB;
  unsigned Q_DET2B_OB;

} AlignCol_t;

typedef struct {
  /* SYSTEM_ALIGNMENT ext */
  DTYPE  x_det2a;
  DTYPE  y_det2a;
  DTYPE  x_det2b;
  DTYPE  y_det2b;
  /* OPTICAL_AXIS ext */
  DTYPE Qdet2Aob[4];
  DTYPE Qdet2Bob[4];

} AlignInfo_t;


typedef struct {
  double tstart;
  double tstop;
  double phi;
} GTIBinInfo_t;

typedef struct {
  double off_axis;
  double duration;
  double phi_median;
  int    gticount;
  GTIBinInfo_t * gti;
} OffAxisBinInfo_t;


typedef struct {
  unsigned ENERG_LO;
  unsigned ENERG_HI;
  unsigned SPECRESP;
} ARFCol_t;


typedef struct {
  int    x_min;
  int    x_max;
  int    y_min;
  int    y_max;
  float  cx;
  float  cy;

  double *vigncorr;
  double *detabscorr;
  double expocorr;
  double weight;

  char   arffile[PIL_LINESIZE];

} RegBoxInfo_t;

typedef struct {
  int           nbox;
  RegBoxInfo_t *boxinfo;
} RegBox_t;


typedef struct {
  int x_width;
  int y_width;
  int x_offset;
  int y_offset;

} PhaImgInfo_t;


typedef struct {
  unsigned DETABS;
} DETABSCol_t;

typedef struct {
  int   nrows;
  float *detabs;
} DETABSInfo_t;


typedef struct {
  unsigned ENERG_LO;
  unsigned ENERG_HI;
  unsigned PSFFILE;
} GrpPsfCol_t;

typedef struct {
  double  energy_lo;
  double  energy_hi;
  char    *psffile;
} GrpPsfRow_t;

typedef struct {
  int          psffile_dim;
  int          nrows;
  GrpPsfRow_t  *row;
} GrpPsfInfo_t;


typedef struct {
  double *en;
  double *corr;
  int n_energy;
} PsfCorrVal_t;


typedef struct {
  char        offaxisfile[PIL_LINESIZE];
  char        offaxishisto[PIL_LINESIZE];
  char        apstophisto[PIL_LINESIZE];
  char        grhisto[PIL_LINESIZE];
  char        optaxisgti[PIL_LINESIZE];
  char        bingti[PIL_LINESIZE];
  char        det1refgti[PIL_LINESIZE];
  char        expomap[PIL_LINESIZE];
  char        aspecthisto[PIL_LINESIZE];
  char        arflistfile[PIL_LINESIZE];
  char        mggti[PIL_LINESIZE];
  char        evtfile[PIL_LINESIZE];
  char        combinexform_in[PIL_LINESIZE];
  char        combinexform_out[PIL_LINESIZE];
  char        rotpsffile[PIL_LINESIZE];
  char        dirname[PIL_LINESIZE];
  char        loc_infile[PIL_LINESIZE];
  char        loc_phafile[PIL_LINESIZE];
  char        loc_filtered_phafile[PIL_LINESIZE];
  char        loc_filtered_infile[PIL_LINESIZE];
  char        xsel_infile[PIL_LINESIZE];
  char        loc_srcregionfile[PIL_LINESIZE];
  char        loc_boxregionfile[PIL_LINESIZE];
  char        addarf_outfile[PIL_LINESIZE];
  } TmpFiles_t;

typedef struct {
  /*----------   Input Parameters ---------------------*/
  struct {
    char        phafile[PIL_LINESIZE];
    char        outfile[PIL_LINESIZE];
    char        optaxisfile[PIL_LINESIZE];
    char        offaxisfile[PIL_LINESIZE];
    char        offaxishisto[PIL_LINESIZE];
    char        apstophisto[PIL_LINESIZE];
    char        grhisto[PIL_LINESIZE];
    char        infile[PIL_LINESIZE];       /* Name of input FITS file  */
    char        pixposfile[PIL_LINESIZE];   /* Name of input pixel location FITS file  */
    char        alignfile[PIL_LINESIZE];
    char        mastaspectfile[PIL_LINESIZE];
    char        attfile[PIL_LINESIZE];
    char        teldef[PIL_LINESIZE];
    char        instrprobmapfile[PIL_LINESIZE];
    char        inexpomapfile[PIL_LINESIZE];
    char        det1reffile[PIL_LINESIZE];
    char        inarffile[PIL_LINESIZE];
    char        grppsffile[PIL_LINESIZE];
    char        psfdir[PIL_LINESIZE];
    char        vignfile[PIL_LINESIZE];
    char        apstopcorrfile[PIL_LINESIZE];
    char        grcorrfile[PIL_LINESIZE];
    char        detabscorrfile[PIL_LINESIZE];
    char        srcregionfile[PIL_LINESIZE];
    BOOL        psfflag;
    BOOL        vignflag;
    BOOL        apstopflag;
    BOOL        grflag;
    BOOL        detabsflag;
    BOOL        extended;
    BOOL        cutmaps;
    BOOL        aberration;
    BOOL        cleanup;
    int         pixbin;
    double      phibin;
    double      apstopphibin;
    double      grphibin;
    int         boxsize;
    int         pilowarf;
    int         pihigharf;
    BOOL        flatflag;
    double      percent;
    BOOL        initseed;

  } par;


/*----------------------------------------------------------------*/
  char taskname[FLEN_FILENAME]; /* Name and version of the task */
  Version_t nustardas_v;          /* NuSTARDAS version */
  BOOL createoffaxisfile;
  BOOL createoffaxishisto;
  BOOL createapstophisto;
  BOOL creategrhisto;
  BOOL getexpomapfile;
  BOOL hist;
  BOOL warning;
  char strhist[256];
  char date[25];

  int             energy_dim;

  ObsInfo_t       obsinfo;
  FilesInfo_t     filesinfo;
  RegionExtInfo_t reginfo;
  TmpFiles_t tmpout;

} Global_t;

extern Global_t global;


/* Prototypes */
int numkarf();
int numkarf_work(void);
int numkarf_getpar(void);
void numkarf_info(void);
int numkarf_checkinput(void);

int GetObsInfo(char *filename, char *extname, ObsInfo_t *obsinfo);
int ReadRegionInfo(char *filename, char *extname, RegionExtInfo_t *reginfo);

int ReadOptAxisFile(char *filename, OptAxisInfo_t ** optinfo, int *optcount, OptAxisKeys_t *optkeys);

int WriteOffAxisFile(AlignInfo_t *aligninfo, OptAxisInfo_t *optinfo, int optcount, OptAxisKeys_t *optkeys, double cx, double cy, char *infile, char *outfile);
int WriteOffAxisExt(AlignInfo_t *aligninfo, OptAxisInfo_t *optinfo, int nrows, OptAxisKeys_t *optkeys, double cx, double cy, FitsFileUnit_t ounit);

int ComputeOffAxisHisto(struct gti_struct *gti, char *infile, double offbinsize, int offbinnum, OffAxisHistoInfo_t **histoinfo, int *histocount);

int WriteOffAxisHistoFile(OffAxisHistoInfo_t *histoinfo, int histocount, char *infile, char *outfile);
int WriteOffAxisHistoExt(OffAxisHistoInfo_t *histoinfo, int nrows, FitsFileUnit_t ounit);

/* int ReadOffAxisHistoFile(char *filename, OffAxisHistoInfo_t ** info, int *count); */
int ReadVignFile(char *filename, VignInfo_t **info, int *count);
int ReadPSFFile(char *filename, int extnum, float psf[PSF_ROWS][PSF_PIXS], double *sumrcts);
int ReadExpoFile(char *filename, float **expomap, ExposureInfo_t *info);

void ComputeVignCorr(VignInfo_t *vigninfo, int vigncount, OffAxisHistoInfo_t *histoinfo, int histocount, double *vigncorr);
void ComputePsfCorr(OffAxisBinInfo_t *bininfo, double *psf_frac_off, int binnum, double *psfcorr);

int CreateARFFile(char *inarf, char *outarf, double *vigncorr, PsfCorrVal_t *psfcorr, ApStopCorrVal_t *apstopcorr, GhostRaysCorrVal_t *grcorr, double *detabscorr);
int CorrectArf(FitsFileUnit_t inunit, FitsFileUnit_t outunit, double *vigncorr, PsfCorrVal_t *psfcorr, ApStopCorrVal_t *apstopcorr, GhostRaysCorrVal_t *grcorr, double *detabscorr);

int GetCenterBinIndex(double binsize, double offset, int imin, int imax, double value);
int GetInitBinIndex(double binsize, double offset, int imin, int imax, double value);

int ComputeOffAxisBin(struct gti_struct *gti, char *infile, OffAxisBinInfo_t *bininfo, int binnum, double binsize);
int WriteBinGTI(char *filename, GTIBinInfo_t * gtiinfo, int gticount);

int ComputePsfFracOff(char *expofile, char *psffile, int bin, double phi, RegionExtInfo_t *reginfo, double *psf_frac_off);

int ComputeBoxFromPhaImg(PhaImgInfo_t *phaimginfo, int boxsize, RegBox_t *box);

int ComputeExpoCorr(char *expofile, float *phaimg, PhaImgInfo_t *phaimginfo, RegBox_t *box);

int ComputeBoxWeight(char *phafile, RegionExtInfo_t *reginfo, VignInfo_t *vigninfo, RegBox_t *box);
int SetFlatBoxWeight(char *phafile, RegBox_t *box);

int ReadPhaImg(char *filename, float **img, PhaImgInfo_t *imginfo);

int WriteArfListFile(RegBox_t *box, char *outfile);

int UpdateArfKeys(ObsInfo_t *obsinfo, char *filename);

int GetPsfBinInfo(char *filename, int *psfbin_dim, double *psfbin);

int CreateTemporaryEvt(char *infile, char *gtifile, char *gtiextname, char *outfile);

int WritePsfCombineXformInFile(char *filename, double phi);

double NormalaziedAt360Angle(double angle);

void InterpolateValues(double down, double up, double this, double valuedown, double valueup, double *value);

int WritePsfFracTmpFile(float fractmp[PSF_ROWS][PSF_PIXS], int bin);

int ComputeApertureStopOffAxisHisto(struct gti_struct *gti, char *infile, ApStopCorrInfo_t *apstopinfo, ApStopOffAxisHistoData_t **histodata, int *histocount, ApStopOffAxisHistoInfo_t *histoinfo);

int ReadApStopCorrFile(char *filename, ApStopCorrData_t ** apstop_data, int *ncount, ApStopCorrInfo_t *apstop_info);

int WriteApStopOffAxisHistoFile(ApStopOffAxisHistoData_t *histoinfo, int histocount, char *infile, char *outfile);
int WriteApStopOffAxisHistoExt(ApStopOffAxisHistoData_t *histoinfo, int nrows, FitsFileUnit_t ounit);

int ComputeApertureStopCorr(struct gti_struct *gti, char *offaxisfile, ApStopCorrVal_t *apstopcorr);

void ComputeApertureCorrection(ApStopCorrInfo_t *apinfo, ApStopCorrData_t *apdata, int apcount, ApStopOffAxisHistoInfo_t *histoinfo, ApStopOffAxisHistoData_t *histodata, int histocount, ApStopCorrVal_t *apstopcorr);

int ReadAlignInfo(char *filename, AlignInfo_t *aligninfo);

void FreeApStopCorrVal(ApStopCorrVal_t *apstopcorr);
void FreePsfCorrVal(PsfCorrVal_t *psfcorr);

int GetApStopCorr(ApStopCorrVal_t *apstopcorr, double en, double *corr);
void FindClosestApStopCorrIndex(ApStopCorrVal_t *apstopcorr, double value, int *index);

double GetAperture(ApStopCorrInfo_t *apinfo, ApStopCorrData_t *apdata, int apcount, double off_axis, double phi, int i_energy, int i_deltax, int i_deltay);


/* ----------------------------------------------------------------------------------------------------------------------- */
/*                          Ghost Rays Correction Routines    ---->>                                                       */
/* ----------------------------------------------------------------------------------------------------------------------- */


int ReadGhostRaysCorrFile(char *filename, GhostRaysCorrData_t ** gr_data, int *ncount, GhostRaysCorrInfo_t *gr_info);

int ComputeGhostRaysOffAxisHisto(struct gti_struct *gti, char *infile, GhostRaysCorrInfo_t *grinfo, GhostRaysOffAxisHistoData_t **histodata, int *histocount, GhostRaysOffAxisHistoInfo_t *histoinfo);

int WriteGhostRaysOffAxisHistoFile(GhostRaysOffAxisHistoData_t *histodata, int histocount, char *infile, char *outfile);
int WriteGhostRaysOffAxisHistoExt(GhostRaysOffAxisHistoData_t *histodata, int nrows, FitsFileUnit_t ounit);

int ComputeGhostRaysCorr(struct gti_struct *gti, char *offaxisfile, double reg_radius, GhostRaysCorrVal_t *grcorr);

void ComputeGhostRaysCorrection(GhostRaysCorrInfo_t *grinfo, GhostRaysCorrData_t *grdata, int grcount, GhostRaysOffAxisHistoInfo_t *histoinfo, GhostRaysOffAxisHistoData_t *histodata, int histocount, double reg_radius, GhostRaysCorrVal_t *grcorr);

void FreeGhostRaysCorrVal(GhostRaysCorrVal_t *grcorr);

int GetGhostRaysCorr(GhostRaysCorrVal_t *grcorr, double en, double *corr);

void FindClosestGhostRaysCorrIndex(GhostRaysCorrVal_t *grcorr, double value, int *index);

double GetGhostRays(GhostRaysCorrInfo_t *grinfo, GhostRaysCorrData_t *grdata, int grcount, double off_axis, double phi, double radius, int i_energy, int i_deltax, int i_deltay);


/* ----------------------------------------------------------------------------------------------------------------------- */
/*                   <<---- Ghost Rays Correction Routines                                                                 */
/* ----------------------------------------------------------------------------------------------------------------------- */

void FreeApStopCorrData(ApStopCorrData_t *apstopdata, int apstopcount);
void FreeGhostRaysCorrData(GhostRaysCorrData_t *grdata, int grcount);


/* ----------------------------------------------------------------------------------------------------------------------- */
/*                             DETABS Correction Routines    ---->>                                                        */
/* ----------------------------------------------------------------------------------------------------------------------- */


int ReadDETABSFile(char *filename, DETABSInfo_t detabsinfo[4]);
int ReadDETABSInfo(FitsFileUnit_t inunit, DETABSInfo_t *detabs_det);

int CreateXselXco(char *evtfile, char *gtifile, char *regfile, char *evtfiltered, char *xcofile);

int GetEvtCnt(char *filename, int evtcnt[4], int *totevt);

int ComputeDETABSCorr(DETABSInfo_t detabsinfo[4], char *evtfile, char *phafile, char *regfile, int detabscount, double *detabscorr);

int CreateRegionFile(RegBoxInfo_t *boxinfo, char *filename);


/* ----------------------------------------------------------------------------------------------------------------------- */
/*                     <<---- DETABS Correction Routines                                                                   */
/* ----------------------------------------------------------------------------------------------------------------------- */


int ReadGrpPsfFile(char *filename, GrpPsfInfo_t *info);

int ComputePsfCorrByEnergy(struct gti_struct *gti, char *offaxisfile, double skyx, double skyy, int skysize, PsfCorrVal_t *psfcorr);

int GetPsfCorr(PsfCorrVal_t *psfcorr, double en, double *corr);
void FindClosestPsfCorrIndex(PsfCorrVal_t *psfcorr, double value, int *index);

int CreateFilteredOptAxisFile(char *infile, char *phafile, char *outfile);
int CreateFilteredDet1RefFile(char *infile, char *gtifile, char *outfile);

int FilterPhaFile(char *evtfile, char *phafile, char *regfile, int pilowarf, int pihigharf, char *phafiltered);


#endif
