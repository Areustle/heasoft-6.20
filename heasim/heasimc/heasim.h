/**
 * \file heasim.h
 * \brief HEADAS Simulator - header file
 * \author David Riethmiller
 * \date $Date: 2016/03/31 21:41:40 $
 * 
 * Contains all typedefs, structs, and function definitions for routines internal to heasim.
 *
 */

#include "cfortran.h"
#include "funcWrappers.h"
#include "xsFortran.h"
#include "fitsio.h"
#include <strings.h>
#include <limits.h>
#include <stdlib.h>
#include <headas_rand.h>  /* headas random number generator */

#ifndef heasim_h 
#define heasim_h

/* There was some concern about using the currently-supported version of the HEASP library,
   namely, that the newest C++ version isn't as mature as it needs to be.  Therefore, 
   we copy the necessary old C heasp functions from the legacy library, and build them along with
   heasim.  If use_legacy_heasp is defined, we use these functions instead of the live
   C++ functions in the heacore/heasp library.  Once the C++ library is well-tested and
   proven to work for all cases, we might consider disabling this option. */
#define use_legacy_heasp

/**************************************************************/
/******* Constant and conversion definitions ******************/

#define STR_MAX_LEN 300             /* for typical-length line strings */
#define STR_LONG_MAX_LEN 5000       /* for line strings which could be very long */
#define MAXRET (1)
#define ARCSEC_PER_DEGREE (3600.0)
#define ARCMIN_PER_DEGREE (60.0)
#define ARCSEC_PER_ARCMIN (60.0)
#define EULER_PI (3.141592654)
#define TWOPI (6.28318531)
#define D2R 0.01745329252  /* degrees to radians */
#define MAX(a,b) ((a) > (b) ? a : b)
#define MIN(a,b) ((a) < (b) ? a : b)

/***************************************************************/
/******* Structure Definitions *********************************/

/**
 * \brief Structure to hold string names of observatory parameters
 * \param[in,out] mission Observatory, e.g. Astro-H, XMM, Suzaku...
 * \param[in,out] instrume Instrument, e.g. SXI, SXS, EMOS, etc.
 * \param[in,out] filter Filter (don't know if this will be used)
 * \param[in,out] instmode Instrument Mode (don't know if this will be used)
 * \param[in,out] rapoint Pointing RA, decimal degrees J2000
 * \param[in,out] decpoint Pointing DEC, decimal degrees J2000
 * \param[in,out] roll Roll angle, decimal degrees
 * \param[in,out] exposure Simulation Exposure Time */
typedef struct{
    /* This structure holds the string names of the observatory
       instruments and filters used. */
    char mission[STR_MAX_LEN];        /* Observatory, e.g. Astro-H, XMM, Suzaku... */
    char instrume[STR_MAX_LEN];         /* Instrument, e.g. SXI, SXS, EMOS, etc. */
    char filter[STR_MAX_LEN];             /* Don't know if this will be used */
    char instmode[STR_MAX_LEN];    /* Instrument Mode - will be used? */
    double rapoint;                    /* Pointing RA, decimal degrees J2000 */
    double decpoint;                   /* Pointing DEC, decimal degrees J2000 */
    double roll;                           /* Roll angle, decimal degrees */
    double exposure;                      /* Simulation Exposure Time */
    int flagsubex;                        /* Flag to use subexposure time */
    double subexposure;                   /* Subdivision of exposure time - will co-add at end */
    double dtpileup;                      /* Time range delta T under which pileup may occur */
    int resample;                         /* Flag to resample sky coordinates within detector pixel */
    int skipfov;                          /* 1 to skip discarding of  out-of-fov events, otherwise 0 */
    int skiprmf;                          /* 1 to skip rmf application, otherwise 0 */
} ObsParams;



/**
 * \brief Structure to hold names of calibration files
 * \param[in,out] psffile PSF filename
 * \param[in,out] vigfile Vignette filename
 * \param[in,out] arffile Ancillary Response File
 * \param[in,out] rmffile Response Matrix File
 * \param[in,out] intbackfile Internal background filename
 * \arfrmftol Tolerance for RMF & ARF comparison  */
typedef struct{
    /* This structure holds the names of the calibration files, read from
       the input parameter file given by the user at run time. */
    char psffile[STR_MAX_LEN];            /* PSF filename */
    char vigfile[STR_MAX_LEN];       /* Vignette filename */
    char arffile[STR_MAX_LEN];            /* Ancillary Response filename */
    char rmffile[STR_MAX_LEN];            /* Response Matrix filename */
    char intbackfile[STR_MAX_LEN];   /* Internal Background filename */
    double arfrmftol;               /* Tolerance for RMF & ARF comparison */
} CalFiles;



/**
 * \brief Structure to hold names of background files
 * \param[in,out] psbackfile Point source catalog from background tool
 * \param[in,out] difbackfile  Diffuse spectrum catalog from background tool */
typedef struct{
    /* This structure holds the names of some various background files. */
    char psbackfile[STR_MAX_LEN];         /* Point source catalog from background tool */
    char difbackfile[STR_MAX_LEN];        /* Diffuse spectram catalog from background tool */
    char pszbackfile[STR_MAX_LEN];        /* heasim point source redshift/intrinsic- absorption list from background tool */
} GeneralBackground;



/**
 * \brief Structure to hold selected mission database parameters
 * \param[in,out] plate_scale plate scale in degree/mm
 * \param[in,out] psf_fwhm Gaussian approx PSF FWHM in arcsec
 * \param[in,out] cdltx x-pixel size in degrees
 * \param[in,out] y-pixel size in degress
 * \param[in,out] crpxx reference pixel in x
 * \param[in,out] crpxy reference pixel in y
 * \param[in,out] skycoords_per_arcsec_x Unit conversion: skycoords per arcsec in x
 * \param[in,out] skycoords_per_arcsec_y Unit conversion: skycoords per arcmin in y
 * \param[in,out] xmin min x number of pixels
 * \param[in,out] ymin min y number of pixels
 * \param[in,out] xmax max x number of pixels
 * \param[in,out] ymax max y number of pixels
 * \param[in,out] PI_REBIN may be unnecessary
 * \param[in,out] max_chan may be unnecessary
 * \param[in,out] FOV_RADIUS field of view radius
 * \param[in,out] npix_x int number of pixels in sky x 
 * \param[in,out] npix_y int number of pixels in sky y */
typedef struct{
    /* This structure holds selected mission database parameters. */
    
    /* MDB direct parameters: */
    double  FOV_RADIUS;
    double  FOCALLEN;      
    double  PSF_FWHM;     
    double  DET_XSCL;  
    double  DET_YSCL;  
    double  DET_XNPIX;   
    double  DET_YNPIX;   
    double  HEA_XPIXSIZ;     
    double  HEA_YPIXSIZ;     
    double  HEA_XNPIX;       
    double  HEA_YNPIX;       
    double  SKY_XNPIX;   
    double  SKY_YNPIX;   
    double  FOC_XPIXSIZ;  
    double  FOC_YPIXSIZ;  
    double  FOC_XNPIX;   
    double  FOC_YNPIX;   
    double  OPTAXIS_FOCX;          
    double  OPTAXIS_FOCY;          
    double  AIM_FOCX;            
    double  AIM_FOCY;            
    double  FOC_ROTD;            
    double  FOC_XOFF;            
    double  FOC_YOFF;            

    /* MDB derived parameters: */
    double plate_scale;                  /* plate scale in degree/mm */
    double cdltx;                        /* x-pixel size in degrees */
    double cdlty;                        /* y-pixel size in degress */
    double crpxx;                        /* reference pixel in x */
    double crpxy;                        /* reference pixel in y */
    double xmin, xmax, ymin, ymax;       /* double number of pixels max/min */
    double psf_fwhm;                     /* Gaussian approx PSF FWHM in arcsec */
    double cdltx_sky;
    double cdlty_sky;
    double crpxx_sky;
    double crpxy_sky;
    double xmin_sky;
    double xmax_sky;
    double ymin_sky;
    double ymax_sky;
    double resampx;
    double resampy;
    double cdltx_foc;
    double cdlty_foc;
    double crpxx_foc;
    double crpxy_foc;
    double xmin_foc;
    double xmax_foc;
    double ymin_foc;
    double ymax_foc;
    double FOC_ROTD_sin;
    double FOC_ROTD_cos;
    double crpxx_det;
    double crpxy_det;
    double cdltx_det;
    double cdlty_det;
    double xmin_det;
    double xmax_det;
    double ymin_det;
    double ymax_det;
    char instmap_name[STR_MAX_LEN];
    int instmap_flag;
    long nx_imap;
    long ny_imap;
    long * array_imap;
} MDB_params;




/**
 * \brief Structure to hold vignette data
 * \param[in,out] extname extension name
 * \param[in,out] vigtype vignette file type
 * \param[in,out] type_desc string vig type description
 * \param[in,out] nrow number of rows in table
 * \param[in,out] ncol number of columns in table
 * \param[in,out] n_energy number of energies (usually = ncol-1)
 * \param[in,out] vig_matrix the ncol-1 x nrow matrix of vignette data
 * \param[in,out] energy_vec array of energies, in keV
 * \param[in,out] angle_vec array of angles, in arcmin
 * \param[in,out] vig_immat image matrix
 * \param[in,out] img_nx x dimension of image matrix
 * \param[in,out] img_ny y dimension of image matrix
 * \param[in,out] crvl1 image x-axis reference pixel
 * \param[in,out] crvl2 image y-axis reference pixel
 * \param[in,out] crpx1 image x-axis reference pixel coord
 * \param[in,out] crpx2 image y-axis reference pixel coord
 * \param[in,out] cdlt1 x pixel increment
 * \param[in,out] cdlt2 y pixel increment */
typedef struct{
    /* This structure holds the vignette data */
    char extname[STR_MAX_LEN];    /* extension name */
    int vigtype;                  /* vignette file type */
    char type_desc[STR_MAX_LEN];  /* character type description */
    long nrow;                    /* number of rows in table */
    long ncol;                    /* number of columns in table */
    long n_energy;                /* number of energies (usually = ncol-1) */
    double ** vig_matrix;         /* the ncol-1 x nrow matrix of vignette data */
    double * energy_vec;          /* array of energies, in keV */
    double * angle_vec;           /* array of angles, in arcmin */
    double ** vig_immat;          /* image matrix */
    long img_nx;                  /* x dimension of image matrix */
    long img_ny;                  /* y dimension of image matrix */
    double crvl1;                 /* image x-axis reference pixel */
    double crvl2;                 /* image y-axis reference pixel */
    double crpx1;                 /* image x-axis reference pixel coord */
    double crpx2;                 /* image y-axis reference pixel coord */
    double cdlt1;                 /* x pixel increment */
    double cdlt2;                 /* y pixel increment */
} Vig_struct;



/**
 * \brief Structure to hold PSF data
 * \param[in,out] psftype psf file type
 * \param[in,out] type_desc string vig type description
 * \param[in,out] ncol number of columns in table     
 * \param[in,out] nrow number of rows in table
 * \param[in,out] n_eef number of energies
 * \param[in,out] n_radii number of radii
 * \param[in,out] energy_vec array of energies, in keV
 * \param[in,out] angle_vec array of angles, in arcmin
 * \param[in,out] radii array of radii
 * \param[in,out] azim_vec array of azimuth data
 * \param[in,out] eef_matrix ncol-1 x nrow matrix of psf data
 * \param[in,out] crvl1 image x-axis reference pixel
 * \param[in,out] crvl1 image x-axis reference pixel
 * \param[in,out] crvl2 image y-axis reference pixel
 * \param[in,out] crpx1 image x-axis reference pixel coord
 * \param[in,out] crpx2 image y-axis reference pixel coord
 * \param[in,out] cdlt1 x pixel increment
 * \param[in,out] cdlt2 y pixel increment
 * \param[in,out] psf_immat img_nx x img_ny image matrix
 * \param[in,out] img_nx x dimension of image matrix
 * \param[in,out] img_ny y dimension of image matrix
 * \param[in,out] n_energy number of energies (usually = ncol-1) */
typedef struct{
    /* This structure holds the PSF data */
    int psftype;                  /* psf file type */
    char type_desc[STR_MAX_LEN];  /* character type description */
    long ncol;                    /* number of columns in table */
    long nrow;                    /* number of rows in table */
    long n_eef;                   /* number of energies */
    long n_radii;                 /* number of radii */
    double * energy_vec;          /* array of energies, in keV */
    double * angle_vec;           /* array of angles, in arcmin */
    double * radii;               /* array of radii */
    double * azim_vec;            /* array of azimuth data */
    double ** eef_matrix;         /* ncol-1 x nrow matrix of psf data */
} PSF_struct;


typedef struct{
    /* This structure holds PSF image data */
    int nimages;           /* number of image extensions */
    int * xdim;            /* array of image x-dimensions */
    int * ydim;            /* array of image y-dimensions */
    double * crvl1;        /* array of image x central pixel coord */
    double * crvl2;        /* array of image y central pixel coord */
    double * crpx1;        /* array of image x central pixel */
    double * crpx2;        /* array of image y central pixel */
    double * cdlt1;        /* array of image x pixel size */
    double * cdlt2;        /* array of image y pixel size */
    double * azim_vec;     /* array of image azimuth values */
    double * energy_vec;   /* array of image energies */
    double * angle_vec;    /* array of image angles */
    double ** prob_array;  /* array of probability arrays */
} PSF_image;


/**
 * \brief Structure to hold three corresponding doubles
 * \param[in,out] t1 the first double
 * \param[in,out] t2 the second double
 * \param[in,out] t3 the third double */
typedef struct triple{
    double t1;
    double t2;
    double t3;
} triple;


/**
 * \brief Structure to hold four corresponding doubles
 * \param[in,out] t1 the first double
 * \param[in,out] t2 the second double
 * \param[in,out] t3 the third double
 * \param[in,out] t4 the fourth double
 * \param[in,out] t5 the fifth double */ 

typedef struct quad{
    double t1;
    double t2;
    double t3;
    double t4;
    double t5;
} quad;


/**
 * \brief Structure to hold torus parameters                                                                                     
 * \param[in,out] num_nh Number of column density elements                                                                       
 * \param[in,out] nhvals Array of column densities                                                                               
 * \param[in,out] num_gam Number of powerlaw indecies                                                                            
 * \param[in,out] gamvals Array of powerlaw index                                                                                
 * \param[in,out] num_theta Number of opening angles                                                                             
 * \param[in,out] thvals Array of opening angles                                                                                 
 * \param[in,out] num_inc Number of inclination angles                                                                           
 * \param[in,out] incvals Array of inclination angles */
typedef struct{
    long num_nh;      /* Number of column density elements */
    double * nhvals;  /* Array of column densities */
    long num_gam;     /* Number of powerlaw indecies */
    double * gamvals; /* Array of powerlaw index */
    long num_theta;   /* Number of opening angles */
    double * thvals;  /* Array of opening angles */
    long num_inc;     /* Number of inclination angles */
    double * incvals; /* Array of inclination angles */
} torus_par_struct;

/**
 * \brief Structure to hold torus spectra data                                                                                   
 * \param[in,out] num_en Number of energies                                                                                      
 * \param[in,out] num_spec Number of spectra                                                                                     
 * \param[in,out] tor_energ_lo Array of low energies                                                                             
 * \param[in,out] tor_energ_hi Array of high energies                                                                            
 * \param[in,out] tor_energ_mid Array of energy midpoints                                                                        
 * \param[in,out] tor_energ_wid Array of energy bin width                                                                        
 * \param[in,out] tor_xspec_energy Array of energies for xspec                                                                   
 * \param[in,out] tor_spec Torus spectra  */
typedef struct{
    long num_en;
    long num_spec;
    double * tor_energ_lo;
    double * tor_energ_hi;
    double * tor_energ_mid;
    double * tor_energ_wid;
    double * tor_xspec_energy;
    double ** tor_spec;
} torus_spec_struct;


typedef struct{
    double *** burst_table;
    int * burst_tabidx;
    int xdim;
    int ydim;
    int zdim;
} burst_table_struct;


#ifdef use_legacy_heasp
#include "legacy_heasp.h"
#endif

/* These are defined in heacore/heasp/Cheasp.h, we give them typedefs here. */
/**
 * \brief ARF typedef - struct is defined in heacore/heasp/Cheasp.h */
typedef struct ARF ARF;

/**
 * \brief RMF typedef - struct is defined in heacore/heasp/Cheasp.h */
typedef struct RMF RMF;

/**
 * \brief PHA typedef - struct is defined in heacore/heasp/Cheasp.h */
typedef struct PHA PHA;



/**********************************************************************/
/**** Function definitions - in alphabetical order. *******************/


int apply_instmap(MDB_params *s_mdb,  /* structure holding MDB params */
                  double xfoc,      /* focx-coordinate to compare against dectector limits, in pixels */
                  double yfoc);     /* focy-coordinate to compare against dectector limits, in pixels */



/**
 * \brief Apply the psf distortion
 * \param[in] s_psf structure containing psf data
 * \param[in] s_mdb structure containing mission database params 
 * \param[in] x_in input x in arcmin, in coord system apropos for psf 
 * \param[in] y_in input y in arcmin, in coord system apropos for psf 
 * \param[in] energy photon energy in keV 
 * \param[out] dx psf displacement in arcmin, in x same coord system 
 * \param[out] dy psf displacement in arcmin, in y same coord system
 * \param[in/out MTstate  "save" state of Mersenne Twister RNG */
void apply_psf(PSF_struct * s_psf,  /* structure containing psf data */
	       PSF_image * s_psf_image,  /* structure containing PSF image data */
	       MDB_params * s_mdb,  /* structure containing mission database params */
	       double x_in,         /* input x in arcmin, in coord system apropos for psf */
	       double y_in,         /* input y in arcmin, in coord system apropos for psf */
	       double energy,       /* photon energy in keV */
	       double *dx,          /* psf displacement in arcmin, in x same coord system */
	       double *dy,         /* psf displacement in arcmin, in y same coord system */
	       HDmt_state *MTstate);      /* "save" state of Mersenne Twister RNG */

/**
 * \brief Apply vignetting
 * \param[in] s_vig the structure holding vig data 
 * \param[in] s_mdb the mission database structure
 * \param[in] x_off off-axis x-displacement in arcmin in relevent coord system
 * \param[in] y_off off-axis y-displacement in arcmin in relevent coord system
 * \param[in] energy photon energy in keV
 * \param[in/out MTstate  "save" state of Mersenne Twister RNG */
int apply_vignette(Vig_struct * s_vig,   /* the structure holding vig data */
		   MDB_params * s_mdb,   /* the mission database structure */
                   double x_off,         /* off-axis x-displacement in arcmin in relevent coord system */
                   double y_off,         /* off-axis y-displacement in arcmin in relevent coord system */
                   double energy,       /* photon energy in keV */
		   HDmt_state *MTstate);      /* "save" state of Mersenne Twister RNG */

/**
 * \brief Concatenate three double arrays
 * \param[in] A   first array
 * \param[in] nA  size of first array
 * \param[in] B   second array
 * \param[in] nB  size of second array
 * \param[in] C   third array
 * \param[in] nC  size of third array */
double * array_concat_dbl(double * A, int nA,
                          double * B, int nB,
                          double * C, int nC);


/**
 * \brief Concatenate three int arrays
 * \param[in] A   first array
 * \param[in] nA  size of first array
 * \param[in] B   second array
 * \param[in] nB  size of second array
 * \param[in] C   third array
 * \param[in] nC  size of third array */
int * array_concat_int(int * A, int nA,
                       int * B, int nB,
                       int * C, int nC);


                                                                                 
/**
 * \brief Concatenate three string arrays
 * \param[in] A   first array
 * \param[in] nA  size of first array
 * \param[in] B   second array
 * \param[in] nB  size of second array
 * \param[in] C   third array 
 * \param[in] nC  size of third array */
char ** array_concat_str(char ** A, int nA,
                         char ** B, int nB,
                         char ** C, int nC);



int burst_table(double expose,
                double ratburst,
                double tburst,
                double trise,
                double tdecay,
                int ntable,
                double * table_time,
                double * table_x);


/* We DO need this one for now - may replace later with C++ version that handles multiple energies */
/**
 * \brief Return the channel corresponding to input energy
 * \param[in] rmf structure containing rmf data
 * \param[in] energy input energy
 * \param[in] NumberPhoton number of photons
 * \param[out] channel the return channel
 * \param[in/out] MTstate  "save" state of RNG */
void C_ReturnChannel(struct RMF *rmf, float energy, long NumberPhoton, long *channel, HDmt_state *MTstate);

/* read the RMF matrix from an open FITS file - if there are multiple RMF extensions then read the one in RMFnumber */

int C_ReadRMFMatrix(fitsfile *fptr, long RMFnumber, struct RMF *rmf);

/* read the RMF ebounds from an open FITS file - if there are multiple EBOUNDS extensions then read the one in EBDnumber */

int C_ReadRMFEbounds(fitsfile *fptr, long EBDnumber, struct RMF *rmf);

/* read the effective areas from an open FITS file - if there are multiple SPECRESP extensions then read the one in ARFFnumber */

int C_ReadARF(fitsfile *fptr, long ARFnumber, struct ARF *arf);

/* read the type I PHA extension from an open FITS file - if there are multiple PHA extensions then read the one in PHAnumber */

int C_ReadPHAtypeI(fitsfile *fptr, long PHAnumber, struct PHA *pha);

int C_SP_read_col(fitsfile *fptr, int datatype, char *column_name, long number_values, void *column_values);

int C_SPII_read_col(fitsfile *fptr, int datatype, char *column_name, long ispec, long number_values, void *column_values);

int C_SP_read_key(fitsfile *fptr, int datatype, char *keyword_name, void *keyword_value, void *keyword_default);

int C_SPII_read_key(fitsfile *fptr, int datatype, char *keyword_name, long ispec, void *keyword_value, void *keyword_default);






/**
 * \brief Check compatibility of ARF and RMF files
 * \param[in] s_cal       structure containing calibration filenames 
 * \param[in] rmf_struct  RMF structure 
 * \param[in] arf_struct  ARF structure 
 * \param[in] nebin       number of arf input energy bins 
 * \param[in] nebin_rmf   number of rmf input energy bins 
 * \param[in] nchan       number of output energy bins [PI channels] from rmf ebounds ext 
 * \param[out] ebin_lo     lower boundary of arf energy grid bins 
 * \param[out] ebin_hi     upper boundary of arf energy grid bins 
 * \param[out] ebin_mid    midpoint of arf energy grid bins 
 * \param[out] ebin_del    grid spacing of arf energy grid bins 
 * \param[out] area        effective area from arf 
 * \param[out] ebin_lo_rmf lower boundary of rmf energy grid bins 
 * \param[out] ebin_hi_rmf upper boundary of rmf energy grid bins 
 * \param[out] emin lower  boundary of output energy grid bins 
 * \param[out] emax        upper boundary of output energy grid bins 
 * \param[out] ecen        midpoint of output energy grid bins 
 * \param[out] edelt       grid spacing output energy grid bins */
int check_arf_rmf(CalFiles * s_cal,       /* structure containing calibration filenames */
                  RMF * rmf_struct,       /* RMF structure */
                  ARF * arf_struct,       /* ARF structure */
                  int nebin,              /* number of arf input energy bins */
                  int nebin_rmf,          /* number of rmf input energy bins */
                  int nchan,              /* number of output energy bins [PI channels] from rmf ebounds ext */
                  double ** ebin_lo,       /* lower boundary of arf energy grid bins */
                  double ** ebin_hi,       /* upper boundary of arf energy grid bins */
                  double ** ebin_mid,      /* midpoint of arf energy grid bins */
                  double ** ebin_del,      /* grid spacing of arf energy grid bins */
                  double ** area,          /* effective area from arf */
                  double ** ebin_lo_rmf,   /* lower boundary of rmf energy grid bins */
                  double ** ebin_hi_rmf,   /* upper boundary of rmf energy grid bins */
                  double ** emin,          /* lower boundary of output energy grid bins */
                  double ** emax,          /* upper boundary of output energy grid bins */
                  double ** ecen,          /* midpoint of output energy grid bins */
                  double ** edelt);        /* grid spacing output energy grid bins */

/**
 * \brief Check that the calibration files actually exist
 * \param[in] s_cal Structure containing calfile names */
int check_calfiles_exist(CalFiles * s_cal);

/**
 * \brief Remove pi_chans=-1 values, and resize x, y, ide, pi_chans arrays.)
 * \param[in/out] xe X position array
 * \param[in/out] ye Y position array
 * \param[in/out] ide Pixel index array
 * \param[in/out] pi_chans PI channel array
 * \param[in] nkeep size of the arrays */
long clean_pi_chan(int ** xe, int ** ye, int **ide, long ** pi_chans, int nkeep);

/**
 * \brief Compare two doubles
 * \param[in] a the first double
 * \param[in] b the second double */
int compare_double(const void *a, const void *b);

/**
 * \brief Compare the first doubles within two "triple" structures
 * \param[in] a the first double
 * \param[in] b the second double */
int compare_triple(const void *a, const void *b);

/**
 * \brief Compare two "triple" structures, first by the first array, then by the second
 * \param[in] a the first double
 * \param[in] b the second double */
int compare_two_level(const void *a, const void *b);

/**
 * \brief Copy a file
 * \param[in] source  Name of the source file to be copied
 * \param[in] target  Name of the file to be copied into
 * \param[in] debug   Flag to print debug info */
int copy_file(char * source, char * target, int debug);


/**
 * \brief Free ARF structure from memory
 * \param[in] arf_struct Structure containing ARF data */
void deallocate_arf_data(ARF * arf_struct);

/**
 * \brief Free data allocated during "check_calfiles_exist" from memory
 * \param[in] ebin_lo     lower boundary of arf energy grid bins
 * \param[in] ebin_hi     upper boundary of arf energy grid bins
 * \param[in] ebin_mid    midpoint of arf energy grid bins
 * \param[in] ebin_del    grid spacing of arf energy grid bins
 * \param[in] area        effective area from arf
 * \param[in] ebin_lo_rmf lower boundary of rmf energy grid bins
 * \param[in] ebin_hi_rmf upper boundary of rmf energy grid bins
 * \param[in] emin        lower boundary of output energy grid bins
 * \param[in] emax        upper boundary of output energy grid bins
 * \param[in] ecen        midpoint of output energy grid bins
 * \param[in] edelt       grid spacing output energy grid bins */
void deallocate_bin_data(double ** ebin_lo,       /* lower boundary of arf energy grid bins */
			 double ** ebin_hi,       /* upper boundary of arf energy grid bins */
			 double ** ebin_mid,      /* midpoint of arf energy grid bins */
			 double ** ebin_del,      /* grid spacing of arf energy grid bins */
			 double ** area,          /* effective area from arf */
			 double ** ebin_lo_rmf,   /* lower boundary of rmf energy grid bins */
			 double ** ebin_hi_rmf,   /* upper boundary of rmf energy grid bins */
			 double ** emin,          /* lower boundary of output energy grid bins */
			 double ** emax,          /* upper boundary of output energy grid bins */
			 double ** ecen,          /* midpoint of output energy grid bins */
			 double ** edelt);        /* grid spacing output energy grid bins */

/**
 * \brief Free burst structure from memory
 * \param[in] btab   Structure containing burst data */
void deallocate_burst_data(burst_table_struct * btab);

/**
 * \brief Free data allocated during imagedis routine
 * \param[in] cprob       cumulative probability matrix
 * \param[in] nel_subimg  number of elements in subimage and cprob
 * \param[in] ra_img      npos-sized 1D array of photon RA
 * \param[in] dec_img     npos-sized 1D array of photon DEC */
void deallocate_imagedis_data(double *** cprob, double ** cprob_vec, int nel_subimg);

/**
 * \brief Free data from background structure
 * \param[in] iback_pha_struct  background structure */
void deallocate_pha_data(PHA * iback_pha_struct);

/**
 * \brief Free PSF data
 * \param[in] s_psf  psf structure */
void deallocate_psf_data(PSF_struct * s_psf, PSF_image * s_psf_image);

/**
 * \brief Free RMF data
 * \param[in] rmf_struct  rmf structure */
void deallocate_rmf_data(RMF * rmf_struct);

/**
 * \brief Free source data from memory
 * \param[in] nsource          number of sources found in input source file 
 * \param[in] ras              array of source RAs 
 * \param[in] decs             array of source DECs 
 * \param[in] colden           array of source column densities 
 * \param[in] spectype         array of source spectral type 
 * \param[in] specpar          value of spectral param for all sources 
 * \param[in] fluxpar          flux for all sources can be 0 for spectral file 
 * \param[in] band_lo          flux lower bandpass for all sources 
 * \param[in] band_hi          flux upper bandpass for all sources 
 * \param[in] sfilename        name of spectral user input file for all sources 
 * \param[in] sformat          spectral sformat file format flag 1 or 2 for all sources 
 * \param[in] sunits           spectral file flux unit tag 1-9 for all sources 
 * \param[in] period           period for all sources 
 * \param[in] pulse_fraction   pulse fraction for all sources 
 * \param[in] tburst,          burst start time for all original sources, 0 for constant 
 * \param[in] trise,           burst risetime for all original sources, 0 for constant 
 * \param[in] tdecay,          burst decay time for all original sources, 0 for constant 
 * \param[in] burst_rat,       burst ratio for all original sources, 0 for constant 
 * \param[in] burst_tabidx,    array of index identifying lookup table for burst
 * \param[in] ifilename        name of image user input file for all sources 
 * \param[in] sd_matrix_size   maximum number of spatial distribution quantities needed 
 * \param[in] sd_param_matrix  matrix of spatial distribution quantities needed to apply source distribution */
void deallocate_source_data(int nsource,                  /* number of sources found in input source file */
                            double ** ras,                 /* array of source RAs */
                            double ** decs,                /* array of source DECs */
                            double ** colden,              /* array of source column densities */
                            int ** spectype,              /* array of source spectral type */
                            double ** specpar,             /* value of spectral param for all sources */
                            double ** fluxpar,             /* flux for all sources, can be 0 for spectral file */
                            double ** band_lo,             /* flux lower bandpass for all sources */
                            double ** band_hi,             /* flux upper bandpass for all sources */
                            char ** sfilename,           /* name of spectral user input file for all sources */
                            int ** sformat,               /* spectral sformat file format flag 1 or 2 for all sources */
                            int ** sunits,                /* spectral file flux unit tag 1-9 for all sources */
                            double ** period,              /* period for all sources */
                            double ** pulse_fraction,      /* pulse fraction for all sources */
			    double ** tburst,             /* burst start time for all original sources, 0 for constant */
                            double ** trise,              /* burst risetime for all original sources, 0 for constant */
                            double ** tdecay,             /* burst decay time for all original sources, 0 for constant */
                            double ** burst_rat,          /* burst ratio for all original sources, 0 for constant */
			    int ** burst_tabidx,          /* array of index identifying lookup table for burst */
                            char ** ifilename,           /* name of image user input file for all sources */
                            int sd_matrix_size,           /* maximum number of spatial distribution quantities needed */
                            double *** sd_param_matrix);   /* matrix of spatial distribution quantities needed to apply source distribution */

/**
 * \brief Free data allocated during sourcedis routine
 * \param[in] dx_arcmin  delta x in arcmin
 * \param[in] dy_arcmin  delta y in arcmin */
void deallocate_sourcedis_data(double ** dx_arcmin, double ** dy_arcmin);


/**
 * \brief Free torus structure arrays
 * \param[in] s_tpar  Torus parameter structure
 * \param[in] s_tspec Torus spectra structure */
void deallocate_torus(torus_par_struct * s_tpar, torus_spec_struct *s_tspec);


/**
 * \brief Free vignette data from memory
 * \param[in] s_vig  structure containing vig data */
void deallocate_vignette_data(Vig_struct * s_vig);

/**
 * \brief The primary work section of the code
 * \param[in] outfile     Output event filename 
 * \param[in] s_obs               Structure containing observatory parameters 
 * \param[in] s_calfiles          Structure containing calibration filenames 
 * \param[in] s_mdb               Structure containing mission database parameters 
 * \param[in] s_vig               Structure containing vignette data 
 * \param[in] s_psf               Structure containing psf data 
 * \param[in] nebin               number of arf input energy bins 
 * \param[in] ebin_lo             lower boundary of arf energy grid bins 
 * \param[in] ebin_hi             upper boundary of arf energy grid bins 
 * \param[in] ebin_mid            midpoint of arf energy grid bins 
 * \param[in] ebin_del            grid spacing of arf energy grid bins 
 * \param[in] area                effective area from arf 
 * \param[in] nchan               number of output energy bins [PI channels] from rmf ebounds ext 
 * \param[in] emin                lower boundary of output energy grid bins 
 * \param[in] emax                upper boundary of output energy grid bins 
 * \param[in] edelt               grid spacing output energy grid bins 
 * \param[in] rmf_struct          Holds RMF data 
 * \param[in] ibspec              internal background spectrum 
 * \param[in] ib_expose           internal background exposure time 
 * \param[in] ib_backscal         internal background scaling factor 
 * \param[in] ib_firsthcan        first background channel number
 * \param[in] nsource             number of sources found in input source file 
 * \param[in] ras                 array of source RAs 
 * \param[in] decs                array of source DECs 
 * \param[in] colden              array of source column densities 
 * \param[in] redshifts           array of redshifts
 * \param[in] intabs              array of intrinsic absorptions
 * \param[in] spectype            array of source spectral type 
 * \param[in] specpar             value of spectral param for all sources 
 * \param[in] fluxpar             flux for all sources can be 0 for spectral file 
 * \param[in] band_lo             flux lower bandpass for all sources 
 * \param[in] band_hi             flux upper bandpass for all sources 
 * \param[in] sfilename           name of spectral user input file for all sources "none" for spectral model 
 * \param[in] sformat             spectral sformat file format flag 1 or 2 for all sources; can be 0 for spectral model 
 * \param[in] sunits              spectral file flux unit tag 1-9 for all sources can be 0 for spectral model 
 * \param[in] period              period for all sources 0 for constant 
 * \param[in] pulse_fraction      pulse fraction for all sources 0 for constant 
 * \param[in] tburst,             burst start time for all original sources, 0 for constant 
* \param[in] trise,               burst risetime for all original sources, 0 for constant 
* \param[in] tdecay,              burst decay time for all original sources, 0 for constant
* \param[in] burst_rat,           burst ratio for all original sources, 0 for constant 
 * \param[in] ifilename           name of image user input file for all sources; "none" for point source or spatial model 
 * \param[in] sd_matrix_size      maximum number of spatial distribution quantities needed 
 * \param[in] sd_param_matrix     matrix of spatial distribution quantities needed to apply source distribution 
 * \param[in/out] spectra_errstatus error status of spectra calls
 * \param[in] debug               debug option 
 * \param[in/out] MTstate         state of random number generator */
int doWork(char * outfile,    /* Output event filename */
	   ObsParams * s_obs,        /* Structure containing observatory parameters */
	   CalFiles * s_calfiles,     /* Structure containing calibration filenames */
	   MDB_params * s_mdb,        /* Structure containing mission database parameters */
	   Vig_struct * s_vig,        /* Structure containing vignette data */
           PSF_struct * s_psf,        /* Structure containing psf data */
	   PSF_image * s_psf_image,   /* Structure containg PSF image data */
           GeneralBackground * s_back,  /* Structure containing background filenames */
	   RMF * rmf_struct,          /* Holds RMF data */
	   ARF * arf_struct,          /* Holds ARF data */
	   burst_table_struct * btab,  /* holds burst data */
	   int nebin,                 /* number of arf input energy bins */
           double * ebin_lo,           /* lower boundary of arf energy grid bins */
           double * ebin_hi,           /* upper boundary of arf energy grid bins */
           double * ebin_mid,          /* midpoint of arf energy grid bins */
           double * ebin_del,          /* grid spacing of arf energy grid bins */
           double * area,              /* effective area from arf */
           int nchan,                 /* number of output energy bins [PI channels] from rmf ebounds ext */
           double * emin,              /* lower boundary of output energy grid bins */
           double * emax,              /* upper boundary of output energy grid bins */
           double * edelt,             /* grid spacing output energy grid bins */
           double * ibspec,            /* internal background spectrum */
           double ib_expose,           /* internal background exposure time */
           double * ib_backscal,       /* internal background scaling factor */
	   long ib_firsthcan,           /* first background channel number */
	   int nsource,               /* number of sources found in input source file */
           double * ras,               /* array of source RAs */
           double * decs,              /* array of source DECs */
           double * colden,            /* array of source column densities */
	   double * redshifts,         /* array of redshifts */
	   double * intabs,            /* array if intrinsic absorptions */
           int * spectype,            /* array of source spectral type */
           double * specpar,           /* value of spectral param for all sources */
           double * fluxpar,           /* flux for all sources, can be 0 for spectral file */
           double * band_lo,           /* flux lower bandpass for all sources */
           double * band_hi,           /* flux upper bandpass for all sources */
           char ** sfilename,        /* name of spectral user input file for all sources, "none" for spectral model */
           int * sformat,             /* spectral sformat file format flag 1 or 2 for all sources; can be 0 for spectral model */
           int * sunits,              /* spectral file flux unit tag 1-9 for all sources, can be 0 for spectral model */
           double * period,            /* period for all sources, 0 for constant */
           double * pulse_fraction,    /* pulse fraction for all sources, 0 for constant */
	   double * tburst,             /* burst start time for all original sources, 0 for constant */
	   double * trise,              /* burst risetime for all original sources, 0 for constant */
	   double * tdecay,             /* burst decay time for all original sources, 0 for constant */
	   double * burst_rat,          /* burst ratio for all original sources, 0 for constant */
           char ** ifilename,        /* name of image user input file for all sources; "none" for point source or spatial model */
           int sd_matrix_size,        /* maximum number of spatial distribution quantities needed */
           double *** sd_param_matrix, /* matrix of spatial distribution quantities needed to apply source distribution */
	   int * spectra_errstatus,    /* error status for spectra calls */
           int debug,                /* debug option */
	   HDmt_state *MTstate);     /* "save" state of Mersenne Twister RNG */

	   
/**
 * \brief Return elevation and azimuth of a given vector
 * \param[in] V     a given vector
 * \param[out] EL   elevation of given vector
 * \param[out] AZ   azimution of given vector */
void elaz8(double V[], double *EL, double *AZ);


/**
 * \brief Use Euler angles E1,E2,E3 to rotate vector R into R1 or vice versa.
 * \param[in,out] R    input vector
 * \param[in,out] R1   rotated vector
 * \param[in] E1       euler 1
 * \param[in] E2       euler 2
 * \param[in] E3       euler 3
 * \param[in] MTEST    rotate forward or backward */
void eulrot8 (double R[], double R1[], double E1, double E2, double E3, int MTEST);


/**
 * \brief  Flag events which occur on the same pixel and within dt as pileup events
 * \param[in] ounit        fitsfile pointer
 * \param[in] nevt         number of events to check
 * \param[in] start_index  index of first event to check
 * \param[in] dtpileup     pileup timescale
 * \param[in] tevt         array of times
 * \param[in] pixid        array of pixel IDs
 * \param[in/out] deadtime total time lost to pileup
 * \param[in/out] n_pileup number of events flagged as pileup
 * \param[in] debug             flag to debug */
int flag_pileup(fitsfile * ounit,
                int nevt,
                int start_index,
                double dtpileup,
                double * tevt,
                int * pixid,
		double * deadtime,
                long * n_pileup,
                int debug);


/**
 * \brief Return the index of an array value nearest to the input value
 * \param[in] pos   value for which vec is to be searched
 * \param[in] nvec  size of 1D array vec
 * \param[in] vec   1D array to be searched */
int find_index(double pos,       /* value for which vec is to be searched */
               int nvec,         /* size of 1D array vec */
               double * vec);    /* 1D array to be searched */

/**
 * \brief Return natural log of gamma function
 * \param[in] xx  input value */
double gammln(double xx);

/**
 * \brief Return a random Gaussian deviate.  (From Numerical Recipes)
 * \param[in/out MTstate  "save" state of Mersenne Twister RNG */
double gasdev(HDmt_state *MTstate);

/* CALDB function definitions.  Uncomment these when ready to add.
int get_caldb_arf(char * arffile, ObsParams * s_obs, Misc * m);
int get_caldb_files(CalFiles * s_cal, ObsParams * s_obs);
int get_caldb_psf(char * psffile, ObsParams * s_obs, Misc * m);
int get_caldb_rmf(char * rmffile, ObsParams * s_obs, Misc * m);
int get_caldb_vignette(char * vigfile, ObsParams * s_obs, Misc * m);
*/

/**
 * \brief Return number of populated lines in a spectra file
 * \param[in] filename  Name of the file to search 
 * \param[in] debug     flag to print debug info */
int get_nlines(char * filename, int debug);

/**
 * \brief Return number of populated lines in a source file, i.e. the number of sources
 * \param[in] filename  Name of the file to search */
int get_nsource(char * filename  /* source input filename */);


/**
 * \brief Read in the simulation parameters from heasim.par
 * \param[in] argc                 number of string inputs
 * \param[in] argv                 string inputs 
 * \param[out] infile              Name of source input filename 
 * \param[out] outfile             Output event filename 
 * \param[out] par_mode            Mode for parameter file 
 * \param[out] debug               Flag for setting debuggin mode (BOOLEAN) 
 * \param[out] clobber             Overwrite existing output file? (BOOLEAN) 
 * \param[out] getinfile   Copy sample source def file to current working dir?  (BOOLEAN) 
 * \param[out] s_calfiles          Structure containing cal filenames (see definition in main) 
 * \param[out] s_back              Structure containing background filenames (see def in main) 
 * \param[out] s_obs               Structure containing observation parameters (see def in main) 
 * \param[out] mdbfile             Name of mission database file 
 * \param[out] seed          Seed for RNG */
int getPars(int argc, char ** argv,         /* string inputs */
            char * infile,                  /* Name of source input filename */
            char * outfile,                 /* Output event filename */
            char * par_mode,                /* Mode for parameter file */
            int * debug,                    /* Flag for setting debuggin mode (BOOLEAN) */
            int * clobber,                  /* Overwrite existing output file? (BOOLEAN) */
            int * getinfile,        /* Copy sample source def file to current working dir?  (BOOLEAN) */
            CalFiles * s_calfiles,          /* Structure containing cal filenames (see definition in main) */
            GeneralBackground * s_back,     /* Structure containing background filenames (see def in main) */
            ObsParams * s_obs,              /* Structure containing observation parameters (see def in main) */
            char * mdbfile,                 /* Name of mission database file */
	    long * seed);  /* Seed for RNG */


/**
 * \brief Query all of the .c files built in heasim and return the most recent CVS date.
 * \param[in\out] version         String version to be returned */
void get_version(char * version);


/**
 * \brief Determine initial pos in sky of npos source photons
 * \param[in] crvl1_img     image x-axis reference pixel 
 * \param[in] crvl2_img     image y-axis reference pixel 
 * \param[in] crpx1_img     image x-axis reference pixel coord 
 * \param[in] crpx2_img     image y-axis reference pixel coord 
 * \param[in] cdlt1_img     image x-axis decrement 
 * \param[in] cdlt2_img     image y-axis decrement 
 * \param[in] nel_subimg    number of elements in subimage and in cprob 
 * \param[in] cprob         nel_subimage x 3 array with cumulative probabily and corresponding x/y pixel values 
 * \param[in] cprob_vec     first column of cprob
 * \param[out] ra_img       npos-sized 1D array of photon RA 
 * \param[out] dec_img      npos-sized 1D array of photon DEC
 * \param[in] MTstate       "save" state of Mersenne Twister RNG */
int imagedis(double crvl1_img,    /* image x-axis reference pixel RA */
	     double crvl2_img,    /* image y-axis reference pixel Dec */
	     double crpx1_img,    /* image x-axis reference pixel */
	     double crpx2_img,    /* image y-axis reference pixel */
	     double cdlt1_img,    /* image x-axis decrement */
	     double cdlt2_img,    /* image y-axis decrement */
	     int nel_subimg,      /* number of elements in subimage and in cprob */
	     double ** cprob,     /* nel_subimage x 3 array with cumulative probabily and corresponding x/y pixel values */
	     double * cprob_vec,  /* first column of cprob */
	     double *ra_img,      /* photon RA */
	     double *dec_img,     /* photon DEC */
	     HDmt_state *MTstate); /* save state of RNG */



/**
 * \brief Initialize all structure contents to zero.
 * \param[in] s_obs   observation param structure
 * \param[in] s_cal   calibration file structure
 * \param[in] s_back  background file structure
 * \param[in] s_mdb    mission database structure
 * \param[in] s_vig   vignette data structure
 * \param[in] s_psf   point spread function structure
 * \param[in] s_psf_image  PSF image structure */
void initialize_structures(ObsParams * s_obs,
                           CalFiles * s_cal,
                           GeneralBackground * s_back,
                           MDB_params *s_mdb,
                           Vig_struct * s_vig,
                           PSF_struct * s_psf,
                           PSF_image * s_psf_image,
			   burst_table_struct * btab);




/**
 * \brief Given y(x) at N points, estimate y(z) at the single point x=z.
 * \param[in] n   number of points where y is evaluated
 * \param[in] x   array of points where y is evaluated
 * \param[in] y   array of function evaluated at x
 * \param[in] z   single point where y(z) is to be estimated*/
double interpol_hunt(int n, double *x, double *y, double z);


/**
 * \brief Determine if pixel is valid
 * \param[in] s_mdb  mission database structure
 * \param[in] x_foc  x coordinate
 * \param[in] y_foc  y coordinate */
int isrealpixel(MDB_params * s_mdb,  /* structure containing mission database parameters */
                double x_foc,
                double y_foc);



int make_burst_tabarray(double exposure,
                        int nsource_new,
                        double * ratburst_new,
                        double * tburst_new,
                        double * trise_new,
                        double * tdecay_new,
                        int * burst_tabidx_new,
                        int nburst_new,
                        int ntable,
                        burst_table_struct * btab);



/**
 * \brief Concatenate three double type matrix, all having the same number of columns
 * \param[in] A  first matrix
 * \param[in] nrowsA  number of rows in first matrix
 * \param[in] B  second matrix
 * \param[in] nrowsB  number of rows in second matrix
 * \param[in] C  third matrix
 * \param[in] nrows C  number of rows in third matrix
 * \param[in] ncols  number of columns */
double ** matrix_concat_dbl(double ** A, int nrowsA,
                            double ** B, int nrowsB,
                            double ** C, int nrowsC,
                            int ncols);


/**
 * \brief Return an unsigned long int seed for use in HDmtInit(seed) */
unsigned long int MTseed(void);

/**
 * \brief Recompute the number of counts in a channel assuming Poisson statistics
 * \param[in] xxm  Mean of channel val
 * \param[in/out MTstate  "save" state of Mersenne Twister RNG */
double poidev(double xxm, HDmt_state *MTstate);


/**
 * \brief Prepare input image to be used to distribute photons probabilistically on the sky
 * \param[in] ifilename    input image filename 
 * \param[in] xmin         first pixel number in x-direction 
 * \param[in] xmax         last pixel number in x-direction 
 * \param[in] ymin         first pixel number in y-direction 
 * \param[in] ymax         last pixel number in y-direction 
 * \param[out] crvl1_img   image x-axis reference pixel 
 * \param[out] crvl2_img   image y-axis reference pixel 
 * \param[out] crpx1_img   image x-axis reference pixel coord 
 * \param[out] crpx2_img   image y-axis reference pixel coord 
 * \param[out] cdlt1_img   image x-axis decrement 
 * \param[out] cdlt2_img   image y-axis decrement 
 * \param[out] nel_subimg  number of elements in subimage and in cprob 
 * \param[out] cprob       nel_subimage x 3 array with cumulative probabily and corresponding x/y pixel values */
int process_image(char * ifilename,   /* input image filename */
                  double xmin,           /* first pixel number in x-direction */
                  double xmax,           /* last pixel number in x-direction */
                  double ymin,           /* first pixel number in y-direction */
                  double ymax,           /* last pixel number in y-direction */
                  double *crvl1_img,      /* image x-axis reference pixel */
                  double *crvl2_img,      /* image y-axis reference pixel */
                  double *crpx1_img,    /* image x-axis reference pixel coord */
                  double *crpx2_img,    /* image y-axis reference pixel coord */
                  double *cdlt1_img,    /* image x-axis decrement */
                  double *cdlt2_img,    /* image y-axis decrement */
                  int *nel_subimg,    /* number of elements in subimage and in cprob */
                  double *** cprob,    /* nel_subimage x 3 array with cumulative probabily and corresponding x/y pixel values */
		  int debug);           /* debug flag */

/**
 * \brief Convert RA,DEC to any pixel coordinates
 * \param[in] ra          input RA 
 * \param[in] dec         input DEC 
 * \param[in] x_ref       reference x-value 
 * \param[in] y_ref       reference y-value 
 * \param[in] ra_ref      RA at (x_refy_ref) 
 * \param[in] dec_ref     DEC at (x_ref y_ref) 
 * \param[in] roll        "roll" angle of y-axis from north postive clockwise for N to x 
 * \param[in] xpixsize    degrees per pixel, x-direction, pos/neg if look up/down 
 * \param[in] ypixsize    degrees per pixel, y-direction 
 * \param[out] x          output x 
 * \param[out] y          output y */
void rd2xy(double ra,        /* input RA */
           double dec,       /* input DEC */
           double x_ref,     /* reference x-value */
           double y_ref,     /* reference y-value */
           double ra_ref,    /* RA at (x_ref,y_ref) */
           double dec_ref,   /* DEC at (x_ref, y_ref) */
           double roll,      /* "roll" angle of y-axis from north, postive clockwise for N to x */
           double xpixsize,  /* degrees per pixel, x-direction, pos/neg if look up/down */
           double ypixsize,  /* degrees per pixel, y-direction */
           double *x,        /* output x */
           double *y);       /* output y */

/**
 * \brief Read ancillary response file
 * \param[in] s_cal   structure containing calibration filenames
 * \param[in] nebin   number of arf input energy bins
 * \param[in] debug   debugging flag */
ARF read_ARF(CalFiles * s_cal,  /* structure containing calibration filenames */
             int *nebin,        /* number of arf input energy bins */
             int debug);        /* debugging flag */


int read_instmap(MDB_params * s_mdb, int debug);

/**
 * \brief Read the internal background file
 * \param[in] s_cal           structure containing calibration filenames
 * \param[in] nchan           number of output energy bins [PI channels] from rmf ebounds ext
 * \param[in] ebounds_TLMIN   lower limit on energy spectrum
 * \param[out] ibspec         internal background spectrum
 * \param[out] ib_backscal    internal background scaling factor
 * \param[out] ib_expose      internal background exposure time  
 * \param[out] ib_firstchan   first channel in background spectrum */
int read_internal_background(CalFiles * s_cal,     /* structure containing background filenames */
                             MDB_params * s_mdb,   /* mdb parameters */
                             int nchan,            /* number of output energy bins [PI channels] from rmf ebounds ext */
			     int ebounds_TLMIN,     /* lower limit on energy spectrum */
			     double ** ibspec,      /* internal background spectrum */
                             double ** ib_backscal, /* internal background scaling factor */
                             double * ib_expose,    /* internal background exposure time */
                             long * ib_firstchan);   /* first channel in background spectrum */



/**
 * \brief Read the mission database file and parse necessary parameters
 * \param[in] mdbfile                 MDB filename 
 * \param[in] mission             Observatory name 
 * \param[in] instrume              Instrument name 
 * \param[in] filter                  Filter name 
 * \param[in] mode                    Mode (CCD read mode?) 
 * \param[out] FOV_RADIUS   Detector FOV radius in arcmin.  For circular FOV 
 * \param[out] FOCALLEN         Focal length in meters 
 * \param[out] PSF_FWHM        Gaussian approx PSF FWHM in arcsec 
 * \param[out] pixelsizex_mic         sky x pixel length in microns 
 * \param[out] pixelsizey_mic         sky y pixel length in microns 
 * \param[out] HEA_XNPIX          number of pixels in sky x 
 * \param[out] HEA_YNPIX          number of pixels in sky y 
 * \param[out] pixelsize_focx_min     foc pixel length in arcmin 
 * \param[out] pixelsize_focy_min     foc pixel width in arcmin 
 * \param[out] FOC_XNPIX      number of pixels in foc_x 
 * \param[out] FOC_YNPIX      number of pixels in foc_y 
 * \param[out] DET_XSCL     det pixel length in microns 
 * \param[out] DET_YSCL     det pixel width in microns 
 * \param[out] DET_XNPIX      number of pixels in det x 
 * \param[out] DET_YNPIX      number of pixels in det y 
 * \param[out] optic_detx             detx of optical axis in pixels 
 * \param[out] optic_dety             dety of optical axis in pixels 
 * \param[out] rot_detx               rotation angle of detx in foc-plane 
 * \param[out] rot_dety               rotation angle of dety in foc-plane 
 * \param[out] aim_detx               aimpoint in detx 
 * \param[out] aim_dety               aimpoint in dety */
int read_mdb(char * mdbfile,                /* MDB filename */
             const char * mission,      /* Observatory name */
             const char * instrume,       /* Instrument name */
             const char * filter,           /* Filter name */
             const char * mode,             /* Mode (CCD read mode?) */
             MDB_params * s_mdb,
	     int debug);


/**
 * \brief Read the psf file and fill structure
 * \param[in] psffile   The psf file
 * \param[out] s_pdf     Structure to hold psf data
 * \param[in] debug      debugging flag */
int read_psf(char * psffile, PSF_struct * s_psf, PSF_image * s_psf_image, int debug);


int read_psf_image(char * psffile, PSF_image * s_psf_image, int debug);


int read_redshift(int n_source,
                  int n_psback,
                  char * zfile,
                  double ** redshifts,
                  double ** intabs,
		  int debug);

/**
 * \brief Read the RMF file and populate structure
 * \param[in] s_cal            Structure containing calibration file names
 * \param[out] nebin_rmf       number of rmf input energy bins
 * \param[out] nchan           number of output energy bins [PI channels] from rmf ebounds ext
 * \param[out] ebounds_TLMIN1  EBOUNDS minimum allowed PI channel value
 * \param[out] ebounds_TLMAX1  EBOUNDS maximum allowed PI channel value */
RMF read_RMF(CalFiles * s_cal,       /* structure containing calibration filenames */
             int *nebin_rmf,         /* number of rmf input energy bins */
             int *nchan,             /* number of output energy bins [PI channels] from rmf ebounds ext */
             long *ebounds_TLMIN1,   /* EBOUNDS minimum allowed PI channel value */
             long *ebounds_TLMAX1,   /* EBOUNDS maximum allowed PI channel value */
	     int debug);             /* debug flag */
	     

/**
 * \brief Read parameters from the source definition file 
 * \param[in] infile              The source input filename 
 * \param[out] nsource            number of sources found in input source file 
 * \param[out] ras                array of source RAs 
 * \param[out] decs               array of source DECs 
 * \param[out] colden             array of source column densities 
 * \param[out] spectype           array of source spectral type 
 * \param[out] specpar            value of spectral param for all sources 
 * \param[out] fluxpar            flux for all sources can be 0 for spectral file 
 * \param[out] band_lo            flux lower bandpass for all sources 
 * \param[out] band_hi            flux upper bandpass for all sources 
 * \param[out] sfilename          name of spectral user input file for all sources "none" for spectral model 
 * \param[out] sformat            spectral sformat file format flag 1 or 2 for all sources; can be 0 for spectral model 
 * \param[out] sunits             spectral file flux unit tag 1-9 for all sources can be 0 for spectral model 
 * \param[out] period             period for all sources 0 for constant 
 * \param[out] pulse_fraction     pulse fraction for all sources 0 for constant 
 * \param[out] tburst,            burst start time for all original sources, 0 for constant
 * \param[out] trise,             burst risetime for all original sources, 0 for constant 
 * \param[out] tdecay,            burst decay time for all original sources, 0 for constant 
 * \param[out] burst_rat,         burst ratio for all original sources, 0 for constant 
 * \param[out] ifilename          name of image user input file for all sources; "none" for point source or spatial model 
 * \param[out] sd_matrix_size     maximum number of spatial distribution quantities needed 
 * \param[out] sd_param_matrix    matrix of spatial distribution quantities needed to apply source distribution 
 * \param[in] debug               flag to debug */
int read_source_data(char * infile,                /* The source input filename */
                     int * nsource,                /* number of sources found in input source file */
                     double ** ras,                /* array of source RAs */
                     double ** decs,               /* array of source DECs */
                     double ** colden,             /* array of source column densities */
                     int ** spectype,              /* array of source spectral type */
                     double ** specpar,            /* value of spectral param for all sources */
                     double ** fluxpar,            /* flux for all sources, can be 0 for spectral file */
                     double ** band_lo,            /* flux lower bandpass for all sources */
                     double ** band_hi,            /* flux upper bandpass for all sources */
                     char *** sfilename,           /* name of spectral user input file for all sources, "none" for spectral model */
                     int ** sformat,               /* spectral sformat file format flag 1 or 2 for all sources; can be 0 for spectral model */
                     int ** sunits,                /* spectral file flux unit tag 1-9 for all sources, can be 0 for spectral model */
                     double ** period,             /* period for all sources, 0 for constant */
                     double ** pulse_fraction,     /* pulse fraction for all sources, 0 for constant */
                     double ** tburst,             /* burst start time for all original sources, 0 for constant */
                     double ** trise,              /* burst risetime for all original sources, 0 for constant */
                     double ** tdecay,             /* burst decay time for all original sources, 0 for constant */
                     double ** burst_rat,          /* burst ratio for all original sources, 0 for constant */
		     int ** burst_tabidx,          /* array of index identifying lookup table for burst */
                     int * nburst,                 /* number of burst sources */
		     char *** ifilename,           /* name of image user input file for all sources; "none" for point source or spatial model */
                     int * sd_matrix_size,         /* maximum number of spatial distribution quantities needed */
                     double *** sd_param_matrix,   /* matrix of spatial distribution quantities needed to apply source distribution */
                     int debug);                   /* flag to debug */


/**
 * \brief Read the vignette file, populate structure
 * \param[in] vigfile   the vignette filename
 * \param[out] s_vig          the vig structure
 * \param[in] debug           flag to debug  */
int read_vignette(char * vigfile, Vig_struct * s_vig, int debug);

/**
 * \brief Remove a substring from a primary string
 * \param[in,out] s     the primary string
 * \param[in] toremove  the string to remove  */
void remove_substring(char *s,const char *toremove);

char * resolve_pathname(char * input_string);

/**
 * \brief Reverse the order of an array
 * \param[in,out]  the array to reverse
 * \param[in] num  number of elements in the array */
void reverse(double * arr, int num);


/**
 * \brief Report the energy range for which RMF sensitivity is nonzero
 * \param[in] rmffile   The RMF fits file
 * \param[out] sens_lo   The min energy sensitivity
 * \param[out] sens_hi   The max energy sensitivity */
void RMFsensitivity(char * rmffile, double *sens_lo, double *sens_hi);


void rollxy(double xin,       /* input x */
            double yin,       /* input y */
            double x_ref,     /* reference x value */
            double y_ref,     /* reference y value */
            double roll,      /* roll */
            double xpixsize,  /* degrees per pixel, x-direction, positive/negative if look-down/up */
            double ypixsize,  /* degrees per pixel, y-direction */
            double *xout,      /* output x */
            double *yout);     /* output y */


/**
 * \brief Return coords of the given direction, transformed to a polar system
 * \param[in] CENLON     longitude in degrees for new north pole 
 * \param[in] CENLAT     latitude in degrees for new north pole 
 * \param[in,out] INLO   longitude of position to be rotated 
 * \param[in,out] INLA   latitude of position to be rotated 
 * \param[in,out] OUTLO  longitude of position in rotated coords 
 * \param[in,out] OUTLA  latitude of position in rotated coords 
 * \param[in] MODE       = 1 rotate to prime (new) system, = -1 rotate from prime (new) system */
void rotpol(double CENLON,  /* longitude in degrees for new north pole */
            double CENLAT,  /* latitude in degrees for new north pole */
            double INLO,    /* longitude of position to be rotated */
            double INLA,    /* latitude of position to be rotated */
            double *OUTLO,  /* longitude of position in rotated coords */
            double *OUTLA,  /* latitude of position in rotated coords */
            int MODE);      /*  = 1 rotate to prime (new) system, = -1 rotate from prime (new) system */


/**
 * \brief Create output fits file, populate header, prepare for data
 * \param[in] outfile          Output event filename 
 * \param[in] clobber          Overwrite existing output file? 
 * \param[in] version          Last known date of simulator update 
 * \param[in] s_obs            Structure containing observation parameters 
 * \param[in] s_mdb            Structure containing derived MDB parameters 
 * \param[in] rmf_struct       Structure containing ebounds information 
 * \param[in] ebounds_TLMIN1   EBOUNDS minimum allowed PI channel value 
 * \param[in] ebounds_TLMAX1   EBOUNDS maximum allowed PI channel value */
int setup_output(char * outfile,       /* Output event filename */
                 int clobber,          /* Overwrite existing output file? */
                 char * version,       /* Last known date of simulator update */
                 ObsParams * s_obs,   /* Structure containing observation parameters */
                 MDB_params * s_mdb,   /* Structure containing derived MDB parameters */
                 RMF * rmf_struct,     /* Structure containing ebounds information */
                 long ebounds_TLMIN1,  /* EBOUNDS minimum allowed PI channel value */
                 long ebounds_TLMAX1, /* EBOUNDS maximum allowed PI channel value */
		 int debug);           /* debug flag */


/**
 * \brief Sort three double arrays according to the first array (DDD = double double double)
 * \param[in,out] arr1  The first array
 * \param[in,out] arr2  The second array
 * \param[in,out] arr3  The third array
 * \param[in] num       Number of elements in arrays */
void sort_arrays_DDD(double *arr1, double *arr2, double *arr3, int num);

/**
 * \brief Sort a double and two int arrays according to the first array (DII = double int int)
 * \param[in,out] arr1  The first array
 * \param[in,out] arr2  The second array
 * \param[in,out] arr3  The third array
 * \param[in] num       Number of elements in arrays */
void sort_arrays_DII(double *arr1, int *arr2, int *arr3, int num);

/**
 * \brief Sort a double, two ints and a long array according to the first array (DIIL = double int int long)
 * \param[in,out] arr1  The first array
 * \param[in,out] arr2  The second array
 * \param[in,out] arr3  The third array
 * \param[in,out] arr4  The fourth array
 * \param[in] num       Number of elements in arrays */
void sort_arrays_DIIL(double *arr1, int *arr2, int *arr3, long *arr4, int num);

/**
 * \brief Sort a double, three ints and a long array according to the first array (DIIIL = double int int int long)
 * \param[in,out] arr1  The first array
 * \param[in,out] arr2  The second array
 * \param[in,out] arr3  The third array
 * \param[in,out] arr4  The fourth array
 * \param[in,out] arr5  The fifth array
 * \param[in] num       Number of elements in arrays */
void sort_arrays_DIIIL(double *arr1, int *arr2, int *arr3, int *arr4, long *arr5, int num);


/**
 * \brief Sort three double arrays according to the first array, then by the second (IDI = int double int)
 * \param[in,out] arr1  The first array 
 * \param[in,out] arr2  The second array
 * \param[in,out] arr3  The third array
 * \param[in] num       Number of elements in arrays */
void sort_two_conditions_IDI(int *arr1, double *arr2, int *arr3, int num);

/**
 * \brief Determine the initial position on the sky of npos photons
 * \param[in] sdmat      matrix of spatial distribution quantities to apply source dist
 * \param[in] npos       number of positions to generate
 * \param[out] dx_arcmin npos-sized 1D array of x offset, in arcmin, according to spatial dist
 * \param[out] dy_arcmin npos-sized 1D array of y offset, in arcmin, according to spatial dist
* \param[in/out MTstate  "save" state of Mersenne Twister RNG */
int sourcedis(double * sdmat,              /* matrix of spatial distribution quantities to apply source dist */
              int npos,                 /* number of positions to generate */
              double * dx_arcmin,       /* npos-sized 1D array of x offset, in arcmin, according to spatial dist */
              double * dy_arcmin,      /* npos-sized 1D array of y offset, in arcmin, according to spatial dist */
	      HDmt_state *MTstate);      /* "save" state of Mersenne Twister RNG */

/**
 * \brief Interpolate input spectrum on energy grid to produce output spectrum
 * \param[in] xspec_energy_in    vector of energy grid boundaries for inspec
 * \param[in] nebin_in           number of energy bins in inspec
 * \param[in] inspec             spectrum to be interpolated
 * \param[in] xspec_energy_out   vector of energy grid boundaries in spectrum to be evaluated
 * \param[in] nebin_out          number of energy bins in spectrum to be evaluated
 * \param[out] outspec           spectrum to be evaluated
 * \param[in] debug              flag to print debug info */
void spec_interp(double * xspec_energy_in,    /* vector of energy grid boundaries for inspec */
		 int nebin_in,                /* number of energy bins in inspec */
		 double * inspec,             /* spectrum to be interpolated */
		 double * xspec_energy_out,   /* vector of energy grid boundaries in spectrum to be evaluated */
		 int nebin_out,               /* number of energy bins in spectrum to be evaluated */
		 double * outspec,            /* spectrum to be evaluated */
		 int debug);

/**
 * \brief Estimate flux of spectrum over bandpass
 * \param[in] en            spectrum energy grid midpoints in keV
 * \param[in] spec          spectrum over en in photons/cm^2/s
 * \param[in] nebins        number of energy bins
 * \param[in] lower_limit   lower limit in keV of bandpass for flux calculations
 * \param[in] upper_limit   upper limit in keV of bandpass for flux calculations
 * \param[out] intspec      flux in keV/cm^2/s of spec over bandpass  */
int spec_scale(double *en,            /* spectrum energy grid midpoints in keV */
	       double *spec,          /* spectrum over en in photons/cm^2/s */
	       int nebins,            /* number of energy bins */
	       double lower_limit,    /* lower limit in keV of bandpass for flux calculations */
	       double upper_limit,    /* upper limit in keV of bandpass for flux calculations */
	       double *intspec);      /* flux in keV/cm^2/s of spec over bandpass */

/**
 * \brief Analytically calculates the flux spectrum over bandpass
 * \param[in] xspec_energy      xspec spectrum energy grid in keV
 * \param[in] index             photon index
 * \param[in] nebins            number of xspec energy bins
 * \param[in] lower_limit       lower limit, in keV, of bandpass for flux calculation
 * \param[in] upper_limit       upper limit, in keV, of bandpass for flux calculation
 * \param[out] intspec          flux in keV/cm2/s of spec over bandpass  */
int spec_scale_plaw(double * xspec_energy,   /* xspec spectrum energy grid in keV */
                    double index,            /* photon index */
                    int nebins,              /* number of xspec energy bins */
                    double lower_limit,      /* lower limit, in keV, of bandpass for flux calculation */
                    double upper_limit,      /* upper limit, in keV, of bandpass for flux calculation */
                    double * intspec);       /* flux in keV/cm2/s of spec over bandpass */



/**
 * \brief Use Poisson stats to get integer values for spectrum when number of counts in bin is low.
 * \param[in,out] specin   the flux array of the spectrum
 * \param[in] Nspec        number of channels in the spectrum
 * \param[in/out MTstate  "save" state of Mersenne Twister RNG */
void specmod(double *specin, int Nspec, HDmt_state *MTstate);



/**
 * \brief Calculate the input spectrum from one of the supported models or a user file.
 * \param[in] nh               absorption column density in cm^-2 
 * \param[in] zred             redshift
 * \param[in] nhint            instrinsic absorption
 * \param[in] spectype         1:power, 2:rs, 3:bb, 4:brem, 5:mono, 6:user 
 * \param[in] specpar          index for spectype=1; kT in keV for spectype=2,3,4; line energy in keV for spectype=5) 
 * \param[in] fluxpar          over the input bandpass in erg/cm^2/sec; may be 0 for spectype=6 
 * \param[in] norm_lower       lower limit of bandpass in keV for flux 
 * \param[in] norm_upper       upper limit of bandpass in keV for flux 
 * \param[in] infile           name of input spectral file, should be "none" for model 
 * \param[in] sformat          1 for twocols: emid, flux; 2 for threecols: emid, de_half, flux 
 * \param[in] sunits           flux unit, an integer 1-9 
 * \param[in] nebin            number of input energy bins - from the arf 
 * \param[in] ebin_mid         midpoint of arf energy grid bins 
 * \param[in] xspec_energy     vector of energy grid boundaries for Xspec model input 
 * \param[out] spec            spectrum in photons/cm^2/sec on arf energy grid 
 * \param[out] flux_abs        absorbed flux over bandpass specified in source file (scalar) 
 * \param[out] flux_unabs      unabsorbed flux over bandpass specified in source file (scalar) 
 * \param[out] dflux_abs       absorbed flux over entire detector bandpass (scalar) 
 * \param[out] dflux_unabs     unabsorbed flux over entire detector bandpass (scalar) 
 * \param[in] debug            debug flag */
int spectra(double nh,              /* absorption column density in cm^-2 */
            double zred,            /* redshift */
            double nhint,           /* intrinsic absorption */
            int spectype,           /* 1:power, 2:rs, 3:bb, 4:brem, 5:mono, 6:user */
            double specpar,         /* index for spectype=1; kT in keV for spectype=2,3,4; line energy in keV for spectype=5) */
            double fluxpar,         /* over the input bandpass in erg/cm^2/sec; may be 0 for spectype=6 */
            double norm_lower,      /* lower limit of bandpass in keV for flux */
            double norm_upper,      /* upper limit of bandpass in keV for flux */
            char * infile,          /* name of input spectral file, should be "none" for model */
            int sformat,            /* 1 for twocols: emid, flux; 2 for threecols: emid, de_half, flux */
            int sunits,             /* flux unit, an integer 1-9 */
            int nebin,              /* number of input energy bins - from the arf */
            double * ebin_mid,      /* midpoint of arf energy grid bins */
            double * xspec_energy,  /* vector of energy grid boundaries for Xspec model input */
            double * spec,          /* spectrum in photons/cm^2/sec on arf energy grid */
            double *flux_abs,       /* absorbed flux over bandpass specified in source file (scalar) */
            double *flux_unabs,     /* unabsorbed flux over bandpass specified in source file (scalar) */
            double *dflux_abs,      /* absorbed flux over entire detector bandpass (scalar) */
            double *dflux_unabs,    /* unabsorbed flux over entire detector bandpass (scalar) */
            int debug);             /* debug flag */


/**
 * \brief Convert a string to uppercase letters
 * \param[in,out] input_string   the string to convert */
int string2upper(char * input_string  /* a string input */);



/**
 * \brief Assign times to events
 * \param[in] expose      simulation exposure time 
 * \param[in] period      period of optional variation in seconds 
 * \param[in] pulsed      amplitude of variation 
 * \param[in] nn          number of events 
 * \param[out] evt_time   array of times assigned to events
 * param[in,out] phase_in   random initial phase      
 * \param[in,out] MTstate  "save" state of Mersenne Twister RNG */
int timeset(double tstart,    /* start time for this exposure or subexposure, aka exp_completed */
            double expose,    /* duration of this exposure or subexposure, aka exposure_block */
            double period,      /* period of optional variation in seconds */
            double pulsed,      /* amplitude of variation */
            long nn,             /* number of events */
            double *evt_time,   /* array of times assigned to events */
            double phase_in,   /* initial phase within this exposure block */
            HDmt_state *MTstate);  /* "save" state of Mersenne Twister RNG */




/**
 * \brief Assign times to burst events
 * \param[in] nn          number of events
 * \param[in] expose      simulation exposure time
 * \param[in] ratburst    ratio of burst count rate at peak to quiescent rate
 * \param[in] tburst      burst start time in seconds after beginning of exposure
 * \param[in] trise       burst (linear) rise-time in seconds
 * \param[in] tdecay      burst (exponential) decay-time in seconds
 * \param[out] evt_time    1D array, of size nn, of event times in seconds after the start of the observation
 * \param[in] MTstate     "save" state of Mersenne Twister RNG */
int timeset_burst(int nn,                /* number of events */
                  double expose,         /* simulation exposure time */
                  double ratburst,       /* ratio of burst count rate at peak to quiescent rate */
                  double tburst,         /* burst start time in seconds after beginning of exposure */
                  double tstart,         /* tstart time for this exposure or subexposure, aka exp_completed */
                  double tblock,         /* duration of this exposure or subexposure, aka exposure_block */
                  int ntable,            /* number time of intervals in table */
                  double * table_time,   /* 1D array of size ntable+1 with lookup-table times in units of the total obs exp time */
                  double * table_x,      /* 1D array of size ntable+1 with lookup-table lightcurve values */
                  double * evt_time,      /* 1D array, of size nn, of event times in seconds after the start of the observation */
                  HDmt_state *MTstate);   /* "save" state of Mersenne Twister RNG */




/**
 * \brief Prepare the torus structures
 * \param[in,out] s_tpar   Torus parameter structure
 * \param[in,out] s_tspec  Torus spectra structure */ 
int torus_prep(torus_par_struct * s_tpar, torus_spec_struct * s_tspec);


/**
 * \brief Calculate the input spectrum from one of the supported models or a user file.
 * \param[in] nh               external absorption column density in cm^-2
 * \param[in] zpar             redshift
 * \param[in] spectype         6:torus, 7: gilli_thick, 8:gilli_mild
 * \param[in] specpar          spectral parameter
 * \param[in] fluxpar          over the input bandpass in erg/cm2/sec
 * \param[in] norm_lower       lower limit of bandpass in keV for flux
 * \param[in] norm_upper       upper limit of bandpass in keV for flux
 * \param[in] nebin            number of input energy bins
 * \param[in] energ_mid        midpoint of energy grid bins
 * \param[in] xspec_energy     vector of energy grid boundaries for Xspec model input
 * \param[in] s_tpar           torus structure containing parameters
 * \param[in] s_tspec          torus structure containing spectra
 * \param[out] spec            spectrum in photons/cm^2/sec on arf energy grid
 * \param[out] flux_abs        absorbed flux over bandpass specified in source file (scalar)
 * \param[out] flux_unabs      unabsorbed flux over bandpass specified in source file (scalar)
 * \param[out] dflux_abs       absorbed flux over entire detector bandpass (scalar)
 * \param[out] dflux_unabs     unabsorbed flux over entire detector bandpass (scalar) */
int torus_spectra(double nh,              /* external absorption column density in cm^-2 */
		  double zpar,            /* redshift */
                  int spectype,           /* 6:torus, 7: gilli_thick, 8:gilli_mild */
                  double specpar,         /* see comment below */
                  double fluxpar,         /* over the input bandpass in erg/cm2/sec */
                  double norm_lower,      /* lower limit of bandpass in keV for flux */
                  double norm_upper,      /* upper limit of bandpass in keV for flux */
                  long nebin,             /* number input energy bins */
                  double * energ_mid,     /* midpoint of energy grid bins */
                  double * xspec_energy,  /* vector of energy grid boundaries for Xspec model input */
                  torus_par_struct * s_tpar, /* torus structure containing parameters */
                  torus_spec_struct *s_tspec, /* torus structure containing spectra */
                  double * spec,         /* spectrum in photons/cm2/sec/channel on input energy grid */
                  double *flux_abs,       /* absorbed flux in erg/cm2/sec over input bandpass */
                  double *flux_unabs,     /* unabsorbed flux in erg/cm2/sec over input bandpass */
                  double *dflux_abs,      /* absorbed flux in erg/cm2/sec over entire detector bandpass */
                  double *dflux_unabs,   /* unabsorbed flux in erg/cm2/sec over entire detector bandpass */
		  int debug);            /* debug flag */


/**
 * \brief Remove white space before and after string
 * \param[in/out] str */
char *trim_string(char *str);


/**
 * \brief Update output fits file checksums
 * \param[in] ounit  pointer to open fits file */
void update_checksums(fitsfile * ounit);

/**
 * \brief Notify user if source falls on detector gap
 * \param[in]  s_obs   structure containing observatory params
 * \param[in]  s_mdb   structure containing MDB params
 * \param[in]  nsource number of sources
 * \param]in]  ras     RA array for sources
 * \param[in]  decs    Dec array for sources */
void warn_if_detector_gap(ObsParams *s_obs,  /* structure containing observatory params */
                          MDB_params *s_mdb, /* structure containing MDB params */
                          int nsource,       /* number of sources */
                          double *ras,        /* RA array for sources */
                          double *decs,      /* Dec array for sources */
			  int debug);        /* debug flag */


/**
 * \brief Write GTI HDU extension at end of fits file
 * \param[in] ounit  Pointer to open fits file
 * \param[in] s_obs  Structure containing observation data */
int write_GTI(fitsfile * ounit, ObsParams * s_obs);

/**
 * \brief Convert pixel coords from x,y to ra,dec.  Opposite of rd2xy.
 * \param[in] x          input x 
 * \param[in] y          input y 
 * \param[in] x_ref      reference x-value 
 * \param[in] y_ref      reference y-value 
 * \param[in] ra_ref     ra at (x_ref,y_ref) 
 * \param[in] dec_ref    dec at (x_ref,y_ref) 
 * \param[in] roll       "roll" angle of y-axis from north 
 * \param[in] xpixsize   degrees per pixel, x-direction, pos/neg if look up/down 
 * \param[in] ypixsize   degrees per pixel, y-direction 
 * \param[out] ra        output RA 
 * \param[out] dec       output Dec */
void xy2rd(double x,         /* input x */
           double y,         /* input y */
           double x_ref,     /* reference x-value */
           double y_ref,     /* reference y-value */
           double ra_ref,    /* ra at (x_ref,y_ref) */
           double dec_ref,   /* dec at (x_ref,y_ref) */
           double theta,     /* "roll" angle of y-axis from north */
           double xpixsize,  /* degrees per pixel, x-direction, pos/neg if look up/down */
           double ypixsize,  /* degrees per pixel, y-direction */
           double *ra,        /* output RA */
           double *dec);      /* output Dec */






/***************************************************************************/
/* ********************** EXTERNAL XSPEC CALLS *****************************/

/**
 * \brief From XSPEC library: Xspec/src/XSUtil/FunctionUtils/xsFortran.cxx */
void FNINIT(void);

/**
 * \brief From XSPEC library */
char * FGMODF(void);

#define Xsrays(xspec_energy, num_energy_bins, xsrays_param,ifl, spec, specerr) \
    CCALLSFSUB6(XSRAYS,xsrays,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsrays_param,ifl, spec, specerr)

#define Xsphab(xspec_energy, num_energy_bins, xsphab_param,ifl, xsphab_trans, xsphab_trans_err) \
    CCALLSFSUB6(XSPHAB,xsphab,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsphab_param,ifl, xsphab_trans, xsphab_trans_err)

#define Xszphb(xspec_energy, num_energy_bins, xsphab_param,ifl, xsphab_trans, xsphab_trans_err) \
    CCALLSFSUB6(XSZPHB,xsphab,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsphab_param,ifl, xsphab_trans, xsphab_trans_err)

#define Xsblbd(xspec_energy, num_energy_bins, xsblbd_param,ifl, spec, specerr) \
    CCALLSFSUB6(XSBLBD,xsblbd,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsblbd_param,ifl, spec, specerr)

#define Xsbrms(xspec_energy, num_energy_bins, xsbrms_param,ifl, spec, specerr) \
    CCALLSFSUB6(XSBRMS,xsbrms,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsbrms_param,ifl, spec, specerr)

#define Xsgaul(xspec_energy, num_energy_bins, xsgaul_param,ifl, spec, specerr) \
    CCALLSFSUB6(XSGAUL,xsgaul,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsgaul_param,ifl, spec, specerr)

#define Xsaped(xspec_energy, num_energy_bins, xsaped_param,ifl, spec, specerr) \
    CCALLSFSUB6(XSAPED,xsgaul,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsaped_param,ifl, spec, specerr)

#define Xsmeka(xspec_energy, num_energy_bins, xsmeka_param,ifl, spec, specerr) \
    CCALLSFSUB6(XSMEKA,xsmeka,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsmeka_param,ifl, spec, specerr)

#define Xsmekl(xspec_energy, num_energy_bins, xsmekl_param,ifl, spec, specerr) \
    CCALLSFSUB6(XSMEKL,xsmekl,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV, \
		xspec_energy, num_energy_bins, xsmekl_param,ifl, spec, specerr)

PROTOCCALLSFSUB6(XSRAYS,xsrays,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
PROTOCCALLSFSUB6(XSPHAB,xsphab,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
PROTOCCALLSFSUB6(XSZPHB,xszphb,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
PROTOCCALLSFSUB6(XSBLBD,xsblbd,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
PROTOCCALLSFSUB6(XSGAUL,xsgaul,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
PROTOCCALLSFSUB6(XSBRMS,xsbrms,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
PROTOCCALLSFSUB6(XSAPED,xsaped,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
PROTOCCALLSFSUB6(XSMEKA,xsmeka,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)
PROTOCCALLSFSUB6(XSMEKL,xsmekl,FLOATV,INT,FLOATV,INT,FLOATV,FLOATV)

/****************************************************************************/
/****************************************************************************/

#endif



/* ! $Log: heasim.h,v $
/* ! Revision 1.96  2016/03/31 21:41:40  driethmi
/* ! Impemented suite of changes to handle burst time assignment.  (There will
/* ! likely be more bug fixes to this implementation soon.)
/* !
/* ! Revision 1.95  2016/03/25 14:47:48  driethmi
/* ! Added declaration for spec_scale_plaw, and defined macros for MAX and MIN.
/* !
/* ! Revision 1.94  2015/12/29 18:15:51  driethmi
/* ! Added ability to skip field of view restriction and/or RMF implementation.
/* ! User sets parameters skipfov and/or skiprmf, respectively, to yes/no.
/* !
/* ! Revision 1.93  2015/11/12 18:07:42  driethmi
/* ! If TLMIN1 is not found in background PHA file, then get it by reading the
/* ! first CHANNEL column entry and assign it to ib_firstchan.
/* !
/* ! Revision 1.92  2015/11/09 14:44:07  driethmi
/* ! Now check for first channel number in background file - usually 0, but
/* ! use ib_firstchan to be safe.  If ib_firstchan != ebounds_TLMIN1, then
/* ! exit with error.
/* !
/* ! Revision 1.91  2015/11/03 18:22:25  driethmi
/* ! Added capability for resampling pixel size.
/* !
/* ! Revision 1.90  2015/10/16 15:40:43  driethmi
/* ! Changes to make sure pixid column is being sorted along with x, y, and pi
/* ! according to time.  Problem didn't show up until pileup was engaged.
/* !
/* ! Revision 1.89  2015/09/30 15:45:24  driethmi
/* ! Modified read_internal_background to process XMM BACKSCAL values in order to
/* ! match units that heasim expects.
/* !
/* ! Revision 1.88  2015/07/01 18:46:34  driethmi
/* ! Cleaned up checksum writing, so this happens at the end of the code.  Heasim
/* ! output now passes ftverify check.
/* !
/* ! Revision 1.87  2015/07/01 15:00:33  driethmi
/* ! When we do time sorting, we need to re-order X, Y, and PI to match.  Several
/* ! mods to accommodate this - new structure "quad" that holds four doubles,
/* ! some new sorting routines.
/* !
/* ! Revision 1.86  2015/06/30 14:02:39  driethmi
/* ! Inserted flag flagsubex to trigger subexposure divisions - hidden, "no"
/* ! by default.
/* !
/* ! Revision 1.85  2015/06/25 18:15:42  driethmi
/* ! Put more print statements under "debug" mode only.  Also changed order of
/* ! "final event count" statements at the end of doWork().  Report that we've removed
/* ! bad PI events before we report final event count; no change in functionaly,
/* ! but let's not confuse the user.
/* !
/* ! Revision 1.84  2015/06/22 21:01:38  driethmi
/* ! Added function to write GTI HDU at end of output fits file.
/* !
/* ! Revision 1.83  2015/06/16 20:34:40  driethmi
/* ! Built in optimization for sub-exposure blocks.  By default, subexposure
/* ! parameter is now hidden and very large.  If subexposure > exposure, then
/* ! recompute the optimal subexposure size.
/* !
/* ! Revision 1.82  2015/06/11 18:35:17  driethmi
/* ! Reworked flag_pileup() routine to accommodate new time-block layout.
/* !
/* ! Revision 1.81  2015/06/05 21:21:19  driethmi
/* ! Added "subexposure" as a parameter to the par file, and modified code to use
/* ! it.  Now user specifies the total exposure time ("exposure"), and the sub-
/* ! exposure time ("subexposure"), and the code divides the simulation into
/* ! the appropriate sub-blocks, adding the final time assignment from the previous
/* ! block to the time events in the next block.
/* !
/* ! Revision 1.80  2015/06/05 20:11:09  driethmi
/* ! Added function declaration for compare_doubles().
/* !
/* ! Revision 1.79  2015/05/29 17:50:48  driethmi
/* ! Added function clean_ip_chans(), which removes events for which pi_chans has
/* ! a value of -1, indicating that this event fell of the end of the channel array.
/* ! It then resizes the xe, ye, ide, and pi_chans arrays to the correct size.  We now
/* ! do this inline with the code, instead of at the end of heasim.
/* !
/* ! Revision 1.78  2015/05/22 17:24:36  driethmi
/* ! Updated parameter names throughout code, changed header keywords/comments
/* ! to match standard values.  Changed input user seed so that seed=0 triggers
/* ! seeding from the system time.
/* !
/* ! Revision 1.77  2015/05/20 20:12:20  driethmi
/* ! Modified mdb parameter names, and instances of these names in the code.  Also
/* ! have updated values for mdb parameters.
/* !
/* ! Revision 1.76  2015/04/14 20:52:10  driethmi
/* ! While looping over sources in doWork(), if there is ever a problem with the
/* ! call to spectra() or torus_spectra(), we set a flag spectra_errstatus to
/* ! -1, and pass this upwards.  Since spectra failures are non-cricital (meaning,
/* ! we simply continue to the next source), this provides a way of reporting
/* ! at the end that problems were encountered.
/* !
/* ! Revision 1.75  2015/04/08 15:37:45  driethmi
/* ! Replaced system call to copy files (during re-sorting at the end of doWork)
/* ! with compiled function that achieves same result.  Other minor cosmetic
/* ! changes.
/* !
/* ! Revision 1.74  2015/04/07 15:47:49  driethmi
/* ! Corrected non-critical compilations warnings; unused variables, etc.
/* !
/* ! Revision 1.73  2015/03/23 20:36:22  driethmi
/* ! timeset() and timeset_burst() now return an error status, which if not zero,
/* ! causes doWork() to exit.
/* !
/* ! Revision 1.72  2015/03/19 20:35:30  driethmi
/* ! Added burst capability, and updated function comment blocks.
/* !
/* ! Revision 1.71  2015/02/18 21:34:07  driethmi
/* ! Corrected bug in process_image, needed to use fits_read_key_dbl instead of
/* ! fits_read_key_flt - lack of precision was causing zeroes to be read.  Also
/* ! improved verbosity of debug statements.
/* !
/* ! Revision 1.70  2015/02/18 20:15:19  driethmi
/* ! Enabled application of redshift data to source simulation.
/* !
/* ! Revision 1.69  2015/02/18 18:15:01  driethmi
/* ! Overwrote pulse bug fix - corrected and re-committed.
/* !
/* ! Revision 1.68  2015/02/18 15:53:41  driethmi
/* ! Preliminary redshift changes, and cleaned up output chatter so debug = 1
/* ! is more useful.
/* !
/* ! Revision 1.66  2015/01/29 19:53:31  driethmi
/* ! Removed difspec_file_background parameter.
/* !
/* ! Revision 1.65  2015/01/28 21:32:20  driethmi
/* ! Enabled torus spectral model for background point sources.
/* !
/* ! Revision 1.64  2015/01/21 17:25:03  driethmi
/* ! Modified heasim to accept data files from sky background tool.
/* !
/* ! Revision 1.63  2014/12/02 19:57:40  driethmi
/* ! Changed floats to doubles for consistency, except where float is required.
/* !
/* ! Revision 1.62  2014/10/02 18:07:31  driethmi
/* ! Added routine to query most recent CVS commit date in all .c files that
/* ! are built with simulator, and return the date string as the version number.
/* !
/* ! Revision 1.61  2014/09/11 21:20:12  driethmi
/* ! Fixed typo in doxygen comments.
/* !
/* ! Revision 1.60  2014/08/19 18:30:41  driethmi
/* ! Added initial capability to read PSF images correctly.
/* !
/* ! Revision 1.59  2014/07/30 19:03:30  driethmi
/* ! Promoted s_mdb pixelsize_* and cdlt* from float to double precision, to
/* ! accomodate values in the mdb file.
/* !
/* ! Revision 1.58  2014/07/28 19:47:22  driethmi
/* ! Commented out deadtime prints for now, and only copy fits file for sorted
/* ! time and pixid if debug is set > 0.
/* !
/* ! Revision 1.57  2014/07/28 15:54:52  driethmi
/* ! Added routine to flag pileup events in output fits file.
/* !
/* ! Revision 1.56  2014/07/25 20:46:03  driethmi
/* ! Added flag_pileup routine to definitions.
/* !
/* ! Revision 1.55  2014/07/25 14:40:28  driethmi
/* ! Added dtpileup to s_obs structure.
/* !
/* ! Revision 1.54  2014/07/10 22:32:43  driethmi
/* ! Previous changes did not completely solve the issue.
/* !
/* ! Revision 1.53  2014/07/07 15:40:10  driethmi
/* ! Adjusted call to imagedis - does not need to pass roll parameter.
/* !
/* ! Revision 1.52  2014/07/01 13:47:16  driethmi
/* ! Current state - added roll angle and image HDU capability.  Still some
/* ! issues to work out with response file and background read.  Also, imagedis
/* ! seems to have execution bottleneck.
/* !
/* ! Revision 1.51  2014/06/12 20:37:20  driethmi
/* ! In xy2rd routine, need ra and dec input to be *ra and *dec, so that
/* ! we can pass them back.
/* !
/* ! Revision 1.50  2014/06/12 18:31:30  driethmi
/* ! Rewrote xy2rd routine, to be inverse of rd2xy, in utils.c.
/* !
/* ! Revision 1.49  2014/06/02 20:48:18  driethmi
/* ! Changed int_spectot from int type to long type.
/* !
/* ! Also fixed array indexing typo with ien.
/* !
/* ! Revision 1.48  2014/06/02 18:54:41  driethmi
/* ! Added switch to use legacy C heasp library instead of active C++ library
/* ! from heacore/heasp.
/* !
/* ! Revision 1.47  2014/05/30 14:51:28  driethmi
/* ! Changed some int to long for safety, also changed some array allocations
/* ! from size spectot to size nn_source.
/* !
/* ! Revision 1.46  2014/05/29 00:12:43  driethmi
/* ! Changed several of the event counters from int to long, i.e. nn_out,
/* ! nn_source, nev, nn_ibspec.  We're probably not at the int/long limit, but
/* ! it's probably prudent to make this change.
/* !
/* ! Revision 1.45  2014/05/23 22:51:24  driethmi
/* ! Added routine that warns the user if sources are specified with RA,DEC
/* ! that lie within the CCD detector gaps of the instrument being used.
/* !
/* ! Revision 1.44  2014/05/23 21:51:38  driethmi
/* ! Changed random number generator such that MTstate is passed down through
/* ! each function using it.  Now, user must specify seed.  If seed > 0, it is
/* ! used to seed the RNG.  If seed <=0, we seed using the system time.
/* !
/* ! Revision 1.43  2014/05/08 20:00:34  driethmi
/* ! Routine that reads in user spectral file was overly stringent about the
/* ! formatting of white space.  Made this routine more robust by removing
/* ! white space before and after the line, and then parsing.
/* !
/* ! Revision 1.42  2014/05/06 15:27:38  driethmi
/* ! Realized that sdmat may also need to be double rather than int.
/* !
/* ! Revision 1.41  2014/05/05 19:56:02  driethmi
/* ! Realized that crvl1_img and crvl2_img need to be doubles and not ints;
/* ! this was throwing off the imagedis() photon placement algorithm.
/* !
/* ! Revision 1.40  2014/04/29 21:11:38  driethmi
/* ! Added function to remove lines from output fits file with PI channel less
/* ! than or equal to zero.  Called during the "finalize" phase.
/* !
/* ! Revision 1.39  2014/04/09 14:44:05  driethmi
/* ! Added routine to report actual nonzero energy sensitivity of RMF.  If,
/* ! for example, spectral lines are given outside of this range, we can expect
/* ! a null simulation result.
/* !
/* ! Revision 1.38  2014/04/03 20:41:06  driethmi
/* ! Updated commenting and minor changes to attempt to comply more with coding
/* ! standards.  Added doxygen tags.
/* !
/* ! Revision 1.37  2014/04/02 20:45:56  driethmi
/* ! Edited/added comments for functions and structs.
/* !
/* ! Revision 1.36  2014/04/01 18:12:34  driethmi
/* ! Added functionality to read psf files.
/* !
/* ! Revision 1.35  2014/03/28 22:49:01  driethmi
/* ! Can now read all types of vignetting files: VIG_TEXT_1, VIG_FITS_1,
/* ! VIG_FITS_2, VIG_IMAGE.  However, VIG_IMAGE is untested, since we have
/* ! no vignette image files to test with.  This case may very well fail.
/* !
/* ! Revision 1.34  2014/03/26 22:15:55  driethmi
/* ! Can now read FITS format vignette file, as long as it contains only one
/* ! "VIGNETTE" data group, and is not an image file.
/* !
/* ! Revision 1.33  2014/03/26 18:11:38  driethmi
/* ! Added capability to read an ascii vignette file.  Stores info to newly
/* ! created vignette structure.  Cannot yet read fits vignette file, and does
/* ! not yet apply vignetting to simulated data.
/* !
/* ! Revision 1.32  2014/03/19 15:05:52  driethmi
/* ! The MIN and MAX macros were unnecessary, removed them.
/* !
/* ! Revision 1.31  2014/03/19 14:52:38  driethmi
/* ! Renamed HDseed() to MTseed().  For "Mersenne Twister."
/* !
/* ! Revision 1.30  2014/03/19 14:22:48  driethmi
/* ! Removed all instances of idum and random_seed, which are now obsolete.
/* !
/* ! Revision 1.29  2014/03/13 17:30:30  driethmi
/* ! Added isrealpixel() routine to doWork.c.  Changed meaning of pixel_status
/* ! in doWork() to be consistent with itself, i.e. pixel_status = 0 if we should
/* ! keep it, = 1 if we should discard.
/* !
/* ! Revision 1.28  2014/03/13 15:21:59  driethmi
/* ! Simplified call to doWork() by elminating several passed parameters, and passing
/* ! instead the structures containing those parameters.
/* !
/* ! Revision 1.27  2014/03/12 19:46:47  driethmi
/* ! Added defs for some new deallocation routines, changed several floats to doubles.
/* !
/* ! Revision 1.26  2014/03/11 17:45:29  driethmi
/* ! Missed a few functions when putting into alphabetical order.
/* !
/* ! Revision 1.25  2014/03/11 15:19:20  driethmi
/* ! Reorderd function definitions into alphabetical order.
/* !
/* ! Revision 1.24  2014/03/10 21:49:14  driethmi
/* ! Added some new deallocation routines.
/* !
/* ! Also modified the level of pointers passed for ifilename and sfilename.
/* ! Was not necessary to pass triple pointer.
/* !
/* ! Revision 1.23  2014/02/28 19:08:21  driethmi
/* ! 1) Fixed many small memory leaks in C code; however, Valgrind still reports some
/* ! "still reachable" blocks in C++ interface code.
/* !
/* ! 2) Added parentheses around (*ifilename) and (*sfilename) in doWork.c, that avoids
/* ! a bus error.
/* !
/* ! 3) doWork() and spectra() now take a debug flag as an argument.
/* !
/* ! 4) doWork() now sorts the avenergy array, and then de_half and inspec are reordered
/* ! to match, before populating binlow and binhigh.
/* !
/* ! Revision 1.22  2014/02/21 15:54:42  driethmi
/* ! Fixed bug to pass ebounds_TLMIN1 and TLMAX1 correctly.
/* !
/* ! Revision 1.21  2014/02/20 21:12:35  driethmi
/* ! Fixed issues with the TLMIN5/TLMAX5 header keyword.  Now duplicates from the
/* ! EBOUNDS TLMIN1/TLMAX1 keyword.
/* !
/* ! Revision 1.20  2014/02/20 18:30:48  driethmi
/* ! Moved the doWork section in heasim.c main to its own function in doWork.c.
/* ! Also moved C_ReturnChannel() to doWork.c and removed the ReturnChannel.c
/* ! file from the repository.
/* !
/* ! Revision 1.19  2014/02/07 15:59:28  driethmi
/* ! Several new routines, commenting.
/* !
/* ! Revision 1.18  2014/01/16 22:45:25  driethmi
/* ! New function definitions for utilities in doWork.
/* !
/* ! Revision 1.17  2014/01/13 22:01:07  driethmi
/* ! Made skycoords_per_arcsec_x and _y.
/* !
/* ! Revision 1.16  2014/01/08 21:33:59  driethmi
/* ! ebounds_struct added to setup_output call.
/* !
/* ! Revision 1.15  2014/01/07 19:34:52  driethmi
/* ! Moved intbackfile from GeneralBackground structure to
/* ! CalFiles structure.
/* !
/* ! Revision 1.14  2014/01/07 16:54:40  driethmi
/* ! Modified to remove EEF_energy_weights, add aim_detx, aim_dety to MDB
/* ! parameter list.
/* !
/* ! Revision 1.13  2014/01/03 19:02:36  driethmi
/* ! The C99 function fmin may not be available on all platforms.  Replaced with
/* ! macros MIN and MAX, defined in heasim.h.
/* !
/* ! Revision 1.12  2013/12/31 22:13:56  driethmi
/* ! Added file "setup_output.c" and calls thereto; sets up the FITS output
/* ! event file and populates some of the header info.  Closes the file when
/* ! finished, which means that we'll need to open again when we're ready
/* ! to write the final sim data to it.
/* !
/* ! Revision 1.11  2013/12/31 14:57:41  driethmi
/* ! Added comments for every variable defined.  Other minor changes.
/* !
/* ! Revision 1.10  2013/12/18 23:05:32  driethmi
/* ! Added some minor helpful comments - in which files new functions may be found,
/* ! marks delineating sections of heasim.c.
/* !
/* ! Revision 1.9  2013/12/18 20:57:01  driethmi
/* ! Added new routine to read source data from input file.  As of yet, nothing
/* ! is actually *done* with the input data, just read.  May still be some bugs,
/* ! haven't tested extensively.
/* !
/* ! Revision 1.8  2013/12/12 19:38:58  driethmi
/* ! Moved allocation statements for HEASP-related arrays out of main and
/* ! into relevant subroutines.  Remember to deallocate at the end of main.
/* ! Not sure if this is correct programming practice, but it makes for much
/* ! cleaner main function.
/* !
/* ! Revision 1.7  2013/12/11 21:28:44  driethmi
/* ! Added HEASP functionality to read fits files.  Now read ARF, RMF, ebounds,
/* ! and internal_background fits files to structures using heasp library.
/* ! Still some uncertainty as to which heasp library to use (legacy C
/* ! or CVS-controlled C++ ?)
/* !
/* ! Revision 1.6  2013/11/27 18:51:17  driethmi
/* ! Packed mission, instrument, filter, instmode, ra, dec, roll, and expose
/* ! into a structure.  Should make passing into functions easier.
/* !
/* ! Revision 1.5  2013/11/27 18:48:32  driethmi
/* ! *** empty log message ***
/* !
/* ! Revision 1.4  2013/11/27 15:33:27  driethmi
/* ! Minor changes.  Fixed commenting in revision history.
/* !
   ! Revision 1.3  2013/11/26 21:08:04  driethmi
   ! Tailored mission database (mdbfile) to contain the parameters required
   !  in the simulator TRF.  All of these parameters are read in; some of
   ! them are not yet used for anything.
   !
 */
