/**
 * \file skyback.h
 * \brief Sky Background - header file
 * \author David Riethmiller
 * \date $Date: 2015/05/19 17:12:16 $
 *
 * Contains all typedefs, structs, and function definitions for routines internal to skyback.
 *
 */

#include "cfortran.h"
#include "funcWrappers.h"
#include "xsFortran.h"
#include "fitsio.h"
#include <strings.h>
#include <stdlib.h>
#include <limits.h>
#include <headas_rand.h>  /* headas random number generator */

/**************************************************************/
/******* Constant and conversion definitions ******************/

#define STR_MAX_LEN 300             /* for typical-length line strings */
#define STR_LONG_MAX_LEN 5000       /* for line strings which could be very long */
#define MAXRET (1)
#define ARCSEC_PER_DEGREE (3600.0)
#define ARCMIN_PER_DEGREE (60.0)
#define ARCSEC_PER_ARCMIN (60.0)
#define EULER_PI (3.141592654)
#define PI 3.14159265359
#define TWOPI (6.28318531)
#define D2R 0.01745329300562540  /* degrees to radians */
#define R2D 57.2957779186820488  /* radians to degrees */

/***************************************************************/
/******* Structure Definitions *********************************/

/**
 * \brief Structure to hold filenames
 * \param[in,out] psource_fileroot heasim point source output filename root
 * \param[in,out] difsource_fileroot heasim diffuse source output filename root
 * \param[in,out] difspec_fileroot heasim diffuse spectrum output filename root
 * \param[in,out] difspec_fitsfile heasim diffuse spectrum output fitsfile
 * \param[in,out] difspec_datfile heasim diffuse spectrum output ascii file
 * \param[in,out] psource_txtfile heasim point source output ascii file
 * \param[in,out] difsource_txtfile heasim diffuse source output ascii file */
typedef struct{
    char psource_fileroot[STR_MAX_LEN];   /* heasim point source output filename root */
    char difsource_fileroot[STR_MAX_LEN]; /* heasim diffuse source output filename root */
    char difspec_fileroot[STR_MAX_LEN];   /* heasim diffuse spectrum output filename root */
    char zsource_fileroot[STR_MAX_LEN];   /* heasim auxiliary point source output filename root */

    char difspec_fitsfile[STR_MAX_LEN];   /* heasim diffuse spectrum output fitsfile */
    char difspec_datfile[STR_MAX_LEN];    /* heasim diffuse spectrum output ascii file */
    char psource_txtfile[STR_MAX_LEN];    /* heasim point source output ascii file */
    char difsource_txtfile[STR_MAX_LEN];  /* heasim diffuse source output ascii file */
    char zsource_txtfile[STR_MAX_LEN];    /* heasim auxiliary point source output ascii file */
} Filenames_struct;


/**
 * \brief Structure to hold observation parameters
 * \param[in,out] exposure Exposure time in sec
 * \param[in,out] ra Right Ascension of center
 * \param[in,out] dec Declination of center
 * \param[in,out] radius Field of View radius
 * \param[in,out] nh_gal Galactic column density */
typedef struct{
    double exposure; /* exposure time in sec */
    double ra;   /* Right Ascension of center */
    double dec;  /* Declination of center */
    double radius;   /* Field of View radius */
    double nh_gal;   /* Galactic column density */
} Obs_params_struct;


/**
 * \brief Structure to hold energy bin data
 * \param[in,out] nebin Number of energy bins
 * \param[in,out] emin Minimum energy
 * \param[in,out] emax Maximum energy
 * \param[in,out] de Energy bin size
 * \param[in,out] energ_lo Array of lower bounds on energy bins
 * \param[in,out] energ_hi Array of upper bounds on energy bins */
typedef struct{
    long nebin;  /* Number of energy bins */
    double emin; /* Minimum energy */
    double emax; /* Maximum energy */
    double de;   /* Energy bin size */
    double * energ_lo;  /* Array of lower bounds on energy bins */
    double * energ_hi;  /* Array of upper bounds on energy bins */
    double * energ_mid; /* Array of middle values of energy bins */
    double * xspec_energy;  /* Energy array input into xspec */
} Engrid_struct;


/**
 * \brief Structure to hold all procedural flags
 * \param[in,out] flaglogns Include emission from log N - log S? 
 * \param[in,out] flaggal Include soft X-ray Galactic and LHB emission? 
 * \param[in,out] flagswcx Include SWCX emission? 
 * \param[in,out] flagdgrb Include diffuse gamma-ray emission? 
 * \param[in,out] flaggrxe Include galactic ridge emission? */
typedef struct{
    int flaglogns; /* Include emission from log N - log S? */
    int flaggal; /* Include soft X-ray Galactic and LHB emission? */
    int flagswcx; /* Include SWCX emission? */
    int flagdgrb; /* Include diffuse gamma-ray emission? */
    int flaggrxe; /* Include galactic ridge emission? */
} Flag_struct;


/**
 * \brief Structure to hold all logN-logS data
 * \param[in,out] slopebright1 LogN-LogS slope 1 -- bright end 
 * \param[in,out] slopefaint1 LogN-LogS slope 1 -- faint end 
 * \param[in,out] fluxbreak LogN-LogS flux at power-law break 
 * \param[in,out] norm1 LogN-LogS normalization in sources/sq-degree for slope 1 
 * \param[in,out] slope2 LogN-LogS slope 2 
 * \param[in,out] norm2 LogN-LogS normalization in sources/sq-degree for slope 2 
 * \param[in,out] fluxsens LogN-LogS flux sensitivity limit 
 * \param[in,out] sigtonoise LogN-LogS signal-to-noise corresponding to flux limit 
 * \param[in,out] fluxmin lower flux limit of logns in erg/cm2/sec -- both components 
 * \param[in,out] fluxmax upper flux limit of logns in erg/cm2/sec -- both components 
 * \param[in,out] slo detection limit of logN-logS 
 * \param[in,out] ctstoflux count-rate to flux conversion in erg/cm2/se/ct 
 * \param[in,out] bandpasslo lower limit  in keV for bandpass over which log N - log S is defined 
 * \param[in,out] bandpasshi upper limit in keV for bandpass over which log N - log S is defined 
 * \param[in,out] spectype1 spectral type for slope-1 source: 0=single spectrum, 1=multi, 2=torus 
 * \param[in,out] specmod1 slope-1 sources spectral model if spectype1=0 
 * \param[in,out] specpar1 slope-1 sources spectral parameter if spectype1=0 
 * \param[in,out] nhmod1 slope-1 sources spectral model NH if spectype1=0 
 * \param[in,out] fabs0 fraction of slope-1 sources, NH<1e21 cm-2 
 * \param[in,out] fabs1 fraction of slope-1 sources, NH=1e21-1e22 cm-2 
 * \param[in,out] fabs2 fraction of slope-1 sources, NH=1e22-1e23 cm-2 
 * \param[in,out] fabs3 fraction of slope-1 sources, NH=1e23-1e24 cm-2 
 * \param[in,out] fabs4 fraction of slope-1 sources, NH=1e24-1e25 cm-2 
 * \param[in,out] fabs5 fraction of slope-1 sources, NH>1e25 cm-2 
 * \param[in,out] fpar0 fraction index=1.5-1.7 if spectype1=1, opening angle <30 if spectype1=2 
 * \param[in,out] fpar1 fraction index=1.7-1.9 if spectype1=1, opening angle 30-45 if spectype1=2 
 * \param[in,out] fpar2 fraction index=1.9-2.1 if spectype1=1, opening angle 45-60 if spectype1=2 
 * \param[in,out] fpar3 fraction index=2.1-2.3 if spectype1=1, opening angle 60-75 if spectype1=2 
 * \param[in,out] fpar4 fraction index=2.3-2.5 if spectype1=1, opening angle 75-90 if spectype1=2 
 * \param[in,out] samespec Do "slope 1" and "slope 2" sources have same spectra? 
 * \param[in,out] specmod2 heasim-supported spectral model; ignored if samespec==yes 
 * \param[in,out] specpar2 heasim-supported spectral model parameter; ignored if samespec==yes 
 * \param[in,out] nhmod2 absorption; ignored if samespec==yes */
typedef struct{
    double slopebright1; /* LogN-LogS slope 1 -- bright end */
    double slopefaint1; /* LogN-LogS slope 1 -- faint end */
    double fluxbreak; /* LogN-LogS flux at power-law break */
    double norm1; /* LogN-LogS normalization in sources/sq-degree for slope 1 */
    double slope2; /* LogN-LogS slope 2 */
    double norm2; /* LogN-LogS normalization in sources/sq-degree for slope 2 */
    double fluxsens; /* LogN-LogS flux sensitivity limit */
    double sigtonoise; /* LogN-LogS signal-to-noise corresponding to flux limit */
    double fluxmin; /* lower flux limit of logns in erg/cm2/sec -- both components */
    double fluxmax; /* upper flux limit of logns in erg/cm2/sec -- both components */
    double slo; /* detection limit of logN-logS */
    double ctstoflux; /* count-rate to flux conversion in erg/cm2/se/ct */
    double bandpasslo; /* lower limit  in keV for bandpass over which log N - log S is defined */
    double bandpasshi; /* upper limit in keV for bandpass over which log N - log S is defined */
    int spectype1; /* spectral type for slope-1 source: 0=single spectrum, 1=multi, 2=torus */
    char specmod1[STR_MAX_LEN]; /* slope-1 sources spectral model if spectype1=0 */
    double specpar1; /* slope-1 sources spectral parameter if spectype1=0 */
    double nhmod1; /* slope-1 sources spectral model NH if spectype1=0 */
    double fabs0; /* fraction of slope-1 sources, NH<1e21 cm-2 */
    double fabs1; /* fraction of slope-1 sources, NH=1e21-1e22 cm-2 */
    double fabs2; /* fraction of slope-1 sources, NH=1e22-1e23 cm-2 */
    double fabs3; /* fraction of slope-1 sources, NH=1e23-1e24 cm-2 */
    double fabs4; /* fraction of slope-1 sources, NH=1e24-1e25 cm-2 */
    double fabs5; /* fraction of slope-1 sources, NH>1e25 cm-2 */
    double fpar0; /* fraction index=1.5-1.7 if spectype1=1, opening angle <30 if spectype1=2 */
    double fpar1; /* fraction index=1.7-1.9 if spectype1=1, opening angle 30-45 if spectype1=2 */
    double fpar2; /* fraction index=1.9-2.1 if spectype1=1, opening angle 45-60 if spectype1=2 */
    double fpar3; /* fraction index=2.1-2.3 if spectype1=1, opening angle 60-75 if spectype1=2 */
    double fpar4; /* fraction index=2.3-2.5 if spectype1=1, opening angle 75-90 if spectype1=2 */
    int samespec; /* Do "slope 1" and "slope 2" sources have same spectra? */
    char specmod2[STR_MAX_LEN]; /* heasim-supported spectral model; ignored if samespec==yes */
    double specpar2; /* heasim-supported spectral model parameter; ignored if samespec==yes */
    double nhmod2; /* absorption; ignored if samespec==yes */
} logN_logS_struct;


/**
 * \brief Structure to hold flux data
 * \param[in,out] swcxOVII in LU;  1 LU == 1 photon s-1 cm-2 str-1 
 * \param[in,out] swcxcont SWCX continuous flux in erg/sec/sq-cm */
typedef struct{
    double swcxOVII; /* in LU;  1 LU == 1 photon s-1 cm-2 str-1 */
    double swcxcont; /* 0.2-2 keV SWCX continuous flux in erg/sec/sq-cm/sq-arcmin */
} Flux_struct;


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


/***************************************************************/
/*********** Function Definitions ******************************/

/**
 * \brief Produce a RNG seed from the system time */
unsigned long int MTseed(void);

/**
 * \brief Initialize all structure members
 * \param[in,out] s_obs Structure containing observation parameters
 * \param[in,out] s_engrid Structure containing energy parameters
 * \param[in,out] s_flags Structure containing procedural flags
 * \param[in,out] s_logNS Structure containing logN-logS data
 * \param[in,out] s_flux Structure contianing flux data */
void initialize_structs(Obs_params_struct * s_obs,
			Engrid_struct * s_engrid,
			Flag_struct * s_flags,
			logN_logS_struct * s_logNS,
			Flux_struct * s_flux);


/**
 * \brief Read in the parameter list from skyback.par
 * \param[in] argc Default input param
 * \param[in] argv Default input param
 * \param[in,out] s_files Structure containing filenames
 * \param[in,out] s_obs Structure containing observation parameters             
 * \param[in,out] s_engrid Structure containing energy parameters   
 * \param[in,out] s_flags Structure containing procedural flags
 * \param[in,out] s_logNS Structure containing logN-logS data                       
 * \param[in,out] s_flux Structure contianing flux data
 * \param[out] debug Print diagnostic info to the screen
 * \param[out] clobber Overwrite output file
 * \param[out] mode Mode for querying automatic parameters
 * \param[out] seed Seed for random number generator */
int getPars(int argc, char ** argv,
	    Filenames_struct * s_files,
	    Obs_params_struct * s_obs,
	    Engrid_struct * s_engrid,
	    Flag_struct * s_flags,
	    logN_logS_struct * s_logNS,
	    Flux_struct * s_flux,
	    int * debug,
	    int * clobber,
	    char * mode,
	    long * seed);

/**
 * \brief Sum background contributions from each type of source
 * \param[in] s_files Structure containing filenames
 * \param[in] s_obs Structure containing observation parameters 
 * \param[in] s_engrid Structure containing energy parameters
 * \param[in] s_flags Structure containing procedural flags
 * \param[in] s_logNS Structure containing logN-logS data
 * \param[in] s_flux Structure contianing flux data
 * \param[in] debug Print diagnostic info to the screen
 * \param[in] clobber Overwrite output file
 * \param[in,out] MTstate State of random number generator */
int doWork(Filenames_struct * s_files,
           Obs_params_struct * s_obs,
           Engrid_struct * s_engrid,
           Flag_struct * s_flags,
           logN_logS_struct * s_logNS,
           Flux_struct * s_flux,
           int debug,
           int clobber,
	   HDmt_state * MTstate);

/**
 * \brief Return the greater of two ints */
int intmax(int a, int b);

/**
 * \brief Return the lesser of two ints */
int intmin(int a, int b);



/**
 * \brief Return the greater of two double */
double dblmax(double a, double b);

/**
 * \brief Return the lesser of two double */
double dblmin(double a, double b);


/**
 * \brief Free all memory tied to the energy grid structure
 * \param[in,out] s_engrid Structure containing energy parameters */
void deallocate_energ_data(Engrid_struct * s_engrid);			


/**
 * \brief Read FITS image and store it as an array
 * \param[in] ifilename Name of FITS file
 * \param[in,out] nx x-dimensions of image
 * \param[in,out] ny y-dimensions of image
 * \param[out] array Array containing image data
* \param[in] debug  flag to print debug info */
int image_to_array(char * ifilename, long * nx, long * ny, double *** array, int debug);

/**
 * \brief Recompute the number of counts in a channel assuming Poisson statistics
 * \param[in] xm  Mean of channel val
 * \param[in/out MTstate  "save" state of Mersenne Twister RNG */
double poidev(double xm, HDmt_state *MTstate);

/**
 * \brief dimensionless N(>S) used to derive point source flux */
double eta(double sl1, double sl2, double xmax, double x);

/**
 * \brief Return natural log of gamma function
 * \param[in] xx  input value */
double gammln(double xx);


/**
 * \brief Generate point source random RA and Dec for some field of view
 * \param[in] ns number of sources
 * \param[in] s_obs structure containing observation parameters
 * \param[in] sincenterdec  sin of central dec
 * \param[in] coscenterdec  cos of central dec
 * \param[out] ra_cat  output catalog of RAs 
 * \param[out] dec_cat  output catalog of DECs 
 * \param[in,out] MTstate  state of random number generator */
int skyrand(int ns,                     /* number of sources */
            Obs_params_struct * s_obs,  /* structure containing observation parameters */
            double sincenterdec,        /* sin of central dec */
            double coscenterdec,        /* cos of central dec */
            double ** ra_cat,           /* output catalog of RAs */
            double ** dec_cat,          /* output catalog of DECs */
            HDmt_state *MTstate);       /* state of random number generator */



/**
 * \brief Calculate the resolved and unresolved background from a population of extragalactic point sources
 * \param[in] s_obs Structure containing observation parameters
 * \param[in] s_logNS Structure containing logN-logS data
 * \param[in] s_engrid Structure containing energy parameters
 * \param[out] ncat Total number of entries in the psource catalog
 * \param[out] ps_cat The point source catalog
 * \param[out] ps_spec Point source spectra
 * \param[out] zcat  Redshfit catalog
 * \param[out] nhcat  Catalog of intrinsic NH
 * \param[in,out] MTstate State of random number generator
 * \param[in] debug  flag to debug */
int psources(Obs_params_struct * s_obs,
             logN_logS_struct * s_logNS,
             Engrid_struct * s_engrid,
             int * ncat,
             double *** ps_cat,
             double ** ps_spec,
             double ** zcat,
             double ** nhcat,
             HDmt_state *MTstate,
	     int debug);


/**
 * \brief convert string description of model type to int
 * \param[in] spec_model String name of spectral model */
int spec_string_to_type(char * spec_model);


/**
 * \brief Populate torus parameter and spectra structures
 * \param[in,out] s_tpar Structure containing torus parameters
 * \param[in,out] s_tspec Structure containing torus spectra
 * \param[in] debug  flag to debug */
int torus_prep(torus_par_struct * s_tpar, torus_spec_struct *s_tspec, int debug);

/**
 * \brief Remove a subtring from a string
 * \param[in,out] s the string to modify
 * \param[in] toremove the substring to remove */
void remove_substring(char *s, const char *toremove);


/**
 * \brief write the table model fits file for the diffuse spectrum
 * \param[in] s_engrid Structure containing energy parameters
 * \param[in] modfilename Name of the model file
 * \param[in] modelname Name of the model
 * \param[in] tot_spec The spectra to be written to file */
int write_table_model(Engrid_struct * s_engrid,
                      char * modfilename,
                      char * modelname,
                      double * tot_spec);

/**
 * \brief Calculate spectrum from one of the supported standard models
 * \param[in] spectype  0:phabs,1:power, 2:rs, 3:bb, 4:brem, 5:mono, 6:torus, 7: gilli_thick, 8:gilli_mild
 * \param[in] zpar   redshift parameter
 * \param[in] specpar  varies depending on the model
 * \param[in] fluxpar  over the input bandpass in erg/cm2/sec 
 * \param[in] norm_lower  lower limit of bandpass in keV for flux 
 * \param[in] norm_upper  upper limit of bandpass in keV for flux 
 * \param[in] nebin  number input energy bins 
 * \param[in] ebin_mid  midpoint of arf energy grid bins 
 * \param[in] xspec_energy  vector of energy grid boundaries for Xspec model input 
 * \param[out] spec  spectrum in photons/cm2/sec/channel on input energy grid
 * \param[in] debug  flag to debug */
int spectra(int spectype,             /* 0:phabs,1:power, 2:rs, 3:bb, 4:brem, 5:mono, 6:torus, 7: gilli_thick, 8:gilli_mild */
	    double zpar,              /* redshift parameter */
	    double specpar,           /* see comment below */
	    double fluxpar,           /* over the input bandpass in erg/cm2/sec */
            double norm_lower,        /* lower limit of bandpass in keV for flux */
            double norm_upper,        /* upper limit of bandpass in keV for flux */
            long nebin,               /* number input energy bins */
	    double * energ_mid,
	    double * xspec_energy,
            double * spec,           /* spectrum in photons/cm2/sec/channel on input energy grid */
	    int debug);

/**
 * \brief Compute simplest flux estimate by summing spec*en over energy bins in bandpass
 * \param[in] en  spectrum energy grid midpoints in keV
 * \param[in] spec  spectrum over en in photons/cm^2/s
 * \param[in] nebin  number of energy bins
 * \param[in] lower_limit  lower limit in keV of bandpass for flux calculations
 * \param[in] upper_limit  upper limit in keV of bandpass for flux calculations
 * \param[out] intspec  flux in keV/cm^2/s of spec over bandpass */
int spec_scale(double *en,          /* spectrum energy grid midpoints in keV */
               double *spec,        /* spectrum over en in photons/cm^2/s */
               int nebin,           /* number of energy bins */
               double lower_limit,  /* lower limit in keV of bandpass for flux calculations */
               double upper_limit,  /* upper limit in keV of bandpass for flux calculations */
               double *intspec);    /* flux in keV/cm^2/s of spec over bandpass */


/**
 * \brief Find smallest index such that vec[index] >= pos
 * \param[in] pos  value for which vec is to be searched
 * \param[in] nvec  size of 1D array vec
 * \param[in] vec 1D array to be searched */
int find_index(double pos,       /* value for which vec is to be searched */
               int nvec,         /* size of 1D array vec */
               double * vec);    /* 1D array to be searched */


/**
 * \brief Identify the correct torus index in a sequential list of spectra
 * \param[in] s_tpar  Structure containing torus parameters
 * \param[in] gamma  power-law index
 * \param[in] nh_22  NH in units of 10^22 cm^-2
 * \param[in] opening  opening angle
 * \param[in] incang  inclination angle
 * \param[out] tor_index  torus index */
int find_torus_index(torus_par_struct * s_tpar,  /* torus parameter structure */
                     double gamma,               /* power-law index */
                     double nh_22,               /* NH in units of 10^22 cm^-2 */
                     double opening,             /* opening angle */
                     double incang,              /* inclination angle */
                     long *tor_index);           /* torus index */



/**
 * \brief Calculate the input spectrum from one of the supported models or a user file.
 * \param[in] spectype         6:torus, 7: gilli_thick, 8:gilli_mild
 * \param[in] zpar             redshift parameter
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
 * \param[in] debug  flag to debug */
int torus_spectra(int spectype,           /* 6:torus, 7: gilli_thick, 8:gilli_mild */
		  double zpar,            /* redshift parameter */
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
		  int debug);


/**
 * \brief Free torus structures from memory
 * \param[in] s_tpar  Structure containing torus parameters
 * \param[in] s_tspec Structure containing torus spectra */
void deallocate_torus(torus_par_struct * s_tpar, torus_spec_struct *s_tspec);


/**
 * \brief Two-point interpolation to estimate y(z) at single point x=z
 * \param[in] n  number of points where y is evaluated
 * \param[in] x  points where y is evaluated
 * \param[in] y  function evaluated at x 
 * \param[in] z  single point where y(z) is to be estimated */
double interpol_hunt(int n,       /* number of points where y is evaluated */
		     double *x,   /* points where y is evaluated */
                     double *y,   /* function evaluated at x */
                     double z);   /* single point where y(z) is to be estimated */

/**
 * \brief Average count over larger map areas of local coverage gap
 * \param[in] array  array of count map
 * \param[in] nx  x map dimension
 * \param[in] ny  y map dimension
 * \param[in] jx  x counts
 * \param[in] jy  y counts
 * \param[in] jdel  delta counts */
double count_avg(double ** array,  /* array of count map */
                 int nx,           /* x map dimension */
                 int ny,           /* y map dimension */
                 int jx,        /* x counts */
                 int jy,        /* y counts */
                 int jdel);     /* delta counts */


/**
 * \brief Compute background contribution from Solar Wind Charge Exchange (SWCX)
 * \param[in] s_obs  Structure containing observation parameters
 * \param[in] s_engrid  Structure containing energy grid parameters
 * \param[in] s_flux  Structure containing fluxes
 * \param[out] swcx_spec  spectra from SWCX
 * \param[in] debug  flag to debug */
int swcx(Obs_params_struct * s_obs,
         Engrid_struct * s_engrid,
         Flux_struct * s_flux,
         double ** swcx_spec,
	 int debug);



/**
 * \brief Compute background contribution from galactic sources                                                                
 * \param[in] s_obs  Structure containing observation parameters
 * \param[in] s_engrid  Structure contianing energy grid parameters
 * \param[out] gal_spec  spectra from galactic sources
 * \param[in] debug  flag to debug */
int galactic(Obs_params_struct * s_obs,
             Engrid_struct * s_engrid,
	     Flux_struct * s_flux,
	     double ** gal_spec,
	     int debug);


/**
 * \brief Compute background contribution from diffuse gamma ray background
 * \param[in] s_obs  Structure containing observation parameters
 * \param[in] s_engrid  Structure contianing energy grid parameters
 * \param[out] dgrb_spec  spectra from diffuse gamma ray background
 * \param[in] debug  flag to debug */
int dgrb(Obs_params_struct * s_obs,
	 Engrid_struct * s_engrid,
	 double ** dgrb_spec,
	 int debug);

/**
 * \brief Compute background contribution from galactic ridge                                                                     
 * \param[in] s_obs  Structure containing observation parameters
 * \param[in] s_engrid  Structure contianing energy grid parameters
 * \param[out] grxe_spec  spectra from galactic ridge
 * \param[in] debug  flag to debug */
int grxe(Obs_params_struct * s_obs,
	 Engrid_struct * s_engrid,
	 double ** grxe_spec,
	 int debug);

/**
 * \brief Query for most recent update of skyback code
 * \param[out] version  Date of most recent update of skyback code */
void get_version(char * version);

/**
 * \brief Trim white space from string
 * \param[in,out] str  String to trim */
char *trim_string(char *str);


/**
 * \brief Resolve relative pathname with ENV variables into absolute pathname
 * \param[in] input_string  Pathname to resolve */
char * resolve_pathname(char * input_string);

/**
 * \brief Compute redshift probabilities
 * \param[in] z0  current redshift
 * \param[in] z   redshift */
double fz(double z0, double z);

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



/*
 ! $Log: skyback.h,v $
 ! Revision 1.16  2015/05/19 17:12:16  driethmi
 ! Added CVS macro to print file revision history at end of file.
 !
*/
