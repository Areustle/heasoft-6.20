/** 
 * \file heasim.c
 * \brief The HEADAS Simulator - main file
 * \author David Riethmiller
 * \date   $Date: 2016/09/01 16:40:41 $
 *
 *  Given input from the user-interface and a source definition file,
 *  perform the simulation and write the output event file.  Based on previous code
 *  written by Nicolas Collins, Steve Snowden, Randall K. Smith, and James Peachey.
 */

#include "heasim.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "headas_utils.h"

#ifndef use_legacy_heasp
#include "Cheasp.h"       /* C-interface to C++ HEASP library */
#endif

#include "fitsio.h"       /* cfitsio defined constants */
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>


int main(int argc, char *argv[]){
    
    /* VARIABLE DECLARATIONS */
    
    /* variables found in parameter file heasim.par */
    /* s1,s2,s3 refer to containing structures, see below */
    char mission[STR_MAX_LEN];                    /* (s1) Observatory, e.g. Astro-H, XMM, Suzaku... */
    char instrume[STR_MAX_LEN];                     /* (s1) Instrument, e.g. SXI, SXS, EMOS, etc. */ 
    char filter[STR_MAX_LEN];                         /* (s1) Don't know if this will be used */
    char instmode[STR_MAX_LEN];                /* (s1) Instrument Mode - will be used? */
    double rapoint=0.0;                                /* (s1) Pointing RA, decimal degrees J2000 */
    double decpoint=0.0;                               /* (s1) Pointing DEC, decimal degrees J2000 */
    double roll=0.0;                                       /* (s1) Roll angle, decimal degrees */
    double exposure=0.0;                                  /* (s1) Simulation Exposure Time */
    char insrcdeffile[STR_MAX_LEN];          /* Name of source input filename */
    char outfile[STR_MAX_LEN];                /* Output event filename */
    char psffile[STR_MAX_LEN] = "";                  /* (s2) PSF filename */
    char vigfile[STR_MAX_LEN] = "";             /* (s2) Vignette filename */
    char rmffile[STR_MAX_LEN] = "";                  /* (s2) Response Matrix filename */
    char arffile[STR_MAX_LEN] = "";                  /* (s2) Ancillary Response filename */
    double arfrmftol=0.0;                          /* (s2) Tolerance for RMF & ARF comparison */
    char intbackfile[STR_MAX_LEN] = "";  /* (s2) Internal Background filename */
    char psbackfile[STR_MAX_LEN];             /* (s3) Point source catalog from background tool */
    char difbackfile[STR_MAX_LEN];            /* (s3) Diffuse spectrum catalog from background tool */
    int getinfile=0;                            /* Copy sample source def file to current working dir?  (BOOLEAN) */
    int debug=0;                                        /* Flag for setting debuggin mode (BOOLEAN) */
    int clobber=0;                                      /* Overwrite existing output file? (BOOLEAN) */
    char par_mode[STR_MAX_LEN];                       /* Mode for parameter file */
    char mdbfile[STR_MAX_LEN];                        /* Name of Mission Database File */
    long seed=0;                                  /* User input RNG seed.  If less than 0, overwritten with system time seed. */
    /* are there more background and ray-tracing parameters to insert here? */

    /* parameter structures, defined in heasim.h */
    ObsParams s_obs;                                 /* Contains params marked "s1" above. */
    CalFiles s_calfiles;                              /* Contains params marked "s2" above. */
    GeneralBackground s_back;                        /* Contains params marked "s3" above. */
    MDB_params s_mdb;                         /* structure to hold the above MDB derived params */

    /* variables related to arf/rmf read */
    int nebin=0;            /* number of arf input energy bins */
    double* ebin_lo = 0;       /* lower boundary of arf energy grid bins */
    double* ebin_hi = 0;       /* upper boundary of arf energy grid bins */
    double* ebin_mid = 0;      /* midpoint of arf energy grid bins */
    double* ebin_del = 0;      /* grid spacing of arf energy grid bins */
    double* area = 0;          /* effective area from arf */
    int nchan=0;            /* number of output energy bins [PI channels] from rmf ebounds ext */
    double* emin = 0;          /* lower boundary of output energy grid bins */
    double* emax = 0;          /* upper boundary of output energy grid bins */
    double* ecen = 0;          /* midpoint of output energy grid bins */
    double* edelt = 0;         /* grid spacing output energy grid bins */
    int nebin_rmf=0;        /* number of rmf input energy bins */
    double* ebin_lo_rmf = 0;   /* lower boundary of rmf energy grid bins */
    double* ebin_hi_rmf = 0;   /* upper boundary of rmf energy grid bins */
    long ebounds_TLMIN1=0;   /* EBOUNDS minimum allowed PI channel value */
    long ebounds_TLMAX1=0;  /* EBOUNDS maximum allowed PI channel value */
    double sens_lo=0.0;       /* minimum energy sensitivity of RMF */
    double sens_hi=0.0;       /* maximum energy sensitivity of RMF */
    
    /* ARF and RMF structures from HEASP package, defined in headas/heacore/heasp/Cheasp.h */
    RMF rmf_struct;       /* Holds RMF data */
    ARF arf_struct;       /* Holds ARF data */

    /* variables related to internal background read */
    double* ibspec = 0;           /* internal background spectrum */
    double  ib_expose=0;        /* internal background exposure time */
    double* ib_backscal = 0;      /* internal background scaling factor */
    long  ib_firstchan = 0;     /* first channel number in background spectrum */

    /* variables related to input source file read */
    int nsource=0;             /* number of sources found in input source file */
    double * ras = 0;             /* array of source RAs */
    double * decs = 0;            /* array of source DECs */
    double * colden = 0;          /* array of source column densities */
    int * spectype = 0;          /* array of source spectral type */
    double * specpar = 0;         /* value of spectral param for all sources */
    double * fluxpar = 0;         /* flux for all sources, can be 0 for spectral file */
    double * band_lo = 0;         /* flux lower bandpass for all sources */
    double * band_hi = 0;         /* flux upper bandpass for all sources */
    char ** sfilename = 0;       /* name of spectral user input file for all sources, "none" for spectral model */
    int * sformat = 0;           /* spectral sformat file format flag 1 or 2 for all sources = 0; can be 0 for spectral model */
    int * sunits = 0;            /* spectral file flux unit tag 1-9 for all sources, can be 0 for spectral model */
    double * period = 0;          /* period for all sources, 0 for constant */
    double * pulse_fraction = 0;  /* pulse fraction for all sources, 0 for constant */
    char ** ifilename = 0;      /* name of image user input file for all sources; "none" for point source or spatial model */
    int sd_matrix_size=0;      /* maximum number of spatial distribution quantities needed */
    double ** sd_param_matrix = 0; /* matrix of spatial distribution quantities needed to apply source distribution */
    
    /* variables related to input background point source catalog file read */
    int nsource_psback=0;             /* number of sources found in input source file */
    double * ras_psback = 0;             /* array of source RAs */
    double * decs_psback = 0;            /* array of source DECs */
    double * colden_psback = 0;          /* array of source column densities */
    int * spectype_psback = 0;          /* array of source spectral type */
    double * specpar_psback = 0;         /* value of spectral param for all sources */
    double * fluxpar_psback = 0;         /* flux for all sources, can be 0 for spectral file */
    double * band_lo_psback = 0;         /* flux lower bandpass for all sources */
    double * band_hi_psback = 0;         /* flux upper bandpass for all sources */
    char ** sfilename_psback = 0;       /* name of spectral user input file for all sources, "none" for spectral model */
    int * sformat_psback = 0;           /* spectral sformat file format flag 1 or 2 for all sources; can be 0 for spectral model */
    int * sunits_psback = 0;            /* spectral file flux unit tag 1-9 for all sources, can be 0 for spectral model */
    double * period_psback = 0;          /* period for all sources, 0 for constant */
    double * pulse_fraction_psback = 0;  /* pulse fraction for all sources, 0 for constant */
    char ** ifilename_psback = 0;      /* name of image user input file for all sources; "none" for point source or spatial model */    
    int sd_matrix_size_psback=0;      /* maximum number of spatial distribution quantities needed */
    double ** sd_param_matrix_psback = 0; /* matrix of spatial distribution quantities needed to apply source distribution */


    /* variables related to input background diffuse source catalog file read */
    int nsource_difback=0;             /* number of sources found in input source file */
    double * ras_difback = 0;             /* array of source RAs */
    double * decs_difback = 0;            /* array of source DECs */
    double * colden_difback = 0;          /* array of source column densities */
    int * spectype_difback = 0;          /* array of source spectral type */
    double * specpar_difback = 0;         /* value of spectral param for all sources */
    double * fluxpar_difback = 0;         /* flux for all sources, can be 0 for spectral file */
    double * band_lo_difback = 0;         /* flux lower bandpass for all sources */
    double * band_hi_difback = 0;         /* flux upper bandpass for all sources */
    char ** sfilename_difback = 0;       /* name of spectral user input file for all sources, "none" for spectral model */
    int * sformat_difback = 0;           /* spectral sformat file format flag 1 or 2 for all sources; can be 0 for spectral model */
    int * sunits_difback = 0;            /* spectral file flux unit tag 1-9 for all sources, can be 0 for spectral model */
    double * period_difback = 0;          /* period for all sources, 0 for constant */
    double * pulse_fraction_difback = 0;  /* pulse fraction for all sources, 0 for constant */
    char ** ifilename_difback = 0;      /* name of image user input file for all sources; "none" for point source or spatial model */
    int sd_matrix_size_difback=0;      /* maximum number of spatial distribution quantities needed */
    double ** sd_param_matrix_difback = 0; /* matrix of spatial distribution quantities needed to apply source distribution */

    /* Combined source catalogs */
    int nsource_new=0;             /* number of sources found in input source file */
    double * ras_new = 0;             /* array of source RAs */
    double * decs_new = 0;            /* array of source DECs */
    double * colden_new = 0;          /* array of source column densities */
    int * spectype_new = 0;          /* array of source spectral type */
    double * specpar_new = 0;         /* value of spectral param for all sources */
    double * fluxpar_new = 0;         /* flux for all sources, can be 0 for spectral file */
    double * band_lo_new = 0;         /* flux lower bandpass for all sources */
    double * band_hi_new = 0;         /* flux upper bandpass for all sources */
    char ** sfilename_new = 0;       /* name of spectral user input file for all sources, "none" for spectral model */
    int * sformat_new = 0;           /* spectral sformat file format flag 1 or 2 for all sources; can be 0 for spectral model */
    int * sunits_new = 0;            /* spectral file flux unit tag 1-9 for all sources, can be 0 for spectral model */
    double * period_new = 0;          /* period for all sources, 0 for constant */
    double * pulse_fraction_new = 0;  /* pulse fraction for all sources, 0 for constant */
    char ** ifilename_new = 0;      /* name of image user input file for all sources; "none" for point source or spatial model */
    double ** sd_param_matrix_new = 0; /* matrix of spatial distribution quantities needed to apply source distribution */

    /* burst parameters */
    double * tburst = 0;            /* burst start time for all original sources, 0 for constant */
    double * trise = 0;             /* burst risetime for all original sources, 0 for constant */
    double * tdecay = 0;            /* burst decay time for all original sources, 0 for constant */
    double * burst_rat = 0;         /* burst ratio for all original sources, 0 for constant */
    int * burst_tabidx = 0;         /* burst table index for all original sources, 0 for constant */
    int nburst = 0;                 /* number of original burst sources*/

    double * tburst_psback = 0;     /* burst start time for all psback sources, 0 for constant */
    double * trise_psback = 0;      /* burst risetime for all psback sources, 0 for constant */
    double * tdecay_psback = 0;     /* burst decay time for all psback sources, 0 for constant */
    double * burst_rat_psback = 0;  /* burst ratio for all psback sources, 0 for constant */
    int * burst_tabidx_psback = 0;  /* burst table index for all psback sources, 0 for constant */
    int nburst_psback = 0;          /* number of psback burst sources*/

    double * tburst_difback = 0;    /* burst start time for all difback sources, 0 for constant */
    double * trise_difback = 0;     /* burst risetime for all difback sources, 0 for constant */
    double * tdecay_difback = 0;    /* burst decay time for all difback sources, 0 for constant */
    double * burst_rat_difback = 0; /* burst ratio for all difback sources, 0 for constant */
    int * burst_tabidx_difback = 0; /* burst table index for all difback sources, 0 for constant */
    int nburst_difback = 0;         /* number of difback burst sources*/

    double * burst_rat_new = 0;     /* burst ratio for all new sources, 0 for constant */
    double * tburst_new = 0;        /* burst start time for all sources, 0 for constant */
    double * trise_new = 0;         /* burst risetime for all sources, 0 for constant */
    double * tdecay_new = 0;        /* burst decay time for all nsources, 0 for constant */
    int * burst_tabidx_new = 0;     /* burst table index for all sources, 0 for constant */
    int nburst_new = 0;             /* number of all burst sources*/

    burst_table_struct btab;        /* structure holding burst table */

    /* misc parameters */
    int errstatus = 0;                        /* error status indicator.  0 to continue, anything else to stop */
    char version[STR_MAX_LEN];                           /* Last known date of simulator update */
    Vig_struct s_vig;      /* Vignette structure */
    int vigtype=0;         /* type of vignette file */
    PSF_struct s_psf;      /* PSF structure */
    int psftype=0;         /* type of psf file */
    PSF_image s_psf_image; /* PSF image structure */
    HDmt_state *MTstate = 0;   /* "save" state of Mersenne Twister RNG */
    unsigned long int RNGseed=0;  /* seed for RNG */
    double * redshifts = 0;
    double * intabs = 0;
    int spectra_errstatus = 0;  /* If there is a problem with any spectra, we set to zero but continue.  */
    int ii = 0;

#pragma mark start of code

    /* +++ CR 2014-09-08 JP: versions are a problem. This is not up to date and probably                                                           one doesn't want to check in a new version of this file every time just to change the date. Suggest                                            using a portion of the string cvs will fill in for the cvs special macro $Id: heasim.c,v 1.90 2016/09/01 16:40:41 asargent Exp $. Then whatever tag                                               was used to check out will be reported. There is support for this in heautils I think. */
    /* +++ DR - agreed.  This is a relatively easy fix, will just require a little research into heautils. */    
    get_version(version);

    printf("\n");
    printf("  Running HEASim, v. %s\n",version);
    printf("\n");
    

    /************************************************************/
    /********************* "GETPARS" STAGE **********************/
    /************************************************************/

    initialize_structures(&s_obs, &s_calfiles, &s_back, &s_mdb, &s_vig, &s_psf, &s_psf_image, &btab);

    /* pass the name of the input parameter file to get_input_params() and read. */
    errstatus = getPars(argc, argv, insrcdeffile, outfile, par_mode,
                        &debug, &clobber, &getinfile, &s_calfiles, &s_back, &s_obs, mdbfile, &seed);
    
    if (errstatus != 0){
	printf("errstatus %d, getPars failed.\n",errstatus);
	return -1;

    } else {

	/* unpack the ObsParams structure */
	strcpy(mission, s_obs.mission);
	strcpy(instrume, s_obs.instrume);
	strcpy(filter, s_obs.filter);
	strcpy(instmode, s_obs.instmode);
	rapoint = s_obs.rapoint;
	decpoint = s_obs.decpoint;
	roll = s_obs.roll;
	exposure = s_obs.exposure;
	
	/* unpack CalFiles structure */
	strcpy(psffile,s_calfiles.psffile);
	strcpy(vigfile,s_calfiles.vigfile);
	strcpy(arffile,s_calfiles.arffile);
	strcpy(rmffile,s_calfiles.rmffile);
	strcpy(intbackfile,s_calfiles.intbackfile);
	arfrmftol = s_calfiles.arfrmftol;
	
	/* unpack the GeneralBackground structure */
	strcpy(psbackfile,s_back.psbackfile);
	strcpy(difbackfile,s_back.difbackfile);
	
	/* Query CALDB if any of the CalFiles params specify "CALDB" in place of a filename */
	/* get_caldb_files(&s_calfiles, &s_obs); */
	
	/*  Print list of calibration files  */
	printf("\n Input Calibration files\n");
	printf("   intbackfile  = %s\n", intbackfile);
	printf("   psffile      = %s\n", psffile);
	printf("   vigfile      = %s\n", vigfile);
	printf("   arffile      = %s\n", arffile);
	printf("   rmffile      = %s\n", rmffile);
    
	/* ! Summary of global simulation info */
	printf("\n Global simulation info \n");
	printf("   Pointing Direction (RA, DEC, ROLL): %f, %f, %f\n", rapoint, decpoint, roll);
	printf("   Exposure Time (s): %f\n", exposure);
	printf("   Mission:           %s\n", mission);
	printf("   Instrument:        %s\n", instrume);
	printf("   dtpileup:          %f\n", s_obs.dtpileup);
	printf("   flagsubex:         %d\n", s_obs.flagsubex);
	printf("   subexposure:       %f\n", s_obs.subexposure);
	printf("   resample:          %d\n", s_obs.resample);
	printf("   skipfov:           %d\n", s_obs.skipfov);

	/* Report debugging mode */
	printf("\n Debugging mode: ");
	if (debug == 1){
	    printf("Engaged\n\n");
	} else printf("Disengaged\n\n");
    }    
    
    /************************************************************/
    /****************** "INITIALIZE" STAGE **********************/
    /************************************************************/

    /* Initialize the Mersenne Twister random number generator.  If user input seed is greater
       than zero, use it.  Otherwise, seed from the system time. */
    if (seed != 0){
	printf(" Using user input seed = %ld\n\n",seed);
	RNGseed = (unsigned long int)seed;
    } else{
	printf(" User input seed is zero, seeding with system time.\n\n");
	RNGseed = MTseed();
    }

    MTstate = HDmt_srand(RNGseed);

    /*
    if (debug == 1){
	ii = 0;
	printf("RNG Test: From main()\n");
	for (ii=0; ii<10; ii++)
	    printf("Random Number %d: %f\n",ii,HDmt_drand(MTstate));
    }
    */	


#pragma mark MDB Read

    /* Read primary mission parameters from mission database file. */
    errstatus = read_mdb(mdbfile, mission, instrume, filter, instmode, &s_mdb, debug);
    if (errstatus != 0){
	printf("errstatus %d, problem reading MDB file!\n",errstatus);
	return -1;
    }

    /* set up the instrument map image array, if available */
    if ( (s_mdb.instmap_flag == 1) && (s_obs.skipfov == 0) ){
	errstatus = read_instmap(&s_mdb, debug);
	if (errstatus != 0){
	    printf("    errstatus %d, could not read instrument map.\n",errstatus);
	    printf("    Resetting s_mdb.instmap_flag to 0.\n\n");
	    s_mdb.instmap_flag = 0;
	}
    }
    
    
#pragma mark ARF/RMF Read
    
    /* verify that all calibration files actually exist */
    errstatus = check_calfiles_exist(&s_calfiles);
    
    if (errstatus != 0){
	printf("errstatus %d, could not find one or more calibration files.\n",errstatus);
	return -1;
    }

    /* +++ 2016-08-24 RSH ***new */
    /* use heasp library routines to read in the response files */
    arf_struct = read_ARF(&s_calfiles, &nebin, debug);
    if (0 == strcasecmp(s_calfiles.rmffile, "none")) {
        /* +++ 2016-08-24 RSH ***new:  TRF diff */
        nchan = nebin;
        nebin_rmf = nebin;
    } else {
        rmf_struct = read_RMF(&s_calfiles, &nebin_rmf, &nchan, &ebounds_TLMIN1, &ebounds_TLMAX1, debug);
    }

    if (debug == 1){
	printf("nebin(arf): %d   nebin(rmf): %d   nchan: %d\n", nebin, nebin_rmf, nchan);
	printf("ebounds_TLMIN1 and TLMAX1: %ld %ld\n\n",ebounds_TLMIN1, ebounds_TLMAX1);
    }

    /* pass response structs and arrays into validation function */
    errstatus = check_arf_rmf(&s_calfiles, &rmf_struct, &arf_struct,
                  nebin, nebin_rmf, nchan,
                  &ebin_lo, &ebin_hi, &ebin_mid, &ebin_del, &area,
                  &ebin_lo_rmf, &ebin_hi_rmf,
                  &emin, &emax, &ecen, &edelt);
    
    if (errstatus != 0){
        printf("errstatus %d, ARF/RMF failed compatibility check.\n",errstatus);
        return -1;
    } 

    if (0 == strcasecmp(s_calfiles.rmffile, "none")) {
        /* +++ 2016-08-25 RSH ***new:  check this */
        ebounds_TLMIN1 = 0;
        ebounds_TLMAX1 = nchan - 1;
    } else {
        /* +++ 2016-08-24 RSH ***new:  TRF diff */
        RMFsensitivity(s_calfiles.rmffile, &sens_lo, &sens_hi);
    }

    if (debug == 1){
	/* print out first and last of every value, especially ib_backscal */
	printf("ARF/RMF first and last values:\n");
	printf("ebin_lo[0 and %d]      = %f  %f\n",nebin-1,ebin_lo[0],ebin_lo[nebin-1]);
	printf("ebin_hi[0 and %d]      = %f  %f\n",nebin-1,ebin_hi[0],ebin_hi[nebin-1]);
	printf("ebin_mid[0 and %d]     = %f  %f\n",nebin-1,ebin_mid[0],ebin_mid[nebin-1]);
	printf("area[0 and %d]         = %f  %f\n",nebin-1,area[0],area[nebin-1]);
	printf("ebin_lo_rmf[0 and %d]  = %f  %f\n",nebin-1,ebin_lo_rmf[0],ebin_lo_rmf[nebin-1]);
	printf("ebin_hi_rmf[0 and %d]  = %f  %f\n",nebin_rmf-1,ebin_hi_rmf[0],ebin_hi_rmf[nebin_rmf-1]);
	printf("emin[0 and %d]         = %f  %f\n",nchan-1,emin[0],emin[nchan-1]);
	printf("emax[0 and %d]         = %f  %f\n",nchan-1,emax[0],emax[nchan-1]);
	printf("edelt[0 and %d]        = %f  %f\n",nchan-1,edelt[0],edelt[nchan-1]);
	printf("\n");
	
    }    

    
#pragma mark Internal Background Read

    if (0 != strcasecmp("none",s_calfiles.intbackfile)){
        errstatus = read_internal_background(&s_calfiles, &s_mdb, nchan, ebounds_TLMIN1, &ibspec, &ib_backscal, &ib_expose, &ib_firstchan);

	if (errstatus != 0){
	    printf("errstatus %d, could not read internal background.\n",errstatus);
	    return -1;
	} else{
	    if (debug == 1){
		printf("PHA first and last values:\n");
		printf("ibspec[0 and %d]      = %f  %f\n",nchan-1,ibspec[0],ibspec[nchan-1]);
		printf("ib_backscal[0 and %d] = %f  %f\n",nchan-1,ib_backscal[0],ib_backscal[nchan-1]);
		printf("\n");
	    }
	}
    }

    
#pragma mark Source Read
    
    /* Read the source definition file */
    errstatus = read_source_data(insrcdeffile,
				 &nsource,
				 &ras,
				 &decs,
				 &colden,
				 &spectype,
				 &specpar,
				 &fluxpar,
				 &band_lo,
				 &band_hi,
				 &sfilename,
				 &sformat,
				 &sunits,
				 &period,
				 &pulse_fraction,
				 &tburst,
				 &trise,
				 &tdecay,
				 &burst_rat,
				 &burst_tabidx,
				 &nburst,
				 &ifilename,
				 &sd_matrix_size,
				 &sd_param_matrix, 
				 debug);
    
    if (errstatus != 0){
	printf("errstatus %d, problem reading source data.\n",errstatus);
	return -1;
    }


    if (0 != strcasecmp("none",s_back.psbackfile)){
	/* Read the point source background file */
	errstatus = read_source_data(s_back.psbackfile, 
				     &nsource_psback,    
				     &ras_psback, 
				     &decs_psback, 
				     &colden_psback, 
				     &spectype_psback,   
				     &specpar_psback, 
				     &fluxpar_psback, 
				     &band_lo_psback, 
				     &band_hi_psback,  
				     &sfilename_psback, 
				     &sformat_psback, 
				     &sunits_psback, 
				     &period_psback, 
				     &pulse_fraction_psback,
				     &tburst_psback, 
				     &trise_psback, 
				     &tdecay_psback, 
				     &burst_rat_psback,
				     &burst_tabidx_psback, 
				     &nburst_psback,
				     &ifilename_psback, 
				     &sd_matrix_size_psback, 
				     &sd_param_matrix_psback, 
				     debug);

	if (errstatus !=0){
	    printf("errstatus %d, problem reading point source background data.\n",errstatus);
	    return -1;
	}
    }


    if (0 != strcasecmp("none",s_back.difbackfile)){
        /* Read the diffuse source background file */
	errstatus = read_source_data(s_back.difbackfile,
				     &nsource_difback,    
				     &ras_difback,
				     &decs_difback,
				     &colden_difback,
				     &spectype_difback,   
				     &specpar_difback,
				     &fluxpar_difback,
				     &band_lo_difback,
				     &band_hi_difback,  
				     &sfilename_difback,
				     &sformat_difback,
				     &sunits_difback,
				     &period_difback, 
				     &pulse_fraction_difback,
				     &tburst_difback,
				     &trise_difback,
				     &tdecay_difback,
				     &burst_rat_difback,
				     &burst_tabidx_difback,
				     &nburst_difback,
				     &ifilename_difback, 
				     &sd_matrix_size_difback, 
				     &sd_param_matrix_difback, 
				     debug);
	if (errstatus !=0){
	    printf("errstatus %d, problem reading diffuse source background data.\n",errstatus);
	    return -1;
	}
    }

    
    /* Concatenate all of the source data */
    nsource_new = nsource + nsource_psback + nsource_difback;
    ras_new = array_concat_dbl(ras, nsource, ras_psback, nsource_psback, ras_difback, nsource_difback);
    decs_new = array_concat_dbl(decs, nsource, decs_psback, nsource_psback, decs_difback, nsource_difback);
    colden_new = array_concat_dbl(colden, nsource, colden_psback, nsource_psback, colden_difback, nsource_difback);
    spectype_new = array_concat_int(spectype, nsource, spectype_psback, nsource_psback, spectype_difback, nsource_difback);
    specpar_new = array_concat_dbl(specpar, nsource, specpar_psback, nsource_psback, specpar_difback, nsource_difback);
    fluxpar_new = array_concat_dbl(fluxpar, nsource, fluxpar_psback, nsource_psback, fluxpar_difback, nsource_difback);
    band_lo_new = array_concat_dbl(band_lo, nsource, band_lo_psback, nsource_psback, band_lo_difback, nsource_difback);
    band_hi_new = array_concat_dbl(band_hi, nsource, band_hi_psback, nsource_psback, band_hi_difback, nsource_difback);
    sfilename_new = array_concat_str(sfilename, nsource, sfilename_psback, nsource_psback, sfilename_difback, nsource_difback);   
    sformat_new = array_concat_int(sformat, nsource, sformat_psback, nsource_psback, sformat_difback, nsource_difback);
    sunits_new = array_concat_int(sunits, nsource, sunits_psback, nsource_psback, sunits_difback, nsource_difback);
    period_new = array_concat_dbl(period, nsource, period_psback, nsource_psback, period_difback, nsource_difback);
    pulse_fraction_new = array_concat_dbl(pulse_fraction, nsource, pulse_fraction_psback, nsource_psback, 
				      pulse_fraction_difback, nsource_difback);
    ifilename_new = array_concat_str(ifilename, nsource, ifilename_psback, nsource_psback, ifilename_difback, nsource_difback);     

    sd_param_matrix_new = matrix_concat_dbl(sd_param_matrix, nsource,
					    sd_param_matrix_psback, nsource_psback,
					    sd_param_matrix_difback, nsource_difback,
					    sd_matrix_size);

    nburst_new = nburst + nburst_psback + nburst_difback;
    burst_tabidx_new = array_concat_int(burst_tabidx, nburst, burst_tabidx_psback, nburst_psback, burst_tabidx_difback, nburst_difback);

    if (debug == 1){
	printf("CONCATENATED SD MATRIX:\n");
	for (ii=0; ii<nsource_new; ii++)
	    printf("%f %f %f %f %f %f %f %f\n",sd_param_matrix_new[ii][0],
		   sd_param_matrix_new[ii][1],
		   sd_param_matrix_new[ii][2],
		   sd_param_matrix_new[ii][3],
		   sd_param_matrix_new[ii][4],
		   sd_param_matrix_new[ii][5],
		   sd_param_matrix_new[ii][6],
		   sd_param_matrix_new[ii][7]);
	printf("\n");
    }

    /* Need to create concatenated burst arrays for source and background, even if zero arrays. */
    tburst_new = array_concat_dbl(tburst, nsource, tburst_psback, nsource_psback,  tburst_difback, nsource_difback);
    trise_new = array_concat_dbl(trise, nsource, trise_psback, nsource_psback,  trise_difback, nsource_difback);
    tdecay_new = array_concat_dbl(tdecay , nsource, tdecay_psback, nsource_psback, tdecay_difback, nsource_difback);
    burst_rat_new = array_concat_dbl(burst_rat, nsource, burst_rat_psback, nsource_psback, burst_rat_difback, nsource_difback);



    if (nburst > 0){
	errstatus = make_burst_tabarray(s_obs.exposure,
					nsource_new,
					burst_rat_new,
					tburst_new,
					trise_new,
					tdecay_new,
					burst_tabidx_new,
					nburst_new,
					1000,
					&btab);
    }


    redshifts = calloc(nsource_new, sizeof(double)); /* redshift array */
    intabs = calloc(nsource_new, sizeof(double)); /* intrinsic-absorption array */
    if ( strcasecmp(s_back.pszbackfile,"none") != 0) {
	errstatus = read_redshift(nsource, nsource_psback, s_back.pszbackfile, &redshifts, &intabs, debug);
	if (errstatus != 0){
	    printf("ERROR: problem reading redshift file %s, exiting.\n",s_back.pszbackfile);
	    return -1;
	}
    }

    if (s_obs.skipfov == 0){
	/* Notify user if specified source falls within detector gaps.  More complex routine? */
	warn_if_detector_gap(&s_obs, &s_mdb, nsource_new, ras_new, decs_new, debug);
    }

#pragma mark Setup Output FITS

    errstatus = setup_output(outfile, clobber, version, &s_obs, &s_mdb, &rmf_struct, ebounds_TLMIN1, ebounds_TLMAX1, debug);

    if (errstatus != 0){
	printf("errstatus %d, could not setup/save output file.\n",errstatus);
	return -1;
    }


#pragma mark Read Vignette File

    vigtype = read_vignette(s_calfiles.vigfile, &s_vig, debug);

    if ( (vigtype > 0) && (debug == 1) ){
        printf("Vignette data:\n");
        printf(" s_vig.ncol = %ld\n", s_vig.ncol);
        printf(" s_vig.nrow = %ld\n", s_vig.nrow);
        printf(" s_vig.vigtype = %d\n", s_vig.vigtype);
        printf(" s_vig.type_desc = %s\n\n", s_vig.type_desc);
    }


#pragma mark Read PSF File

    psftype = read_psf(s_calfiles.psffile, &s_psf, &s_psf_image, debug);

    if ( (psftype > 0) && (debug == 1) ){
	printf("PSF data:\n");
	printf(" s_psf.ncol = %ld\n", s_psf.ncol);
        printf(" s_psf.nrow = %ld\n", s_psf.nrow);
        printf(" s_psf.psftype = %d\n", s_psf.psftype);
        printf(" s_psf.type_desc = %s\n\n", s_psf.type_desc);
    }

    /*********** BEGIN doWork PHASE ******************/

    errstatus = doWork(outfile, 
		       &s_obs, 
		       &s_calfiles, 
		       &s_mdb, 
		       &s_vig, 
		       &s_psf, 
		       &s_psf_image, 
		       &s_back, 
		       &rmf_struct,
		       &arf_struct, 
		       &btab,
		       nebin, 
		       ebin_lo, 
		       ebin_hi, 
		       ebin_mid, 
		       ebin_del, 
		       area, 
		       nchan, 
		       emin, 
		       emax, 
		       edelt, 
		       ibspec, 
		       ib_expose, 
		       ib_backscal, 
		       ib_firstchan, 
		       nsource_new, 
		       ras_new, 
		       decs_new, 
		       colden_new, 
		       redshifts, 
		       intabs,
		       spectype_new, 
		       specpar_new, 
		       fluxpar_new, 
		       band_lo_new, 
		       band_hi_new, 
		       sfilename_new, 
		       sformat_new, 
		       sunits_new,
		       period_new, 
		       pulse_fraction_new, 
		       tburst_new, 
		       trise_new, 
		       tdecay_new, 
		       burst_rat_new, 
		       ifilename_new, 
		       sd_matrix_size, 
		       &sd_param_matrix_new, 
		       &spectra_errstatus, 
		       debug, 
		       MTstate);

    if (errstatus != 0){
        printf("errstatus %d, problem in doWork.\n",errstatus);
        return -1;
    }
		

    /************ BEGIN finalize PHASE *************/

    /* CLEAN UP */
    
    deallocate_bin_data(&ebin_lo, &ebin_hi, &ebin_mid, &ebin_del, &area, &ebin_lo_rmf, &ebin_hi_rmf,
			&emin, &emax, &ecen, &edelt);

    if (0 != strcasecmp("none",s_calfiles.intbackfile)){
	free(ibspec);
	free(ib_backscal);
    }
    
    if (s_mdb.instmap_flag == 1)
        free(s_mdb.array_imap);


    /* free all arrays from source file read */

    /* Clean up redshift data */
    free(redshifts);
    free(intabs);

    deallocate_source_data(nsource,  &ras, &decs, &colden, &spectype, &specpar, &fluxpar,
                           &band_lo, &band_hi, sfilename, &sformat, &sunits, &period, &pulse_fraction,
			   &tburst, &trise, &tdecay, &burst_rat, &burst_tabidx,
                           ifilename, sd_matrix_size, &sd_param_matrix);

    if (0 != strcasecmp("none",s_back.psbackfile))
	deallocate_source_data(nsource_psback,  &ras_psback, &decs_psback, &colden_psback, &spectype_psback, &specpar_psback,
			       &fluxpar_psback, &band_lo_psback, &band_hi_psback, sfilename_psback, &sformat_psback,
			       &sunits_psback, &period_psback, &pulse_fraction_psback, 
			       &tburst_psback, &trise_psback, &tdecay_psback, &burst_rat_psback, &burst_tabidx_psback,
			       ifilename_psback, sd_matrix_size, &sd_param_matrix_psback);

    if (0 != strcasecmp("none",s_back.difbackfile))
	deallocate_source_data(nsource_difback,  &ras_difback, &decs_difback, &colden_difback, &spectype_difback, &specpar_difback,
                               &fluxpar_difback, &band_lo_difback, &band_hi_difback, sfilename_difback, &sformat_difback,
                               &sunits_difback, &period_difback, &pulse_fraction_difback, 
			       &tburst_difback, &trise_difback, &tdecay_difback, &burst_rat_difback, &burst_tabidx_difback,
			       ifilename_difback, sd_matrix_size, &sd_param_matrix_difback);

    deallocate_source_data(nsource_new,  &ras_new, &decs_new, &colden_new, &spectype_new, &specpar_new,
			   &fluxpar_new, &band_lo_new, &band_hi_new, sfilename_new, &sformat_new,
			   &sunits_new, &period_new, &pulse_fraction_new, 
			   &tburst_new, &trise_new, &tdecay_new, &burst_rat_new, &burst_tabidx_new,
			   ifilename_new, sd_matrix_size, &sd_param_matrix_new);
    

    /* +++ 2016-08-26 RSH ***new */
    if (0 != strcasecmp(s_calfiles.rmffile, "none")) deallocate_rmf_data(&rmf_struct);
    deallocate_arf_data(&arf_struct);
    if (vigtype > 0) deallocate_vignette_data(&s_vig);
    if (psftype > 0) deallocate_psf_data(&s_psf, &s_psf_image);
    if (nburst > 0) deallocate_burst_data(&btab);

    HDmt_destroy_state(MTstate);

    if (spectra_errstatus != 0){
	printf("\nHeasim finished, but there were problems with the source spectra.\n");
	printf("    output file: %s\n\n",outfile);
    } else {
	printf("\nHeasim finished with no errors, output file: %s\n\n",outfile);
    }

    return(0);
}


/*
 ! $Log: heasim.c,v $
 ! Revision 1.90  2016/09/01 16:40:41  asargent
 ! Removed extraneous print statement for skiprmf
 !
 ! Revision 1.89  2016/08/26 20:38:41  rshill
 ! (1) Deleted skiprmf parameter (use rmffile=none instead)
 ! (2) inserted setting ebounds_TLMIN1, ebounds_TLMAX2, and nebin_rmf at main
 ! level for rmffile=none.
 ! (3) smaller corrections from comparison to TRF.
 !
 ! Revision 1.88  2016/08/25 22:07:38  rshill
 ! Implemented skiprmf=yes and rmffile=none.
 !
 ! Revision 1.87  2016/03/31 21:41:40  driethmi
 ! Impemented suite of changes to handle burst time assignment.  (There will
 ! likely be more bug fixes to this implementation soon.)
 !
 ! Revision 1.86  2016/03/25 14:48:46  driethmi
 ! Now only execute warn_if_detector_gap if skipfov is zero.
 !
 ! Revision 1.85  2016/03/23 19:12:13  driethmi
 ! Changed list of print statements reporting parameter values at start of code.
 !
 ! Revision 1.84  2015/12/29 18:15:51  driethmi
 ! Added ability to skip field of view restriction and/or RMF implementation.
 ! User sets parameters skipfov and/or skiprmf, respectively, to yes/no.
 !
 ! Revision 1.83  2015/11/12 18:07:42  driethmi
 ! If TLMIN1 is not found in background PHA file, then get it by reading the
 ! first CHANNEL column entry and assign it to ib_firstchan.
 !
 ! Revision 1.82  2015/11/09 14:44:07  driethmi
 ! Now check for first channel number in background file - usually 0, but
 ! use ib_firstchan to be safe.  If ib_firstchan != ebounds_TLMIN1, then
 ! exit with error.
 !
 ! Revision 1.81  2015/09/30 15:45:24  driethmi
 ! Modified read_internal_background to process XMM BACKSCAL values in order to
 ! match units that heasim expects.
 !
 ! Revision 1.80  2015/07/13 19:07:11  driethmi
 ! Changed startup message, no "*" characters.
 !
 ! Revision 1.79  2015/06/26 17:19:46  driethmi
 ! Fixed typo in print statement.
 !
 ! Revision 1.78  2015/06/26 16:51:06  driethmi
 ! Fixed typo in print statement reporting RNG seed.
 !
 ! Revision 1.77  2015/06/25 18:15:42  driethmi
 ! Put more print statements under "debug" mode only.  Also changed order of
 ! "final event count" statements at the end of doWork().  Report that we've removed
 ! bad PI events before we report final event count; no change in functionaly,
 ! but let's not confuse the user.
 !
 ! Revision 1.76  2015/06/16 20:34:40  driethmi
 ! Built in optimization for sub-exposure blocks.  By default, subexposure
 ! parameter is now hidden and very large.  If subexposure > exposure, then
 ! recompute the optimal subexposure size.
 !
 ! Revision 1.75  2015/05/27 18:48:30  driethmi
 ! More cosmetic changes to output.
 !
 ! Revision 1.74  2015/05/27 18:45:39  driethmi
 ! Minor cosmetic changes to terminal output.
 !
 ! Revision 1.73  2015/05/22 17:24:36  driethmi
 ! Updated parameter names throughout code, changed header keywords/comments
 ! to match standard values.  Changed input user seed so that seed=0 triggers
 ! seeding from the system time.
 !
 ! Revision 1.72  2015/05/11 14:02:15  driethmi
 ! Block to check HEADAS and HEASIM_SUPPORT env variabiles unnecessary, removed.
 !
 ! Revision 1.71  2015/05/06 17:07:08  driethmi
 ! Check for ENV variables before running code.
 !
 ! Revision 1.70  2015/05/04 16:16:33  driethmi
 ! Implemented better variable initializations.
 !
 ! Revision 1.69  2015/04/14 20:52:10  driethmi
 ! While looping over sources in doWork(), if there is ever a problem with the
 ! call to spectra() or torus_spectra(), we set a flag spectra_errstatus to
 ! -1, and pass this upwards.  Since spectra failures are non-cricital (meaning,
 ! we simply continue to the next source), this provides a way of reporting
 ! at the end that problems were encountered.
 !
 ! Revision 1.68  2015/04/07 15:47:49  driethmi
 ! Corrected non-critical compilations warnings; unused variables, etc.
 !
 ! Revision 1.67  2015/03/24 19:07:48  driethmi
 ! Removed instrument map from initial calfile reporting - at that point in the
 ! code, the instmap file name has not yet been read.
 !
 ! Revision 1.66  2015/03/24 18:00:08  driethmi
 ! Report the instrument map file along with calibration files.
 !
 ! Revision 1.65  2015/03/19 21:01:35  driethmi
 ! Removed unnecessary instance of resolve_pathname().
 !
 ! Revision 1.64  2015/03/19 20:35:30  driethmi
 ! Added burst capability, and updated function comment blocks.
 !
 ! Revision 1.63  2015/02/18 21:34:07  driethmi
 ! Corrected bug in process_image, needed to use fits_read_key_dbl instead of
 ! fits_read_key_flt - lack of precision was causing zeroes to be read.  Also
 ! improved verbosity of debug statements.
 !
 ! Revision 1.62  2015/02/18 20:15:18  driethmi
 ! Enabled application of redshift data to source simulation.
 !
 ! Revision 1.61  2015/02/18 18:15:01  driethmi
 ! Overwrote pulse bug fix - corrected and re-committed.
 !
 ! Revision 1.60  2015/02/18 15:53:41  driethmi
 ! Preliminary redshift changes, and cleaned up output chatter so debug = 1
 ! is more useful.
 !
 ! Revision 1.59  2015/01/29 19:53:31  driethmi
 ! Removed difspec_file_background parameter.
 !
 ! Revision 1.58  2015/01/29 00:49:57  driethmi
 ! Heasim now complains and exists if it can't find input source files or
 ! skyback files.
 !
 ! Revision 1.57  2015/01/28 21:32:20  driethmi
 ! Enabled torus spectral model for background point sources.
 !
 ! Revision 1.56  2015/01/21 17:25:02  driethmi
 ! Modified heasim to accept data files from sky background tool.
 !
 ! Revision 1.55  2014/12/02 19:57:40  driethmi
 ! Changed floats to doubles for consistency, except where float is required.
 !
 ! Revision 1.54  2014/10/02 18:07:31  driethmi
 ! Added routine to query most recent CVS commit date in all .c files that
 ! are built with simulator, and return the date string as the version number.
 !
 ! Revision 1.53  2014/09/05 19:34:36  driethmi
 ! Updated comment blocks before each function to reflect most recent versions.
 !
 ! Revision 1.52  2014/08/19 18:30:41  driethmi
 ! Added initial capability to read PSF images correctly.
 !
 ! Revision 1.51  2014/07/25 18:21:15  driethmi
 ! Previous change was not complete.
 !
 ! Revision 1.49  2014/07/25 14:54:28  driethmi
 ! Added line to report value of dtpileup along with other global sim info.
 !
 ! Revision 1.48  2014/07/25 14:41:54  driethmi
 ! Added capability to sort output fits file in time.
 !
 ! Revision 1.47  2014/07/01 13:47:16  driethmi
 ! Current state - added roll angle and image HDU capability.  Still some
 ! issues to work out with response file and background read.  Also, imagedis
 ! seems to have execution bottleneck.
 !
 ! Revision 1.46  2014/06/02 18:54:41  driethmi
 ! Added switch to use legacy C heasp library instead of active C++ library
 ! from heacore/heasp.
 !
 ! Revision 1.45  2014/05/28 23:38:25  driethmi
 ! Changed all fprintf(stderr,"") to printf("").  Log file does not catch
 ! print statements sent to stderr.
 !
 ! Revision 1.44  2014/05/23 22:51:24  driethmi
 ! Added routine that warns the user if sources are specified with RA,DEC
 ! that lie within the CCD detector gaps of the instrument being used.
 !
 ! Revision 1.43  2014/05/23 21:51:38  driethmi
 ! Changed random number generator such that MTstate is passed down through
 ! each function using it.  Now, user must specify seed.  If seed > 0, it is
 ! used to seed the RNG.  If seed <=0, we seed using the system time.
 !
 ! Revision 1.42  2014/05/13 15:57:22  driethmi
 ! Fixed typo - was copying s_back.ps_file_background to s_back.spec_file_background.
 !
 ! Revision 1.41  2014/04/29 21:11:38  driethmi
 ! Added function to remove lines from output fits file with PI channel less
 ! than or equal to zero.  Called during the "finalize" phase.
 !
 ! Revision 1.40  2014/04/09 14:44:05  driethmi
 ! Added routine to report actual nonzero energy sensitivity of RMF.  If,
 ! for example, spectral lines are given outside of this range, we can expect
 ! a null simulation result.
 !
 ! Revision 1.39  2014/04/03 20:41:06  driethmi
 ! Updated commenting and minor changes to attempt to comply more with coding
 ! standards.  Added doxygen tags.
 !
 ! Revision 1.38  2014/04/02 20:47:43  driethmi
 ! Finished psf and vignetting functions, moved read_vig and read_psf into
 ! initialize.c, moved apply_vig and apply_psf into doWork.c.  Removed
 ! vig.c and psf.c from repository.  Edited Makefile to not build
 ! vig.c and psf.c
 !
 ! Revision 1.37  2014/04/01 18:12:34  driethmi
 ! Added functionality to read psf files.
 !
 ! Revision 1.36  2014/03/28 22:49:01  driethmi
 ! Can now read all types of vignetting files: VIG_TEXT_1, VIG_FITS_1,
 ! VIG_FITS_2, VIG_IMAGE.  However, VIG_IMAGE is untested, since we have
 ! no vignette image files to test with.  This case may very well fail.
 !
 ! Revision 1.35  2014/03/26 22:15:55  driethmi
 ! Can now read FITS format vignette file, as long as it contains only one
 ! "VIGNETTE" data group, and is not an image file.
 !
 ! Revision 1.34  2014/03/26 18:11:38  driethmi
 ! Added capability to read an ascii vignette file.  Stores info to newly
 ! created vignette structure.  Cannot yet read fits vignette file, and does
 ! not yet apply vignetting to simulated data.
 !
 ! Revision 1.33  2014/03/19 14:14:39  driethmi
 ! Removed all instances of idum and random_seed, which are now obsolete.
 !
 ! Revision 1.32  2014/03/13 15:21:59  driethmi
 ! Simplified call to doWork() by elminating several passed parameters, and passing
 ! instead the structures containing those parameters.
 !
 ! Revision 1.31  2014/03/10 21:50:45  driethmi
 ! Moved some deallocation/free calls into finalize.c.
 !
 ! Also modified pointer levels for ifilename and sfilename - was not necessary
 ! to pass triple pointer.
 !
 ! Revision 1.30  2014/02/28 19:08:21  driethmi
 ! 1) Fixed many small memory leaks in C code; however, Valgrind still reports some
 ! "still reachable" blocks in C++ interface code.
 !
 ! 2) Added parentheses around (*ifilename) and (*sfilename) in doWork.c, that avoids
 ! a bus error.
 !
 ! 3) doWork() and spectra() now take a debug flag as an argument.
 !
 ! 4) doWork() now sorts the avenergy array, and then de_half and inspec are reordered
 ! to match, before populating binlow and binhigh.
 !
 ! Revision 1.29  2014/02/21 21:20:14  driethmi
 ! Minor changes, cleaning up comments, etc.
 !
 ! Revision 1.28  2014/02/21 15:54:42  driethmi
 ! Fixed bug to pass ebounds_TLMIN1 and TLMAX1 correctly.
 !
 ! Revision 1.27  2014/02/20 21:12:35  driethmi
 ! Fixed issues with the TLMIN5/TLMAX5 header keyword.  Now duplicates from the
 ! EBOUNDS TLMIN1/TLMAX1 keyword.
 !
 ! Revision 1.26  2014/02/20 18:30:48  driethmi
 ! Moved the doWork section in heasim.c main to its own function in doWork.c.
 ! Also moved C_ReturnChannel() to doWork.c and removed the ReturnChannel.c
 ! file from the repository.
 !
 ! Revision 1.25  2014/02/14 19:01:28  driethmi
 ! Added print statements to report the response file energy range.
 !
 ! Revision 1.24  2014/02/07 16:04:12  driethmi
 ! Several new algorithms, commenting.
 !
 ! Revision 1.22  2014/01/13 22:08:56  driethmi
 ! Minor changes for consistency - made all "return" values which signify
 ! a failure to -1.
 !
 ! Revision 1.21  2014/01/13 22:00:42  driethmi
 ! Made skycoords_per_arcsec_x and _y, removed other skycoord conversions.
 ! Code also exits on failure of ARF/RMF compatibility check.
 !
 ! Revision 1.20  2014/01/10 22:28:15  driethmi
 ! Changed some comments.
 !
 ! Revision 1.19  2014/01/09 16:12:32  driethmi
 ! Moved getPars(), read_mdb(), and check_calfiles_exist() to initialize.c
 !
 ! Revision 1.18  2014/01/08 21:33:20  driethmi
 ! Comment changes, ebounds_struct added to setup_output call.
 !
 ! Revision 1.17  2014/01/07 19:34:52  driethmi
 ! Moved intbackfile from GeneralBackground structure to
 ! CalFiles structure.
 !
 ! Revision 1.16  2014/01/07 16:54:40  driethmi
 ! Modified to remove EEF_energy_weights, add aim_detx, aim_dety to MDB
 ! parameter list.
 !
 ! Revision 1.15  2014/01/02 15:03:34  driethmi
 ! "Version" string needs to be 8 characters.  Maybe can change this later,
 ! but for now stick to 8.
 !
 ! Revision 1.14  2013/12/31 22:13:56  driethmi
 ! Added file "setup_output.c" and calls thereto; sets up the FITS output
 ! event file and populates some of the header info.  Closes the file when
 ! finished, which means that we'll need to open again when we're ready
 ! to write the final sim data to it.
 !
 ! Revision 1.13  2013/12/31 14:58:58  driethmi
 ! Cosmetic changes and re-arranging code blocks for efficiency.
 !
 ! Revision 1.12  2013/12/30 18:45:35  driethmi
 ! 1) Removed all old code.  Will construct remainder of simulator code entirely
 ! from TRF algorithms.  (Saved old code file as heasim_old.c, will keep in CVS
 ! repository until it can be discarded.)
 !
 ! 2) Some functions required minor adjustments, mostly with variable names,
 ! to be more consistent with TRF.
 !
 ! 3) Added comments defining every single variable definition, and a comment
 ! block before every function.
 !
 ! Revision 1.11  2013/12/18 23:05:32  driethmi
 ! Added some minor helpful comments - in which files new functions may be found,
 ! marks delineating sections of heasim.c.
 !
 ! Revision 1.10  2013/12/18 20:57:01  driethmi
 ! Added new routine to read source data from input file.  As of yet, nothing
 ! is actually *done* with the input data, just read.  May still be some bugs,
 ! haven't tested extensively.
 !
 ! Revision 1.9  2013/12/12 19:38:58  driethmi
 ! Moved allocation statements for HEASP-related arrays out of main and
 ! into relevant subroutines.  Remember to deallocate at the end of main.
 ! Not sure if this is correct programming practice, but it makes for much
 ! cleaner main function.
 !
 ! Revision 1.8  2013/12/11 21:28:44  driethmi
 ! Added HEASP functionality to read fits files.  Now read ARF, RMF, ebounds,
 ! and internal_background fits files to structures using heasp library.
 ! Still some uncertainty as to which heasp library to use (legacy C
 ! or CVS-controlled C++ ?)
 !
 ! Revision 1.7  2013/11/27 18:51:17  driethmi
 ! Packed mission, instrume, filter, instmode, ra, dec, roll, and expose
 ! into a structure.  Should make passing into functions easier.
 !
 ! Revision 1.6  2013/11/27 18:48:32  driethmi
 ! *** empty log message ***
 !
 ! Revision 1.5  2013/11/26 21:08:04  driethmi
 ! Tailored mission database (mdbfile) to contain the parameters required
 ! in the simulator TRF.  All of these parameters are read in; some of
 ! them are not yet used for anything.
 !
 ! Revision 1.4  2013/11/26 16:05:21  driethmi
 ! We now read mission-specific information from a mission database file,
 ! rather than an extended block of if-then statements with redundant code.
 ! mdbfile is now one of the parameters read in via getPars.
 !
 ! Revision 1.3  2013/11/26 14:51:24  driethmi
 ! Removed sample_input_params from the repository.  File is now unnecessary,
 ! now that APE inputs are reinstated.
 !
 ! Revision 1.2  2013/11/25 22:21:09  driethmi
 ! "APE" functionality complete.  Also renamed files from "heasimc" to "heasim" base.
 !
 ! Revision 1.32  2013/11/25 22:17:15  driethmi
 ! Restored "APE" command functionality for parameter input.
 !
 ! Revision 1.31  2013/11/19 16:42:12  driethmi
 ! Added comments and calling sequence descriptions to new routines.  Removed
 ! some debugging print statements.
 !
 ! The system calls such as system("rm quicksim*") and system("rm output/*")
 ! serve just to clean up the previous output.  The user may remove or comment
 ! out these lines as appropriate.
 !
 ! Revision 1.30  2013/11/11 16:42:25  driethmi
 ! Merged quicksim_function.c and heasimc.c into one single file, called
 ! heasimc.c.  The resulting function takes an ASCII file with all
 ! relevent input parameters as an argument to main() at runtime.
 !
 */
