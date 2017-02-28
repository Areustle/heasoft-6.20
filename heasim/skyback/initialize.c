#include "fitsio.h"
#include "skyback.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <ctype.h>
#include <math.h>
#include <stdbool.h>


/* FUNCTION NAME: getPars                                                                                */
/*                                                                                                       */
/* CALLING SEQUENCE:                                                                                     */
/*     errstatus = getPars(argc, argv, &s_files, &s_obs, &s_engrid, &s_flags, &s_logNS, &s_flux,         */
/*		    &debug, &clobber, mode, &seed);                                                */
/*                                                                                                       */
/* PURPOSE: read in the sky background parameters from skyback.par                                       */
/*                                                                                                       */
/* INPUTS: argc, argv                                                                                    */
/*                                                                                                       */
/* OUTPUTS: infile, outfile, par_mode, debug, clobber, get_sample_infile s_calfiles,                     */
/*             s_back, s_obs, mdbfile                                                                    */
/*                                                                                                       */
/* EXTERNAL DEPENDENCIES                                                                                 */
/*   ape libraries                                                                                       */
/*                                                                                                       */
/* CALLED BY                                                                                             */
/*   main()                                                                                              */
/*                                                                                                       */

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
	    long * seed){


    /* READ AS BOOLEANS - we will return as ints */
    char bool_flaglogns;
    char bool_flaggal;
    char bool_flagswcx;
    char bool_flagdgrb;
    char bool_flaggrxe;
    char bool_samespec;
    char bool_debug;
    char bool_clobber;

    /* internal variables */
    int status=eOK;      /* status tracker */
    int num_vars;     /* number of parameters we expect to find */
    int * status_array;  /* array of flags indicating whether we found that parameter */
    int ii=0;
    char *temp;
    double abs_total=0., par_total=0.;

    /* Make a list of parameters to be read.  Truncate list with NULL. Make sure params are read in same order below. */
    char * param_list[] = {"outfileroot",
			   "exposure",
			   "ra",
			   "dec",
			   "radius",
			   "emin",
			   "emax",
			   "de",
			   "flaglogns",
			   "flaggal",
			   "flagswcx",
			   "flagdgrb",
			   "flaggrxe",
			   "slopebright1",
			   "slopefaint1",
			   "fluxbreak",
			   "norm1",
			   "slope2",
			   "norm2",
			   "fluxsens",
			   "sigtonoise",
			   "fluxmin",
			   "fluxmax",
			   "ctstoflux",
			   "bandpasslo",
			   "bandpasshi",
			   "spectype1",
			   "specmod1",
			   "specpar1",
			   "nhmod1",
			   "fabs0",
			   "fabs1",
			   "fabs2",
                           "fabs3",
			   "fabs4",
			   "fabs5",
			   "fpar0",
                           "fpar1",
                           "fpar2",
                           "fpar3",
                           "fpar4",
			   "samespec",
			   "specmod2",
			   "specpar2",
			   "nhmod2",
			   "swcxOVII",
                           "swcxcont",
			   "seed",
			   "clobber",
			   "debug",
			   "mode",
			   NULL};

    /* Get the number of non-NULL variables in the param list */
    for (num_vars = 0; param_list[num_vars] != NULL; num_vars++);

    status_array = calloc(num_vars, sizeof(int)); /* allocate and initialize all flags to zero */

    /* Initialize APE and begin reading. */
    status = ape_trad_init(argc, argv);
    if (eOK != status) {
        fprintf(stderr,"skyback: ape_trad_init failed\n");
        fprintf(stderr,"skyback: check PFILES paths for skyback.par\n");
        return -1;
    }

    ii=0;

    /* APE populate s_files structure.  String values must be strcpy'ed and freed. */
    status_array[ii++] = ape_trad_query_string("outfileroot",&temp);

    /* Append the appropriate prefix to the outfileroot string */
    sprintf(s_files->psource_fileroot,"%s%s","pscat_",temp);
    sprintf(s_files->difsource_fileroot,"%s%s","difcat_",temp);
    sprintf(s_files->difspec_fileroot,"%s%s","difspec_",temp);
    sprintf(s_files->zsource_fileroot,"%s%s","pszcat_",temp);
    free(temp);

    /* APE populate s_obs structure. */
    status_array[ii++] = ape_trad_query_double("exposure", &s_obs->exposure);
    status_array[ii++] = ape_trad_query_double("ra", &s_obs->ra);
    status_array[ii++] = ape_trad_query_double("dec", &s_obs->dec);
    status_array[ii++] = ape_trad_query_double("radius", &s_obs->radius);

    /* APE populate s_engrid structure. */
    status_array[ii++] = ape_trad_query_double("emin", &s_engrid->emin);
    status_array[ii++] = ape_trad_query_double("emax", &s_engrid->emax);
    status_array[ii++] = ape_trad_query_double("de", &s_engrid->de);

    /* APE populate s_flags structure.  Convert booleans to int. */
    status_array[ii++] = ape_trad_query_bool("flaglogns", &bool_flaglogns);
    if (1 == bool_flaglogns){ s_flags->flaglogns = 1; } else { s_flags->flaglogns = 0;}
    
    status_array[ii++] = ape_trad_query_bool("flaggal", &bool_flaggal);
    if (1 == bool_flaggal){ s_flags->flaggal = 1; } else { s_flags->flaggal = 0;}

    status_array[ii++] = ape_trad_query_bool("flagswcx", &bool_flagswcx);
    if (1 == bool_flagswcx){ s_flags->flagswcx = 1; } else { s_flags->flagswcx = 0;}

    status_array[ii++] = ape_trad_query_bool("flagdgrb", &bool_flagdgrb);
    if (1 == bool_flagdgrb){ s_flags->flagdgrb = 1; } else { s_flags->flagdgrb = 0;}

    status_array[ii++] = ape_trad_query_bool("flaggrxe", &bool_flaggrxe);
    if (1 == bool_flaggrxe){ s_flags->flaggrxe = 1; } else { s_flags->flaggrxe = 0;}

    if (s_flags->flaglogns){
	/* APE populate s_logNS structure */
	status_array[ii++] = ape_trad_query_double("slopebright1", &s_logNS->slopebright1);
	status_array[ii++] = ape_trad_query_double("slopefaint1", &s_logNS->slopefaint1);
	status_array[ii++] = ape_trad_query_double("fluxbreak", &s_logNS->fluxbreak);
	status_array[ii++] = ape_trad_query_double("norm1", &s_logNS->norm1);
	status_array[ii++] = ape_trad_query_double("slope2", &s_logNS->slope2);
	status_array[ii++] = ape_trad_query_double("norm2", &s_logNS->norm2);
	status_array[ii++] = ape_trad_query_double("fluxsens", &s_logNS->fluxsens);
	status_array[ii++] = ape_trad_query_double("sigtonoise", &s_logNS->sigtonoise);
	status_array[ii++] = ape_trad_query_double("fluxmin", &s_logNS->fluxmin);
	status_array[ii++] = ape_trad_query_double("fluxmax", &s_logNS->fluxmax);
	status_array[ii++] = ape_trad_query_double("ctstoflux", &s_logNS->ctstoflux);
	status_array[ii++] = ape_trad_query_double("bandpasslo", &s_logNS->bandpasslo);
	status_array[ii++] = ape_trad_query_double("bandpasshi", &s_logNS->bandpasshi);
	status_array[ii++] = ape_trad_query_int("spectype1", &s_logNS->spectype1);
	
	status_array[ii++] = ape_trad_query_string("specmod1", &temp);
	strcpy(s_logNS->specmod1,temp);
	free(temp);
	
	status_array[ii++] = ape_trad_query_double("specpar1", &s_logNS->specpar1);
	status_array[ii++] = ape_trad_query_double("nhmod1", &s_logNS->nhmod1);
	status_array[ii++] = ape_trad_query_double("fabs0", &s_logNS->fabs0);
	status_array[ii++] = ape_trad_query_double("fabs1", &s_logNS->fabs1);
	status_array[ii++] = ape_trad_query_double("fabs2", &s_logNS->fabs2);
	status_array[ii++] = ape_trad_query_double("fabs3", &s_logNS->fabs3);
	status_array[ii++] = ape_trad_query_double("fabs4", &s_logNS->fabs4);
	status_array[ii++] = ape_trad_query_double("fabs5", &s_logNS->fabs5);
	status_array[ii++] = ape_trad_query_double("fpar0", &s_logNS->fpar0);
	status_array[ii++] = ape_trad_query_double("fpar1", &s_logNS->fpar1);
	status_array[ii++] = ape_trad_query_double("fpar2", &s_logNS->fpar2);
	status_array[ii++] = ape_trad_query_double("fpar3", &s_logNS->fpar3);
	status_array[ii++] = ape_trad_query_double("fpar4", &s_logNS->fpar4);
	
	status_array[ii++] = ape_trad_query_bool("samespec", &bool_samespec);
	if (1 == bool_samespec){ s_logNS->samespec = 1; } else { s_logNS->samespec = 0;}
	
	status_array[ii++] = ape_trad_query_string("specmod2", &temp);
	strcpy(s_logNS->specmod2,temp);
	free(temp);
	
	status_array[ii++] = ape_trad_query_double("specpar2", &s_logNS->specpar2);
	status_array[ii++] = ape_trad_query_double("nhmod2", &s_logNS-> nhmod2);

    } else {
	/* If the logns flag is not set, populate the 32 s_logNS status variables with eOK. */
	for (int jj=0; jj<32; jj++)  status_array[ii++] = eOK;
    }


    if (s_flags->flagswcx){
	/* APE populate s_flux structure */
	status_array[ii++] = ape_trad_query_double("swcxOVII", &s_flux->swcxOVII);
	status_array[ii++] = ape_trad_query_double("swcxcont", &s_flux->swcxcont);

    } else {
	/* If the swcx flag is not set, populate both s_flux status variables with eOK. */
	for (int jj=0; jj<2; jj++)  status_array[ii++] = eOK;
    }

    /* APE populate loose variables */
    status_array[ii++] = ape_trad_query_long("seed", seed);

    status_array[ii++] = ape_trad_query_bool("clobber",&bool_clobber);
    if (1 == bool_clobber){ *clobber = 1; } else { *clobber = 0;}

    status_array[ii++] = ape_trad_query_bool("debug",&bool_debug);
    if (1 == bool_debug){ *debug = 1; } else { *debug = 0;}

    status_array[ii++] = ape_trad_query_string("mode",&temp);
    strcpy(mode,temp);
    free(temp);


    /* Check that we've read everything */
    for (ii=0; ii<num_vars; ii++)
        if (status_array[ii] != eOK){
            fprintf(stderr,"problem with entry number %d: %s\n",ii,param_list[ii]);
            status=-1;
        }
    free(status_array);


    /********** Do some validation checks: ***********/

    if ( (s_flags->flaglogns == 0) &&
	 (s_flags->flaggal == 0) &&
	 (s_flags->flagswcx == 0) &&
	 (s_flags->flagdgrb == 0) &&
	 (s_flags->flaggrxe == 0) &&
	 ( (*debug) == 0) ){  /* If debug is set, this warning appears in main(). */
	printf("WARNING: All flags set to 0, sky background will have NO source contributions.\n");
    }

    if (s_engrid->emin >= s_engrid->emax){
	printf("Error: emin=%e must be less than emax=%e.\n",s_engrid->emin, s_engrid->emax);
	status = -1;
    }

    if ( (0 > s_obs->ra) || (s_obs->ra > 360) ){
	printf("Error: ra out of bounds (0 <= ra <= 360): %e\n",s_obs->ra);
	status = -1;
    }

    if ( (-90 > s_obs->dec) || (s_obs->dec > 90) ){
        printf("Error: dec out of bounds (-90 <= dec <= 90): %e\n",s_obs->dec);
        status = -1;
    }

    if (s_flags->flaglogns){
	/* These validation checks are only necessary if we're doing point sources. */
	
	if (s_logNS->bandpasslo >= s_logNS->bandpasshi){
	    printf("Error: bandpasslo=%e must be less than bandpasshi=%e\n",s_logNS->bandpasslo,s_logNS->bandpasshi);
	    status = -1;
	}
	
	
	if (s_logNS->nhmod1 < 0){
	    printf("Error: nhmod1 must be >= 0: %e\n",s_logNS->nhmod1);
	    status = -1;
	}
	
	if (s_logNS->nhmod2 < 0){
	    printf("Error: nhmod2 must be >= 0: %e\n",s_logNS->nhmod2);
	    status = -1;
	}
	
	if ( (s_logNS->fluxmin >= s_logNS->fluxbreak) || (s_logNS->fluxbreak >= s_logNS->fluxmax) ){
	    printf("Error: need (fluxmin <= fluxbreak <= fluxmax): %e, %e, %e\n", s_logNS->fluxmin, s_logNS->fluxbreak, s_logNS->fluxmax);
	    status = -1;
	}
	
	if ( (s_logNS->spectype1 < 0) || (s_logNS->spectype1 > 2) ){
	    printf("Error: need (0 <= spectype1 <= 2): %d\n",s_logNS->spectype1);
	    status = -1;
	}
	
	abs_total = s_logNS->fabs0 + s_logNS->fabs1 + s_logNS->fabs2 + s_logNS->fabs3 + s_logNS->fabs4 + s_logNS->fabs5;
	if (abs_total != 1.0){
	    printf("Error: Absorption fractions must total 1.0:\n");
	    printf("  fabs0 = %e\n",s_logNS->fabs0);
	    printf("  fabs1 = %e\n",s_logNS->fabs1);
	    printf("  fabs2 = %e\n",s_logNS->fabs2);
	    printf("  fabs3 = %e\n",s_logNS->fabs3);
	    printf("  fabs4 = %e\n",s_logNS->fabs4);
	    printf("  fabs5 = %e\n",s_logNS->fabs5);
	    printf("  TOTAL  = %e\n",abs_total);
	    status = -1;
	}
	
	par_total = s_logNS->fpar0 + s_logNS->fpar1 + s_logNS->fpar2 + s_logNS->fpar3 + s_logNS->fpar4;
	if (par_total != 1.0){
	    printf("Error: Parameter fractions must total 1.0:\n");
	    printf("  fpar0 = %e\n",s_logNS->fpar0);
	    printf("  fpar1 = %e\n",s_logNS->fpar1);
	    printf("  fpar2 = %e\n",s_logNS->fpar2);
	    printf("  fpar3 = %e\n",s_logNS->fpar3);
	    printf("  fpar4 = %e\n",s_logNS->fpar4);
	    printf("  TOTAL  = %e\n",par_total);
	    status = -1;
	}
    }
    
    /* Use the root filenames to set the full filenames */
    sprintf(s_files->difspec_fitsfile,"%s.fits",s_files->difspec_fileroot);
    sprintf(s_files->difspec_datfile,"%s.dat",s_files->difspec_fileroot);
    sprintf(s_files->psource_txtfile,"%s.txt",s_files->psource_fileroot);
    sprintf(s_files->difsource_txtfile,"%s.txt",s_files->difsource_fileroot);
    sprintf(s_files->zsource_txtfile,"%s.txt",s_files->zsource_fileroot);

    return status;
}


/* FUNCTION NAME: initialize_structs                                           */
/*                                                                             */
/* PURPOSE:                                                                    */
/*    Assign values to all members of structures (with exception               */
/*    of not-yet-allocated arrays), such that Valgrind won't complain          */
/*    about uninitialized values.                                              */
/*                                                                             */
/* CALLING SEQUENCE:                                                           */
/*    initialize_structs(&s_obs, &s_engrid, &s_flags, &s_logNS, &s_flux);      */
/*                                                                             */
/* INPUTS: &s_obs, &s_cal, &s_back, &s_mdb, &s_vig, &s_psf                     */
/*                                                                             */
/* OUTPUTS: none                                                               */
/*                                                                             */
/* CALLED BY:  main()                                                          */
/*                                                                             */

void initialize_structs(Obs_params_struct * s_obs,
                        Engrid_struct * s_engrid,
                        Flag_struct * s_flags,
                        logN_logS_struct * s_logNS,
                        Flux_struct * s_flux){

    s_obs->exposure = 0.0; /* exposure time in sec */
    s_obs->ra = 0.0;
    s_obs->dec = 0.0;
    s_obs->radius = 0.0;
    s_obs->nh_gal = 0.0;  /* nh_gal is set near the beginning of doWork() */

    s_engrid->nebin = 0;
    s_engrid->emin = 0.0;
    s_engrid->emax = 0.0;
    s_engrid->de = 0.0;
    s_engrid->energ_lo = 0;
    s_engrid->energ_hi = 0;
    s_engrid->energ_mid = 0;
    s_engrid->xspec_energy = 0;

    s_flags->flaglogns = 0; /* Include emission from log N - log S? */
    s_flags->flaggal = 0; /* Include soft X-ray Galactic and LHB emission? */
    s_flags->flagswcx = 0; /* Include SWCX emission? */
    s_flags->flagdgrb = 0; /* Include diffuse gamma-ray emission? */
    s_flags->flaggrxe = 0; /* Include galactic ridge emission? */

    s_logNS->slopebright1 = 0.0; /* LogN-LogS slope 1 -- bright end */
    s_logNS->slopefaint1 = 0.0; /* LogN-LogS slope 1 -- faint end */
    s_logNS->fluxbreak = 0.0; /* LogN-LogS flux at power-law break */
    s_logNS->norm1 = 0.0; /* LogN-LogS normalization in sources/sq-degree for slope 1 */
    s_logNS->slope2 = 0.0; /* LogN-LogS slope 2 */
    s_logNS->norm2 = 0.0; /* LogN-LogS normalization in sources/sq-degree for slope 2 */
    s_logNS->fluxsens = 0.0; /* LogN-LogS flux sensitivity limit */
    s_logNS->sigtonoise = 0.0; /* LogN-LogS signal-to-noise corresponding to flux limit */
    s_logNS->fluxmin = 0.0; /* lower flux limit of logns in erg/cm2/sec -- both components */
    s_logNS->fluxmax = 0.0; /* upper flux limit of logns in erg/cm2/sec -- both components */
    s_logNS->slo = 0.0;  /* detection limit for logN-logS */
    s_logNS->ctstoflux = 0.0; /* count-rate to flux conversion in erg/cm2/se/ct */
    s_logNS->bandpasslo = 0.0; /* lower limit  in keV for bandpass over which log N - log S is defined */
    s_logNS->bandpasshi = 0.0; /* upper limit in keV for bandpass over which log N - log S is defined */
    s_logNS->spectype1 = 0; /* spectral type for slope-1 source: 0=single spectrum, 1=multi, 2=torus */
    s_logNS->specpar1 = 0.0; /* slope-1 sources spectral parameter if spectype1=0 */
    s_logNS->nhmod1 = 0.0; /* slope-1 sources spectral model NH if spectype1=0 */
    s_logNS->fabs0 = 0.0; /* fraction of slope-1 sources, NH<1e21 cm-2 */
    s_logNS->fabs1 = 0.0; /* fraction of slope-1 sources, NH=1e21-1e22 cm-2 */
    s_logNS->fabs2 = 0.0; /* fraction of slope-1 sources, NH=1e22-1e23 cm-2 */
    s_logNS->fabs3 = 0.0; /* fraction of slope-1 sources, NH=1e23-1e24 cm-2 */
    s_logNS->fabs4 = 0.0; /* fraction of slope-1 sources, NH=1e24-1e25 cm-2 */
    s_logNS->fabs5 = 0.0; /* fraction of slope-1 sources, NH>1e25 cm-2 */
    s_logNS->fpar0 = 0.0; /* fraction index=1.5-1.7 if spectype1=1, opening angle <30 if spectype1=2 */
    s_logNS->fpar1 = 0.0; /* fraction index=1.7-1.9 if spectype1=1, opening angle 30-45 if spectype1=2 */
    s_logNS->fpar2 = 0.0; /* fraction index=1.9-2.1 if spectype1=1, opening angle 45-60 if spectype1=2 */
    s_logNS->fpar3 = 0.0; /* fraction index=2.1-2.3 if spectype1=1, opening angle 60-75 if spectype1=2 */
    s_logNS->fpar4 = 0.0; /* fraction index=2.3-2.5 if spectype1=1, opening angle 75-90 if spectype1=2 */
    s_logNS->samespec = 0; /* Do "slope 1" and "slope 2" sources have same spectra? */
    s_logNS->specpar2 = 0.0; /* heasim-supported spectral model parameter; ignored if samespec==yes */
    s_logNS->nhmod2 = 0.0; /* absorption; ignored if samespec==yes */

    s_flux->swcxOVII = 0.0;
    s_flux->swcxcont = 0.0;


}



/*
 ! $Log: initialize.c,v $
 ! Revision 1.11  2015/05/19 17:11:19  driethmi
 ! Made some of the param querying conditional; if the logns flag is not set,
 ! then we don't bother to read the 32 params attached to the logns struct.
 ! Similarly for the swcx flag.
 !
*/


