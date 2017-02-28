/**
 * \file skyback.c
 * \brief Compute sky background - main file
 * \author David Riethmiller
 * \date $Date: 2015/07/13 19:02:21 $
 *
 */

#include "skyback.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "fitsio.h"
#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>


int main(int argc, char *argv[]){

    /* VARIABLE DECLARATIONS */

    Filenames_struct s_files;
    Obs_params_struct s_obs;
    Engrid_struct s_engrid;
    Flag_struct s_flags;
    logN_logS_struct s_logNS;
    Flux_struct s_flux;
    int debug = 0;
    int clobber = 0;
    char mode[STR_MAX_LEN];
    long seed = 0;
    HDmt_state * MTstate = 0;   /* "save" state of Mersenne Twister RNG */
    unsigned long int RNGseed=0;  /* seed for RNG */
    char version[STR_MAX_LEN];
    int errstatus = 0;
    int ii = 0;
    double sthresh = 0.0;
    double cr_thresh = 0.0;

    get_version(version);

    printf("\n");
    printf("  Running skyback, v. %s\n",version);
    printf("\n");

    /* INITIALIZE STAGE */

    /* 0: Initialize all non-array structure variables to zero. */
    initialize_structs(&s_obs, &s_engrid, &s_flags, &s_logNS, &s_flux);

    /* 1: Open and read parameter file, populate structs */
    errstatus = getPars(argc, argv, &s_files, &s_obs, &s_engrid, &s_flags, &s_logNS, &s_flux,
			&debug, &clobber, mode, &seed);

    if (errstatus != 0){
	printf("Error: problem reading or validating parameters.\n");
	return -1;
    }

    if (debug == 1){
	/* Print out the parameters */
	printf("\nParameters:\n");
	printf("  s_files->difspec_fitsfile:   %s\n",s_files.difspec_fitsfile);
	printf("  s_files->difspec_datfile:    %s\n",s_files.difspec_datfile);
	printf("  s_files->psource_txtfile:    %s\n",s_files.psource_txtfile);
	printf("  s_files->difsource_txtfile:  %s\n",s_files.difsource_txtfile);
	printf("  s_files->zsource_txtfile:    %s\n",s_files.zsource_txtfile);

	printf("\n");
	printf("  s_obs->exposure:             %e\n",s_obs.exposure);
	printf("  s_obs->ra:                   %e\n",s_obs.ra);
	printf("  s_obs->dec:                  %e\n",s_obs.dec);
	printf("  s_obs->radius:               %e\n",s_obs.radius);
	printf("\n");
	printf("  s_engrid->emin:              %e\n",s_engrid.emin);
	printf("  s_engrid->emax:              %e\n",s_engrid.emax);
	printf("  s_engrid->de:                %e\n",s_engrid.de);
	printf("\n");
	printf("  s_flags->flaglogns:          %d\n",s_flags.flaglogns);
	printf("  s_flags->flaggal:            %d\n",s_flags.flaggal);
	printf("  s_flags->flagswcx:           %d\n",s_flags.flagswcx);
	printf("  s_flags->flagdgrb:           %d\n",s_flags.flagdgrb);
	printf("  s_flags->flaggrxe:           %d\n",s_flags.flaggrxe);

	if ( (s_flags.flaglogns == 0) &&
	     (s_flags.flaggal == 0) &&
	     (s_flags.flagswcx == 0) &&
	     (s_flags.flagdgrb == 0) &&
	     (s_flags.flaggrxe == 0) ){
	    printf("  WARNING: All flags set to 0, sky background will have NO source contributions.\n");
	}

	if (s_flags.flaglogns){
	    printf("\n");
	    printf("  s_logNS->slopebright1:       %e\n",s_logNS.slopebright1);
	    printf("  s_logNS->slopefaint1:        %e\n",s_logNS.slopefaint1);
	    printf("  s_logNS->fluxbreak:          %e\n",s_logNS.fluxbreak);
	    printf("  s_logNS->norm1:              %e\n",s_logNS.norm1);
	    printf("  s_logNS->slope2:             %e\n",s_logNS.slope2);
	    printf("  s_logNS->norm2:              %e\n",s_logNS.norm2);
	    printf("  s_logNS->fluxsens:           %e\n",s_logNS.fluxsens);
	    printf("  s_logNS->sigtonoise:         %e\n",s_logNS.sigtonoise);
	    printf("  s_logNS->fluxmin:            %e\n",s_logNS.fluxmin);
	    printf("  s_logNS->fluxmax:            %e\n",s_logNS.fluxmax);
	    printf("  s_logNS->ctstoflux:          %e\n",s_logNS.ctstoflux);
	    printf("  s_logNS->bandpasslo:         %e\n",s_logNS.bandpasslo);
	    printf("  s_logNS->bandpasshi:         %e\n",s_logNS.bandpasshi);
	    printf("  s_logNS->spectype1:          %d\n",s_logNS.spectype1);
	    printf("  s_logNS->specmod1:           %s\n",s_logNS.specmod1);
	    printf("  s_logNS->specpar1:           %e\n",s_logNS.specpar1);
	    printf("  s_logNS->nhmod1              %e\n",s_logNS.nhmod1);
	    printf("  s_logNS->fabs0:              %e\n",s_logNS.fabs0);
	    printf("  s_logNS->fabs1:              %e\n",s_logNS.fabs1);
	    printf("  s_logNS->fabs2:              %e\n",s_logNS.fabs2);
	    printf("  s_logNS->fabs3:              %e\n",s_logNS.fabs3);
	    printf("  s_logNS->fabs4:              %e\n",s_logNS.fabs4);
	    printf("  s_logNS->fabs5:              %e\n",s_logNS.fabs5);
	    printf("  s_logNS->fpar0:              %e\n",s_logNS.fpar0);
	    printf("  s_logNS->fpar1:              %e\n",s_logNS.fpar1);
	    printf("  s_logNS->fpar2:              %e\n",s_logNS.fpar2);
	    printf("  s_logNS->fpar3:              %e\n",s_logNS.fpar3);
	    printf("  s_logNS->fpar4:              %e\n",s_logNS.fpar4);
	    printf("  s_logNS->samespec:           %d\n",s_logNS.samespec);
	    printf("  s_logNS->specmod2:           %s\n",s_logNS.specmod2);
	    printf("  s_logNS->specpar2:           %e\n",s_logNS.specpar2);
	    printf("  s_logNS->nhmod2              %e\n",s_logNS.nhmod2);
	}

	if (s_flags.flagswcx){
	    printf("\n");
	    printf("  s_flux->swcxOVII:        %e\n",s_flux.swcxOVII);
	    printf("  s_flux->swcxcont:        %e\n",s_flux.swcxcont);
	}

	printf("\n");
	printf("  debug:         %d\n",debug);
	printf("  clobber:       %d\n",clobber);
	printf("  mode:          %s\n",mode);
	printf("  seed:          %ld\n",seed);
	printf("\n");
    }

    /* 2: Set up the random number generator state. */
    if (seed != 0){
        printf("\nUsing user input seed = %ld\n\n",seed);
        RNGseed = (unsigned long int)seed;
    } else{
        printf("\nUser input seed is zero, seeding with system time.\n\n");
        RNGseed = MTseed();
    }
    MTstate = HDmt_srand(RNGseed);

    /* 3: Generate energy grid for spectra, populate arrays in s_engrid struct. */
    s_engrid.nebin = floor( (s_engrid.emax - s_engrid.emin)/s_engrid.de) + 1;
    s_engrid.energ_hi = calloc(s_engrid.nebin, sizeof(double));
    s_engrid.energ_lo = calloc(s_engrid.nebin, sizeof(double));
    s_engrid.energ_mid = calloc(s_engrid.nebin, sizeof(double));
    s_engrid.xspec_energy = calloc(s_engrid.nebin+1, sizeof(double));
    for(ii=0; ii<s_engrid.nebin; ii++){
	s_engrid.energ_lo[ii] = s_engrid.emin + (double)ii * s_engrid.de;
	s_engrid.energ_hi[ii] = s_engrid.energ_lo[ii] + s_engrid.de;
	s_engrid.energ_mid[ii] = 0.5 * (s_engrid.energ_lo[ii] + s_engrid.energ_hi[ii]);
	s_engrid.xspec_energy[ii] = s_engrid.energ_lo[ii];
    }
    /* set the last array value, since xspec_energy has one extra */
    s_engrid.xspec_energy[s_engrid.nebin] = s_engrid.energ_hi[s_engrid.nebin-1];



    /* 4: Prepare for logN - logS */
    if (s_flags.flaglogns == 1){
	/* minimum detectable flux: if none is given, assume sn threshold and calculate corresponding count rate and flux */
	if (s_logNS.fluxsens != 0){
	    sthresh = s_logNS.fluxsens;
	} else {
	    cr_thresh = s_logNS.sigtonoise * s_logNS.sigtonoise/s_obs.exposure;
	    sthresh = s_logNS.ctstoflux * cr_thresh;
	}
	/* "detection limit" of logN - logS */
	s_logNS.slo = dblmax(s_logNS.fluxmin,sthresh);
    }

    /* END INITIALIZE STAGE */
	
    

    

    /* doWork(); */
    errstatus = doWork(&s_files, &s_obs, &s_engrid, &s_flags, &s_logNS, &s_flux,
		       debug, clobber, MTstate);


    /* finalize(); */
    deallocate_energ_data(&s_engrid);
    HDmt_destroy_state(MTstate);


    printf("\nSky Background computation complete.\n");
    printf("Primary output FITS file: %s\n",s_files.difspec_fitsfile);
    printf("See also:\n   %s (ASCII energy vs. spectra)\n   %s (ASCII heasim point source input)\n   %s (ASCII heasim diffuse source input)\n\n",
	   s_files.difspec_datfile,
	   s_files.psource_txtfile,
	   s_files.difsource_txtfile);

    return 0;
}




/*
 ! $Log: skyback.c,v $
 ! Revision 1.16  2015/07/13 19:02:21  driethmi
 ! Changed startup message, removed "*" characters from banner.
 !
 ! Revision 1.15  2015/06/30 14:20:26  driethmi
 ! Replaced "welcome" print statement with one more like the simulator's.
 ! Reports the version of skyback.
 !
 ! Revision 1.14  2015/05/19 17:09:49  driethmi
 ! Cosmetic changes to output format, also changed RNG seed input functionality;
 ! now seed=0 triggers seeding from system time.
 !

*/
