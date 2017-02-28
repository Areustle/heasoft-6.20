/**
   \file doWork.c
    \brief HEADAS Simulator - contains routines needed for "doWork" section
    \author  David Riethmiller
    \date $Date: 2016/08/26 20:38:41 $
*/


#include "heasim.h"
#ifndef use_legacy_heasp
#include "Cheasp.h"       /* C-interface to C++ HEASP library */
#endif

#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "fitsio.h"       /* cfitsio defined constants */
#include <stdlib.h>
#include <stdio.h>
#include <funcWrappers.h>  /* Xspec defs, funcWrappers.cxx */
#include <math.h>
#include <ctype.h>
#include <sys/time.h>
#include <string.h>
#include <unistd.h>

/* FUNCTION NAME: doWork                                                                                                                     */
/*                                                                                                                                           */
/* CALLING SEQUENCE:                                                                                                                         */
/*    errstatus = doWork(outfile, &s_obs, &s_calfiles, &s_mdb, &s_vig, &s_psf, &s_psf_image, &s_back, &rmf_struct,                   */
/*		         nebin, ebin_lo, ebin_hi, ebin_mid, ebin_del, area, nchan, emin,                                                     */
/*		         emax, edelt, ibspec, ib_expose, ib_backscal, nsource_new, ras_new, decs_new, colden_new, redshifts, intabs,         */
/*		         spectype_new, specpar_new, fluxpar_new, band_lo_new, band_hi_new, sfilename_new, sformat_new, sunits_new,           */
/*		         period_new, pulse_fraction_new, tburst_new, trise_new, tdecay_new, burst_rat_new, ifilename_new, sd_matrix_size,    */
/*		         &sd_param_matrix_new, debug, MTstate);                                                                              */
/*                                                                                                                                           */
/*                                                                                                                                           */
/* PURPOSE:                                                                                                                                  */
/*   The simulated events are constructed here and written to the output event file. The code loops over sources in the                      */
/*   source definition file (plus background sources). For each source                                                                       */
/*   (1) the absorbed input spectrum (photons/cm^2/s vs keV) is evaluated on the input energy grid taken from the ARF file [spectra].        */
/*   (2) This is converted to cts/channel vs channel (still on the input energy grid) by multiplying by the exposure time and                */
/*          effective area (ARF) [or geometric area for ray-tracing].                                                                        */
/*   (3) the sum over channels yields the total number of events. The code now loops over channels and over counts in each channel.          */
/*   (4) For each event a position on the sky (ra,dec) is calculated based on the source distribution [sourcedis] that,  if an               */
/*          input image is provided, is based on a calculated probability matrix for the images or subimages [process_image, imagedis].      */
/*   (5) For the non-raytracing option a photon may be discarded based on the vignetting function [vignette].                                */
/*   (6) A position in detector sky coordinates (x,y) is then computed following application of the PSF [heasim_psf_apply]                   */ 
/*          or ray-tracing and the appropriate coordinate transformations. The ray-tracing may need to invoke a separate energy grid.        */ 
/*   (7) The events are assigned a discrete (x,y) value corresponding to the detector pixels in sky coordinates, and                         */
/*   (8) discarded if scattered out of the FOV [isrealpixel] . The code then loops again over input energy channels, and over                */ 
/*          the counts in each channel, corrected for vignetting and events outside the FOV,                                                 */
/*   (9) redistributing the energies according to the RMF and assigning an output energy (the output energy grid, derived from               */ 
/*          the EBOUNDS extension of the RMF may differ from the input grid).                                                                */
/*   (10) The events are then assigned a time, and                                                                                           */
/*   (11) new rows are added to the output event file.                                                                                       */
/*                                                                                                                                           */
/* INPUTS:                                                                                                                                   */
/*   outfile s_obs, s_calfiles, s_mdb, s_vig, s_psf, nebin, ebin_lo, ebin_hi, ebin_mid, ebin_del, area, nchan, emin,                 */
/*   emax, edelt, &rmf_struct, ibspec, ib_expose, ib_backscal, nsource, ras, decs, colden, spectype, specpar, fluxpar, band_lo, band_hi,     */
/*   sfilename, sformat, sunits, period, pulse_fraction, ifilename, sd_matrix_size, &sd_param_matrix, debug                                  */
/*                                                                                                                                           */
/* OUTPUTS:                                                                                                                                  */
/*   the resultant FITS event file                                                                                                           */
/*                                                                                                                                           */
/* CALLED BY:                                                                                                                                */
/*   main()                                                                                                                                  */
/*                                                                                                                                           */
/* SUBROUTINES:                                                                                                                              */
/*   spectra(), specmod(), timeset(), C_ReturnChannel(), MTseed()                                                                            */
/*                                                                                                                                           */


int doWork(char * outfile,    /* Output event filename */
           ObsParams * s_obs,        /* Structure containing observatory parameters */
           CalFiles * s_calfiles,     /* Structure containing calibration filenames */
           MDB_params * s_mdb,        /* Structure containing mission database parameters */
	   Vig_struct * s_vig,        /* Structure containing vignette data */
	   PSF_struct * s_psf,        /* Structure containing psf data */
	   PSF_image * s_psf_image,   /* Structure containing PSF image data */
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
	   long ib_firstchan,           /* first background channel number */
           int nsource,               /* number of sources found in input source file */
           double * ras,               /* array of source RAs */
           double * decs,              /* array of source DECs */
           double * colden,            /* array of source column densities */
	   double * redshifts,         /* array of redshifts */
	   double * intabs,            /* array of intrinsic absorptions */
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
	   int * spectra_errstatus,      /* error status of spectra calls */
           int debug,                /* debug option */
           HDmt_state *MTstate){      /* "save" state of Mersenne Twister RNG */



    /* doWork internal parameters */
    double * xspec_energy=0;     /* vector of energy grid boundaries for Xspec model input */
    double * specin=0;           /* spectrum in photons/cm^2/sec on arf energy grid */
    double * specout=0;          /* copy of specin, to be corrected for vignetting, events outside FOV */
    long * int_specout=0;         /* integer cast of specout */
    double flux_abs=0;           /* absorbed flux over bandpass specified in source file (scalar) */
    double flux_unabs=0;         /* unabsorbed flux over bandpass specified in source file (scalar) */
    double dflux_abs=0;          /* absorbed flux over entire detector bandpass (scalar) */
    double dflux_unabs=0;        /* unabsorbed flux over entire detector bandpass (scalar) */
    double ra_src=0;             /* singular instance of ras array */
    double dec_src=0;            /* singular instance of decs array */
    double nh=0;                 /* singular instance of colden array */
    int sptype=0;                /* singular instance of spectype array */
    double spar=0;               /* singular instance of specpar array */
    double fpar=0;               /* singular instance of fluxpar array */
    double bpass_lo=0;           /* singular instance of band_lo array */
    double bpass_hi=0;           /* singular instance of band_hi array */
    char sname[STR_MAX_LEN]="\0";              /* singular instance of sfilename array */
    int sform=0;                 /* singular instance of sformat array */
    int sun=0;                   /* singular instance of sunits array */
    double per=0;                /* singular instance of period array */
    double pfrac=0;              /* singular instance of pulse_fraction array */
    char iname[STR_MAX_LEN]="\0";              /* singular instance of ifilename array */
    double * double_ebin_mid=0;  /* ebin_mid converted to double precision */
    long iloop=0, ien=0, j=0;        /* loop index */
    double xs=0;                 /* x position */
    double ys=0;                 /* y position */
    long spectot=0;              /* number of spectral bins for current source */
    double xss=0, yss=0;           /* x,y coords */
    double xssp=0, yssp=0;         /* rolled xss,yss */
    double xsspr=0,ysspr=0;        /* rolled & resampled xss,yss */
    double xssr=0, yssr=0;         /* unrolled & resampled xss,yss */
    long nn_source=0;                /* counter for number of source events */
    long nn_source_block=0;          /* counter for number of events from the current source in the current block*/
    long nev=0;                /* counter for TOTAL number of events */
    long nn_out=0;             /* counter for events lost due to vignetting or out of FOV */
    long ndisc = 0;            /* counter for discarded counts in one spectral bin */
    long nkeep = 0;            /* counter for kept counts in one spectral bin */
    int * xe=0;                  /* array of pixel positions x */
    int * ye=0;                  /* array of pixel positions y */
    int * ide=0;                 /* array of pixel IDs */
    long * pi_chans=0;           /* PI channels for current spectral bin */
    double * evt_time=0;         /* event times for current source */
    fitsfile * ounit=NULL;     /* FITS file structure to read/write */
    int colnum=0;                /* FITS column number */
    int firstrow=0;              /* FITS first row to write */
    int firstelem=0;             /* FITS first element to write */
    int fstat=0;               /* FITS status */
    int i=0, ii=0, jj=0;             /* loop index */
    int errstatus=0;           /* error status */
    double * sdmat=0;            /* simplified list of sd_param_matrix */
    int imat=0;                  /* loop index over sd_param_matrix */
    int xref=0, yref=0;        /* reference pixels corresponding to pointing ra and dec */
    double roll_pnt = 0.0;     /* roll reference point */
    double energy_ien=0;         /* energy of each photon in bin */
    long pi_discarded=0;

    /* PROCESS_IMAGE STUFF: */
    double crvl1_img=0;           /* image x-axis reference pixel */
    double crvl2_img=0;           /* image y-axis reference pixel */
    double crpx1_img=0.0;       /* ra of x-axis ref pixel */
    double crpx2_img=0.0;       /* dec of x-axis ref pixel */
    double cdlt1_img=0.0;       /* increment, in degrees, along x-axis */
    double cdlt2_img=0.0;       /* increment, in degrees, along y-axis */
    int nel_subimg=0;          /* number of elements in subimage and in cprob */
    double ** cprob=0;            /* nel_subimg x 3 array with cumulative probability and corresponding x/y pix values */
    double * cprob_vec=0;        /* vector containing the first column of cprob matrix */

    /* IMAGEDIS/SOURCEDIS STUFF: */
    double ra_img=0;
    double dec_img=0;
    double dx_arcmin[1];
    double dy_arcmin[1];

    /* VIGNETTE STUFF */
    double dx_amin=0.0, dy_amin=0.0;
    int vig_status = 0;

    double x_off_amin=0.0, y_off_amin=0.0;

    int raytrace=0;     /* keep this for now, until we add this to the input parameter options */
    int pixel_status=1; /* pixel status flag: 1=keep, 0=discard */
    
    double * rescale=0;
    double * double_ibspec=0;
    int * int_ibspec=0;
    int alloc_tot=0;
    long nn_ibspec_block=0;
    long nn_ibspec=0;
    int photon_discard=0;        /* flag to discard photon */
    int flagxy=0;                /* xy flag */
    int flag_focxy=0;
    double xfoc=0.0, yfoc=0.0;

    /* discreet sky coords */
    double xsky=0, ysky=0;
    int xsky_pix=0,  ysky_pix=0;

    /* pixel ID bookkeeping */
    int id_det=0;

    /* torus structs */
    torus_par_struct s_tpar;
    torus_spec_struct s_tspec;

    double phase_in = 0.0;

    double zred = 0.0;
    double nhint = 0.0;

    /* BURST STUFF */
    double btime = 0.0;  /* single instance of burst start time array*/
    double rtime = 0.0;  /* single instance of  burst risetime array */
    double dtime = 0.0;  /* single instance of burst decay time array*/
    double brat = 0.0;   /* single instance of burst ratio array*/
    int btabidx = 0;
    double * table_time = 0;
    double * table_x = 0;
    int ntable = btab->xdim-1;
    int nburst = btab->zdim;
    

    /* PILEUP STUFF */
    double deadtime = 0.0;
    long n_pileup = 0;
    double pileup_frac = 0.0;
    double deadtime_frac = 0.0;

    /* RESAMPLE STUFF */
    int skip_resampling = 0;
    int pi_ien = 0;

    printf("\nInitializing Xspec models:\n");
    FNINIT(); /* found in Xspec/XSUtil/FunctionUtils/xsFortran.cxx */
    printf("Done initializing Xspec models.\n\n");

    /* set up the instrument map image array


    /* Open the output fits file for appending data, and move to "EVENTS extension */
    fits_open_file(&ounit, outfile, READWRITE, &fstat);
    fits_movnam_hdu(ounit, BINARY_TBL, "EVENTS", 0, &fstat);
	

    /* vector of energy grid boundaries, size nebin+1; required for Xspec model code when
       calling spectra; more efficient to set this once here */

    xspec_energy = calloc( (nebin+1), sizeof(double));   /* FREE AT END OF DOWORK */
    specin = calloc(nebin, sizeof(double));
    specout = calloc(nebin, sizeof(double));
    int_specout = calloc(nebin, sizeof(long));
    double_ebin_mid = calloc(nebin, sizeof(double));

    for (i=0; i<nebin; i++){
        xspec_energy[i] = ebin_lo[i];
        double_ebin_mid[i] = (double)ebin_mid[i];
    }
    xspec_energy[i] = ebin_hi[i-1];

    /* Initialize some of the torus struct values */
    s_tpar.num_nh = 0;
    s_tpar.num_gam = 0;
    s_tpar.num_theta = 0;
    s_tpar.num_inc = 0;
    s_tpar.nhvals = 0;
    s_tpar.gamvals = 0;
    s_tpar.thvals = 0;
    s_tpar.incvals = 0;
    
    s_tspec.num_en = 0;
    s_tspec.num_spec = 0;
    s_tspec.tor_energ_lo = 0;
    s_tspec.tor_energ_hi = 0;
    s_tspec.tor_energ_mid = 0;
    s_tspec.tor_energ_wid = 0;
    s_tspec.tor_xspec_energy = 0;
    s_tspec.tor_spec = 0;
    if (0 != strcasecmp(s_back->psbackfile,"none")){
	errstatus = torus_prep(&s_tpar, &s_tspec);
	if (errstatus != 0){
	    printf("ERROR: failed to populate torus.\n");
	    return -1;
	}
    }
    
    *spectra_errstatus = 0;

    nev = 0;  /* Counter for total number of events in the simulation. */
    nn_source = 0;
    nn_ibspec = 0;


    /* set up for possible exposure time subdivisions */
    double exposure_block = 0.0; /* sub-exposure time length */
    double exp_completed = 0.0;  /* total exposure time completed */
    double exp_remaining = s_obs->exposure;  /* total exposure time remaining */
    double exp_final = 0.0;      /* exposure time left in final block */
    int Nblocks = 1;             /* number of sub-exposure blocks */
    int Nexp_counter = 0;        /* number of sub-exposure time blocks completed */


    if (s_obs->flagsubex){    /* If the flag to do subexposures is set */

	/* If any of the sources are variable, then do all in one time block. */
	/* SKIP THIS FOR NOW - TIME CHANGE 20160331
	for (int isrc=0; isrc<nsource; isrc++){
	    if ( (period[isrc] != 0) || (pulse_fraction[isrc] != 0) ){
		printf("Source %d is periodic, setting subexposure = exposure.\n",isrc+1);
		s_obs->subexposure = s_obs->exposure;
	    } else if ( (tburst[isrc] != 0) || (trise[isrc] != 0) || (tdecay[isrc] != 0) || (burst_rat[isrc] != 0) ){
		printf("Source %d is burst, setting subexposure = exposure.\n",isrc+1);
		s_obs->subexposure = s_obs->exposure;
	    }
	}
	*/
	
	Nblocks = (int)(s_obs->exposure/s_obs->subexposure);   /* number of sub-exposure blocks */
	
	/* If the subexposure is greater than the exposure, then optimize the exposure block setting. */
	if (s_obs->subexposure > s_obs->exposure){
	    double total_flux = 0.0;        /* total flux of all sources, in erg/s/cm^2 */
	    double eff_area = 0.0;          /* detector area in cm^2 */
	    int arf_index = 0;              /* index in ARF array of Area vs Energy */
	    double aveE = 0.0;              /* average photon energy, in erg */
	    double kev2erg = 1.60217e-09;   /* keV to erg conversion */
	    long nev_est = 0.0;             /* estimated total number of events */
	    
	    /* Get the total flux from all sources */
	    for (int isrc=0; isrc<nsource; isrc++)  total_flux += fluxpar[isrc];
	    
	    /* Get the largest effective area and corresponding energy from ARF table */
	    arf_index = 0;
	    eff_area = 0.0;
	    for (long arfi=0; arfi<arf_struct->NumberEnergyBins; arfi++){
		if ( arf_struct->EffArea[arfi] > eff_area ){
		    arf_index = arfi;
		    eff_area = arf_struct->EffArea[arf_index];
		}
	    }
	    aveE = (arf_struct->LowEnergy[arf_index] + arf_struct->HighEnergy[arf_index]) / 2.0 * kev2erg;
	    
	    /* Estimate the total number of expected events */
	    nev_est = (long)(total_flux * s_obs->exposure * eff_area / aveE);
	    
	    /* Optimize the number of blocks so there are ~500,000 events in each block. */
	    Nblocks = (int)(nev_est / 5.e5);
	    
	    /* Possibly we will have fewer than 500,000 total events - can do in just one block. */
	    if (Nblocks < 1) Nblocks = 1;
	    
	    /* Recompute the size of the exposure block, based on the number of blocks */
	    exposure_block = floor(s_obs->exposure / Nblocks);

	    /* adjust Nblocks */
	    Nblocks = (int)(s_obs->exposure / exposure_block);
	    

	    printf("Optimizing sub-exposures for estimated %ld events...\n",nev_est);
	} else {
	    exposure_block = s_obs->subexposure;
	    
	    /* Handle the case where the user has naively set subexposure = 0 */
	    if (exposure_block == 0.0) exposure_block = s_obs->exposure;

	}  /* end if s_obs->subexposure > s_obs->exposure */

    } else {  /* Flag to do subexposures is NOT set, set subexposure = exposure, for one single block - DEFAULT */
	exposure_block = s_obs->subexposure = s_obs->exposure;
	Nblocks = 1;
    }

    /* There may be some exposure time left over */
    exp_final = s_obs->exposure - Nblocks * exposure_block;

    if ( (exp_final > 0) && (exposure_block + exp_final) < (1.1 * exposure_block) ){
	Nblocks--;
	exp_final += exposure_block;
    }

    printf("Will simulate exposures such that:\n");
    printf("  %d sub-exposures of %ds\n",Nblocks,(int)exposure_block);
    if ( exp_final > 0 ){
	printf("  1 sub-exposure of %ds\n",(int)(exp_final));
	Nblocks++;
    }  /* end if exp_final */


    printf("\nProcessing %d sources (set \"debug=yes\" for more detail)...\n",nsource);

    /* begin WHILE loop over exposure blocks */
    while (exp_completed < s_obs->exposure){
	Nexp_counter++;

	exp_remaining = s_obs->exposure - exp_completed;

	if (exp_remaining < (1.1 * exposure_block)){
	    /* If the final "leftover" block is less than 10% of the block size, add it to the current block */
	    exposure_block = exp_remaining;

	} else if ( (exp_completed+exposure_block) > s_obs->exposure ){
	    /* If our exposure block is larger than the exposure time remaining, trim it */
	    exposure_block = s_obs->exposure - exp_completed;
	}

	printf("\n===== Processing exposure %d of %d (%lds - %lds). =====\n",
	       Nexp_counter,Nblocks,(long)exp_completed,(long)(exp_completed+exposure_block));

	long nev_block = 0;  /* Number of events in the current sub-exposure block */
 	
	/* begin loop over sources */
	for (iloop=0; iloop<nsource; iloop++){
	
	    double flux_corr = 1.0;

	    if (debug == 1) printf("\n  Processing source %ld of %d\n",iloop+1,nsource);

	    /* phase_in = -1.0; TIME CHANGE 20160331 */
	
	    strcpy(iname,ifilename[iloop]);
	    strcpy(sname,sfilename[iloop]);
	    ra_src = (double)(ras[iloop]);
	    dec_src = (double)(decs[iloop]);
	    nh = (double)(colden[iloop]);
	    zred = redshifts[iloop];
	    nhint = intabs[iloop];
	    sptype = (int)(spectype[iloop]);
	    spar = (double)(specpar[iloop]);
	    fpar = (double)(fluxpar[iloop]);
	    bpass_lo = (double)(band_lo[iloop]);
	    bpass_hi = (double)(band_hi[iloop]);
	    if (bpass_lo <= 0.0) bpass_lo = (double)(ebin_lo[0]);
	    if (bpass_hi <= 0.0) bpass_hi = (double)(ebin_hi[nebin-1]);
	    sform = (int)(sformat[iloop]);
	    sun = (int)(sunits[iloop]);
	    per = (double)(period[iloop]);
	    pfrac = (double)(pulse_fraction[iloop]);
	    btime = (double)(tburst[iloop]);          
	    rtime = (double)(trise[iloop]);           
	    dtime = (double)(tdecay[iloop]);          
	    brat = (double)(burst_rat[iloop]);	
	    if (nburst > 0) btabidx = (int)(btab->burst_tabidx[iloop]);

	    sdmat = calloc(sd_matrix_size, sizeof(double));
	    for (imat = 0; imat<sd_matrix_size; imat++)
		sdmat[imat] = (*sd_param_matrix)[iloop][imat];

	    if (debug == 1){
		printf("  sname = %s\n",sname);
		printf("  iname = %s\n",iname);
	    }

	    /* (x,y) = (0,0) is defined as corresponding to the pointing ra and dec */
	    xref = 0.0;
	    yref = 0.0;
	    roll_pnt = 0.0;

	    /* assumes 7 source types, with image as the 7th */
	    if (sdmat[0] == 7){
		errstatus = process_image(iname,sdmat[1],sdmat[2],sdmat[3],sdmat[4],
					  &crvl1_img, &crvl2_img, &crpx1_img, &crpx2_img, &cdlt1_img, &cdlt2_img,
					  &nel_subimg, &cprob, debug); 

		cprob_vec = calloc(nel_subimg, sizeof(double));
		for (ii=0; ii<nel_subimg; ii++)
		    cprob_vec[ii] = (double)cprob[ii][0];

		if (errstatus != 0){
		    printf("errstatus %d, could not read image file.\n",errstatus);
		    return -1;
		}


		if (debug == 1){
		    char cprobfile[STR_MAX_LEN];
		    sprintf(cprobfile,"output/debug_cprob_src%ld.txt",iloop+1);
		    FILE *fp = fopen(cprobfile,"w");
		    fprintf(fp,"crvl1_img = %f\n",crvl1_img);
		    fprintf(fp,"crvl2_img = %f\n",crvl2_img);
		    fprintf(fp,"crpx1_img = %f\n",crpx1_img);
		    fprintf(fp,"crpx2_img = %f\n",crpx2_img);
		    fprintf(fp,"cdlt1_img = %f\n",cdlt1_img);
		    fprintf(fp,"cdlt2_img = %f\n",cdlt2_img);
		    fprintf(fp,"nel_subimg = %d\n\n",nel_subimg);
		    fprintf(fp,"cprob matrix:\n\n");
		    for (ii=0; ii<nel_subimg; ii++)
			fprintf(fp,"%f  %f  %f\n",cprob[ii][0],cprob[ii][1],cprob[ii][2]);
		    fclose(fp);

		    printf("doWork.c: nel_subimg = %d\n",nel_subimg);
		}

		/* set source (x,y) coords to 0 for now. */
		xs=0.;
		ys=0.;

		if (debug == 1) printf("Image: RA: %f  DEC: %f   xs: %f  ys: %f\n",ra_src,dec_src,xs,ys);

	    } else {
		/* convert source (ra,dec) to (x,y) */
		rd2xy(ra_src, dec_src, xref, yref, s_obs->rapoint, s_obs->decpoint, roll_pnt, s_mdb->cdltx,
		      s_mdb->cdlty, &xs, &ys);
	    }
	
	    /* If the flux is not constant, need to adjust the average flux in this block relative to the average over the entire exposure*/
	    if ( (brat == 0.0) && (per == 0.0) ){
		flux_corr = 1.0; /* constant case */
	    } else if (brat == 0.0) { /* periodic case*/
		phase_in = TWOPI * exp_completed / per; /* phase at beginning of block*/
		double phase_out = TWOPI * (exp_completed+exposure_block) / per; /* phase at end of block*/
		flux_corr = 1.0 + ( pfrac * ( (per/TWOPI) / exposure_block) ) * (cos(phase_in) - cos(phase_out) );
	    } else { /* burst case*/
		double tbeg = 0;
		double tend = 0;
		int jbeg = 0;
		double weight1 = 0;
		double weight2 = 0;
		double xbeg = 0;
		int jend = 0;
		double xend = 0;
		double delx = 0;

		table_time = calloc(ntable+1,sizeof(double));
		table_x = calloc(ntable+1,sizeof(double));

		for (int itable = 0; itable <= ntable; itable++) {
		    table_time[itable] = btab->burst_table[itable][0][btabidx-1];
		    table_x[itable] = btab->burst_table[itable][1][btabidx-1];
		}
		
		tbeg = exp_completed  / s_obs->exposure; /*normalized time at beginning of interval */
		tend =  (exp_completed+exposure_block) / s_obs->exposure; /*normalized time at end of interval */
		jbeg = find_index(tbeg, ntable+1, table_time) - 1; /*first table entry outside interval */
		if (jbeg < 1) jbeg=1;
		
		/* Interpolate to get the cumulative probabilities at the endpoint of the subexposure*/
		weight1 = (table_time[jbeg] - tbeg)/(table_time[jbeg] - table_time[jbeg-1]);
		weight2 = (tbeg - table_time[jbeg-1])/(table_time[jbeg] - table_time[jbeg-1]); 
		xbeg =  table_x[jbeg] * weight2 + table_x[jbeg-1] * weight1;
		jend = find_index(tend, ntable+1, table_time); /*last table entry outside interval */
		
		if (jend > ntable) jend=ntable;
		weight1 = (table_time[jend] - tend)/(table_time[jend] - table_time[jend-1]);
		weight2 = (tend - table_time[jend-1])/(table_time[jend] - table_time[jend-1]); 
		xend =  table_x[jend] * weight2 + table_x[jend-1] * weight1;
		
		delx = xend  - xbeg; /* the probability of being somewhere in this interval*/
		/* delx is the probability of being somewhere in this interval. Since this is proportional to the      
		   integrated flux over this interval, the average flux over the observation should be scaled by          
		   the following factor for this subexposure*/
		
		flux_corr = (s_obs->exposure  / exposure_block) *delx;

	    }

	    if (sptype < 7){
		/* Get spectrum specin, absorbed flux flux_abs, and unabsorbed flux flux_unabs */
		errstatus = spectra(nh, zred, nhint, sptype, spar, flux_corr * fpar, bpass_lo, bpass_hi, sname, sform,
				    sun, nebin, double_ebin_mid, xspec_energy, specin, &flux_abs,
				    &flux_unabs, &dflux_abs, &dflux_unabs, debug); 
	    } else {
		errstatus = torus_spectra(nh, zred, sptype-1, spar, fpar, bpass_lo, bpass_hi, nebin, double_ebin_mid,
					  xspec_energy, &s_tpar, &s_tspec, specin, &flux_abs, &flux_unabs, &dflux_abs, &dflux_unabs, debug);
	    }

	    /* Report some values */
            printf("\n  Processing source: %ld\n",iloop);
            if (debug == 1){
                printf("   ra_src:   %f\n",ra_src);
                printf("   dec_src:  %f\n",dec_src);
                printf("   nh:       %f\n",nh);
                printf("   zred:     %f\n",zred);
                printf("   nhint:    %f\n",nhint);
                printf("   sptype:   %d\n",sptype);
                printf("   spar:     %f\n",spar);
                printf("   fpar:     %f\n",fpar);
                printf("   bpass_lo: %f\n",bpass_lo);
                printf("   bpass_hi: %f\n",bpass_hi);
                printf("   sform:    %d\n",sform);
                printf("   sun:      %d\n",sun);
            }
            printf("\n");

	    /* While looping through sources, if this condition is ever true, then forever set spectra_errstatus to -1.
	       We do this because if spectra() has problems, we merely set spec[] to zero and continue to the next source;
	       must have a way to report at the end that problems were encountered. */
	    if (errstatus != 0) *spectra_errstatus = -1; 

	    /* loop over energy bins, multiply input spectrum by exposure time and effective area
	       [geometric area or ray-tracing?] to convert input spectrum from photons/cm^2/s to
	       counts/bin: */
	    for (ien=0; ien<nebin; ien++){
		specin[ien] *= exposure_block * area[ien];
		specout[ien] = specin[ien];
	    }

	    /* adjust for low count bins using Poisson stats - AFTER THIS, SPECOUT HAS ONLY INTEGER VALUES! */
	    specmod(specout, nebin, MTstate);

	    for (i=0; i<nebin; i++)
		int_specout[i] = (long)specout[i];  /* just to be clear */


	    /* sum the counts in the spectrum */
	    spectot = 0;
	    for (ien=0; ien<nebin; ien++)
		spectot += int_specout[ien];

	    nn_source_block = 0;  /* counter for number of events */
	    nn_out = 0;     /* counter for events lost to vignetting or out-of-fov */

	    for (ien=0; ien<nebin; ien++){ /* loop over input energy bins */

		xe = calloc(specout[ien], sizeof(int));
		ye = calloc(specout[ien], sizeof(int));
		ide = calloc(specout[ien], sizeof(int));

		energy_ien = double_ebin_mid[ien]; /* energy of each photon in this bin */

		/* If rmffile=none, each event will be assigned the PI bin (based on the rmf EBOUNDS
                   extension) that this energy  falls into. We calculate that here once for each bin and apply
                   below to each event in this bin */
                /* +++ 2016-08-25 RSH ***new:  check this */
                if (0 == strcasecmp(s_calfiles->rmffile, "none")) {
                    pi_ien = find_index(energy_ien, nchan, emax);
                }

		ndisc = 0;  /* counter for discarded counts in one spectral bin */
		nkeep = 0;  /* counter for kept counts in one spectral bin */

		/* begin loop over counts in this bin */
		for (j=0; j<int_specout[ien]; j++){
		    photon_discard = 0;

		    /* begin source distribution conditional (image or not) */
		    if (sdmat[0] == 7){
			errstatus = imagedis(crvl1_img, crvl2_img, crpx1_img, crpx2_img, cdlt1_img,
					     cdlt2_img, nel_subimg, cprob, cprob_vec, &ra_img, &dec_img, MTstate);
			/* returns (ra_img, dec_image) = ra and dec for a single photon based on the image input */

			/* convert source (ra_img,dec_img) to (xss,yss) */
			rd2xy(ra_img, dec_img, xref, yref, s_obs->rapoint, s_obs->decpoint, roll_pnt,
			      s_mdb->cdltx, s_mdb->cdlty, &xss, &yss);


		    } else {
			errstatus = sourcedis(sdmat, 1, dx_arcmin, dy_arcmin, MTstate);
			/* returns (dx_arcmin, dy_arcmin) = offset, measured in arcmin, in x and y from input source position */

			xss = xs + dx_arcmin[0] / (60.0 * s_mdb->cdltx);
			yss = ys + dy_arcmin[0] / (60.0 * s_mdb->cdlty);

		    } /* end source distribution conditional */


		    if (raytrace == 1){ /* we've hard coded raytrace to 0 for now. */
			s_vig->vigtype = 0;
			s_psf->psftype = 0;
			/* coordinate transformations */
			/* additional adjustments (# photons, # energies...?) */
			/* call raytrace */
		    } else {
			/* calculate the offsets from the optical axis in acrmin in the relevent coord system. */
			flagxy = 0;
			if ( (xss < s_mdb->xmin) || (xss > s_mdb->xmax) ) flagxy = 1;
			if ( (yss < s_mdb->ymin) || (yss > s_mdb->ymax) ) flagxy = 1;

			if ( (flagxy == 1) && (s_obs->skipfov == 0) ){
			    photon_discard = 1;
			} else {

			    /* roll (xss,yss) to (xssp, yssp) */
			    rollxy(xss,yss,xref,yref,s_obs->roll,s_mdb->cdltx,s_mdb->cdlty,&xssp,&yssp);

			    /* translate and rescale/flip */
			    xfoc = s_mdb->AIM_FOCX + xssp * (s_mdb->cdltx / s_mdb->cdltx_foc);
			    yfoc = s_mdb->AIM_FOCY + yssp * (s_mdb->cdlty / s_mdb->cdlty_foc);

			    /* off-axis relative to optical axis */
			    x_off_amin = 60.0 * -s_mdb->cdltx_foc * (xfoc - s_mdb->OPTAXIS_FOCX);
			    y_off_amin = 60.0 * s_mdb->cdlty_foc * (yfoc - s_mdb->OPTAXIS_FOCY);
			}

			/* Vignette correction (photon by photon) */
			if ( (s_vig->vigtype > 0) && (photon_discard == 0) ){
			    /* discard probabilistically according to energy and position in focal plane */
			    vig_status = apply_vignette(s_vig, s_mdb, x_off_amin, y_off_amin, energy_ien, MTstate);
			    if (vig_status == 1) photon_discard = 1;
			}
		    
			/* PSF correction */
			if ( (vig_status == 0) && (photon_discard == 0) ){
			    if (s_psf->psftype > 0){

				/* return (dx_amin, dy_amin), the psf-induced displacement in arcmin in the
				   foc coord-sys based on the initial off-axis angle and energy. */
				apply_psf(s_psf, s_psf_image, s_mdb, x_off_amin, y_off_amin, energy_ien, &dx_amin, &dy_amin, MTstate); 
				xfoc += dx_amin / (60.0 *fabs(s_mdb->cdltx_foc));
				yfoc += dy_amin / (60.0 *fabs(s_mdb->cdlty_foc));
				flag_focxy = 0;
				if ( (xfoc < s_mdb->xmin_foc) || (xfoc > s_mdb->xmax_foc) ) flag_focxy = 1;
				if ( (yfoc < s_mdb->ymin_foc) || (yfoc > s_mdb->ymax_foc) ) flag_focxy = 1;
				if ( (flag_focxy == 1) && (s_obs->skipfov == 0) ) photon_discard = 1;

			    } /* end if (psftype > 0) */
			} /* end if ( (vig_status == 0) && (photon_discard == 0) ) */			   
		    } /* end if raytrace */

		    if ( (vig_status == 0) && (photon_discard == 0) ){
			if  (s_obs->skipfov==0) {
			    if (s_mdb->instmap_flag == 0) {
				pixel_status = isrealpixel(s_mdb, xfoc, yfoc);
			    } else {
				pixel_status = apply_instmap(s_mdb, xfoc, yfoc);
			    }   
			} else {
                            pixel_status=1;
                        }
			if (pixel_status==0) {   
			    photon_discard = 1;
			} else {
			    
			    double tmpx = 0.0;
			    double tmpy = 0.0;
			    double x_det = 0.0;
			    double y_det = 0.0;
			    double xdet_resamp = 0.0;
			    double ydet_resamp = 0.0;
			    double jdetx = 0.0;
			    double jdety = 0.0;
			    double xdet_ran = 0.0;
			    double ydet_ran = 0.0;
			    double xfoc_resamp = 0.0;
			    double yfoc_resamp = 0.0;
			    double xfoc_pix = 0.0;
			    double yfoc_pix = 0.0;
			    int resamp_ok = 0;

			    /* calculate sky coordinates */

			    /* x and y distances from center of foc in foc pixels: */
			    tmpx = (xfoc - s_mdb->crpxx_foc);
			    tmpy = (yfoc - s_mdb->crpxy_foc);

			    /* x and y distances from corner of detector in foc-pixels: */
			    x_det = s_mdb->resampx * s_mdb->FOC_XOFF + s_mdb->crpxx_det  + s_mdb->FOC_ROTD_cos*tmpx + s_mdb->FOC_ROTD_sin*tmpy;
			    y_det = s_mdb->resampy * s_mdb->FOC_YOFF + s_mdb->crpxy_det  - s_mdb->FOC_ROTD_sin*tmpx + s_mdb->FOC_ROTD_cos*tmpy; 

			    /* x and y distances from corner of detector in det-pixels: */                 
			    xdet_resamp = (x_det-0.5) / s_mdb->resampx;
			    ydet_resamp = (y_det-0.5) / s_mdb->resampy;
			    
			    jdetx = 1 + (int)(xdet_resamp);
			    jdety = 1 + (int)(ydet_resamp);
                            
			    /* (unique) pixel designation for pile-up calculation: */
			    if (s_obs->dtpileup <= 0.0) {
                                id_det = 0;
                            } else  {
                                id_det = s_mdb->DET_XNPIX*(jdety-1) + (jdetx-1);
			    }


			    if  ( (s_mdb->resampx >1.5) || (s_mdb->resampy > 1.5) ){
				resamp_ok = 1;
			    } else {
				resamp_ok = 0;
			    }

			    
			    if ( (resamp_ok == 0) && (s_obs->resample == 1) ){
				//printf("Detector pixel size within 50%% of sky pixel size, resampling will be skipped.\n");
				skip_resampling = 1;
			    }			    

                            if  ( (s_obs->resample == 1) && (resamp_ok == 1) ) {

				/* randomize in the det-pix: */
				xdet_ran = 0.5 + s_mdb->resampx  * (jdetx - 1 + HDmt_drand(MTstate) );
				ydet_ran = 0.5 + s_mdb->resampy  * (jdety - 1 + HDmt_drand(MTstate) );

				/* x and y distances from center of detector in foc-pixels: */
				tmpx = (xdet_ran  - s_mdb->crpxx_det); 
				tmpy = (ydet_ran  - s_mdb->crpxy_det); 

				/* x and y distances from corner of foc system in foc-pixels: */
				xfoc_resamp = -s_mdb->resampx * s_mdb->FOC_XOFF + s_mdb->crpxx_foc + s_mdb->FOC_ROTD_cos*tmpx 
				    - s_mdb->FOC_ROTD_sin*tmpy;
				yfoc_resamp = -s_mdb->resampy * s_mdb->FOC_YOFF + s_mdb->crpxy_foc + s_mdb->FOC_ROTD_sin*tmpx 
				    + s_mdb->FOC_ROTD_cos*tmpy; 

				/* reapply instrument map: */                     
				xfoc_pix = (int)(xfoc_resamp + 0.5);
				yfoc_pix = (int)(yfoc_resamp + 0.5);


				if (s_obs->skipfov==0) {
				    if (s_mdb->instmap_flag == 0) {
					pixel_status = isrealpixel(s_mdb, xfoc_resamp, yfoc_resamp);
				    } else {
					pixel_status = apply_instmap(s_mdb, xfoc_resamp, yfoc_resamp);
				    }
				} else {
                                    pixel_status=1;
                                }
				if (pixel_status == 0) { photon_discard = 1; }

			    } else {
				xfoc_resamp = xfoc;
				yfoc_resamp = yfoc;
			    }
			    
			    if (photon_discard != 1) {
				
				/* x and y distances from pointing: */  
				xsspr = (xfoc_resamp - s_mdb->AIM_FOCX) * (s_mdb->cdltx_foc/s_mdb->cdltx);
				ysspr = (yfoc_resamp - s_mdb->AIM_FOCY) * (s_mdb->cdlty_foc/s_mdb->cdlty);
				
				/* unroll: */
				rollxy(xsspr, ysspr, xref, yref,-s_obs->roll, -s_mdb->cdltx_foc, s_mdb->cdlty_foc, &xssr, &yssr);
				
				/* discrete sky coords: */
				xsky = s_mdb->crpxx_sky + xssr * (s_mdb->cdltx/s_mdb->cdltx_sky);
				ysky = s_mdb->crpxy_sky + yssr * (s_mdb->cdlty/s_mdb->cdlty_sky);
				xsky_pix = (int)(xsky + 0.5);
				ysky_pix = (int)(ysky + 0.5);
				nkeep++;
				
				/* nkeep  + ndisc = j +1 (j is the index of this loop over counts in this bin) */
				/* store the event (sky) x and y one energy bin at a time */
				xe[nkeep-1] = xsky_pix;
				ye[nkeep-1] = ysky_pix;
				ide[nkeep-1] = id_det;
				
			    } /* endif photon_discard != 1 */
			} /* endif pixel_status == 0 */
		    } /* endif vig_status == 0 */
		
		    if (photon_discard == 1) ndisc++;
		
		} /* end loop over counts in this bin */
	    
		nn_out += ndisc;  /* increment discarded counts for this source */
		int_specout[ien] = nkeep;  /* number of "good" events in this bin for this source */

		if (nkeep > 0){

		    pi_chans = calloc(nkeep, sizeof(long));
		
		    /* assign all of the events at this energy to the corresponding PI channel from the rmf EBOUNDS
                       extension, as calculated above */
		    
                    /* +++ 2016-08-25 RSH ***new:  check this */
		    if (0 != strcasecmp(s_calfiles->rmffile, "none")) {
			C_ReturnChannel(rmf_struct, ebin_mid[ien], nkeep, pi_chans, MTstate);
			
			/* Remove pi_chan=-1 and resize xe, ye, and pi_chan arrays */
			int new_nkeep = clean_pi_chan(&xe, &ye, &ide, &pi_chans, nkeep);
			pi_discarded += nkeep - new_nkeep;
			nkeep = new_nkeep;
			
		    } else {
                        for (int iii=0; iii<nkeep; iii++) {
                            pi_chans[iii] = pi_ien;
                        }
                    }

		    nev_block += nkeep;

		    evt_time = calloc(nkeep, sizeof(double));

		    if (brat == 0.0){
			/* if the burst ratio is zero, i.e. no burst modeling */
			errstatus = timeset(exp_completed,exposure_block,per,pfrac,nkeep,evt_time,phase_in,MTstate);
		    } else {
			/* if we ARE doing a burst, call alternate timeset routine */
			errstatus = timeset_burst(nkeep, s_obs->exposure, brat, btime, exp_completed, 
						  exposure_block, ntable, table_time, table_x, evt_time,MTstate);
		    }

		    /* Add the exposure time completed in the previous time block to each time element in this block */
		    /* for (int kk=0; kk<nkeep; kk++)
		       evt_time[kk] += exp_completed; */

		    if (errstatus != 0) return errstatus;

		    /* Add new rows: insert nkeep rows starting with row number nev+1, with columns for evt_time in TIME
		       column, xe in X column, ye in Y column, pi_chans in PI column, pileup flag in flag pileup column,
		       pixel ID in PIXEL_DESIG column*/
		
		    firstrow = nev+1;
		    firstelem = 1;
		
		    colnum = 1;  /* column 1 contains time */
		    fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, nkeep, evt_time, &fstat);
		
		    colnum = 2;  /* column 2 contains X */
		    fits_write_col(ounit, TINT, colnum, firstrow, firstelem, nkeep, xe, &fstat);
		
		    colnum = 3;  /* column 3 contains Y */
		    fits_write_col(ounit, TINT, colnum, firstrow, firstelem, nkeep, ye, &fstat);
		
		    colnum = 4;  /* column 4 contains PI */
		    fits_write_col(ounit, TLONG, colnum, firstrow, firstelem, nkeep, pi_chans, &fstat);

		    /* column 5, the pileup flag, will be populated with zeroes. */
		
		    colnum = 6;  /* column 6 contains PIXEL_DESIG */
		    fits_write_col(ounit, TINT, colnum, firstrow, firstelem, nkeep, ide,  &fstat); 

		    nn_source_block += nkeep;
		    nev += nkeep;
		
		    free(pi_chans);
		    free(evt_time);
		}
		free(xe);
		free(ye);
		free(ide);
	    } /* end loop over input energy bins */

	    nn_source += nn_source_block;

	    if (debug == 1){
		printf("  nn_source = %ld\n",nn_source);
		printf("  nn_out = %ld\n",nn_out);
		printf("  nev = %ld\n",nev);
		printf("  pi_discarded = %ld\n",pi_discarded);
	    }

	    if (sdmat[0] == 7){
		deallocate_imagedis_data(&cprob, &cprob_vec, nel_subimg);
	    }

	    free(sdmat);

	}/* end loop over sources */

	if (brat != 0){
	    free(table_x);
	    free(table_time);
	}

	
	/* If we have a background file specified, apply the background */
        if (0 != strcasecmp(s_calfiles->intbackfile,"none")){

            /* Set up for internal background */
            rescale = calloc(nchan, sizeof(double));
            double_ibspec = calloc(nchan, sizeof(double));
            for (ii=0; ii<nchan; ii++){
                rescale[ii] = (exposure_block / ib_expose) / ib_backscal[ii];
                double_ibspec[ii] = (double)ibspec[ii] * rescale[ii];
            }

            specmod(double_ibspec, nchan, MTstate);
            int_ibspec = calloc(nchan, sizeof(int));
            alloc_tot=0;  /* need to know how to allocate xe, ye, pi */

            for (ii=0; ii<nchan; ii++){
                int_ibspec[ii] = (int)double_ibspec[ii];
                alloc_tot += int_ibspec[ii];
            }

            /* these were free'd after end of source loop */
            xe = calloc(alloc_tot, sizeof(int));
            ye = calloc(alloc_tot, sizeof(int));
            ide = calloc(alloc_tot, sizeof(int));
            pi_chans = calloc(alloc_tot, sizeof(long));

            nn_ibspec_block = 0;  /* counter for internal background events */

            for (ii=0; ii<nchan; ii++){   /* loop over energy channels */
                for (jj=0; jj<int_ibspec[ii]; jj++){   /* loop over counts in each bin */

		    double tmpx = 0.0;
		    double tmpy = 0.0;
		    double x_det = 0.0;
		    double y_det = 0.0;
		    double xdet_resamp = 0.0;
		    double ydet_resamp = 0.0;
		    double jdetx = 0.0;
		    double jdety = 0.0;
		    double xdet_ran = 0.0;
		    double ydet_ran = 0.0;
		    double xfoc_resamp = 0.0;
		    double yfoc_resamp = 0.0;
		    double xfoc_pix = 0.0;
		    double yfoc_pix = 0.0;
		    int resamp_ok = 0;


                    /* assign a random foc(x,y) pixel */
                    do {
                        xfoc = s_mdb->xmin_foc + HDmt_drand(MTstate) * (s_mdb->xmax_foc -s_mdb->xmin_foc);
                        yfoc = s_mdb->ymin_foc + HDmt_drand(MTstate) * (s_mdb->ymax_foc -s_mdb->ymin_foc);

			if  (s_obs->skipfov==0) {
			    if (s_mdb->instmap_flag == 0) {
				pixel_status = isrealpixel(s_mdb, xfoc, yfoc);
			    } else {
				pixel_status = apply_instmap(s_mdb, xfoc, yfoc);
			    }
			} else {
                            pixel_status=1;
                        }

                    } while (pixel_status != 1);

                    /* calculate sky coordinates */
		    
		    /* x and y distances from center of foc in foc pixels: */
		    tmpx = (xfoc - s_mdb->crpxx_foc); 
		    tmpy = (yfoc - s_mdb->crpxy_foc); 

		    /* x and y distances from corner of detector in foc-pixels: */
		    x_det =  s_mdb->resampx * s_mdb->FOC_XOFF + s_mdb->crpxx_det + s_mdb->FOC_ROTD_cos*tmpx + s_mdb->FOC_ROTD_sin*tmpy;
		    y_det = s_mdb->resampy * s_mdb->FOC_YOFF + s_mdb->crpxy_det - s_mdb->FOC_ROTD_sin*tmpx + s_mdb->FOC_ROTD_cos*tmpy;

		    /* x and y distances from corner of detector in det-pixels: */                                     
		    xdet_resamp = (x_det-0.5) / s_mdb->resampx;
		    ydet_resamp = (y_det-0.5) / s_mdb->resampy;

		    jdetx = 1 + (int)(xdet_resamp);
		    jdety = 1 + (int)(ydet_resamp);
        
		    /* (unique) pixel designation for pile-up calculation: */
		    if (s_obs->dtpileup <= 0.0) {
			id_det = 0;
		    } else  {
			id_det = s_mdb->DET_XNPIX*(jdety-1) + (jdetx-1);
		    }

		    if  ( (s_mdb->resampx > 1.5) || (s_mdb->resampy > 1.5) ){
			resamp_ok = 1;
		    } else {
			resamp_ok = 0;
		    }	 

		    if ( (resamp_ok == 0) && (s_obs->resample == 1) ){
			//printf("Detecor pixel size within 50%% of sky pixel size, resampling will be skipped.\n");
			skip_resampling = 1;
		    }

		    if  ( (s_obs->resample == 1) && (resamp_ok == 1) ) {

			/* randomize on detector grid: */
			xdet_ran = 0.5 + s_mdb->resampx  * (jdetx - 1 + HDmt_drand(MTstate) );
			ydet_ran = 0.5 + s_mdb->resampy  * (jdety - 1 + HDmt_drand(MTstate) );

			/* x and y distances from center of detector in foc-pixels: */
			tmpx = (xdet_ran  - s_mdb->crpxx_det); 
			tmpy = (ydet_ran  - s_mdb->crpxy_det); 
			xfoc_resamp = -s_mdb->resampx * s_mdb->FOC_XOFF + s_mdb->crpxx_foc + s_mdb->FOC_ROTD_cos*tmpx - s_mdb->FOC_ROTD_sin*tmpy;
			yfoc_resamp = -s_mdb->resampy *s_mdb->FOC_YOFF + s_mdb->crpxy_foc + s_mdb->FOC_ROTD_sin*tmpx + s_mdb->FOC_ROTD_cos*tmpy;

			/* reapply instrument map: */                                        
			xfoc_pix = (int)(xfoc_resamp + 0.5);
			yfoc_pix = (int)(yfoc_resamp + 0.5);


			if  (s_obs->skipfov==0) {
			    if (s_mdb->instmap_flag == 0) {
				pixel_status = isrealpixel(s_mdb, xfoc_resamp, yfoc_resamp);
			    } else {
				pixel_status = apply_instmap(s_mdb, xfoc_resamp, yfoc_resamp);
			    }
			} else {
                            pixel_status=1;
                        }

			if (pixel_status==0) {photon_discard = 1;}

		    } else {
			xfoc_resamp = xfoc;
			yfoc_resamp = yfoc;
		    }

		    if (photon_discard != 1) {
			/* x and y distances from pointing: */  
			xsspr = (xfoc_resamp - s_mdb->AIM_FOCX) * (s_mdb->cdltx_foc/s_mdb->cdltx);
			ysspr = (yfoc_resamp - s_mdb->AIM_FOCY) * (s_mdb->cdlty_foc/s_mdb->cdlty);

			/* unroll: */
			rollxy(xsspr, ysspr, xref, yref,-s_obs->roll, -s_mdb->cdltx_foc, s_mdb->cdlty_foc, &xssr, &yssr);                              
			
			/* discrete sky coords: */
			xsky = s_mdb->crpxx_sky + xssr * (s_mdb->cdltx/s_mdb->cdltx_sky);
			ysky = s_mdb->crpxy_sky + yssr * (s_mdb->cdlty/s_mdb->cdlty_sky);
			xsky_pix = (int)(xsky + 0.5);
			ysky_pix = (int)(ysky + 0.5);

			xe[nn_ibspec_block] = xsky_pix;
			ye[nn_ibspec_block] = ysky_pix;
			ide[nn_ibspec_block] = id_det;
			pi_chans[nn_ibspec_block] = (long)ii + ib_firstchan;

			nn_ibspec_block++;
		    } /* end if photon_discard != 1 */ 
                } /* end loop over counts in each bin */
            } /* end loop over input energy channels */

            /* Remove pi_chan=-1 and resize xe, ye, and pi_chan arrays */
            int new_nn_ibspec_block = clean_pi_chan(&xe, &ye, &ide, &pi_chans, nn_ibspec_block);
            pi_discarded += nn_ibspec_block - new_nn_ibspec_block;
            alloc_tot = new_nn_ibspec_block;

            evt_time = calloc(alloc_tot, sizeof(double));
	    errstatus = timeset(exp_completed, exposure_block, 0.0, 0.0, nn_ibspec_block, evt_time, 0.0, MTstate);

	    /* Add the exposure time completed in the previous time block to each time element in this block */
	    /* for (int kk=0; kk<nn_ibspec_block; kk++)
	       evt_time[kk] += exp_completed; */

            firstrow = nev+1;
            firstelem = 1;

            colnum = 1;  /* column 1 contains time */
            fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, alloc_tot, evt_time, &fstat);

            colnum = 2;  /* column 2 contains X */
            fits_write_col(ounit, TINT, colnum, firstrow, firstelem, alloc_tot, xe, &fstat);

            colnum = 3;  /* column 3 contains Y */
            fits_write_col(ounit, TINT, colnum, firstrow, firstelem, alloc_tot, ye, &fstat);

            colnum = 4;  /* column 4 contains PI */
            fits_write_col(ounit, TLONG, colnum, firstrow, firstelem, alloc_tot, pi_chans, &fstat);

            /* column 5, the pileup flag, will be populated with zeroes. */

            colnum = 6;  /* column 6 contains PIXEL_DESIG */
            fits_write_col(ounit, TINT, colnum, firstrow, firstelem, alloc_tot, ide,  &fstat);

            free(xe);
            free(ye);
            free(ide);
            free(pi_chans);
            free(evt_time);
            free(rescale);
            free(int_ibspec);
            free(double_ibspec);

            /* update our place in the fits column */
            nev += alloc_tot;  /* contains counts for ALL sources */
            nev_block += alloc_tot;
	    nn_ibspec += nn_ibspec_block;

        } /* Done adding backgound events to this block */



	/* Sort the time values for this block, plus the previous 30 values */
	long first_index = nev - nev_block + 1;

	if (first_index > 30){
	    first_index -= 30;
	    nev_block += 30;
	}

	double * time_block = calloc(nev_block, sizeof(double));
	int * pixid_block = calloc(nev_block, sizeof(int));
	int * x_block = calloc(nev_block, sizeof(long));
	int * y_block = calloc(nev_block, sizeof(long));
	long * pi_block = calloc(nev_block, sizeof(long));

	/* Read only the section of values we assigned during this sub-exposure block, and sort by time */
	fits_read_col_dbl(ounit, 1, first_index, 1, nev_block, 0, time_block, NULL, &fstat);
	fits_read_col_int(ounit, 2, first_index, 1, nev_block, 0, x_block, NULL, &fstat);
	fits_read_col_int(ounit, 3, first_index, 1, nev_block, 0, y_block, NULL, &fstat);
	fits_read_col_lng(ounit, 4, first_index, 1, nev_block, 0, pi_block, NULL, &fstat);
	fits_read_col_int(ounit, 6, first_index, 1, nev_block, 0, pixid_block, NULL, &fstat);

	sort_arrays_DIIIL(time_block, x_block, y_block, pixid_block, pi_block, nev_block);

	/* Now overwrite the table section with the newly time-sorted values */
	fits_write_col(ounit, TDOUBLE, 1, first_index, 1, nev_block, time_block, &fstat);
	fits_write_col(ounit, TINT, 2, first_index, 1, nev_block, x_block, &fstat);
	fits_write_col(ounit, TINT, 3, first_index, 1, nev_block, y_block, &fstat);
	fits_write_col(ounit, TLONG, 4, first_index, 1, nev_block, pi_block, &fstat);
	fits_write_col(ounit, TINT, 6, first_index, 1, nev_block, pixid_block, &fstat);

	/* If we're doing pileup, call the flag_pileup routine using the newly sorted times. */
	if (s_obs->dtpileup > 0){
            if (fits_read_col_int(ounit, 6, first_index, 1, nev_block, 0, pixid_block, NULL, &fstat))
                printf("fstat = %d\n",fstat);

	    /* flag_pileup writes pileup flag to the output file */
            flag_pileup(ounit, nev_block, first_index, s_obs->dtpileup, time_block, pixid_block, &deadtime, &n_pileup, debug);
        }

	free(time_block);
	free(pixid_block);
	free(x_block);
	free(y_block);
	free(pi_block);

	/* Update the total exposure time we've completed */
	exp_completed += exposure_block;

    } /* end WHILE loop over exposure time increments */

    printf("\n...done processing sources.\n\n");

    /* Report rows with PI=-1 removedfrom output fits file */
    printf("\nRemoved %ld total (source+background) events that exceeded the PI channel array.\n\n",pi_discarded);

    /* Report if we did resampling of pixel sizes */
    if (skip_resampling) printf("Skipped resampling of one or more pixels because detector size was within 50%% of sky pixel size.\n\n");

    printf("Final number of:\n");
    printf("               source events = %10ld\n", (long)nn_source);
    printf("           background events = %10ld\n", (long)nn_ibspec);
    printf("         -------------------------------------\n");
    printf("                total events = %10ld\n", (long)nev);

    if (s_obs->dtpileup > 0){
	deadtime /= ( s_mdb->DET_XNPIX *  s_mdb->DET_YNPIX );
	pileup_frac = (double)n_pileup / (double)nev;
	deadtime_frac = deadtime / s_obs->exposure;
	printf("Number of piled-up source events = %ld\n", n_pileup);
	/* printf("Fraction of \"dead\" time lost to pileup = %le\n",deadtime_frac); */
    }
    printf("\n");

    free(xspec_energy);
    free(specin);
    free(specout);
    free(int_specout);
    free(double_ebin_mid);

    if (0 != strcasecmp(s_back->psbackfile,"none")){
        deallocate_torus(&s_tpar, &s_tspec);
    }

    if (fits_delete_col(ounit,6,&fstat))  /* Column 6 is pixel_id */
        printf("Failed to remove pixel_id column.  Fits error code %d\n",fstat);

    /* write a GTI extension */
    errstatus = write_GTI(ounit, s_obs);

    /* Write the checksums */
    update_checksums(ounit);

    /* close the output file again */
    fits_close_file(ounit, &fstat);


    return errstatus;
}






/* FUNCTION NAME: spec_scale                                                                       */
/*                                                                                                 */
/* CALLING SEQUENCE:                                                                               */
/*   result = spec_scale(en, spec, nebins, lower_limit, upper_limit, intspec);                     */
/*                                                                                                 */
/* PURPOSE:                                                                                        */
/*   Calculates the simpliest estimate of the flux in keV/cm^2/s (intspec) of a spectrum in        */
/*   photons/cm^2/s (spec) vs keV (en) over the bandpass [lower_limit, upper_limit] by summing     */
/*   spec*en over energy bins within the bandpass.  Previously called nrc_scale()                  */
/*                                                                                                 */
/* INPUTS:                                                                                         */
/*   en, spec, nebins, lower_limit, upper_limit                                                    */
/*                                                                                                 */
/* OUTPUTS:                                                                                        */
/*   intspec                                                                                       */
/*                                                                                                 */
/* CALLED BY:                                                                                      */
/*   spectra()                                                                                     */
/*                                                                                                 */
/* SUBROUTINES:                                                                                    */
/*   find_index()                                                                                  */
/*                                                                                                 */

int spec_scale(double *en,         /* spectrum energy grid midpoints in keV */
	      double *spec,        /* spectrum over en in photons/cm^2/s */
	      int nebin,           /* number of energy bins */
	      double lower_limit,  /* lower limit in keV of bandpass for flux calculations */
	      double upper_limit,  /* upper limit in keV of bandpass for flux calculations */
	      double *intspec){    /* flux in keV/cm^2/s of spec over bandpass */

    int ix_lo=0;         /* low value */
    int ix_hi=0;         /* high value */
    int idx=0;           /* index */
    int errstatus=0;   /* error status */

    *intspec = 0.0;

    if (lower_limit <= en[0]){
	ix_lo = 0;
    }else
	ix_lo = find_index(lower_limit, nebin, en);

    if (upper_limit >= en[nebin-1]){
	ix_hi = nebin-1;
    }else{
	ix_hi = find_index(upper_limit, nebin, en);
	/* subtract 1 to include only energies within the bandpass */
	ix_hi--;
    }

    if (ix_hi <= ix_lo){
	*intspec = 0.0;
    }else
	for (idx=ix_lo; idx <= ix_hi; idx++)
	    *intspec += (spec[idx]*en[idx]);
    
    return errstatus;

}



/* FUNCTION NAME: spec_scale_plaw                                                                  */
/*                                                                                                 */
/* CALLING SEQUENCE:                                                                               */
/*   errstatus = spec_scale_plaw(xspec_energy, specpar, nebin+1, lower_limit, upper_limit, &t);    */
/*                                                                                                 */
/* PURPOSE:                                                                                        */
/*   Analytically calculates the flux in keV/cm2/s  (intspec) of a power-law spectrum with the      */
/*   given index over the bandpass [lower_limit, upper_limit].                                     */
/*                                                                                                 */
/* INPUTS:                                                                                         */
/*   xspec_energy, specpar, nebin, lower_limit, upper_limit                                        */
/*                                                                                                 */
/* OUTPUTS:                                                                                        */
/*   intspec                                                                                       */
/*                                                                                                 */
/* CALLED BY:                                                                                      */
/*   spectra()                                                                                     */
/*                                                                                                 */
/* SUBROUTINES:                                                                                    */
/*   find_index()                                                                                  */
/*                                                                                                 */
int spec_scale_plaw(double * xspec_energy,   /* xspec spectrum energy grid in keV */
		    double index,            /* photon index */
		    int ne,                  /* number of xspec energy bins */
		    double lower_limit,      /* lower limit, in keV, of bandpass for flux calculation */
		    double upper_limit,      /* upper limit, in keV, of bandpass for flux calculation */
		    double * intspec){       /* flux in keV/cm2/s of spec over bandpass */
    
    double en_lo = 0.0;
    double en_hi = 0.0;
    int ix_lo = 0;
    int ix_hi = 0;
    double slope = 0.0;
    double factor = 0.0;

    /* Cannot go outside of instrument bandpass */ 
    en_lo = MAX(lower_limit, xspec_energy[0]);  
    en_hi = MIN(upper_limit, xspec_energy[ne-1]);

    /* The bins fully in the bandpass: ix_lo should always be >0, ix_hi always<nebins-1*/ 
    ix_lo = find_index(en_lo, ne, xspec_energy);
    ix_hi = find_index(en_hi, ne, xspec_energy) - 1;

    if (index != 1) {
	slope = 1 - index;
	factor = 1 / slope;
    } else {
	factor = 1.0;
    }

    /* Integrate power-law over each bin*/ 
    (*intspec) = 0.0;
    if (ix_hi > ix_lo) {
	int idx = 0;
	for (idx=ix_lo; idx <= ix_hi; idx++) {    
	    if (index != 1) {          
		(*intspec) += (pow(xspec_energy[idx+1],slope) - pow(xspec_energy[idx],slope));
	    }  else {
		/* Natural log*/ 
		(*intspec) += log(xspec_energy[idx+1]/xspec_energy[idx]);
	    }
	}
    }

    /* Correct for bins partially in bandpass*/ 
    if (en_lo < xspec_energy[ix_lo]) {
	if (index != 1) {          
	    (*intspec) += (pow(xspec_energy[ix_lo],slope) - pow(en_lo,slope));
	} else {
	    (*intspec) += log(xspec_energy[ix_lo]/en_lo);
	}
    }

    if (en_hi  > xspec_energy[ix_hi]) {
	if (index != 1) {          
	    (*intspec) += (pow(en_hi,slope) - pow(xspec_energy[ix_hi],slope));
	} else {
	    (*intspec) += log(en_hi/xspec_energy[ix_hi]);
	}
    }


    (*intspec) *= factor;

    return 0;
}
		    


/* FUNCTION NAME: spectra                                                                         */
/*                                                                                                */
/* CALLING SEQUENCE:                                                                              */
/*    errstatus = spectra(nh, zred, nhint, sptype, spar, fpar, bpass_lo, bpass_hi, sname, sform,  */
/*		    sun, nebin, double_ebin_mid, xspec_energy, specin, &flux_abs,                 */
/*		    &flux_unabs, &dflux_abs, &dflux_unabs, debug);                                */
/*                                                                                                */
/* PURPOSE:                                                                                       */
/*   The purpose of spectra is to calculate the input spectrum in photons/cm^2/sec from one of    */
/*   the supported standard models or from a user file.  The spectrum is used in creating the     */
/*   simulated event list in doWork.  This has been revised to include absorption, and            */
/*   accomodate the new source definition file format and spectral parameters (i.e. units).       */
/*   The spectral type, spectral parameters, and input energy scale (from the ARF) are input      */
/*   to produce the output spectrum in photons/cm^2/s vs keV in each bin on the input energy      */
/*   grid.  For model spectra, the input flux determines the model normalization; for user        */
/*   spectra the flux is determined by summing the spectrum.                                      */
/*                                                                                                */
/* INPUTS:                                                                                        */
/*   nh, zred, nhint, spectype, specpar, fluxpar, norm_lower, norm_upper, infile, sformat,        */
/*     sunits, nebin,  evin_mid, xspec_energy, debug                                              */
/*                                                                                                */
/* OUTPUTS:                                                                                       */
/*   spec, flux_abs, flux_unabs, dflux_abs, dflux_unabs                                           */
/*                                                                                                */
/* SUBROUTINES:                                                                                   */
/*   spec_scale(), spec_interp(), sort_arrays_DDD()                                               */
/*                                                                                                */
/* CALLED BY:                                                                                     */
/*   doWork()                                                                                     */
/*                                                                                                */

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
	    int debug){             /* debug flag */

    /* some converstion factors */
    double onekeVphoton2ergs = 1.60218e-9;    /* keV to erg */
    double eV2keV = 1.0e-03;                  /* eV to keV */
    double angstrom2keV = 12.39842;           /* angstrom to keV */

    int errstatus=0;        /* error status */
    int ifln = 1;           /* the spectrum number being calculated, always 1; required for Xspec model code */
    double xspwlw_param[1]; /* Xspec powerlaw params - ONLY ONE THAT SHOULD BE DOUBLE!*/
    float xsrays_param[3];  /* Xspec xsrays params */
    float xsblbd_param[1];  /* Xspec xsblbd params */
    float xsbrms_param[1];  /* Xspec xsbrms params */
    float xsgaul_param[2];  /* Xspec xsgaul params */
    float xszphb_param[2];  /* Xspec xsphab params */
    int i=0;                  /* loop index */

    double * specerr = 0;                 /* uncertainty on spec...needed by Xspec but we never use it */
    float * float_xspec_energy = 0;       /* float cast of xspec_energy */
    float * float_spec = 0;               /* float cast of spec */
    float * float_specerr = 0;            /* float cast of specerr */
    float * float_xsphab_trans_err = 0;   /* uncertainty on float_xsphab_trans */
    double * xspec_energy_file = 0;       
    float * float_xsphab_trans_fore = 0;
    float * float_xsphab_trans_intr = 0;

    double * avenergy = 0;   /* array of average energy - read from user spec file*/
    double * inspec = 0;     /* array of input spectra - read from user spec file */
    double * binlow = 0;     /* array of low bins */
    double * binhigh = 0;    /* array of high bins */
    double bintemp = 0.0;      /* temporary bin holder */
    double * de_half = 0;    /* half of bin - read from user spec file */
    double * delenergy = 0;  /* binhigh - binlow */

    FILE *fp;                   /* File to be read */
    char line[STR_MAX_LEN];     /* string line in file */
    int n_commentlines=0;       /* number of comment lines or spaces in file */
    int numlines=0;             /* number of valid source lines in file */
    int len=0;                    /* string length */
    int thisline=0;             /* loop index */
    double t=0.0;               /* flux t in keV/cm^2/sec */

    /* Allocate these here, deallocate at end of spectra */
    specerr = calloc(nebin, sizeof(double));  /* this is never used for anything... */
    float_xspec_energy = calloc( (nebin+1), sizeof(float));
    float_spec = calloc(nebin, sizeof(float)); /* will be converted to double later. */
    float_specerr = calloc(nebin, sizeof(float));

    for (i=0; i<=nebin; i++)
	float_xspec_energy[i] = (float)xspec_energy[i];

    /* Get the spectrum */
    if (1 == spectype){
	if (debug == 1) printf("  Spectra: Powerlaw\n");
	xspwlw_param[0] = (double)specpar;
	/* C_powerLaw() defined in Xspec/src/XSFunctions/funcWrapper.h */
	C_powerLaw(xspec_energy, nebin, xspwlw_param, ifln, spec, specerr, "pl");
    
    } else if (2 == spectype){
	if (debug == 1) printf("  Spectra: xsrays (raymond-smith) \n");
	xsrays_param[0] = (float)specpar;
	xsrays_param[1] = 1.0;
	xsrays_param[2] = 0.0;
	Xsrays(float_xspec_energy, nebin, xsrays_param, ifln, float_spec, float_specerr);
    
    } else if (3 == spectype){
	if (debug == 1) printf("  Spectra: xsblbd (blackbody) \n");
	xsblbd_param[0] = (float)specpar;
	Xsblbd(float_xspec_energy, nebin, xsblbd_param, ifln, float_spec, float_specerr);
	
    } else if (4 == spectype){
	if (debug == 1) printf("  Spectra: xsbrms (bremsstrahlung)\n");
	xsbrms_param[0] = (float)specpar;
	Xsbrms(float_xspec_energy, nebin, xsbrms_param, ifln, float_spec, float_specerr);

    } else if (5 == spectype){
	if (debug == 1) printf("  Spectra: xsgaul (mono-energetic)\n");
	xsgaul_param[0] = (float)specpar;
	xsgaul_param[1] = -0.1;
	Xsgaul(float_xspec_energy, nebin, xsgaul_param, ifln, float_spec, float_specerr);

    } else if (6 == spectype){
	int found = 0;
	int fstat = 0;

	/* CASE 6: THE USER SPECIFIES A SPECTRA FILE FOR INPUT */

	/* Complain if the user's spectra file does not exist. */
        fits_file_exists(infile, &found, &fstat);
        if ( (found == 2) || (found == 1) ){
	    if (debug == 1) printf("  Spectra: user file %s\n",infile);
        } else {
            printf("ERROR: Problem accessing user input spectra file: %s\n",infile);
	    printf("       You've chosen to input a user-defined spectra file for this source,\n");
	    printf("       but that file doesn't seem to exist.  Please check the source\n");
	    printf("       entry in insrcdeffile.\n");
	    printf("       Setting spectra to zero for this source, and continuing.\n");
	    free(specerr);
	    free(float_xspec_energy);
	    free(float_spec);
	    free(float_specerr);
	    for (int ii=0; ii<nebin; ii++)
		spec[ii] = 0.0;
	    return -1;
	}

	/* Use get_nsources to find the number of lines in the file.  Written for a different
	   application, but the algorithm suffices here. */
	numlines = get_nlines(infile,debug);

	/* Allocate here, remember to free!!! */
        avenergy = calloc(numlines, sizeof(double));
        inspec = calloc(numlines, sizeof(double));
        binlow = calloc(numlines, sizeof(double));
        binhigh = calloc(numlines, sizeof(double));
        de_half = calloc(numlines, sizeof(double));
        delenergy = calloc(numlines, sizeof(double));
	xspec_energy_file = calloc( (numlines+1), sizeof(double));

	fp = fopen(infile,"r");
	while(fgets(line,sizeof(line),fp) != NULL){

	    /* strip trailing '\n' if it exists */
	    len = strlen(line)-1;  if(line[len] == '\n') line[len] = 0;

	    /* strip white space at beginning and end of line */
	    trim_string(line);

	    /* Ignore commented, alpha character, or zero-length lines */
	    if ( (line[0] == '#') ||
		 (line[0] == '!') ||
		 (line[0] == '@') ||
		 (isalpha(line[0])) ||
		 (strlen(line) == 0) ||
		 (line[0] == ' ') ){
		n_commentlines++;

	    }else{

		if (sformat == 1){ /* there are two fields to read */
		    avenergy[thisline] = atof(strtok(line," \t"));
		    inspec[thisline] = atof(strtok(NULL," \t"));

		} else if (sformat == 2){ /* there are three fields to read */
		    avenergy[thisline] = atof(strtok(line," \t"));
		    de_half[thisline] = atof(strtok(NULL," \t"));
		    inspec[thisline] = atof(strtok(NULL," \t"));

		} else { /* we only allow sformat 1 or 2. */
		    printf("Error: sformat %d not supported.\n",sformat);
		    errstatus = -1;
		}
		thisline++;
	    }
	}
	fclose(fp);


        /* sort avenergy, then reorder de_half and inspec to match.  If sformat = 1,
           and de_half isn't populated, it still shouldn't matter, because the only
           sorting operation is being done on avenergy; we'll just be reordering garbage. */
        sort_arrays_DDD(avenergy, de_half, inspec, numlines);

	/* begin sformat conditional */
	if (sformat == 1){
	    /* derive the bin boundaries, with no gaps, from the midpoint energy grid.  Choose
	       the first bin lower boundary to avoid the possibility of a negative boundary
	       for the first bin lower boundary. */
	    binlow[0] = 1.5 * avenergy[0] - 0.5*avenergy[1];
	    if (binlow[0] < 0.0) binlow[0] = 0.5*avenergy[0];
	    
	    /* loop over input spectral file transformed energies */
	    for (i=1; i<numlines; i++){
		binlow[i] = 2.0 * avenergy[i-1]-binlow[i-1];
		binhigh[i-1] = binlow[i];
	    }
	    binhigh[i-1] = 2.0 * avenergy[i-1]-binhigh[i-2];

	} else if (sformat == 2){
	    for (i=0; i<numlines; i++){
		binhigh[i] = avenergy[i] + de_half[i];
		binlow[i] = avenergy[i] - de_half[i];
	    }
	} else{
	    /* this case should already have been caught above... */
	    printf("Error: sformat %d not supported.\n",sformat);
	    errstatus = -1;
	} /* done with sformat conditional */

	/* loop over input spectral file, transform binlow and binhigh to keV */
	for (i=0; i<numlines; i++){
	    if ( (sunits == 4) || (sunits == 7) ){
		binlow[i] *= eV2keV;
		binhigh[i] *= eV2keV;
	    } else if ( (sunits == 3) || (sunits == 6) || (sunits == 9) ){
		bintemp = binlow[i];
		binlow[i] = angstrom2keV / binhigh[i];
		binhigh[i] = angstrom2keV / bintemp;
	    }
	}

	/* Uncomment to check reversal:
	for (i=0; i<5; i++) printf("inspec[%d]: %f\n",i,inspec[i]);
	for (i=numlines-5; i<numlines; i++) printf("inspec[%d]: %f\n",i,inspec[i]);
	*/
	/* reverse the spectrum array order if versus wavelength so spectrum has increasing energy */
	if ( (sunits == 3) || (sunits == 6) || (sunits == 9) ){
		reverse(binlow,numlines);
		reverse(binhigh,numlines);
		reverse(inspec,numlines);
	}
	/*
        for (i=0; i<5; i++) printf("inspec[%d]: %f\n",i,inspec[i]);
        for (i=numlines-5; i<numlines; i++) printf("inspec[%d]: %f\n",i,inspec[i]);
	*/  

	for (i=0; i<numlines; i++){
	    avenergy[i] = (binhigh[i] + binlow[i])/2.0;
	    delenergy[i] = binhigh[i]-binlow[i];
	    xspec_energy_file[i] = binlow[i];

	    /* ergs to keV */
	    if (4 <= sunits) inspec[i] /= onekeVphoton2ergs;
	        
	    if (2 == sunits){
		inspec[i] *= delenergy[i];
	    } else if (3 == sunits){
		/*inspec[i] *= (angstrom2keV * delenergy[i]) / (avenergy[i]*avenergy[i]);*/
		inspec[i] *= (angstrom2keV * ((1.0/binlow[i]) - (1.0/binhigh[i])));
	    } else if (4 == sunits){
		inspec[i] *=  delenergy[i] / avenergy[i] / eV2keV;
	    } else if (5 == sunits){
		inspec[i] *= delenergy[i] /avenergy[i];
	    } else if (6 == sunits){
		/*inspec[i] *= angstrom2keV * delenergy[i] / avenergy[i] / avenergy[i] / avenergy[i];*/
		inspec[i] *= ((angstrom2keV / avenergy[i]) * ((1.0/binlow[i]) - (1.0/binhigh[i])));
	    } else if ( (sunits == 7) || (sunits == 8) || (sunits == 9) ){
		/* sunists 7,8,9 */
		inspec[i] *= avenergy[i];
	    }
	}

	xspec_energy_file[numlines] = binhigh[numlines-1];


	spec_interp(xspec_energy_file, numlines, inspec, xspec_energy, nebin, spec, debug);


	/* DONE WITH CASE 6 WHERE USER SPECIFIES SPECRA FILE FOR INPUT */
	free(avenergy);
	free(inspec);
	free(binlow);
	free(binhigh);
	free(de_half);
	free(delenergy);
	free(xspec_energy_file);

    } else {
	printf("Error: Spectype not supported.\n");
	errstatus = -1;
    }
    /* end of spectype conditional */


    /* now take care of float-to-double cast */
    if ( (spectype == 2) ||
	 (spectype == 3) ||
	 (spectype == 4) ||
	 (spectype == 5) ){
	for (i=0; i<nebin; i++)
	    spec[i] = (double)float_spec[i];
    }

    /* if xspec_energy was zero, then spec may have erroneous infinities... */
    for (i=0; i<nebin; i++)
	if (spec[i] == 1./0.)
	    spec[i] = 0.0;
    /*
    for (i=0; i<10; i++)
	printf("spec[%d] = %e\n",i,spec[i]);
    for (i=nebin-10; i<nebin; i++)
        printf("spec[%d] = %e\n",i,spec[i]);
    */

    /* calculate the flux in erg/cm2/sec in spec between norm_lower_limit , norm_upper_limit
       if a user spectrum this is the flux over the bandpass in the source definition file; if a model  
       spectrum the flux is an (fixed) input parameter and the  spectrum must be renormalized.
       These are unabsorbed fluxes: */
    
    if (6 == spectype){
	norm_lower = ebin_mid[0];
	norm_upper = ebin_mid[nebin-1];
    }

    /* calculate the flux t in keV/cm^2/sec, convert to cgs */
    if (1 == spectype){
    	// FOR NOW, DON'T USE THIS errstatus = spec_scale_plaw(xspec_energy, specpar, nebin+1, norm_lower, norm_upper, &t);
	errstatus = spec_scale(ebin_mid, spec, nebin, norm_lower, norm_upper, &t);
    } else {
	errstatus = spec_scale(ebin_mid, spec, nebin, norm_lower, norm_upper, &t);
    }
    
    t *= onekeVphoton2ergs;
    if (6 == spectype){
	*flux_unabs = t;
    } else {
	*flux_unabs = fluxpar;
	for (i=0; i<nebin; i++)
	    if (t > 0){
		spec[i] *= *flux_unabs / t;
	    } else {
		spec[i] = 0.0;
	    }
    }

    if (debug == 1){
	printf("  t = %e\n",t);
	printf("  flux_unabsorbed: %e\n",*flux_unabs);
    }

    /* Since the absorbed spectrum is to be returned, this is the place to calculate the
       unabsorbed flux over the entire detector bandpass. */
    errstatus = spec_scale(ebin_mid, spec, nebin, ebin_mid[0], ebin_mid[nebin-1], &t);
    *dflux_unabs = t*onekeVphoton2ergs;
    if (debug == 1) printf("  dflux_unabsorbed: %e\n",*dflux_unabs);

    /* Now absorb the spectrum and calculate absorbed flux. */

    /* initialize transmissions and spectra */
    float_xsphab_trans_fore = calloc(nebin, sizeof(double));
    float_xsphab_trans_intr = calloc(nebin, sizeof(double));
    float_xsphab_trans_err = calloc(nebin, sizeof(float));

    for (int iebin= 0; iebin< nebin; iebin++) { 
        float_xsphab_trans_fore[iebin]=1.0; 
        float_xsphab_trans_intr[iebin]=1.0;
    }

    if (nh > 0){  
	xszphb_param[0] = (float)(nh/1.0e22);
	xszphb_param[1] = (float)(0.0);
	Xszphb(float_xspec_energy, nebin, xszphb_param, ifln, float_xsphab_trans_fore, float_xsphab_trans_err);
    }

    if (nhint > 0){
	xszphb_param[0] = (float) (nhint/ 1.0e+22);    
	xszphb_param[1] = (float) (zred);
	Xszphb(float_xspec_energy, nebin, xszphb_param, ifln, float_xsphab_trans_intr,float_xsphab_trans_err);
    }

    /* absorb the spectrum and calculate the absorbed flux over the detector bandpass */
    for (int ii=0; ii<nebin; ii++)
	spec[ii] *= (double)float_xsphab_trans_fore[ii] * (double)float_xsphab_trans_intr[ii];
    
    
    errstatus = spec_scale(ebin_mid, spec, nebin, ebin_mid[0], ebin_mid[nebin-1], &t);
    *dflux_abs = t * onekeVphoton2ergs;

    errstatus = spec_scale(ebin_mid, spec, nebin, norm_lower, norm_upper, &t);
    *flux_abs = t * onekeVphoton2ergs;

    if (debug == 1){
	printf("  flux_absorbed: %e\n",*flux_abs);
	printf("  dflux_absorbed: %e\n",*dflux_abs);
    }

    free(specerr);
    free(float_specerr);
    free(float_xspec_energy);
    free(float_spec);
    free(float_xsphab_trans_fore);
    free(float_xsphab_trans_intr);
    free(float_xsphab_trans_err);
    
    return errstatus;
}



/* FUNCTION NAME: spec_interp                                                                     */
/*                                                                                                */
/* CALLING SEQUENCE:                                                                              */
/*   spec_interp(xspec_energy_file, numlines, inspec, xspec_energy, nebin, spec);                 */
/*                                                                                                */
/* PURPOSE:                                                                                       */
/*   spec_interp interpolates inspec, defined on an energy grid                                   */
/*   with nebin_in+1 bin boundaries at xspec_energy_in onto xspec_energy_out with nebin_out+1     */
/*   bin boundaries - producing output spectrum outspec.                                          */
/*                                                                                                */
/* INPUTS:                                                                                        */
/*  xspec_energy_in, nebin_in, inspec, xspec_energy_out, nebin_out                                */
/*                                                                                                */
/* OUTPUTS:                                                                                       */
/*   outspec                                                                                      */
/*                                                                                                */
/* SUBROUTINES:                                                                                   */
/*   interpol_hunt()                                                                              */
/*                                                                                                */
/* CALLED BY:                                                                                     */
/*   spectra()                                                                                    */
/*                                                                                                */


void spec_interp(double * xspec_energy_in,   /* vector of energy grid boundaries for inspec */
		 int nebin_in,                /* number of energy bins in inspec */
		 double * inspec,             /* spectrum to be interpolated */
		 double * xspec_energy_out,   /* vector of energy grid boundaries in spectrum to be evaluated */
		 int nebin_out,               /* number of energy bins in spectrum to be evaluated */ 
		 double * outspec,           /* spectrum to be evaluated */
		 int debug){

    int i=0;               /* loop index */
    double * binlo_out=0;  /* array of low bins */
    double * binhi_out=0;  /* array of high bins */
    double * int_spec=0;   /* interpolated spectrum */   
    double intspec_hi=0;   /* int_spec high value */
    double intspec_lo=0;   /* int_spec low value */
    long zero_bins=0;    /* counter for bins assinged zero flux */
    long nonzero_bins=0; /* counter for bins assigned nonzero flux */

    if (debug) printf("Running spec_interp...\n");

    binlo_out = calloc(nebin_out, sizeof(double));
    binhi_out = calloc(nebin_out, sizeof(double));

    if (debug) printf("nebin_in = %d\n",nebin_in);
    
    /* outspec energy grid bin boundaries. xspec_energy_out is calloc'ed to size nebin+1 in main (as
       xspec_energy), passed down through spectra. */
    for (i=0; i<nebin_out; i++){
	binlo_out[i] = xspec_energy_out[i];
	binhi_out[i] = xspec_energy_out[i+1];
	/* printf("delbin: %f\n",binhi_out[i]-binlo_out[i]); */
    }
    int_spec = calloc( (nebin_in+1), sizeof(double));
    /* cumulative flux on xspec_energy_in */
    int_spec[0] = 0.0;
    for (i=1; i<=nebin_in; i++) int_spec[i] = inspec[i-1];
    for (i=1; i<=nebin_in; i++) int_spec[i] += int_spec[i-1];

    /* Note that binhi_out and binlo_out trace back to the ARF energy range, while
       xspec_energy_in traces back to the user input file.  */

    if (debug) printf("Input spectrum energy range: %f-%f.  Energy bins outside this range will be assigned zero flux.\n",
	   xspec_energy_in[0], xspec_energy_in[nebin_in]);

    for (i=0; i<nebin_out; i++){
	if ( (binhi_out[i] <= xspec_energy_in[0]) ||
	     (binlo_out[i] >= xspec_energy_in[nebin_in]) ){
	    outspec[i] = 0.0; /* no flux outside the bounds of the input spectrum */
	    zero_bins++;
	    
	    /*printf("   Attempted bin energy range: %f-%f.  Zero flux assigned.\n",binlo_out[i],binhi_out[i]);*/

	} else {
	    /* actual interpolation is done with interpol_hunt() */
	    intspec_lo = 0.0;
	    if (binlo_out[i] >= xspec_energy_in[0])
		intspec_lo = interpol_hunt(nebin_in+1, xspec_energy_in, int_spec, binlo_out[i]);

	    intspec_hi = int_spec[nebin_in];
	    if (binhi_out[i] <= xspec_energy_in[nebin_in])
		intspec_hi = interpol_hunt(nebin_in+1, xspec_energy_in, int_spec, binhi_out[i]);
	    outspec[i] = (intspec_hi - intspec_lo);

	    nonzero_bins++;
	    /*
	    if (i<10)
		printf(" %f <= %f <= %f <= %f\n",
		       xspec_energy_in[0], binlo_out[i],binhi_out[i],xspec_energy_in[nebin_in]);
	    if (i>nebin_out-10)
		printf(" %f <= %f <= %f <= %f\n",
		       xspec_energy_in[0], binlo_out[i],binhi_out[i],xspec_energy_in[nebin_in]);
	    */
	}
    }    

    if (debug) printf("   Assigned zero flux to %ld bins, nonzero flux to %ld bins.\n",zero_bins,nonzero_bins);
    
    free(binlo_out);
    free(binhi_out);
    free(int_spec);
}




/* FUNCTION NAME: specmod                                                                         */
/*                                                                                                */
/* CALLING SEQUENCE:                                                                              */
/*   specmod(specout, nebin, MTstate);                                                            */
/*                                                                                                */
/* PURPOSE:                                                                                       */
/*   specmod uses Poisson statistics to get integer values for the                                */
/*   spectrum when the number of model counts in a bin is low.                                    */
/*                                                                                                */
/* INPUTS:                                                                                        */
/*   Nspec                                                                                        */
/*                                                                                                */
/* INPUTS/OUTPUTS:                                                                                */
/*   specin is modified                                                                           */
/*                                                                                                */
/* SUBROUTINES:                                                                                   */
/*   poidev()                                                                                     */
/*                                                                                                */
/* CALLED BY:                                                                                     */
/*   doWork()                                                                                     */
/*                                                                                                */

void specmod(double *specin,   /* the flux array of the spectrum */
	     int Nspec,        /* number of channels in the spectrum */
	     HDmt_state *MTstate){      /* "save" state of Mersenne Twister RNG */

    int ii;      /* loop index */
    double mean; /* arithmetic mean */

    for (ii=0;ii<Nspec;ii++) {

        /* If the input spectrum has less than 10 counts in a bin, use Poisson statistics */
	/*   to get a value.  This is obviously much more important for values less than 1. */

        if (specin[ii] < 10) {
            mean = specin[ii];
	    /* poidev uses the Mersenne Twister random number generator */
            specin[ii] = poidev((double)mean, MTstate);
        }
    }
}




/* FUNCTION NAME: timeset                                                                         */
/*                                                                                                */
/* CALLING SEQUENCE:                                                                              */
/*   timeset(exposure,per,pfrac,nn_source,evt_time,MTstate);                                      */
/*                                                                                                */
/* PURPOSE:                                                                                       */
/*   Assign times, according to the temporal characteristics of a source, to nn events and        */
/*   returns these in a one-dimensional array (evt_tim) of size nn.  Currently, only constant     */
/*   and sinusoidally varying sources are supported.                                              */
/*                                                                                                */
/* INPUTS:                                                                                        */
/*   nn, period, pulsed, expose,                                                                  */
/*                                                                                                */
/* OUTPUTS:                                                                                       */
/*   evt_time                                                                                     */
/*                                                                                                */
/* NOTES:                                                                                         */
/*   Modified from original version to use Mersenne Twister random generator.                     */
/*                                                                                                */
/* SUBROUTINES: MTseed()                                                                          */
/*                                                                                                */
/* CALLED BY:                                                                                     */
/*   doWork()                                                                                     */
/*                                                                                                */


int timeset(double tstart,    /* start time for this exposure or subexposure, aka exp_completed */
	    double expose,    /* duration of this exposure or subexposure, aka exposure_block */
	    double period,      /* period of optional variation in seconds */
	    double pulsed,      /* amplitude of variation */
	    long nn,             /* number of events */
	    double *evt_time,   /* array of times assigned to events */
	    double phase_in,   /* initial phase within this exposure block */
	    HDmt_state *MTstate){  /* "save" state of Mersenne Twister RNG */

    long i=0;                        /* loop index */
    double small_number = 1.0e-9; /* a small number */
    double omega=0;                 /* frequency */
    long ncycle=0;                   /* number of cycles included in expose */
    double cosphase=0;              /* cosine of phase */
    long ntable=0;                   /* table size is ntable+1 */
    double dt_table=0;              /* table spacing */
    long itable=0;                   /* table loop index */
    double * table_time=0;          /* lookup time in table */
    double * table_phase=0;         /* lookup phase in table */
    double * table_x=0;             /* uniform distribution in table */
    long itim=0;                   /* time index */
    double x=0;                     /* a random number on uniform unit interval*/
    long icycle=0;                   /* a random number between 1 and ncycle */
    long table_ind=0;                /* table index */
    double weight1=0, weight2=0;      /* interpolation weights */
    double time_int=0;              /* interpolation result */
    double timex=0;                 /* phased interpolation result */


    /* if the source is constant */
    if ( (period == 0.0) || (pulsed < small_number) ){
	for (i=0; i<nn; i++){
	    evt_time[i] = tstart + expose * HDmt_drand(MTstate);
	}
    } else {
	/* if the source is periodic */
	omega = TWOPI / period;
	cosphase = cos(phase_in);
	ntable = 1000;
	dt_table = 1.0 / (double)ntable;
	ncycle = floor(expose/period) +1;

	/* allocate the needed arrays */
	table_time = calloc( (ntable+1), sizeof(double));
	table_phase = calloc( (ntable+1), sizeof(double));
	table_x = calloc( (ntable+1), sizeof(double));
	
	/* lookup table, mapping uniform distribution (table_x) onto lightcurve (table_time) for a single cycle */
	for (itable = 0; itable<=ntable; itable++){
	    table_time[itable] = dt_table * (double)itable;
	    table_phase[itable] = TWOPI * table_time[itable];
	    table_x[itable] = (table_phase[itable] + pulsed * (cosphase - cos(table_phase[itable] + phase_in)))/TWOPI;
	}

	/* time assignment */
	while (itim < nn){
	    icycle = (int)(1 + HDmt_drand(MTstate) * (double)(ncycle));
	    x = HDmt_drand(MTstate);
	    table_ind = find_index(x, ntable, table_x);
	    if (table_ind < 1) table_ind=1;
	    if (table_ind > ntable) table_ind=ntable;
	    weight1 = (table_x[table_ind] - x) / (table_x[table_ind] - table_x[table_ind-1]);
	    weight2 = (x - table_x[table_ind-1]) / (table_x[table_ind] - table_x[table_ind-1]);
	    time_int = table_time[table_ind] * weight2 + table_time[table_ind-1]*weight1;

	    /* phase the interpolation result */
	    timex = period * (icycle-1) + period * time_int;
	    if (timex <= expose){
		evt_time[itim] = tstart + timex;
		itim++;
	    }else
		continue;
	}

	/* clean up */
	free(table_time);
	free(table_phase);
	free(table_x);
	
    }

    return 0;
} /* end subroutine timeset */


/* FUNCTION NAME: timeset_burst                                                                   */
/*                                                                                                */
/* CALLING SEQUENCE:                                                                              */
/*   timeset_burst(nn, expose, ratburst, tburst, trise, tdecay, evt_time, MTstate)                */
/*                                                                                                */
/* PURPOSE:                                                                                       */
/*    Assign times, according to the light-curve of a single burst with linear risetime and       */
/*    exponential decay, to nn events and returns these in a one-dimensional array (evt_time)     */
/*    of size nn. This algorithm has been tested in sxsbranch (and in python), and works by       */
/*    mapping the lightcurve onto a uniform distribution using the integral of the lightcurve.    */
/*    Because the light curve is only piecewise-continuous, a lookup table is employed. This      */
/*    algorithm is more complex than that in sxsbranch - the intervals in the table, rather       */
/*    than uniformly spaced, vary to better resolve the decay and (especially) rise times.        */
/*                                                                                                */
/* INPUTS:                                                                                        */
/*   nn, expose, ratburst, tburst, trise, MTstate                                                 */
/*                                                                                                */
/* OUTPUTS:                                                                                       */
/*   evt_time                                                                                     */
/*                                                                                                */
/*                                                                                                */
/* SUBROUTINES: MTseed()                                                                          */
/*                                                                                                */
/* CALLED BY:                                                                                     */
/*   doWork()                                                                                     */
/*                                                                                                */

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
		  HDmt_state *MTstate){   /* "save" state of Mersenne Twister RNG */

    double small_number = 1.0e-9;
    int ii = 0;

    double tstop = tstart + tblock;

    if ( (ratburst < small_number) || (tburst >= tstop) ) {
	/* constant */
	for (ii=0; ii<nn; ii++) 
	    evt_time[ii] = tstart + tblock * HDmt_drand(MTstate);

    } else {
	double tbeg = 0.0;
	double tend = 0.0;
	int jbeg=0;
	int jend=0;
	double xbeg = 0.0;
	double xend = 0.0;
	double weight1 = 0.0;
	double weight2 = 0.0;
	double delx = 0.0;

	/* First, trim the table to the interval of interest */
	tbeg = tstart / expose; /*normalized time at beginning of interval */
	tend = tstop / expose; /*normalized time at end of interval */
	jbeg = find_index(tbeg, ntable+1, table_time) - 1; /*first table entry outside interval */
	if (jbeg < 1) jbeg=1;

	/* Interpolate to get the cumulative probabilities at the endpoint of the subexposure*/
	weight1 = (table_time[jbeg] - tbeg)/(table_time[jbeg] - table_time[jbeg-1]);
	weight2 = (tbeg - table_time[jbeg-1])/(table_time[jbeg] - table_time[jbeg-1]) ;
	xbeg =  table_x[jbeg] * weight2 + table_x[jbeg-1] * weight1;
	jend = find_index(tend, ntable+1, table_time); /*last table entry outside interval */
	if (jend > ntable) jend=ntable;
	weight1 = (table_time[jend] - tend)/(table_time[jend] - table_time[jend-1]);
	weight2 = (tend - table_time[jend-1])/(table_time[jend] - table_time[jend-1]);
	xend =  table_x[jend] * weight2 + table_x[jend-1] * weight1;

	/* the probabilities spanned by this interval:*/
	/* xbeg = table_x[jbeg]; */
	/* xend = table_x[jend]; */
	delx = xend  - xbeg; /* the probability of being somewhere in this interval*/

	/* Now, assign time */ 
	int itim = 0;
	while (itim < nn) {
	    /* random number on uniform probability interval in this time interval: */
	    double x = xbeg + delx * HDmt_drand(MTstate); 
	    int table_ind = find_index(x, ntable+1, table_x);
	    if (table_ind < 1) table_ind=1;
	    if (table_ind > ntable) table_ind=ntable;
	    weight1 = (table_x[table_ind] - x)/(table_x[table_ind] - table_x[table_ind-1]);
	    weight2 = (x - table_x[table_ind-1])/(table_x[table_ind] - table_x[table_ind-1]); 
	    double time_int = table_time[table_ind] * weight2 + table_time[table_ind-1] * weight1;
	    if (time_int <= 1.0) {
		evt_time[itim] = expose * time_int;
		itim = itim +1;
	    } else { 
		continue;
	    }
	}/* end while loop */
    } /* end constant/burst conditional */
    return 0;
} /* end timeset_burst routine */




/* FUNCTION NAME: process_image                                                      */
/*                                                                                   */
/* CALLING SEQUENCE:                                                                 */
/*   errstatus = process_image(iname,sdmat[1],sdmat[2],sdmat[3],sdmat[4], &crvl1_img,*/ 
/*			     &crvl2_img, &crpx1_img, &crpx2_img, &cdlt1_img,         */
/*			     &cdlt2_img, &nel_subimg, &cprob, debug);                */
/*                                                                                   */
/* PURPOSE:                                                                          */
/*    The purpose of process_image() is to prepare an input image (in sky            */
/*    coordinates) to be used to distribute input photons on the sky for             */
/*    subsequent simulation in doWork imagedis(), according to the telescope         */
/*    and detector characteristics.  This revised approach treats the input          */
/*    image as a probability distribution in parallel to the way that other          */
/*    extended source distributions are treated.  Thus, as the code loops            */
/*    over counts in the input spectrum it places photons in the sky with a          */
/*    probability proportional to the intensity of the image (more likely to         */
/*    place it where the image is brighter, etc.).                                   */
/*                                                                                   */
/* INPUTS: ifilename, xmin, xmax, ymin, ymax                                         */
/*                                                                                   */
/* OUTPUTS: crvl1_img, crvl2_img, crpx1_img, crpx2_img, cdlt1_img, cdlt2_img,        */
/*             nel_subimg, cprob                                                     */
/*                                                                                   */
/* CALLED BY: doWork()                                                               */
/*                                                                                   */

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
                  double *** cprob,   /* nel_subimage x 3 array with cumulative probabily and corresponding x/y pixel values */
		  int debug){

    fitsfile * iunit = NULL;
    int fstat=0;
    char * com = NULL;
    long naxis1=0, naxis2=0, nx=0, ny=0, num_AXIS=0;
    double crval1=0, crval2=0, cdelt1=0, cdelt2=0, crpix1=0, crpix2=0;
    int imx1=0, imy1=0, imx2=0, imy2=0;
    long naxes[10] = {0};
    int simple=0, bitpix=0, naxis=0;
    long pcount=0, gcount=0;
    int extend=0, anyf=0;
    double * image_list=0;    
    double imtot=0;
    int isub=0;
    int *imx_list=0, *imy_list=0;
    double * image_value=0;
    int imx=0, imy=0;
    int index=0;
    int ii=0;
    double * prob=0;
    double sumP=0;

    int HDU_num=1;
    int HDU_TYPE=-1;
    char * HDU=0;
    char tempstring[STR_MAX_LEN];
    char * extnum=0;
    char * xten=0;

    printf("\nProcessing image...\n");

    /* 0. determine if ifilename is given with a HDU image extention */
    /* 1. open ifilename */
    /* 2. move to image extention.  If no extention given, then assume image in primary HDU. */
    /* 3. confirm that it is an image. */
    /* 4. read naxis1, naxis2, crval2, crval2, crpix1, crpix2, cdelt1, cdelt2 from the header (fits_read_imghdr) */
    
    /* parse the filename, see if HDU extension is given, otherwise = 1 */
    HDU_num = 1;
    strcpy(tempstring,ifilename);
    ifilename = strtok(tempstring,"[");
    extnum = strtok(NULL,"]");
    if (extnum != NULL)
	HDU_num = atoi(extnum);
    printf("  Image file: %s  \n",ifilename);
    if (debug == 1) printf("  Expect image in HDU header %d\n",HDU_num);


    /* try to open the fits file */
    fits_open_file(&iunit, ifilename, READONLY, &fstat);
    if (fstat > 0) {
        printf("ERROR: Failed to open user input image file %s.\n",ifilename);
        return(fstat);
    }

    /* move to HDU_num */
    fits_movabs_hdu(iunit, HDU_num, &HDU_TYPE, &fstat);
    if (HDU_TYPE == IMAGE_HDU){	HDU="IMAGE_HDU";}
    else if (HDU_TYPE == ASCII_TBL) { HDU="ASCII_TBL"; }
    else if (HDU_TYPE == BINARY_TBL) {HDU="BINARY_TBL"; } 
    else { HDU="unknown"; }
    if (debug == 1) printf("  Moving to HDU %d, type %s.\n",HDU_num,HDU);
    if (fstat > 0) {
        printf("  ERROR: Failed to locate IMAGE HDU in image file %s.\n",ifilename);
	printf("       Are you certain this is a FITS image file?\n");
        return(fstat);
    }

    /* If not in the primary HDU, check that XTENSION keyword is "IMAGE" */
    if (HDU_num > 1){
	fits_read_key_longstr(iunit, "XTENSION", &xten, com, &fstat);
	if (debug == 1) printf("  Checking XTENSION keyword: %s\n",xten);
	if (0 != strcasecmp(xten,"IMAGE")){
	    printf("  ERROR! Image XTENSION is not IMAGE, cannot read!\n");
	    fstat = -1;
	    free(xten);  /* fits_read_key_longstr allocates this, need to free. */
	    return(fstat);
	} else {
	    free(xten);
	}
    } else {
	if (debug == 1) printf("  Image should be found in primary HDU.\n");
    }

    /* Check that NAXIS keyword is 2.  Otherwise it's impossible that this is an image. */
    fits_read_key_lng(iunit, "NAXIS", &num_AXIS, com, &fstat);
    if (num_AXIS != 2){
	printf("  ERROR! NAXIS keyword indicates dimensions other than 2!  Not an image!\n");
	fstat = -1;
	return(fstat);
    } else {
	if (debug == 1) printf("  Image NAXIS keyword confirmed at 2 dimensions.\n");
    }

    fits_read_imghdr(iunit, 10, &simple, &bitpix, &naxis, naxes, &pcount, &gcount, &extend, &fstat);
    naxis1 = naxes[0];
    naxis2 = naxes[1];

    if (debug == 1){
	printf("  NAXIS1 = %ld\n", naxes[0]);
	printf("  NAXIS2 = %ld\n", naxes[1]);
    }

    /* Read the remaining keywords.  Report values, or print error message if there's a problem. */
    fits_read_key_dbl(iunit, "CRVAL1", &crval1, com, &fstat);
    if (fstat > 0) { printf("Failed to read CRVAL1.\n"); return fstat; } else { if (debug == 1) printf("  CRVAL1 = %f\n",crval1); }
    
    fits_read_key_dbl(iunit, "CRVAL2", &crval2, com, &fstat);
    if (fstat > 0) { printf("Failed to read CRVAL2.\n"); return fstat; } else { if (debug == 1) printf("  CRVAL2 = %f\n",crval2); }
    
    fits_read_key_dbl(iunit, "CDELT1", &cdelt1, com, &fstat);
    if (fstat > 0) { printf("Failed to read CDELT1.\n"); return fstat; } else { if (debug == 1) printf("  CDELT1 = %f\n",cdelt1); }
    
    fits_read_key_dbl(iunit, "CDELT2", &cdelt2, com, &fstat);
    if (fstat > 0) { printf("Failed to read CDELT2.\n"); return fstat; } else { if (debug == 1) printf("  CDELT2 = %f\n",cdelt2); }
    
    fits_read_key_dbl(iunit, "CRPIX1", &crpix1, com, &fstat);
    if (fstat > 0) { printf("Failed to read CRPIX1.\n"); return fstat; } else { if (debug == 1) printf("  CRPIX1 = %f\n",crpix1); }
    
    fits_read_key_dbl(iunit, "CRPIX2", &crpix2, com, &fstat);
    if (fstat > 0) { printf("Failed to read CRPIX2.\n"); return fstat; } else { if (debug == 1) printf("  CRPIX2 = %f\n",crpix2); }
    
    
    /* number of columns and rows in image; pixels numbers ([1,nx],[1,ny]) */
    nx = naxis1;
    ny = naxis2;

    /* needed for converting to simulator coordinates */
    *crvl1_img = (double)crval1;
    *crvl2_img = (double)crval2;
    *crpx1_img = (double)crpix1;
    *crpx2_img = (double)crpix2;
    *cdlt1_img = (double)cdelt1;
    *cdlt2_img = (double)cdelt2;

    /* Set subimage boundaries; whole image if xmin=xmax=ymin=ymax=0 */
    if ( (xmin <= 0) && (xmax <= 0) && (ymin <= 0) && (ymax <= 0) ){
	imx1 = 1;
	imy1 = 1;
	imx2 = nx;
	imy2 = ny;
    } else {
	if (xmin < 1) { imx1 = 1; } else { imx1 = xmin; }
	if (xmax > nx) { imx2 = nx; } else { imx2 = xmax; }
	if (ymin < 1) { imy1 = 1; } else { imy1 = ymin; }
	if (ymax > ny) { imy2 = ny; } else { imy2 = ymax; }
    }

    /* number of subimage elements */
    *nel_subimg = (imx2 - imx1 + 1) * (imy2 - imy1 + 1);
    if (debug == 1) printf("  process_image: nel_subimg = %d\n",*nel_subimg);

    /* Read the entire image into the 1D array image_list with nx*ny elements.*/
    /* The 1D array image_list should be related to the 2D array image by:
       image[i,j] = image_list[(j-1)*nx+i-1] */
    image_list = calloc(nx * ny, sizeof(double));
    fits_read_2d_dbl(iunit, 0, 0, naxes[0], naxes[0], naxes[1], image_list, &anyf, &fstat);
    
    /* Set up for cumulative probability matrix.  First, normalize image list (sum, then divide
       each pixel by total subimage counts.) */
    imtot = 0.0;
    isub = 0;

    imx_list = calloc( *nel_subimg, sizeof(int));
    imy_list = calloc( *nel_subimg, sizeof(int));
    image_value = calloc( *nel_subimg, sizeof(double));

    for (imx=imx1; imx<imx2+1; imx++){ /* loop over subimage columns */
	for (imy=imy1; imy<imy2+1; imy++){ /* loop over subimage rows */
	    index = (imy-1) * nx + imx-1;  /* index in original image list */
	    
	    /* keep original image pixel coords */
	    imx_list[isub] = imx;
	    imy_list[isub] = imy;
    
	    /* put original image pixel intensity into subimage probability */
	    if (image_list[index] > 0){
		image_value[isub] = (double)image_list[index];
		imtot += image_value[isub];
	    } else {
		image_value[isub] = 0.0;
	    }

	    isub++;  /* increment subimage counter */

	} /* end loop over subimage rows */
    } /* end loop over subimage columns */

    /* normalize the subimage probability */
    prob = calloc(*nel_subimg, sizeof(double));
    for (ii=0; ii<*nel_subimg; ii++)
	prob[ii] = image_value[ii]/imtot;

    /* check: is the sum of normalized probability 1.0? */
    sumP=0.;
    for (ii=0; ii<*nel_subimg; ii++)
	sumP += (double)prob[ii];
    if (debug == 1) printf("  Normalized prob total: %f\n",sumP);
    
    /* sort prob, imx_list, and imy_list together, according to prob */
    /* Just use unsorted straightforward prob for now. */
    /* sort_arrays_DII(prob, imx_list, imy_list, *nel_subimg); */

    /* use calloc to zero-initialize cprob to a nel_subimg x 3 matrix */
    *cprob = (double **)calloc(*nel_subimg, sizeof(double *));
    for (ii=0; ii<*nel_subimg; ii++)
	(*cprob)[ii] = (double *)calloc(3, sizeof(double));

    /* populate the first row of cprob[][] with prob[] */
    for (ii=0; ii<*nel_subimg; ii++)
	(*cprob)[ii][0] = prob[ii];

    /* now modify the first row of cprob[][] to be a running total of itself,
       and set the second and third rows to be imx_list and imy_list. */
    for (ii=0; ii<*nel_subimg; ii++){
	if (ii > 0)  /* skip the 0th element - can't total itself */
	    (*cprob)[ii][0] += (*cprob)[ii-1][0];
	(*cprob)[ii][1] = imx_list[ii];
	(*cprob)[ii][2] = imy_list[ii];
    }

    /* close fits file */
    fits_close_file(iunit, &fstat);

    free(image_list);
    free(imx_list);
    free(imy_list);
    free(image_value);
    free(prob);

    printf("...done processing image.\n\n");
    return fstat;
}


/* FUNCTION NAME:  imagedis                                                            */
/*                                                                                     */
/* PURPOSE:                                                                            */
/*   Determine the initial position on the sky of npos source photons before being     */
/*   affected by the telescope and detector, base on an input image distribution,      */
/*   treating the image as a probability distribution.                                 */
/*                                                                                     */
/* CALLING SEQUENCE:                                                                   */
/*   errstatus = imagedis(crvl1_img, crvl2_img, crpx1_img, crpx2_img, cdlt1_img,       */
/*                          cdlt2_img, nel_subimg, cprob, cprob_vec, ra_img, dec_img,  */
/*                          MTstate);                                                  */
/*                                                                                     */
/* INPUTS: crvl1_img, crvl2_img, crpx1_img, crpx2_img, cdlt1_img, cdlt2_img,           */
/*           nel_subimg, cprob, npos                                                   */
/*                                                                                     */
/* OUTPUTS: ra_img, dec_img                                                            */
/*                                                                                     */
/* SUBROUTINES:  rotpol(), MTseed()                                                    */
/*                                                                                     */
/* CALLED BY:  doWork()                                                                */
/*                                                                                     */

int imagedis(double crvl1_img,    /* image x-axis reference pixel RA */
	     double crvl2_img,    /* image y-axis reference pixel Dec */
	     double crpx1_img,    /* image x-axis reference pixel */
	     double crpx2_img,    /* image y-axis reference pixel */
	     double cdlt1_img,    /* image x-axis decrement */
	     double cdlt2_img,    /* image y-axis decrement */
	     int nel_subimg,      /* number of elements in subimage and in cprob */
	     double ** cprob,     /* nel_subimage x 3 array with cumulative probabily and corresponding x/y pixel values */
	     double * cprob_vec,
	     double *ra_img,      /* photon RA */
	     double *dec_img,     /* photon DEC */
	     HDmt_state *MTstate){ /* save state of RNG */
    
    int idx_subimg=0;
    double x = 0.0;
    double imx=0.0, imy=0.0;
    double fimx=0.0, fimy=0.0;
    double theta_roll = 0.0;  /* image is in sky-coords, so no roll in deriving ra, dec */

    x = HDmt_drand(MTstate);  /* returns a double between 0 and 1 */
    
    /* find smallest index for which cprob_vec[index] >= x */
    idx_subimg = find_index(x, nel_subimg, cprob_vec);
    imx = cprob[idx_subimg][1];
    imy = cprob[idx_subimg][2];
    
    /* randomize within input image pixel */
    fimx = imx - 0.5 + HDmt_drand(MTstate);
    fimy = imy - 0.5 + HDmt_drand(MTstate);
    
    /* convert (fimx,fimy) to (ra_img, dec_img) and pass back. */
    xy2rd(fimx, fimy, crpx1_img, crpx2_img, crvl1_img, crvl2_img, theta_roll, cdlt1_img, cdlt2_img, ra_img, dec_img);

    return 0;
}




/* FUNCTION NAME:  sourcedis                                                           */
/*                                                                                     */
/* PURPOSE:                                                                            */
/*   Determine the initial position on the sky of npos source photons before being     */
/*   affected by the telescope and detector, based on the spatial distribution (point  */
/*   source or one of the extended models; for an image imagedis is used) provided by  */
/*   the user.  With the exception of point sources, this is implemented by treating   */
/*   the spatial distribution as a probability distribution.                           */
/*                                                                                     */
/* CALLING SEQUENCE:                                                                   */
/*   errstatus = sourcedis(sdmat, spectot, dx_arcmin, dy_arcmin, MTstate);             */
/*                                                                                     */
/* INPUTS: sdmat, npos                                                                 */
/*                                                                                     */
/* OUTPUTS: dx_arcmin, dy_arcmin                                                       */
/*                                                                                     */
/* SUBROUTINES: gasdev(), MTseed()                                                     */
/*                                                                                     */
/* CALLED BY:  doWork()                                                                */
/*                                                                                     */

int sourcedis(double * sdmat,         /* matrix of spatial distribution quantities to apply source dist */
	      int npos,               /* number of positions to generate */
	      double *dx_arcmin,      /* npos-sized 1D array of x offset, in arcmin, according to spatial dist */
	      double *dy_arcmin,      /* npos-sized 1D array of y offset, in arcmin, according to spatial dist */
	      HDmt_state *MTstate){   /* "save" state of Mersenne Twister RNG */

    int errstatus=0;
    int ipos=0;
    double ranx=0.0, rany=0.0;
    double theta=0.0;
    double ran_pow=0.0;
    double small=0.05;   /* used for power-law distribution */
    double tmslope=0.0;
    double ratio=0.0;
    double rfac=0.0;
    double rr=0.0;
    double aa=0.0;
    double xx=0.0;
    double tt=0.0;
    
    /* Begin loop over positions to compute */
    for (ipos = 0; ipos<npos; ipos++){

	/* Begin source distribution type conditional */
	if (sdmat[0] == 1){  /* Point Source - no displacement */
	    /*printf("Distributing source as point source (single pixel).\n");*/
	    dx_arcmin[ipos] = 0.0;
	    dy_arcmin[ipos] = 0.0;

	} else if (sdmat[0] == 2){	/* Gaussian distribution  */
	    /* Used: sdmat[1] = gauss_sigx = fwhm_x / 2.35482 */
	    /*       sdmat[2] = gauss_sigy = fwhm_y / 2.35482 */
	    /*       sdmat[3] = gauss_cos = cos(pos_angle)    */
	    /*       sdmat[4] = gauss_sin = sin(pos_angle)    */
	    /*printf("Distributing source as Gaussian.\n");*/
	    ranx = sdmat[1] * gasdev(MTstate);
	    rany = sdmat[2] * gasdev(MTstate);
	    dx_arcmin[ipos] = ranx * sdmat[3] - rany * sdmat[4];
	    dy_arcmin[ipos] = ranx * sdmat[4] + rany * sdmat[3];
	    
	} else if (sdmat[0] == 3){	/* Power Law distribution */
	    /* Used: sdmat[1] = slope                         */
	    /*       sdmat[2] = rmin                          */
	    /*       sdmat[3] = rmax                          */
	    /*       sdmat[4] = rmin/rmax                     */
	    /*printf("Distributing source as powerlaw.\n");*/
	    theta = TWOPI * HDmt_drand(MTstate);
	    ran_pow = HDmt_drand(MTstate);
	    ratio = sdmat[4];

	    /* small is needed to avoid divergence at 0 if steeper than index 2. */
	    if (sdmat[1] != 2){
		tmslope = 2.0 - sdmat[1];

		if ( (sdmat[1] < 2.0) && (ratio < small) )  ratio = small;
		
		rfac = ran_pow * (1.0-pow(ratio,tmslope)) + pow(ratio,tmslope);
		rr = sdmat[3] * pow(rfac,1.0/tmslope);
	    } else { /* slope = 2 */
		rr = sdmat[3] * pow(ratio,1.0-ran_pow);
	    }

	    dx_arcmin[ipos] = rr * cos(theta);
	    dy_arcmin[ipos] = rr * sin(theta);

	} else if (sdmat[0] == 4){  /* Elliptical distribution */
	    /* Used: sdmat[1] = ellipticity                    */
	    /*       sdmat[2] = ellip_cos = cos(pos_angle)     */
	    /*       sdmat[3] = ellip_sin = sin(pos_angle)     */
	    /*       sdmat[4] = rmin*rmin                      */
	    /*       sdmat[5] = rmax*rmax - rmin*rmin          */
	    /*printf("Distributing source as elliptical.\n");*/
	    theta = TWOPI * HDmt_drand(MTstate);
	    aa = sqrt(sdmat[4]+sdmat[5]*HDmt_drand(MTstate));
	    ranx = aa * cos(theta);
	    rany = sdmat[1] * aa * sin(theta);
	    dx_arcmin[ipos] = ranx * sdmat[3] - rany * sdmat[4];
	    dy_arcmin[ipos] = ranx * sdmat[4] + rany * sdmat[3];


	} else if (sdmat[0] == 5){  /* Beta model                */
	    /* Used: sdmat[1] = 1.0/(1.5 - 3.0*beta)             */
	    /*       sdmat[2] = core_radius                      */
	    /*       sdmat[3] = ellipticity                      */
	    /*       sdmat[4] = beta_cos = cos(pos_angle)        */
	    /*       sdmat[5] = beta_sin = sin(pos_angle)        */
	    /*       sdmat[6] = (1+(rmin/core_radius)^2)^bslope  */
	    /*       sdmat[7] = (1+(rmax/core_radius)^2)^bslope  */
	    /*printf("Distributing source as beta model.\n");*/
	    xx = HDmt_drand(MTstate);
	    if (sdmat[1] != 0){
		tt = pow( (xx*(sdmat[7]-sdmat[6])+sdmat[6]), sdmat[1]);
	    } else {
		tt = exp(xx * (sdmat[7]-sdmat[6]) + sdmat[6]);
	    }
	    rr = sdmat[2] * sqrt(tt-1.0);
	    theta = TWOPI * HDmt_drand(MTstate);
	    ranx = rr * cos(theta);
	    rany = sdmat[3] * rr * sin(theta);
	    dx_arcmin[ipos] = ranx * sdmat[4] - rany * sdmat[5];
            dy_arcmin[ipos] = ranx * sdmat[5] + rany * sdmat[4];

	} else if (sdmat[0] == 6){  /* Flat distribution */
	    /* Used: sdmat[1] = rmin*rmin                */
	    /*       sdmat[2] = rmax*rmax - rmin*rmin    */
	    /*printf("Distributing source as flat distribution.\n");*/
	    theta = TWOPI * HDmt_drand(MTstate);
	    rr = sqrt( sdmat[1] + sdmat[2] * HDmt_drand(MTstate) );
	    dx_arcmin[ipos] = rr * cos(theta);
	    dy_arcmin[ipos] = rr * sin(theta);

	} else {  /* sdmat[0] does not match any distribution used */
	    errstatus = -1;
	    printf("Error: Bad value of sdmat[0]\n");
	    
	} /* End source distribution type conditional */
    } /* end loop over positions to compute */
	
    return errstatus;
}
 
 
 
/* FUNCTION NAME: isrealpixel                                            */
/*                                                                       */
/* PURPOSE:                                                              */
/*    Determine whether a photon, after redistriobution using the psf,   */
/*    falls outside the detector FOV.  Used if no instrument map is      */
/*    available.                                                         */
/*                                                                       */
/* CALLING SEQUENCE:                                                     */
/*    result = isrealpixel(&s_mdb, x_foc, y_foc)                         */
/*                                                                       */
/* INPUTS: s_mdb, x_foc, y_foc                                           */
/*                                                                       */
/* OUTPUTS: returns 0 if pixel is to be discarded, otherwise 1           */
/*                                                                       */
/* CALLED BY:  doWork()                                                  */
/*                                                                       */
 
int isrealpixel(MDB_params * s_mdb,  /* structure containing mission database parameters */
		double x_foc,
		double y_foc){
    
    int isrpix = 1;    /* real pixel status - default is to accept */

    double dxcen=0, dycen=0;
    double rs_amin=0;
    double tmpx=0, tmpy=0;
    double x_det=0, y_det=0;
    int xdet_pix=0, ydet_pix=0;

    /* distance from center of detector in foc-pixels (i.e., not a true det) */
    /* dxcen = x_foc - s_mdb->FOC_XOFF; */
    /* dycen = y_foc - s_mdb->FOC_YOFF; */
    dxcen = x_foc - s_mdb->crpxx_foc + s_mdb->resampx * s_mdb->FOC_XOFF;
    dycen = y_foc - s_mdb->crpxy_foc + s_mdb->resampy * s_mdb->FOC_YOFF;
    rs_amin = 60.0*sqrt( pow(s_mdb->cdltx_foc*dxcen,2) + pow(s_mdb->cdlty_foc*dycen,2) ); 

    if  ( (s_mdb->FOV_RADIUS > 0) && (rs_amin > s_mdb->FOV_RADIUS) ) {
        isrpix = 0;
    } else {
	/* detx/y distances from center of detector in foc-pixels (i.e., not a true det) */
	tmpx = (x_foc - s_mdb->crpxx_foc); 
	tmpy = (y_foc - s_mdb->crpxy_foc); 
	x_det =  s_mdb->resampx * s_mdb->FOC_XOFF + s_mdb->crpxx_det  + s_mdb->FOC_ROTD_cos*tmpx + s_mdb->FOC_ROTD_sin*tmpy;
	y_det =  s_mdb->resampy * s_mdb->FOC_YOFF + s_mdb->crpxy_det  - s_mdb->FOC_ROTD_sin*tmpx + s_mdb->FOC_ROTD_cos*tmpy;
	xdet_pix = (int)(x_det + 0.5);
	ydet_pix = (int)(y_det + 0.5);

	if  ( (xdet_pix < s_mdb->xmin_det) || (xdet_pix > s_mdb->xmax_det) ||
	      (ydet_pix < s_mdb->ymin_det) || (ydet_pix > s_mdb->ymax_det) ) {
	    isrpix = 0;
	}  
    }
    return isrpix;
}

/* FUNCTION NAME: apply_instmap                                          */
/*                                                                       */
/* PURPOSE:                                                              */
/*    Determine whether a photon, aster redistribution using the PSF,    */
/*    falls outside of the detector FOV, or in an inactive part of the   */
/*    detector (e.g. pixel gab or dead pixel).  The map is assumed to    */
/*    be in simulator "foc" coordinates.                                 */
/*                                                                       */
/* CALLING SEQUENCE:                                                     */
/*    pixel_status = apply_instmap(s_mdb, xfoc, yfoc);                   */
/*                                                                       */
/* INPUTS: s_mdb, xfoc, yfoc                                             */
/*                                                                       */
/* OUTPUTS: returns 0 if pixel is to be discarded, otherwise 1           */
/*                                                                       */
/* CALLED BY:  doWork()                                                  */
/*                                                                       */

int apply_instmap(MDB_params * s_mdb,  /* structure holding MDB params */
		  double xfoc,      /* focx-coordinate to compare against dectector limits, in pixels */
		  double yfoc){     /* focy-coordinate to compare against dectector limits, in pixels */

    int xfoc_pix=0, yfoc_pix=0;
    int index=0;
    int map_value=0;
    int isrpix = 1;

    xfoc_pix = (int)(xfoc + 0.5);
    yfoc_pix = (int)(yfoc + 0.5);

    index = (yfoc_pix-1) * s_mdb->nx_imap + xfoc_pix-1;
    map_value = s_mdb->array_imap[index];

    if (map_value == 0)
	isrpix = 0;

    return isrpix;
}

/* FUNCTION NAME: apply_vignette                                                        */
/*                                                                                      */
/* PURPOSE:                                                                             */
/*   Determine probabailistically whether a photon of some energy and off-axis          */
/*   position is to be discarded based on the vignetting function specified in one      */
/*   of the general three formats specified in the Input/Output files section:          */
/*   a) a flat ASCII or FITS table with the vegnetting versus off-axis angle for a      */
/*   series of energies, b) a FITS file with separate extensions for different          */
/*   energies specified as header keywords, or c) an image cube: a FITS file with       */
/*   separate extensions containing images for different energies specified as          */
/*   header keywords.                                                                   */
/*                                                                                      */
/* CALLING SEQUENCE:                                                                    */
/*   vig_status = apply_vignette(s_vig, s_mdb, x_off_amin, y_off_amin, energy_ien, MTstate);     */
/*                                                                                      */
/* INPUTS: s_vig, s_mdb, x_off, y_off, energy                                           */
/*                                                                                      */
/* OUTPUTS: returns 0 for keep pixel, 1 for discard pixel                               */
/*                                                                                      */
/* SUBROUTINES: MTseed()                                                                */
/*                                                                                      */
/* CALLED BY:  doWork()                                                                 */
/*                                                                                      */

int apply_vignette(Vig_struct * s_vig,   /* the structure holding vig data */
                   MDB_params * s_mdb,   /* the mission database structure */
                   double x_off,         /* off-axis x-displacement in arcmin in relevent coord system */
                   double y_off,         /* off-axis y-displacement in arcmin in relevent coord system */
                   double energy,       /* photon energy in keV */
		   HDmt_state *MTstate){      /* "save" state of Mersenne Twister RNG */

    double score=0.0;                  /* a ranking to determine which value to use */
    double score_best=0.0;             /* the winning value */
    double r=0.0;                      /* photon radial offset from optical axis in arcmin */
    double bignumber=1.0e30;           /* a large number */
    double vig_frac=0;                   /* vignette fraction */
    double * vigfunc=0;                  /* vignetting function */
    double ** vig_img=0;                 /* vignette image matrix */
    double x_off_mm=0, y_off_mm=0;         /* x & y offset in millimeters */
    double x_off_pix=0, y_off_pix=0;       /* x & y offset in pixels */
    int flag_vignette = 0;             /* 1=discard photon, 0=keep photon */
    int idx=0, i_angle=0, imx=0, imy=0, ii=0;    /* loop index */
    int nx=0, ny=0;                        /* dimensions of vig image */
    int n_energy=0, n_angle=0;             /* number of energies, angles */
    int index_best_match=0;              /* index of the winning value */
    int imx_off_pix=0, imy_off_pix=0;      /* image x & y offset pixels */
    int index=0;                         /* index */

    n_energy = s_vig->ncol-1;
    n_angle = s_vig->nrow;
    vigfunc = calloc(n_angle, sizeof(double));

    /* allocate arrays and img matrix */
    vig_img = calloc(n_energy, sizeof(double *));
    for (ii=0; ii<n_energy; ii++)
        vig_img[ii] = calloc(n_angle, sizeof(double));

    score_best = bignumber;
    for (idx=0; idx<n_energy; idx++){
        score = fabs(s_vig->energy_vec[idx] - energy);
        if (score < score_best){
            score_best = score;
            index_best_match = idx;
        }
    }

    if (s_vig->vigtype != 3){    /* if not a vignette image */

        r = sqrt(x_off*x_off + y_off*y_off);

        /* populate the vig function to use */
        for(i_angle = 0; i_angle<n_angle; i_angle++)
            vigfunc[i_angle] = s_vig->vig_matrix[index_best_match][i_angle];

        /* interpolate to find vig function at photon radial position r */
        vig_frac = 1.0 - interpol_hunt(n_angle,s_vig->angle_vec, vigfunc, r);
        if (HDmt_drand(MTstate) < vig_frac) flag_vignette = 1;

    } else {  /* if we do have a vignette image */
        nx = s_vig->ncol;
        ny = s_vig->nrow;

        /* populate the vignetting image to use */
        for (imx=1; imx<nx+1; imx++){
            for (imy=1; imy<ny+1; imy++){
                index = (imy-1) * nx + imx-1;
                vig_img[imx-1][imy-1] = s_vig->vig_immat[index_best_match][index];
            }
        }

        /* convert to a common coord system (or at least scale) */
        x_off_mm = x_off / (60.0 * s_mdb->plate_scale);
        y_off_mm = y_off / (60.0 * s_mdb->plate_scale);

        /* vig-image pixel coords of off-axis location */
        x_off_pix = s_vig->cdlt1 * x_off_mm + s_vig->crpx1;
        y_off_pix = s_vig->cdlt2 * x_off_mm + s_vig->crpx2;

        /* which vig image pixel is (x_off_pix, y_off_pix) in?  round up */
        imx_off_pix = floor(x_off_pix) + 1;
        imy_off_pix = floor(y_off_pix) + 1;

        vig_frac = 1.0 - vig_img[imx_off_pix-1][imy_off_pix-1];
        flag_vignette = 0;
        if (HDmt_drand(MTstate) < vig_frac) flag_vignette = 1;
    }

    free(vigfunc);
    for (ii=0; ii<n_energy; ii++)
        free(vig_img[ii]);
    free(vig_img);
    return flag_vignette;
}



/* FUNCTION NAME:  apply_psf                                                 */
/*                                                                           */
/* PURPOSE:                                                                  */
/*   Scatter a photon at an off-axis position (x_in, y_in) in arcmin by      */
/*   (dx,dy) in arcmin using the psf for the telescope being simulated.      */
/*                                                                           */
/* CALLING SEQUENCE:                                                         */
/*   apply_psf(s_psf, s_psf_image, s_mdb,  x_off_amin, y_off_amin,           */
/*              energy_ien,  &dx_amin, &dy_amin, MTstate);                   */
/*                                                                           */
/* INPUTS: s_psf, s_mdb, x_in, y_in, energy                                  */
/*                                                                           */
/* OUTPUTS: dx, dy                                                           */
/*                                                                           */
/* SUBROUTINES: gasdev(), MTseed()                                           */
/*                                                                           */
/* CALLED BY:  doWork()                                                      */
/*                                                                           */

void apply_psf(PSF_struct * s_psf, /* structure containing psf data */
	       PSF_image * s_psf_image, /* structure containing psf image data */
	       MDB_params * s_mdb,  /* structure containing mission database params */
	       double x_in,         /* input x in arcmin, in coord system apropos for psf */
	       double y_in,         /* input y in arcmin, in coord system apropos for psf */
	       double energy,       /* photon energy in keV */
	       double *dx,          /* psf displacement in arcmin, in x same coord system */
	       double *dy,         /* psf displacement in arcmin, in y same coord system */
	       HDmt_state *MTstate){      /* "save" state of Mersenne Twister RNG */

    double sigma=0.0;                /* Gaussian FWHM */
    double psf_theta = 0.0;          /* theta angle */
    double psf_rs = 0.0;             /* radius */
    double off_axis=0.0;             /* off-axis radial distance */
    double score = 0.0;              /* a ranking to determine which value to use */
    double score_best=0.0;           /* the winning value */
    double score_energy=0.0;         /* the energy to use */
    double score_angle=0.0;          /* the anlge to use */
    double bignumber = 1.0e30;       /* a large number */
    int idx=0, i_radii=0;            /* loop index */
    int index_best_match = 0;        /* index of the winning value */
    double * psf_EEF=0;                /* the PSF function */
    int n_radii = 0;                 /* number of radii */
    double randval=0.0;              /* a random number between 0 and 1 */
    int psf_offset=0;                /* index for which psf_EEF is larger than randval */
    double weight1=0.0, weight2=0.0; /* weighting values */
    double x_idx=0., y_idx=0.;       /* x & y scattering distance */
    double dist=0.0;                     /* radial scattering distance */
    double dn=0.0;                       /* radial distnace weighting */
    double x_img=0.0, y_img=0.0;             /* x & y image coordinates */
    int nx_psf=0, ny_psf=0, nindex=0;        /* psf image dimensions */
    double x_off_mm=0.0, y_off_mm=0.0;       /* x & y offsets in millimeters */
    double x_off_px=0.0, y_off_px=0.0;       /* x & y offsets in pixels */
    double x_off_amin=0.0, y_off_amin=0.0;   /* x & y offsets in arcmin */
    int imx=0, imy=0;                /* psf-image pixel where photon is scattered to */

    double EEF_energy_weight = 1.0;  /* energy weight - obsolete */
    double EEF_angle_weight = 1.0;   /* angle weight - obsolete */

    if (s_psf->psftype == 5){
        /* apply a theoretical gaussian deviation for unit variance */
        sigma = s_mdb->psf_fwhm / 2.35482;
        psf_theta = TWOPI * HDmt_drand(MTstate);
        psf_rs = sigma * gasdev(MTstate)/60.0;
        *dx = psf_rs * sin(psf_theta);
        *dy = psf_rs * cos(psf_theta);

    } else if (s_psf->psftype != 3){  /* if this is NOT an image */
        /* for the input photon energy and position, find best match EEF */
        off_axis = sqrt(x_in*x_in + y_in*y_in);
        score_best = bignumber;

        /* begin loop over EEF profiles */
        for (idx=0; idx<s_psf->n_eef; idx++){
            if ( (s_psf->energy_vec[idx] + energy) == 0){
                score_energy = 1.0;
            } else{
                score_energy = EEF_energy_weight * fabs(s_psf->energy_vec[idx] - energy) /
                    (s_psf->energy_vec[idx] + energy);
            }

            if ( (s_psf->angle_vec[idx] + off_axis) == 0){
                score_angle = 1.0;
            } else {
		score_angle = EEF_angle_weight * fabs(s_psf->angle_vec[idx] - off_axis) /
                    (s_psf->angle_vec[idx] + off_axis);
            }

            score = (score_energy + score_angle);

            if (score < score_best){
                score_best = score;
                index_best_match = idx;
            }
        } /* end loop over EEF profiles */

        /* allocate EEF function */
        n_radii = s_psf->n_radii;
        psf_EEF = calloc(n_radii, sizeof(double));

        /* set EEF to use */
        for (i_radii = 0; i_radii<n_radii; i_radii++)
            psf_EEF[i_radii] = s_psf->eef_matrix[index_best_match][i_radii];

        /* Ensure EEF is normalized */
        if (psf_EEF[n_radii-1] != 1)
            for (i_radii = 0; i_radii<n_radii; i_radii++)
                psf_EEF[i_radii] = psf_EEF[i_radii] / psf_EEF[n_radii-1];

        randval = HDmt_drand(MTstate);

        /* find smallest psf_offset for which psf_EEF[psf_offest] >= randval */
        psf_offset = find_index(randval,n_radii,psf_EEF);

        /* interpolate the bracketing EEF values and radii to derive the radius corresponding to                                                       
           EEF=randval - the amount to scatter radially */
        if (psf_offset != 0){
            /* set the weights */
            weight1 = (psf_EEF[psf_offset] - randval) / (psf_EEF[psf_offset] - psf_EEF[psf_offset-1]);
            weight2 = (randval - psf_EEF[psf_offset-1]) / (psf_EEF[psf_offset] - psf_EEF[psf_offset-1]);

            /* make sure weights are valid */
            if ( (weight1 < 0) || (weight2 < 0) ){
                printf("ERROR: weights cannot be less than zero!\n");
                *dx = *dy = 1.0/0.0;
                return;
            }

            psf_rs = s_psf->radii[psf_offset]*weight2 + s_psf->radii[psf_offset-1]*weight1;

        } else {
            psf_rs = s_psf->radii[psf_offset];
        }

        /* scatter event in PSF azimuthal direction */
        psf_theta = TWOPI * HDmt_drand(MTstate);
        psf_rs /= 60.0;
        *dx = psf_rs * sin(psf_theta);
        *dy = psf_rs * cos(psf_theta);

        free(psf_EEF);

    } else { /* psf is an image cube */

        score_best = bignumber;

        /* begin loop over energies */
        for (idx=0; idx<s_psf_image->nimages; idx++){

            /* use fractional differences to put energy and angles on more equal footing */
            if ( (s_psf_image->energy_vec[idx] + energy) == 0){
                score_energy = 1.0;
            } else{
                score_energy = EEF_energy_weight * fabs(s_psf_image->energy_vec[idx] - energy) /
                    (s_psf_image->energy_vec[idx] + energy);
            }

	    /* Revised/simplified to weight by relative distance, normalized to off-axis distance of event */
	    x_idx = s_psf_image->angle_vec[idx] * cos(D2R * s_psf_image->azim_vec[idx]);
            y_idx = s_psf_image->angle_vec[idx] * sin(D2R * s_psf_image->azim_vec[idx]);
            dist = sqrt( (x_in-x_idx)*(x_in-x_idx) + (y_in-y_idx)*(y_in-y_idx) );
	    dn = sqrt( (x_in*x_in) + (y_in*y_in) );
	    if (dn == 0){
		score_angle = 0.0;
	    } else {
		score_angle = dist / dn;
	    }

            score = (score_energy + score_angle);

	    /* Other alternatives:
	       - use 1/mean(energy_vec)
	       - use 1/mean(angle_vec)
	       - use 1/psf_fwhm
	       - use dist
	    */

            if (score < score_best){
                score_best = score;
                index_best_match = idx;
		x_img = x_idx;
		y_img = y_idx;
            }
        } /* end loop over energies */

        nx_psf = s_psf_image->xdim[index_best_match];
        ny_psf = s_psf_image->ydim[index_best_match];
        nindex = nx_psf * ny_psf;

	/*
        cum_psf_img = calloc(nindex, sizeof(double));
        for (index=0; index<nindex; index++){
	    cum_psf_img[index] = s_psf_image->prob_array[index_best_match][index];
	    }*/

	/* scatter the photon */
        randval = HDmt_drand(MTstate);
	idx = find_index(randval,nindex, s_psf_image->prob_array[index_best_match]);

	/* psf-image pixel where photon is scattered to: index = (imy-1) * nx + imx -1 */
        imx = idx%nx_psf + 1;
        imy = idx/nx_psf + 1;  /* yes, nx_psf here is correct, not ny_psf! */

        /* off-axis position of the pixel center in mm */
	x_off_px = (imx-1) + HDmt_drand(MTstate);
	y_off_px = (imy-1) + HDmt_drand(MTstate);

        x_off_mm = s_psf_image->crvl1[index_best_match] + 
	    s_psf_image->cdlt1[index_best_match] * (x_off_px - s_psf_image->crpx1[index_best_match]);

	y_off_mm = s_psf_image->crvl2[index_best_match] +
	    s_psf_image->cdlt2[index_best_match] * (y_off_px - s_psf_image->crpx2[index_best_match]);


        /* off-axis position of the pixel center in arcmin */
        x_off_amin = 60.0 * s_mdb->plate_scale * x_off_mm;
        y_off_amin = 60.0 * s_mdb->plate_scale * y_off_mm;

        /* offset in arcmin */
        *dx = x_off_amin - x_img;
        *dy = y_off_amin - y_img;

    }/* end psf_type conditional */

    return;
}


/* FUNCTION NAME:  flag_pileup                                               */
/*                                                                           */
/* PURPOSE:                                                                  */
/*   Flag events which occur in the same pixel within some time dtpileup     */
/*   of each other.                                                          */
/*                                                                           */
/* CALLING SEQUENCE:                                                         */
/*   flag_pileup(ounit, nev_block, first_index, s_obs->dtpileup, time_block, */
/*                 pixid_block, &deadtime, &n_pileup, debug);                */
/*                                                                           */
/* INPUTS: ounit, nev_block, first_index, dtpileup, time_block, pixid_block, */
/*                  deadtime, n_pileup, debug                                */
/*                                                                           */
/* OUTPUTS: deadtime, n_pileup, modifies output fits file                    */
/*                                                                           */
/* CALLED BY:  doWork()                                                      */
/*                                                                           */

int flag_pileup(fitsfile * ounit,
                int nevt,
                int start_index,
                double dtpileup,
                double * tevt,
                int * pixid,
		double * deadtime,
		long * n_pileup,
                int debug){

    int fstat = 0;
    int * pflag=0;

    double del_dt = 0.0;    /*initialize deadtime increment */
    int ifirst_in = 0;     /*initialize first event in queue*/
    int inext = 1;         /*initialize next event to check if in group*/

    int grpdone = 0;  /*initialize "group done" flag each start*/
    int ngrp = 1;     /*initialize number of events in this group*/
    long ii=0;

    /* Allocate the arrays */
    pflag = calloc(nevt, sizeof(int));   /* The actual pileup flag array */

    /* Sort first by pixid, then by time */
    sort_two_conditions_IDI(pixid, tevt, pflag, nevt);

    /* Do the actual flagging */
    while (ifirst_in < nevt-1) {
        grpdone = 0; /*initialize "group done" flag each start*/
        ngrp = 1; /*initialize number of events in this group*/

        while ((inext < nevt) & (grpdone == 0)){
            /* keep adding to the group if next event is in same pixel and within dtpileup*/
            if ( ((tevt[inext]-tevt[ifirst_in])  <  dtpileup) && (pixid[ifirst_in] == pixid[inext]) ){
                inext++;
                ngrp++;
            } else {
                grpdone = 1; /*if any criteria fail, this group is done*/
            } /*endif ((tevt[inext]-tevt[ifirst_in])  <  dtpileup) && (pixid[ifirst_in] ==  pixid[inext]) */
        } /*end do while ((inext < nevt) & (grpdone == 0)) */

        if (ngrp > 1) { /* a group w/ one member is not a group*/
            for (ii = ifirst_in; ii < ifirst_in+ngrp ; ii++) {
                pflag[ii]=1; /*set pileup flag for all events in group*/
            } /*endfor*/
            (*n_pileup) += ngrp; /*increment number of piled-up events*/
            del_dt = tevt[ifirst_in+ngrp-1] - tevt[ifirst_in];
            (*deadtime) += del_dt; /*increment piled-up "deadtime*/
        } /*endif (ngrp > 1) */

        ifirst_in = inext; /*next "unprocessed" event is first in next group*/
        inext = ifirst_in + 1; /*re-initialize next event to check if in group*/

    } /*enddo while (ifirst_in < nevt-1) */

    /* Re-sort by time */
    sort_arrays_DII(tevt, pixid, pflag, nevt);

    /* write the flag column to file */
    fits_write_col(ounit, TINT, 5, start_index, 1, nevt, pflag, &fstat);

    free(pflag);

    return fstat;
}



/* FUNCTION NAME: torus_prep                                                */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Extract spectra and information necessary to identify spectra from     */
/*   the Brigthman and Nandra (2011) torus model as written in              */
/*   torus1006.fits for a given opening angle, power-law index, NH, and     */
/*   inclination.                                                           */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   errstatus = torus_prep(&s_tpar, &s_tspec);                             */
/*                                                                          */
/* INPUTS/OUTPUTS: s_tpar, s_tspec                                          */
/*                                                                          */
/* CALLED BY: doWork                                                        */
/*                                                                          */

int torus_prep(torus_par_struct * s_tpar, torus_spec_struct * s_tspec){

    fitsfile * iunit = NULL;
    int fstat=0;
    long nrow=0;
    char * com = NULL;
    int ii=0;
    int rownum = 0;
    int anynull = 0;
    int col_numval = 0, col_val = 0, col_energ_lo = 0, col_energ_hi = 0;
    char * headas = 0;
    char tor1006[STR_MAX_LEN];
    long * numvals_arr = 0;

    printf("\nPopulating torus structs...\n");

    headas = getenv("HEADAS");
    sprintf(tor1006,"%s%s",headas,"/refdata/torus1006.fits");
    fits_open_file(&iunit, tor1006, READONLY, &fstat);

    if (fstat != 0){
        printf("   ERROR: Could not open fits file %s.\n",tor1006);
        return -1;
    }

    /* move to the PARAMETERS extension */
    fits_movnam_hdu(iunit, BINARY_TBL, "PARAMETERS", 0, &fstat);

    /* We assume there are four ROWS:
          1) nH
          2) PhoIndex
          3) Theta_tor
          4) Theta_inc
    */

    /* ####### POPULATE THE TORUS_PAR_STRUCT ########### */

    /* Determine which column is NUMBVALS and VALUE */
    fits_get_colnum(iunit, CASEINSEN, "NUMBVALS", &col_numval, &fstat);
    fits_get_colnum(iunit, CASEINSEN, "VALUE", &col_val, &fstat);

    /* Get the number of parameters in each of the four arrays */
    numvals_arr = calloc(4, sizeof(long));
    fits_read_col_lng(iunit, col_numval, 1, 1, 4, 0, numvals_arr, &anynull, &fstat);
    s_tpar->num_nh = numvals_arr[0];
    s_tpar->num_gam = numvals_arr[1];
    s_tpar->num_theta = numvals_arr[2];
    s_tpar->num_inc = numvals_arr[3];
    free(numvals_arr);

    /* Populate the nH parameters */
    rownum = 1;
    s_tpar->nhvals = calloc(s_tpar->num_nh, sizeof(double));
    fits_read_col_dbl(iunit, col_val, rownum, 1, s_tpar->num_nh, 0, s_tpar->nhvals, &anynull, &fstat);
    /*for (ii=0; ii<s_tpar->num_nh; ii++) printf("nhvals[%d] = %f\n",ii,s_tpar->nhvals[ii]);*/


    /* Populate the PhoIndex parameters */
    rownum = 2;
    s_tpar->gamvals = calloc(s_tpar->num_gam, sizeof(double));
    fits_read_col_dbl(iunit, col_val, rownum, 1, s_tpar->num_gam, 0, s_tpar->gamvals, &anynull, &fstat);
    /*for (ii=0; ii<s_tpar->num_gam; ii++) printf("gamvals[%d] = %f\n",ii,s_tpar->gamvals[ii]);*/

    /* Populate the Theta_tor parameters */
    rownum = 3;
    s_tpar->thvals = calloc(s_tpar->num_theta, sizeof(double));
    fits_read_col_dbl(iunit, col_val, rownum, 1, s_tpar->num_theta, 0, s_tpar->thvals, &anynull, &fstat);
    /*for (ii=0; ii<s_tpar->num_theta; ii++) printf("thvals[%d] = %f\n",ii,s_tpar->thvals[ii]);*/

    /* Populate the Theta_inc parameters */
    rownum = 4;
    s_tpar->incvals = calloc(s_tpar->num_inc, sizeof(double));
    fits_read_col_dbl(iunit, col_val, rownum, 1, s_tpar->num_inc, 0, s_tpar->incvals, &anynull, &fstat);
    /*for (ii=0; ii<s_tpar->num_inc; ii++) printf("incvals[%d] = %f\n",ii,s_tpar->incvals[ii]);*/


    /* ########## TORUS_PAR_STRUCT COMPLETE, BEGIN TORUS_SPEC_STRUCT ######### */

    s_tspec->num_spec = s_tpar->num_nh * s_tpar->num_gam * s_tpar->num_theta * s_tpar->num_inc;


    /* move to ENERGIES extension */
    fits_movnam_hdu(iunit, BINARY_TBL, "ENERGIES", 0, &fstat);

    /* num_en should equal the number of rows */
    fits_read_key_lng(iunit, "NAXIS2", &s_tspec->num_en, com, &fstat);

    /* Get column numbers for ENERG_LO and ENERG_HI */
    fits_get_colnum(iunit, CASEINSEN, "ENERG_LO", &col_energ_lo, &fstat);
    fits_get_colnum(iunit, CASEINSEN, "ENERG_HI", &col_energ_hi, &fstat);

    /* Allocate the arrays */
    s_tspec->tor_energ_hi = calloc(s_tspec->num_en, sizeof(double));
    s_tspec->tor_energ_lo = calloc(s_tspec->num_en, sizeof(double));


    /* Read the two energy columns */
    fits_read_col_dbl(iunit, col_energ_lo, 1, 1, s_tspec->num_en, 0, s_tspec->tor_energ_lo, &anynull, &fstat);
    fits_read_col_dbl(iunit, col_energ_hi, 1, 1, s_tspec->num_en, 0, s_tspec->tor_energ_hi, &anynull, &fstat);
    /*for (ii=0; ii<20; ii++) printf("ROW: %d  LO: %f  HI: %f\n",ii,s_tspec->tor_energ_lo[ii],s_tspec->tor_energ_hi[ii]);*/


    /* move to SPECTRA extension */
    fits_movnam_hdu(iunit, BINARY_TBL, "SPECTRA", 0, &fstat);

    /* Make sure same number of rows equals num_spec */
    fits_read_key_lng(iunit, "NAXIS2", &nrow, com, &fstat);
    if (nrow != s_tspec->num_spec){
        printf("ERROR: Torus SPECTRA extension must have same number of rows as num_spec!\n");
        fstat = -1;
    }

    /* Allocate and read the spectra matrix */
    s_tspec->tor_spec = calloc(s_tspec->num_spec, sizeof(double *));
    for (ii=0; ii<s_tspec->num_spec; ii++){
        s_tspec->tor_spec[ii] = calloc(s_tspec->num_en, sizeof(double));
        fits_read_col_dbl(iunit, 2, ii+1, 1, s_tspec->num_en, 0, s_tspec->tor_spec[ii], &anynull, &fstat);
        if (fstat != 0){
            printf("ERROR: could not read torus spectra at row %d!\n",ii+1);
            fstat = -1;
            break;
        }
    }

    /* Allocate and populate the derived arrays */
    s_tspec->tor_energ_mid = calloc(s_tspec->num_en, sizeof(double));
    s_tspec->tor_energ_wid = calloc(s_tspec->num_en, sizeof(double));
    s_tspec->tor_xspec_energy = calloc(s_tspec->num_en+1, sizeof(double));

    for (int ien=0; ien<s_tspec->num_en; ien++){
        s_tspec->tor_energ_mid[ien] = 0.5 * (s_tspec->tor_energ_lo[ien] + s_tspec->tor_energ_hi[ien]);
        s_tspec->tor_energ_wid[ien] = fabs(s_tspec->tor_energ_lo[ien] - s_tspec->tor_energ_hi[ien]);
        s_tspec->tor_xspec_energy[ien] = s_tspec->tor_energ_lo[ien];
    }

    /* tor_xspec_energy has one extra slot to populate */
    s_tspec->tor_xspec_energy[s_tspec->num_en] = s_tspec->tor_energ_hi[s_tspec->num_en-1];

    fits_close_file(iunit,&fstat);


    printf("...done populating torus structs.\n");


    return fstat;

}


/* FUNCTION NAME: torus_spectra                                             */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Calculate the input spectraum in photons/cm^2/sec/channel from         */
/*   Brightman/Nandra torus and Gilli models with reflection.               */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/* errstatus = torus_spectra(nh, zred, sptype-1, spar, fpar, bpass_lo,      */
/*                           bpass_hi, nebin, double_ebin_mid, xspec_energy,*/
/*                           &s_tpar, &s_tspec, specin, &flux_abs,          */
/*                           &flux_unabs, &dflux_abs, &dflux_unabs, debug); */
/*                                                                          */
/* INPUTS: spectype, specpar, fluxpar, norm_upper, norm_lower, nebin,       */
/*          energ_mid, xspec_energy, s_tpar, s_tspec                        */
/*                                                                          */
/* OUTPUTS: spec                                                            */
/*                                                                          */
/* CALLED BY: doWork()                                                      */
/*                                                                          */

int torus_spectra(double nh,              /* external absorption column density in cm^-2 */
		  double zpar,            /* redshift */
		  int spectype,           /* 6:torus, 7: gilli_thick, 8:gilli_mild */
                  double specpar,         /* see comment below */
                  double fluxpar,         /* over the input bandpass in erg/cm2/sec */
                  double norm_lower,      /* lower limit of bandpass in keV for flux */
                  double norm_upper,      /* upper limit of bandpass in keV for flux */
                  long nebin,             /* number input energy bins */
                  double * ebin_mid,
                  double * xspec_energy,
                  torus_par_struct * s_tpar, /* torus structure containing parameters */
                  torus_spec_struct *s_tspec, /* torus structure containing spectra */
                  double * spec,         /* spectrum in photons/cm2/sec/channel on input energy grid */
		  double *flux_abs,       /* absorbed flux in erg/cm2/sec over input bandpass */
		  double *flux_unabs,     /* unabsorbed flux in erg/cm2/sec over input bandpass */
		  double *dflux_abs,      /* absorbed flux in erg/cm2/sec over entire detector bandpass */
		  double *dflux_unabs,   /* unabsorbed flux in erg/cm2/sec over entire detector bandpass */
		  int debug){             /* debug flag */

    double onekeVphoton2ergs = 1.60218e-9;
    int index = 0;
    int itheta = 0;
    int i1b = 0;
    int iangle = 0;
    int i2b = 0;
    int igamma = 0;
    int i3b = 0;
    int iabs = 0;
    double nh_tor=0.0, gamma=0.0, opnangle=0.0, incangle=0.0;
    float * float_xspec_energy = 0;
    float * float_xsphab_tor = 0;
    float * float_xsphab_trans = 0;
    float * float_xsphab_trans_err = 0;
    double * inspec = 0;
    double * int_spec = 0;
    int ii = 0;
    double * binlo_out = 0;
    double * binhi_out = 0;
    double tt = 0;
    int ifl = 1;
    float xsphab_param[1];  /* Xspec xsphab params */
    double zfac = 0.0;
    double * tor_rshift_xspec_energy = 0;

    /* TRF says to unpack torus structure here...we'll just use struct names */

    if (debug == 1) printf("Calculating torus spectra...\n");

    index = (int)specpar;

    if ( (spectype == 7) || (spectype == 8) ){
        itheta = index % s_tpar->num_theta;
        i1b = index / s_tpar->num_theta;

        iangle = i1b % s_tpar->num_inc;
        i2b = i1b / s_tpar->num_inc;

        igamma = i2b % s_tpar->num_gam;
        i3b = i2b / s_tpar->num_gam;

        iabs = i3b % s_tpar->num_nh;

        nh_tor = s_tpar->nhvals[iabs];
        gamma = s_tpar->gamvals[igamma];
        opnangle = s_tpar->thvals[iangle];
        incangle = s_tpar->incvals[itheta];

    } else {
        if (spectype != 6){
            printf("ERROR: Type %d is not a supported torus spectype, returning.\n",spectype);
            for (int ii=0; ii<nebin; ii++)
                spec[ii] = 0.0;
            return -1;  /* Premature return here is ok, nothing has been allocated yet, no memory leaks here. */
        }
    }

    float_xsphab_tor = calloc(s_tspec->num_en, sizeof(float));

    if (spectype == 8) {
	float * float_tor_xspec_energy = calloc(s_tspec->num_en+1, sizeof(float));
	float * float_xsphab_tor_err = calloc(s_tspec->num_en, sizeof(float));
	for (int ien=0; ien<s_tspec->num_en+1; ien++)
	    float_tor_xspec_energy[ien] = (float)xspec_energy[ien];

	xsphab_param[0] = (float) (nh_tor / 1.0e+22);
	Xsphab(float_tor_xspec_energy, s_tspec->num_en, xsphab_param, ifl, float_xsphab_tor, float_xsphab_tor_err);
	/* don't need float_xsphab_tor_err */
	free(float_xsphab_tor_err);
	free(float_tor_xspec_energy);
    }
    
    inspec = calloc(s_tspec->num_en, sizeof(double));
    int_spec = calloc(s_tspec->num_en+1, sizeof(double));

    for (int ien=0; ien<s_tspec->num_en; ien++){
        /* spectrum over torus energy grid for these parameters: */
        double pl_spec = 0.0;
        double refl_spec = 0.0;
        double inspec_ien = s_tspec->tor_spec[index][ien];
        double ewid = s_tspec->tor_energ_wid[ien];
	double emid = s_tspec->tor_energ_mid[ien];

        if ( (spectype == 7) || (spectype == 8) ){
            /* torus reprocessed - reflection only for these cases (per keV) */
            pl_spec = pow(emid, -gamma);
            refl_spec = inspec_ien / ewid - pl_spec;

            if (spectype == 7){
                /* reprocessed (reflection) only */
                inspec_ien = ewid * refl_spec;
            } else {
                /* plaw plus some reprocessed (reflection) */
		inspec_ien = ewid * (0.37 * refl_spec + (double)float_xsphab_tor[ien] * pl_spec);
            }
        }
        inspec[ien] = inspec_ien;
    }

    /* now done with float_xsphab_tor */
    free(float_xsphab_tor);

    /* to redshift the torus spectrum diminish the flux and energy by 1+z */
    zfac = 1.0 + zpar;
    tor_rshift_xspec_energy = calloc(s_tspec->num_en + 1, sizeof(double));
    for (int ien=0; ien<s_tspec->num_en+1; ien++)
	tor_rshift_xspec_energy[ien] = s_tspec->tor_xspec_energy[ien] / zfac;

    /* cumulative flux of torus model: */
    int_spec[0] = 0.0;
    for (int iien=1; iien < s_tspec->num_en; iien++) int_spec[iien] = inspec[iien-1] / zfac;
    for (int iien=1; iien < s_tspec->num_en; iien++) int_spec[iien] += int_spec[iien-1];

    /* interpolate from torus energy grid to input energy grid */
    binlo_out = calloc(nebin, sizeof(double));
    binhi_out = calloc(nebin, sizeof(double));
    for (ii = 0; ii<nebin; ii++) {
        binlo_out[ii] = xspec_energy[ii];
        binhi_out[ii] = xspec_energy[ii+1];
    }

    for (ii = 0; ii < nebin; ii++) {
        double intspec_lo = 0.0;
        double intspec_hi = 0.0;

        if ( (binhi_out[ii] <= s_tspec->tor_energ_lo[0]) || (binlo_out[ii] >= s_tspec->tor_energ_hi[s_tspec->num_en-1]) ) {
            /* no flux outside the bounds of the input spectrum: */
            spec[ii] = 0.0;
        }  else {
            intspec_lo = 0.0;
            if (binlo_out[ii] >= s_tspec->tor_energ_lo[0])  {
                intspec_lo = interpol_hunt(s_tspec->num_en+1, tor_rshift_xspec_energy, int_spec, binlo_out[ii]);
                /* interpol_hunt from heasim; repace with ahmath version */
            }
            intspec_hi = int_spec[s_tspec->num_en];
            if (binhi_out[ii] <= s_tspec->tor_energ_hi[s_tspec->num_en-1])  {
                intspec_hi = interpol_hunt(s_tspec->num_en+1, tor_rshift_xspec_energy, int_spec, binhi_out[ii]);
            }
            spec[ii] = (intspec_hi - intspec_lo);
        }
    }

    free(binlo_out);
    free(binhi_out);
    
    tt = 0;
    spec_scale(ebin_mid, spec, nebin, norm_lower, norm_upper, &tt);
    tt = tt * onekeVphoton2ergs;
    *flux_unabs = fluxpar;
    for (int ii = 0; ii < nebin; ii++) {
	if (tt > 0){
	    spec[ii] = spec[ii] * (*flux_unabs) / tt;
	} else {
	    spec[ii] = 0.0;
	}
    }
    if (debug == 1) printf("flux_unabs = %e\n",*flux_unabs);
    /* Since the absorbed spectrum is to be returned, this is the place to calculate the unabsorbed
       flux over the entire detector bandpass */
    spec_scale(ebin_mid, spec, nebin, ebin_mid[0], ebin_mid[nebin-1], &tt);
    *dflux_unabs = tt  * onekeVphoton2ergs; 
    if (debug == 1) printf("dflux_unabs = %e\n",*dflux_unabs);

    /* Now, absorb the spectrum (and calculate the absorbed flux) */

    float_xsphab_trans = calloc(nebin, sizeof(float));
    float_xsphab_trans_err = calloc(nebin, sizeof(float));
    float_xspec_energy = calloc(nebin+1, sizeof(float));

    for (int ien=0; ien<nebin+1; ien++)
        float_xspec_energy[ien] = (float)xspec_energy[ien];

    /* multiply NH units by 10^22 for input into Xspec */
    xsphab_param[0] = (float)(nh / 1.0e+22);
    Xsphab(float_xspec_energy, nebin, xsphab_param, ifl, float_xsphab_trans, float_xsphab_trans_err);

    for (ii = 0; ii < nebin ; ii++)
	spec[ii] = spec[ii] * (double)float_xsphab_trans[ii];

    spec_scale(ebin_mid, spec, nebin, ebin_mid[0], ebin_mid[nebin-1], &tt);
    *dflux_abs = tt  * onekeVphoton2ergs; 
    spec_scale(ebin_mid, spec, nebin, norm_lower, norm_upper, &tt);
    *flux_abs = tt  * onekeVphoton2ergs; 

    if (debug == 1){
	printf("flux_abs = %e\n",*flux_abs);
	printf("dflux_abs = %e\n",*dflux_abs);
    }

    free(inspec);
    free(int_spec);
    free(float_xspec_energy);
    free(float_xsphab_trans);
    free(float_xsphab_trans_err);

    return 0;
}


/* FUNCTION NAME: read_redshift                                             */
/*                                                                          */
/* PURPOSE: read the redshift file output from skyback                      */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*     errstatus = read_redshift(nsource, nsource_psback,                   */
/*                  s_back.pszbackfile, &redshifts, &intabs);    */
/*                                                                          */
/* INPUTS: n_source, n_psback, zfile                                        */
/*                                                                          */
/* OUTPUTS: redshifts, intabs, and returns error status                     */
/*                                                                          */
/* CALLED BY: main()                                                        */
/*                                                                          */


int read_redshift(int n_source,
		  int n_psback,
		  char * zfile,
		  double ** redshifts,
		  double ** intabs,
		  int debug){

    int numlines = 0;
    FILE * fz = 0;
    char line[STR_MAX_LEN];
    char temp_line[STR_MAX_LEN];
    int iz = 0;
    int errstatus = 0;

    /* First, check that the source input file exists. */
    errstatus = access( zfile, F_OK );
    if ( errstatus != 0 ){
	printf("Error: cannot find redshift file %s, exiting.\n",zfile);
	return -1;
    } else {
	numlines = get_nlines(zfile,debug);
	if (numlines != n_psback){
	    printf("WARNING: incorrect number of lines in redshift file %s.  Expected %d, found %d.\n",
		   zfile, n_psback, numlines);
	    printf("Keeping all redshifts and intabs set to zero, and continuing.\n");
	    return 0;
	} else {
	    printf("Reading redshift file...\n");
	    fz = fopen(zfile,"r");
	    /* Only redshifts for point sources should be non-zero */
	    iz = n_source;
	    while(fgets(line,sizeof(line),fz) != NULL){
		/* strip trailing '\n' if it exists */
		int len = strlen(line)-1;
		if(line[len] == '\n')
		    line[len] = 0;

		/* remove spaces in line */
		int kk=0;
		for (int jj=0; jj<=len; jj++)
		    if (line[jj] != ' ')
			temp_line[kk++] = line[jj];
		temp_line[kk] = 0;
		strcpy(line,temp_line);
		strcpy(temp_line,"");

		(*redshifts)[iz] = atof(strtok(line,","));
		(*intabs)[iz] = atof(strtok(NULL,","));
		iz++;
	    }
	    fclose(fz);
	}
    }

    
    printf("...done.\n");
    return 0;
}


/* FUNCTION NAME: write_GTI                                                             */
/*                                                                                      */
/* PURPOSE:                                                                             */
/*   Write a GTI HDU extension at the end of the fits file                              */
/*                                                                                      */
/* CALLING SEQUENCE: errstatus = write_GTI(ounit,s_obs);                                */
/*                                                                                      */
/* INPUTS:  ounit, s_obs                                                                */
/*                                                                                      */
/* OUTPUTS: write GTI extension to file                                                 */
/*                                                                                      */
/* CALLED BY: doWork()                                                                  */
/*                                                                                      */


int write_GTI(fitsfile * ounit, ObsParams * s_obs){
    
    char * ttype[] = {"START","STOP"};
    char * tform[] = {"1D","1D"};
    char * tunit[] = {"s","s"};

    int fstat = 0;
    char * comment = 0;
    char date[STR_MAX_LEN];
    char dateobs[STR_MAX_LEN];
    char dateend[STR_MAX_LEN];
    char creator[STR_MAX_LEN];
    long mjdrefi = 0;
    double mjdreff = 0.0;

    int anynull = 0;
    long nev = 0;
    double firsttime[1];
    double lasttime[1];

    /* Read some keywords from the EVENTS HDU, so we can use them in the GTI HDU */
    fits_movnam_hdu(ounit,BINARY_TBL,"EVENTS",0,&fstat);
    fits_read_key_str(ounit, "DATE", date, comment, &fstat);
    fits_read_key_str(ounit, "DATE-OBS", dateobs, comment, &fstat);
    fits_read_key_str(ounit, "DATE-END", dateend, comment, &fstat);
    fits_read_key_str(ounit, "CREATOR", creator, comment, &fstat);
    fits_read_key_lng(ounit, "MJDREFI", &mjdrefi, comment, &fstat);
    fits_read_key_dbl(ounit, "MJDREFF", &mjdreff, comment, &fstat);
    fits_read_key_lng(ounit, "NAXIS2", &nev, comment, &fstat);

    if (nev > 0){
	fits_read_col_dbl(ounit, 1, 1, 1, 1, 0, firsttime, &anynull, &fstat);
	fits_read_col_dbl(ounit, 1, nev, 1, 1, 0, lasttime, &anynull, &fstat);
    } else {
	printf("No events to use, skipping GTI population.\n");
	return fstat;
    }

    /* Create and populate the GTI HDU header */
    fits_insert_btbl(ounit,0,2,ttype,tform,tunit,"GTI",0,&fstat);
    fits_modify_key_str(ounit,"TTYPE1","START","label for field 1",&fstat);
    fits_modify_key_str(ounit,"TTYPE2","STOP","label for field 2",&fstat);    
    fits_write_key_str(ounit,"HDUCLASS","OGIP","File conforms to OGIP/GSFC conventions",&fstat);
    fits_write_key_str(ounit,"HDUCLAS1","GTI","File contains Good Time Intervals", &fstat);
    fits_write_key_str(ounit,"TELESCOP",s_obs->mission,"Name of the Mission  ***", &fstat);
    fits_write_key_str(ounit,"INSTRUME",s_obs->instrume,"Name of the instrument  ***", &fstat);
    fits_write_key_str(ounit,"FILTER",s_obs->filter,"Filter", &fstat);
    fits_write_key_str(ounit,"DATE-OBS",dateobs,"UT date and time of observation start",&fstat);
    fits_write_key_str(ounit,"DATE-END",dateend,"UT date and time of observation end",&fstat);
    fits_write_key_str(ounit,"DATAMODE","STANDARD","Datamode all set to STANDARD", &fstat);
    fits_write_key_str(ounit,"OBJECT","SIMULATION","Name of the Object ***", &fstat);
    fits_write_key_str(ounit,"OBS_MODE","POINTING","Pointing or Slew", &fstat);
    fits_write_key_str(ounit,"CREATOR",creator,"Name of code that generated this file", &fstat);
    fits_write_key_lng(ounit,"MJDREFI",mjdrefi,"MJD integer SC clock start",&fstat);
    fits_write_key_fixdbl(ounit,"MJDREFF",mjdreff,6,"MJD fraction SC clock start",&fstat);
    fits_write_key_str(ounit,"TIMESYS","TT","fundamental time system: Terrestial Time", &fstat);
    fits_write_key_str(ounit,"TIMEUNIT","s","unit for TSTART, TSTOP, TIMEZERO, TELPASE", &fstat);
    fits_write_key_str(ounit,"TIMEREF","LOCAL","spatial reference frame", &fstat);
    fits_write_key_str(ounit,"TASSIGN","SATELLITE","location of time assignment", &fstat);
    fits_write_key_fixdbl(ounit,"TIMEZERO",0.0,6,"Offset for the TIME column ***",&fstat);
    fits_write_key_fixdbl(ounit,"TSTART",0.0,6,"Start time ***",&fstat);
    fits_write_key_fixdbl(ounit,"TSTOP",s_obs->exposure,6,"Start time ***",&fstat);
    fits_write_key_str(ounit,"DATE",date,"file creation date (YYYY-MM-DDThh:mm:ss UT)",&fstat);

    /* Write the first and last times to data columns */
    fits_write_col_dbl(ounit,1,1,1,1,firsttime,&fstat);
    fits_write_col_dbl(ounit,2,1,1,1,lasttime,&fstat);

    return fstat;
}


/*
  ! $Log: doWork.c,v $
  ! Revision 1.150  2016/08/26 20:38:41  rshill
  ! (1) Deleted skiprmf parameter (use rmffile=none instead)
  ! (2) inserted setting ebounds_TLMIN1, ebounds_TLMAX2, and nebin_rmf at main
  ! level for rmffile=none.
  ! (3) smaller corrections from comparison to TRF.
  !
  ! Revision 1.149  2016/04/18 13:55:27  driethmi
  ! Corrected bug that caused mismatched timing in pulsed source across subexposures.
  !
  ! Revision 1.148  2016/04/08 18:17:38  driethmi
  ! Corrected bug in timeset() - variable "ncycle" was initialized to zero, but
  ! then never set.
  !
  ! Revision 1.147  2016/04/05 14:43:21  driethmi
  ! Several minor corrections to bring code into sync with updated TRF.
  !
  ! Revision 1.146  2016/03/31 21:41:40  driethmi
  ! Impemented suite of changes to handle burst time assignment.  (There will
  ! likely be more bug fixes to this implementation soon.)
  !
  ! Revision 1.145  2016/03/30 18:49:54  driethmi
  ! Modified print comment reporting source number to be less cryptic.
  !
  ! Revision 1.144  2016/03/30 18:06:04  driethmi
  ! For now, avoid using spec_scale_plaw for analytic power law spectrum.  Will
  ! implement a better algorithm later.
  !
  ! Revision 1.143  2016/03/25 14:55:06  driethmi
  ! Implemented spec_scale_plaw routine.
  !
  ! Revision 1.142  2016/03/24 15:10:58  driethmi
  ! Added block to report some values on the current source as we loop over
  ! sources.
  !
  ! Revision 1.141  2016/03/23 20:30:17  driethmi
  ! In the pathological case where there are zero events in the output EVENTS
  ! HDU, population of the GTI extension is now skipped to avoid an attempt
  ! to read the empty events column.
  !
  ! Revision 1.140  2015/12/29 18:24:05  driethmi
  ! Renamed misleading variable "ichan" to generic loop variable name "iii",
  ! since it's not really a loop over channels.
  !
  ! Revision 1.139  2015/12/29 18:15:51  driethmi
  ! Added ability to skip field of view restriction and/or RMF implementation.
  ! User sets parameters skipfov and/or skiprmf, respectively, to yes/no.
  !
  ! Revision 1.138  2015/11/25 16:24:37  driethmi
  ! Fixed bug in background - use nn_ibspec_block instead of nkeep.
  !
  ! Revision 1.137  2015/11/13 03:19:33  driethmi
  ! Minor bug fixes related to resampling rescaling.
  !
  ! Revision 1.136  2015/11/12 18:49:26  driethmi
  ! Commented out reporting of deadtime statistics, which are not very useful
  ! and can be confusing.
  !
  ! Revision 1.135  2015/11/12 18:34:47  driethmi
  ! Removed SXS special case in isrealpixel() calls - this is now unnecessary.
  !
  ! Revision 1.134  2015/11/12 18:31:17  driethmi
  ! Corrected a bug in the new resampling algorithm. The detector offsets in the mdb
  ! file taken from the teldef are in detector pixel units but, since we do
  ! everything in FOC we need to multiply by the ratio of those decrements to get
  ! the correct value.
  !
  ! Revision 1.133  2015/11/12 18:07:42  driethmi
  ! If TLMIN1 is not found in background PHA file, then get it by reading the
  ! first CHANNEL column entry and assign it to ib_firstchan.
  !
  ! Revision 1.132  2015/11/09 14:44:07  driethmi
  ! Now check for first channel number in background file - usually 0, but
  ! use ib_firstchan to be safe.  If ib_firstchan != ebounds_TLMIN1, then
  ! exit with error.
  !
  ! Revision 1.131  2015/11/05 21:33:16  driethmi
  ! Inserted updated resampling capabilities which account for nonzero roll.
  !
  ! Revision 1.130  2015/11/04 20:22:25  driethmi
  ! Minor changes to resampling in doWork.c
  !
  ! Revision 1.129  2015/11/03 18:30:07  driethmi
  ! Added resampling capability for background events.
  !
  ! Revision 1.128  2015/11/03 18:22:25  driethmi
  ! Added capability for resampling pixel size.
  !
  ! Revision 1.127  2015/10/16 15:40:43  driethmi
  ! Changes to make sure pixid column is being sorted along with x, y, and pi
  ! according to time.  Problem didn't show up until pileup was engaged.
  !
  ! Revision 1.126  2015/08/05 14:39:23  driethmi
  ! Added line to report the correct number of subexposures.
  !
  ! Revision 1.125  2015/07/10 14:10:07  driethmi
  ! In debug mode, now write files containing the cprob matrix output from
  ! process_image().
  !
  ! Revision 1.124  2015/07/02 14:21:31  driethmi
  ! Fixed type in background event block - nkeep should be nn_ibspec_block.
  !
  ! Revision 1.123  2015/07/01 18:46:34  driethmi
  ! Cleaned up checksum writing, so this happens at the end of the code.  Heasim
  ! output now passes ftverify check.
  !
  ! Revision 1.122  2015/07/01 17:37:29  driethmi
  ! If the final time block is less than 10% of the exposure block,
  ! then add it to the penultimate exposure block.
  !
  ! Revision 1.121  2015/07/01 15:00:33  driethmi
  ! When we do time sorting, we need to re-order X, Y, and PI to match.  Several
  ! mods to accommodate this - new structure "quad" that holds four doubles,
  ! some new sorting routines.
  !
  ! Revision 1.120  2015/06/30 14:23:31  driethmi
  ! In timeset(), commented out random assignment of phase_in, set to zero.
  !
  ! Revision 1.119  2015/06/30 14:02:39  driethmi
  ! Inserted flag flagsubex to trigger subexposure divisions - hidden, "no"
  ! by default.
  !
  ! Revision 1.118  2015/06/30 13:38:25  driethmi
  ! Corrected two instances in the background application, where s_obs->subexposure
  ! or s_obs->exposure should be exposure_block.
  !
  ! Revision 1.117  2015/06/26 17:23:49  driethmi
  ! Fixed typo - closed fits file too soon.
  !
  ! Revision 1.116  2015/06/26 16:50:04  driethmi
  ! Reordered the final commands in doWork - write the checksums, then remove
  ! the pixel id column, then write the GTI extension, then close the fits file.
  !
  ! Revision 1.115  2015/06/25 18:52:29  driethmi
  ! Changes to FITS keywords.
  !
  ! Revision 1.114  2015/06/25 18:15:42  driethmi
  ! Put more print statements under "debug" mode only.  Also changed order of
  ! "final event count" statements at the end of doWork().  Report that we've removed
  ! bad PI events before we report final event count; no change in functionaly,
  ! but let's not confuse the user.
  !
  ! Revision 1.113  2015/06/25 18:02:32  driethmi
  ! Moved background source computation inside the time block loop.  Updated
  ! output statements to print total number of events, total source events,
  ! and total background events.
  !
  ! Revision 1.112  2015/06/22 21:11:15  driethmi
  ! Now remove pixel_id column regardless of whether debug is set; debug
  ! should not alter functionality, only print more output to screen.
  !
  ! Revision 1.111  2015/06/22 21:01:37  driethmi
  ! Added function to write GTI HDU at end of output fits file.
  !
  ! Revision 1.110  2015/06/18 21:21:28  driethmi
  ! Changed time block optimization to go by 500,000 events, rather than 100,000.
  !
  ! Revision 1.109  2015/06/18 18:11:31  driethmi
  ! Inserted temporary fixes for periodic source cases in time block mode - solution
  ! is to force subexposure=exposure if we're doing a period source, so only
  ! one time block happens.
  !
  ! Revision 1.108  2015/06/17 19:22:53  driethmi
  ! Changed the start time argument passed to timeset_burst() to account for
  ! subexposure time blocks - instead of btime, now pass in btime-exp_completed.
  !
  ! Revision 1.107  2015/06/17 19:16:15  driethmi
  ! Changed subexposure print statement to occur regardless of debug mode.
  !
  ! Revision 1.106  2015/06/16 20:34:40  driethmi
  ! Built in optimization for sub-exposure blocks.  By default, subexposure
  ! parameter is now hidden and very large.  If subexposure > exposure, then
  ! recompute the optimal subexposure size.
  !
  ! Revision 1.105  2015/06/11 19:54:12  driethmi
  ! Commenting and minor cosmetic changes.
  !
  ! Revision 1.104  2015/06/11 18:54:05  driethmi
  ! Added purposely redundant set of nev=0 before the loop over blocks begins,
  ! just for reader clarity.
  !
  ! Revision 1.103  2015/06/11 18:35:17  driethmi
  ! Reworked flag_pileup() routine to accommodate new time-block layout.
  !
  ! Revision 1.102  2015/06/05 21:42:28  driethmi
  ! Minor cosmetic changes.
  !
  ! Revision 1.101  2015/06/05 21:36:19  driethmi
  ! Forgot to add command to actually write sorted time to fits file corrected.
  ! Also added more comments.
  !
  ! Revision 1.100  2015/06/05 21:21:19  driethmi
  ! Added "subexposure" as a parameter to the par file, and modified code to use
  ! it.  Now user specifies the total exposure time ("exposure"), and the sub-
  ! exposure time ("subexposure"), and the code divides the simulation into
  ! the appropriate sub-blocks, adding the final time assignment from the previous
  ! block to the time events in the next block.
  !
  ! Revision 1.99  2015/06/05 20:10:21  driethmi
  ! Added capability to divide user-set exposure into sub-exposures, and then
  ! increment the time values for counts in sequential exposures.  Solves the
  ! end time-sorting problem for low RAM-capable machines.
  !
  ! Revision 1.98  2015/05/29 18:34:02  driethmi
  ! Corrected typo when calling clean_pi_chans for the background events; arrays
  ! of size nn_ibspec are passed in, rather than size nkeep.
  !
  ! Revision 1.97  2015/05/29 17:50:48  driethmi
  ! Added function clean_ip_chans(), which removes events for which pi_chans has
  ! a value of -1, indicating that this event fell of the end of the channel array.
  ! It then resizes the xe, ye, ide, and pi_chans arrays to the correct size.  We now
  ! do this inline with the code, instead of at the end of heasim.
  !
  ! Revision 1.96  2015/05/22 17:24:36  driethmi
  ! Updated parameter names throughout code, changed header keywords/comments
  ! to match standard values.  Changed input user seed so that seed=0 triggers
  ! seeding from the system time.
  !
  ! Revision 1.95  2015/05/20 20:12:19  driethmi
  ! Modified mdb parameter names, and instances of these names in the code.  Also
  ! have updated values for mdb parameters.
  !
  ! Revision 1.94  2015/05/05 19:50:10  driethmi
  ! Fixed column removal block at end of doWork().  Needed to move to the correct
  ! HDU extension before attempting to remove columns.
  !
  ! Revision 1.93  2015/05/04 16:16:32  driethmi
  ! Implemented better variable initializations.
  !
  ! Revision 1.92  2015/04/28 15:42:51  driethmi
  ! Fixed some variable declarations to be initialized immediately, i.e.
  ! instead of "double xyz;" now have "double xyz=0.0".
  !
  ! Revision 1.91  2015/04/14 20:52:10  driethmi
  ! While looping over sources in doWork(), if there is ever a problem with the
  ! call to spectra() or torus_spectra(), we set a flag spectra_errstatus to
  ! -1, and pass this upwards.  Since spectra failures are non-cricital (meaning,
  ! we simply continue to the next source), this provides a way of reporting
  ! at the end that problems were encountered.
  !
  ! Revision 1.90  2015/04/14 19:36:13  driethmi
  ! Added check in spectra(), if user has supplied a self-defined spectra file,
  ! that the file actually exists.  If not, we set the spectra for that source
  ! to zero, and proceed to process any remaining sources.
  !
  ! Revision 1.89  2015/04/08 15:37:45  driethmi
  ! Replaced system call to copy files (during re-sorting at the end of doWork)
  ! with compiled function that achieves same result.  Other minor cosmetic
  ! changes.
  !
  ! Revision 1.88  2015/04/07 15:47:49  driethmi
  ! Corrected non-critical compilations warnings; unused variables, etc.
  !
  ! Revision 1.87  2015/04/01 21:16:57  driethmi
  ! Corrected possible bug in spectra routines - if energy grid is outside
  ! specrtra's bandpass transmission is zero; then the returned spectra should
  ! be zero, not infinity.
  !
  ! Revision 1.86  2015/03/26 20:45:30  driethmi
  ! Added capability to find torus1006.fits in refdata directory.
  !
  ! Revision 1.85  2015/03/24 15:45:10  driethmi
  ! If debug mode is NOT engaged, remove energy_in and pixel_id columns from
  ! output fits file.
  !
  ! Revision 1.84  2015/03/24 14:16:21  driethmi
  ! Corrected mistake in timeset_burst - dtn4 = dtn1 should happen AFTER dtn1
  ! is redefined.  Also added validation test to ensure that tburst >= 0.
  !
  ! Revision 1.83  2015/03/23 20:51:26  driethmi
  ! Previous changes required one additional mod, dtn4 = dtn1 in this case.
  !
  ! Revision 1.82  2015/03/23 20:36:22  driethmi
  ! timeset() and timeset_burst() now return an error status, which if not zero,
  ! causes doWork() to exit.
  !
  ! Revision 1.81  2015/03/23 20:26:02  driethmi
  ! Corrected problem in timeset_burst() - small number should be set to 1.0e-9, not
  ! 1.0e9.  Also reworked the block that populates dt_table to avoid segfault.
  !
  ! Revision 1.80  2015/03/19 20:35:30  driethmi
  ! Added burst capability, and updated function comment blocks.
  !
  ! Revision 1.79  2015/02/20 21:31:49  driethmi
  ! Re-inserted redshift changes, cleaned up output chatter.
  !
  ! Revision 1.76  2015/02/18 21:34:07  driethmi
  ! Corrected bug in process_image, needed to use fits_read_key_dbl instead of
  ! fits_read_key_flt - lack of precision was causing zeroes to be read.  Also
  ! improved verbosity of debug statements.
  !
  ! Revision 1.75  2015/02/18 20:15:18  driethmi
  ! Enabled application of redshift data to source simulation.
  !
  ! Revision 1.74  2015/02/18 18:52:51  driethmi
  ! Improved diagnostic output, restricted more output to debug == 1.
  !
  ! Revision 1.73  2015/02/18 18:15:01  driethmi
  ! Overwrote pulse bug fix - corrected and re-committed.
  !
  ! Revision 1.72  2015/02/18 15:53:41  driethmi
  ! Preliminary redshift changes, and cleaned up output chatter so debug = 1
  ! is more useful.
  !
  ! Revision 1.70  2015/02/11 21:39:09  driethmi
  ! Removed some unnecessary test print statements.
  !
  ! Revision 1.69  2015/01/29 17:48:23  driethmi
  ! torus_prep now complains and exits if torus1006.fits or a symlink to it
  ! does not exist in the current directory.
  !
  ! Revision 1.68  2015/01/28 21:32:20  driethmi
  ! Enabled torus spectral model for background point sources.
  !
  ! Revision 1.67  2015/01/21 17:25:02  driethmi
  ! Modified heasim to accept data files from sky background tool.
  !
  ! Revision 1.66  2014/12/02 19:57:40  driethmi
  ! Changed floats to doubles for consistency, except where float is required.
  !
  ! Revision 1.65  2014/09/05 19:34:36  driethmi
  ! Updated comment blocks before each function to reflect most recent versions.
  !
  ! Revision 1.64  2014/08/19 18:30:40  driethmi
  ! Added initial capability to read PSF images correctly.
  !
  ! Revision 1.63  2014/08/12 14:44:04  driethmi
  ! Replaced system calls to "fsort" with calls to "ftsort" to avoid depencence
  ! on the ftools library.
  !
  ! Revision 1.62  2014/08/05 14:13:02  driethmi
  ! Implemented temporary fix for SXS mdb/instmap mismatch.
  !
  ! Revision 1.61  2014/08/05 13:58:38  driethmi
  ! Only execute flag_pileup if dtpileup is specified greater than zero.
  !
  ! Revision 1.60  2014/07/31 13:45:48  driethmi
  ! Minor changes to detector ID computation.
  !
  ! Revision 1.59  2014/07/28 19:47:22  driethmi
  ! Commented out deadtime prints for now, and only copy fits file for sorted
  ! time and pixid if debug is set > 0.
  !
  ! Revision 1.58  2014/07/28 15:54:52  driethmi
  ! Added routine to flag pileup events in output fits file.
  !
  ! Revision 1.57  2014/07/25 18:21:15  driethmi
  ! Previous change was not complete.
  !
  ! Revision 1.56  2014/07/25 18:18:52  driethmi
  ! Moved the fits sort on time and the PI=-1 removal to inside doWork().
  !
  ! Revision 1.55  2014/07/25 14:41:54  driethmi
  ! Added capability to sort output fits file in time.
  !
  ! Revision 1.54  2014/07/11 18:16:58  driethmi
  ! Corrected typos in ysky.
  !
  ! Revision 1.53  2014/07/09 20:44:17  driethmi
  ! Changes in resampling and randomization of distribution pixels.
  !
  ! Revision 1.52  2014/07/01 14:36:04  driethmi
  ! Fixed type in RNG function call, typo in do-while loop conditional statement.
  !
  ! Revision 1.51  2014/07/01 13:47:15  driethmi
  ! Current state - added roll angle and image HDU capability.  Still some
  ! issues to work out with response file and background read.  Also, imagedis
  ! seems to have execution bottleneck.
  !
  ! Revision 1.50  2014/06/25 18:06:25  driethmi
  ! Added capability to specify HDU header when reading in image to process_image.
  !
  ! Revision 1.49  2014/06/16 15:27:45  driethmi
  ! Some changes in apply_psf routine.
  !
  ! Revision 1.48  2014/06/12 19:17:20  driethmi
  ! Changed sourcedis algorithm to rearrange position loops.
  !
  ! Revision 1.47  2014/06/12 18:55:47  driethmi
  ! Corrected typo in previous commit.
  !
  ! Revision 1.46  2014/06/12 18:54:05  driethmi
  ! Changes in apply_psf algorithm.  Should correct problems with Gaussian
  ! option.
  !
  ! Revision 1.45  2014/06/11 20:53:04  driethmi
  ! Rewrote loops to minmize memory usage, i.e. we now process one energy bin
  ! at a time, instead of one source at a time.
  !
  ! Revision 1.44  2014/06/02 20:48:18  driethmi
  ! Changed int_spectot from int type to long type.
  !
  ! Also fixed array indexing typo with ien.
  !
  ! Revision 1.43  2014/06/02 18:54:41  driethmi
  ! Added switch to use legacy C heasp library instead of active C++ library
  ! from heacore/heasp.
  !
  ! Revision 1.42  2014/05/30 14:51:28  driethmi
  ! Changed some int to long for safety, also changed some array allocations
  ! from size spectot to size nn_source.
  !
  ! Revision 1.41  2014/05/29 00:12:43  driethmi
  ! Changed several of the event counters from int to long, i.e. nn_out,
  ! nn_source, nev, nn_ibspec.  We're probably not at the int/long limit, but
  ! it's probably prudent to make this change.
  !
  ! Revision 1.40  2014/05/23 22:51:24  driethmi
  ! Added routine that warns the user if sources are specified with RA,DEC
  ! that lie within the CCD detector gaps of the instrument being used.
  !
  ! Revision 1.39  2014/05/23 21:51:38  driethmi
  ! Changed random number generator such that MTstate is passed down through
  ! each function using it.  Now, user must specify seed.  If seed > 0, it is
  ! used to seed the RNG.  If seed <=0, we seed using the system time.
  !
  ! Revision 1.38  2014/05/08 20:00:34  driethmi
  ! Routine that reads in user spectral file was overly stringent about the
  ! formatting of white space.  Made this routine more robust by removing
  ! white space before and after the line, and then parsing.
  !
  ! Revision 1.37  2014/05/08 14:00:19  driethmi
  ! Fixed bug in "if" clause to catch correct case.
  !
  ! Revision 1.36  2014/05/06 15:27:38  driethmi
  ! Realized that sdmat may also need to be double rather than int.
  !
  ! Revision 1.35  2014/05/06 13:41:35  driethmi
  ! Realized that imtot needs to be a double rather than an int.
  !
  ! Revision 1.34  2014/05/05 20:09:43  driethmi
  ! Cosmetic changes in print output.
  !
  ! Revision 1.33  2014/05/05 19:56:02  driethmi
  ! Realized that crvl1_img and crvl2_img need to be doubles and not ints;
  ! this was throwing off the imagedis() photon placement algorithm.
  !
  ! Revision 1.32  2014/05/05 13:57:04  driethmi
  ! Fixed bugs related to user-input wavelenth spectral file.
  !
  ! Revision 1.31  2014/05/02 18:17:29  driethmi
  ! Fixed bug in doWork that could void an energy bin of all photons.  Instead
  ! we now carry through the entire photon list before reducing int_specout.
  !
  ! Also added some debugging options to isrealpixel().
  !
  ! Revision 1.30  2014/04/30 18:14:30  driethmi
  ! Changed the return of isrealpixel() to be 1 if accept, 0 if reject.  Also
  ! ammended the logic in the routine.
  !
  ! Revision 1.29  2014/04/28 20:12:57  driethmi
  ! (xref,yref) should be set to (0,0) instead of (crpxx,crpxy).  We want
  ! to set in simulator coordinates, not sky coordinates.
  !
  ! Revision 1.28  2014/04/28 13:11:21  driethmi
  ! Made changes
  !
  ! Revision 1.27  2014/04/09 14:44:05  driethmi
  ! Added routine to report actual nonzero energy sensitivity of RMF.  If,
  ! for example, spectral lines are given outside of this range, we can expect
  ! a null simulation result.
  !
  ! Revision 1.26  2014/04/03 20:41:06  driethmi
  ! Updated commenting and minor changes to attempt to comply more with coding
  ! standards.  Added doxygen tags.
  !
*/
