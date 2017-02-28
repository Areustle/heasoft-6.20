/** \file doWork.c                                                                                                                               
    \brief skyback - contains routines needed for "doWork" section                                                                      
    \author  David Riethmiller                                                                                                                   
    \date $Date: 2015/10/06 14:54:46 $                                                                                                           
*/


#include "skyback.h"
#include "ape/ape_error.h"
#include "ape/ape_trad.h"
#include "fitsio.h"       /* cfitsio defined constants */
#include <stdlib.h>
#include <stdio.h>
#include <funcWrappers.h>  /* Xspec defs, funcWrappers.cxx */
#include <math.h>
#include <ctype.h>
//#include <xlocale.h>
#include <sys/time.h>
#include <string.h>


/* FUNCTION NAME: doWork                                                    */
/*                                                                          */
/* PURPOSE:                                                                 */
/*    Perform the primary work of the sky background task; sum              */
/*    background contributions from each type of source.                    */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*    errstatus = doWork(&s_files, &s_obs, &s_engrid, &s_flags, &s_logNS,   */
/*                       &s_flux, debug, clobber, MTstate);                 */
/*                                                                          */
/* INPUTS: s_files, s_obs, s_engrid, s_flags, s_logNS, s_flux, debug,       */
/*         clobber, MTstate                                                 */
/*                                                                          */
/* OUTPUTS: errstatus, output FITS file with background data                */
/*                                                                          */
/* CALLED BY: main()                                                        */
/*                                                                          */

int doWork(Filenames_struct * s_files,
	   Obs_params_struct * s_obs,
	   Engrid_struct * s_engrid,
	   Flag_struct * s_flags,
	   logN_logS_struct * s_logNS,
	   Flux_struct * s_flux,
	   int debug,
	   int clobber,
	   HDmt_state * MTstate){
    
    int ira=0, idec=0;
    int ncat=0;  /* total number of sources */
    double ** ps_cat = 0;
    double ** nh_arr = 0;
    long nx_nh=0, ny_nh=0;
    FILE *fp = 0;
    FILE *fzz = 0;
    char bpass_string[STR_MAX_LEN];
    int errstatus=0;
    int ii=0;
    double * ps_spec = 0;
    double * tot_spec = 0;
    int nebin = 0;
    double * nhcat = 0;
    double * zcat = 0;
    double * swcx_spec = 0;
    double * gal_spec = 0;
    double * dgrb_spec = 0;
    double * grxe_spec = 0;
    char * headas = 0;
    char ranh[STR_MAX_LEN];


    if (clobber == 1){
        printf("Clobber engaged, removing files.\n\n");
	errstatus = remove(s_files->difspec_fitsfile);
        errstatus = remove(s_files->difspec_datfile);
	errstatus = remove(s_files->difsource_txtfile);
	errstatus = remove(s_files->psource_txtfile);
	errstatus = remove(s_files->zsource_txtfile);
    }


    /* Initialize all of the Xspec routines */
    FNINIT();

    /* get galactic NH: */
    /* open ranh.fits, put image values in array nh_arr */
    headas = getenv("HEADAS");
    sprintf(ranh,"%s%s",headas,"/refdata/ranh.fits");
    errstatus = image_to_array(ranh, &nx_nh, &ny_nh, &nh_arr, debug);

    /* Identify map locations.  Should really use header info, but some of these are wrong. */
    /* (this is the same as "cosmicbackground" from quicksim) */
    ira = floor(s_obs->ra) + 1;
    if (ira < 1) ira = 1;
    if (ira > 360) ira = 360;
    idec = 91 + floor(s_obs->dec);
    if(idec<1) idec = 1;
    if (idec > 180) idec = 180;

    /* array index from [0-180, 0-89] for nh_arr */
    s_obs->nh_gal = 1.0e18 * nh_arr[idec/2-1][ira/2-1];


    if (s_flags->flaglogns == 1){

	int icat = 0;
	double ra_cat = 0.0;
	double dec_cat = 0.0;
	double flux_cat = 0.0;
	char smod_string[STR_MAX_LEN];
	double abs_cat = 0.0;
	double specpar = 0.0;

	printf("\n======== Computing point source contributions. ========\n");
	

	/* psources returns a psource catalog and a single diffuse spectrum to be merged */
	psources(s_obs, s_logNS, s_engrid, &ncat, &ps_cat, &ps_spec, &zcat, &nhcat, MTstate, debug);
	
	printf("   Total number of sources: %d\n",ncat);

	/* Write the point source catalog lines into source definition file. */
	/* ps_cat[icat,0] = ra for icat_th point source */
	/* ps_cat[icat,1] = dec for icat_th point source */
	/* ps_cat[icat,2] = NH for icat_th point source */
	/* ps_cat[icat,3] = specid for icat_th point source */
	/* ps_cat[icat,4] = spectral parameter for icat_th point source */
	/* ps_cat[icat,5] = flux for icat_th point source */
	
	sprintf(bpass_string,"%f-%f",s_logNS->bandpasslo,s_logNS->bandpasshi);
	
	for (icat=0; icat<ncat; icat++){ /* begin loop over sources */
	    ra_cat = ps_cat[icat][0];
	    dec_cat = ps_cat[icat][1];
	    flux_cat = ps_cat[icat][5];
	    
	    switch( (int)(ps_cat[icat][3]) ){  /* Cases for values of specid = ps_cat[icat][3] */
		
	    case 0: /* specid = 0: "catalog-1" (broken power-law logN-logS) sources with identical spectra determined by user input */
		sprintf(smod_string,"%s",s_logNS->specmod1); 
		abs_cat = s_logNS->nhmod1 + s_obs->nh_gal;
		specpar = s_logNS->specpar1;
		break;
		
	    case 1: /* specid = 1: "multi" (Gilli) option in Compton thin regime */
		strcpy(smod_string,"pow");
		abs_cat = ps_cat[icat][2];
		specpar = ps_cat[icat][4];
		break;

	    case 2: /* specid = 2: "torus" option */
		strcpy(smod_string,"torus");
		abs_cat = ps_cat[icat][2];
		specpar = ps_cat[icat][4];
		break;
		
	    case 3: /* specid = 3: "multi" (Gilli) option in very Compton thick regime */
		strcpy(smod_string,"gilli_thick");
		abs_cat = ps_cat[icat][2];
		specpar = ps_cat[icat][4];
		break;
		
	    case 4: /* specid = 4: "multi" (Gilli) option in mildly Compton thick regime */
		strcpy(smod_string,"gilli_mild");
		abs_cat = ps_cat[icat][2];
		specpar = ps_cat[icat][4];
		break;
		
	    case 5: /* specid = 5: "catalog-2" sources with identical spectra determined by user input */
		sprintf(smod_string,"%s",s_logNS->specmod2); 
		abs_cat = s_logNS->nhmod2 + s_obs->nh_gal;
		specpar = s_logNS->specpar2;
		break;
		
	    } /* end switch */
	    
	    /* write a source line into psource_txtfile */
	    if (icat == 0){
		fp = fopen(s_files->psource_txtfile,"w");
	    } else {
		fp = fopen(s_files->psource_txtfile,"a");
	    }
	    fprintf(fp,"%f, %f, %e, %s, %f, %e, %s, none, 1, 1\n",
		    ra_cat, dec_cat, abs_cat, smod_string, specpar, flux_cat, bpass_string);
	    fclose(fp);
	    
	    if (icat == 0){
		fzz = fopen(s_files->zsource_txtfile,"w");
	    } else {
		fzz = fopen(s_files->zsource_txtfile,"a");
	    }
	    fprintf(fzz,"%f, %e \n", zcat[icat], nhcat[icat]);
	    fclose(fzz);    



	} /* end for loop over sources */

	printf("...done with point sources.\n");
    } /* end if flaglogns == 1 */    


    if (s_flags->flaggal == 1){
	printf("\n======== Computing galactic contributions. ========\n");
	galactic(s_obs, s_engrid, s_flux, &gal_spec, debug); /* returns single diffuse spectrum to be merged */
	printf("...done with galactic contributions.\n");
    }
    
    if (s_flags->flagswcx == 1){
	printf("\n======== Computing SWCX contributions. ========\n");
	swcx(s_obs, s_engrid, s_flux, &swcx_spec, debug); /* returns single diffuse multi-mono model to be merged */
	printf("...done with SWCX contributions.\n");
    }
    
    if (s_flags->flaggrxe == 1){
	printf("\n======== Computing GRXE contributions. ========\n");
	grxe(s_obs, s_engrid, &grxe_spec, debug);
	printf("...done with GRXE contributions.\n");
    }

    if (s_flags->flagdgrb == 1){
	printf("\n======== Computing DGRB contributions. ========\n");
	dgrb(s_obs, s_engrid, &dgrb_spec, debug);
	printf("...done with DGRB contributions.\n");
    }

    nebin = s_engrid->nebin;
    tot_spec = calloc(nebin, sizeof(double));

    /* merge diffuse spectra from summing unresolved point source, galactic (halo, lhb), and swcx: */
    fp = fopen(s_files->difspec_datfile,"w");
    for (int iebin=0; iebin<nebin; iebin++){

	/* Add in contributions from enabled sources */
	tot_spec[iebin] = 0.0;

	if (s_flags->flaglogns == 1)
	    tot_spec[iebin] += ps_spec[iebin];
	
	if (s_flags->flaggal == 1)
	    tot_spec[iebin] += gal_spec[iebin];
	
	if (s_flags->flagswcx == 1)
	    tot_spec[iebin] += swcx_spec[iebin];
	
	//if (s_flags->flagdgrb == 1)
	//tot_spec[iebin] += dgrb_spec[iebin];

	//if (s_flags->flaggrxe == 1)
	//tot_spec[iebin] += grxe_spec[iebin];
	
	fprintf(fp,"%e %e\n",s_engrid->energ_mid[iebin],tot_spec[iebin]);
    }
    fclose(fp);

    /* write source line into difsource_txtfile */
    /* to be treated by heasim as a flat model with the above spectrum, intrinsic and Galactic absorption already included. */
    /* ra, dec, 0.0, "user", 0, 0, 0.0-0.0, name_spec, 1, 1, extmod(flat,0,radius) */
    fp = fopen(s_files->difsource_txtfile,"w");
    fprintf(fp,"%f, %f, %f, %s, %d, %d, %s, %s, %d, %d, extmod(flat,0,%f)\n",
	    s_obs->ra, s_obs->dec, 0.0, "user", 0, 0, "0.0-0.0", s_files->difspec_datfile,
	    1, 1, s_obs->radius);
    fclose(fp);

    
    /* write the table model fits file for the diffuse spectrum */
    write_table_model(s_engrid, s_files->difspec_fileroot, s_files->difspec_fileroot, tot_spec);
    
    for (ii=0; ii<nx_nh+1; ii++)
	free(nh_arr[ii]);
    free(nh_arr);

    
    if (s_flags->flaglogns == 1){
	if (ncat > 1){
	    for (ii=0; ii<ncat; ii++)
		free(ps_cat[ii]);
	}
	free(ps_cat);
	free(ps_spec);
	free(nhcat);
	free(zcat);
    }

    if (s_flags->flaggal == 1)
	free(gal_spec);

    if (s_flags->flagswcx == 1)
	free(swcx_spec);

    if (s_flags->flagdgrb == 1)
        free(dgrb_spec);

    if (s_flags->flaggrxe == 1)
        free(grxe_spec);

    free(tot_spec);

    return errstatus;
}


/* FUNCTION NAME: psources                                                  */
/*                                                                          */
/* PURPOSE:                                                                 */
/*    Calculate the resolved and unresolved background from a population    */
/*    of extragalactic point sources.                                       */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*  psources(s_obs, s_logNS, s_engrid, &ncat, &ps_cat, &ps_spec, MTstate);  */
/*                                                                          */
/* INPUTS: s_obs, s_logNS, s_engrid, MTstate                                */
/*                                                                          */
/* OUTPUTS: ncat, ps_cat, ps_spec, zcat, nhcat, MTstate                     */
/*                                                                          */
/* CALLED BY: doWork()                                                      */
/*                                                                          */

int psources(Obs_params_struct * s_obs,
	     logN_logS_struct * s_logNS,
	     Engrid_struct * s_engrid,
	     int * ncat,
	     double *** ps_cat,
	     double ** ps_spec,
	     double ** zcat,
	     double ** nhcat,
	     HDmt_state *MTstate,
	     int debug){
    
    double area = 0.0;
    double frat_max_min = 0.0;
    double frat_thr_min = 0.0;
    double frat_max_thr = 0.0;

    /* type 2 flux parameters */
    double alpha_2 = 0.0;
    double omal2 = 0.0;
    double tmal2 = 0.0;
    double totnsrc = 0.0;
    double exp_nsrc_2 = 0.0;
    int ns_2 = 0;
    double estflux_unr_2 = 0.0;
    double flux_tot_2 = 0.0;
    double flux_res_2 = 0.0;
    double flux_unr_2 = 0.0;
    int itype2 = 0;
    double * scat_2=0;
    double xrand = 0.0;

    /* type 1 flux parameters */
    double alpha_1b = 0.0;
    double omal1b = 0.0;
    double tmal1b = 0.0;
    double alpha_1f = 0.0;
    double omal1f = 0.0;
    double tmal1f = 0.0;
    double ymax=0.0, ymin=0.0, ylo=0.0;
    double exp_nsrc_1 = 0.0;
    double ncon = 0.0;
    double fcon = 0.0;
    double flux_tot_1 = 0.0;
    double flux_res_1 = 0.0;
    double flux_unr_1 = 0.0;
    double estflux_unr_1 = 0.0;
    int ntable = 0;
    double * cumprob;
    double * ytable;
    double bin = 0.0;
    double ytemp = 0.0;
    int itable = 0;
    double xrand1 = 0.0;
    int itype1 = 0;
    int ii=0;
    double * scat_1;
    int ns_1 = 0;

    double sincenterdec = 0.0;
    double coscenterdec = 0.0;

    double * ra_cat_1=0;
    double * dec_cat_1=0;
    double * ra_cat_2=0;
    double * dec_cat_2=0;

    int errstatus = 0;

    int igrid = 0;

    double cprob_abs[6];
    double cprob_par[6];

    double * par_cat_1 = 0;
    double * nh_cat_1 = 0;

    double abs_grid_lo[6];
    double abs_grid_hi[6];
    double abs_grid_mid[6];
    double prob_abs[6];
    double prob_par[6];
    double par_grid_lo[6];
    double par_grid_hi[6];
    double par_grid_mid[6];

    double ctheta = 0.0;
    double incang = 0.0;
    double gam_gtor = 0.0;

    int index = 0;
    int iebin = 0;

    double * spec1 = 0;
    double * spec2 = 0;
    double * xsphab_trans1 = 0;
    double * xsphab_trans2 = 0;

    long torus_index = 0;
    int spectype = 0;

    double * xsphab_trans_gal = 0;

    torus_par_struct s_tpar;
    torus_spec_struct s_tspec;
    double gam_tor = 0.0;

    /* redshift-related variables */
    double zmax = 0.0;
    double fzcon = 0.0;
    int nredshifts = 0;
    double * ztor = 0;
    double * pred = 0;
    double z0 = 0.0;
    


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



    /* TRF says to unpack structures here - do we need to do this? */

    /* ======================GET FLUXES FOR POINT SOURCES BLOCK =========================*/
    /* calculate the total expected number of sources, catlog fluxes for both "types" */
    
    /* area in sq-deg */
    area = PI * s_obs->radius * s_obs->radius / 3600.0;
    
    frat_max_min = s_logNS->fluxmax / s_logNS->fluxmin;
    frat_thr_min = s_logNS->slo / s_logNS->fluxmin;
    frat_max_thr = s_logNS->fluxmax / s_logNS->slo;

    /* ================== "type 2" logN-logS (single slope) ===================*/
    /* alpha = slope of dN/dS */
    alpha_2 = s_logNS->slope2 + 1;
    omal2 = 1 - alpha_2;
    tmal2 = 2 - alpha_2;

    /* multiply by sq-degrees to get total type-2 sources in FOV: */
    totnsrc = area * s_logNS->norm2; 

    if  (alpha_2 == 1) {   
	/* expected number of type-2 sources above threshold: */
	exp_nsrc_2 = totnsrc * ( (log(frat_max_min) - log(frat_thr_min)) / log(frat_max_min) );

	/* total integrated flux: */
	flux_tot_2 = totnsrc * s_logNS->fluxmin * ( (frat_max_min - 1) / log(frat_max_min) );

	/* estimated unresolved flux = integrated flux below threhsold: */
	estflux_unr_2 = totnsrc * s_logNS->fluxmin * ( (frat_thr_min - 1) / log(frat_max_min) );

    } else {

	if  (alpha_2 == 2) {    
	    /* expected number of type-2 sources above threshold: */
	    exp_nsrc_2 = totnsrc * ( ((1.0/frat_thr_min) - (1.0/frat_max_min)) / (1.0 - (1.0/frat_max_min)) );

	    /* total integrated flux: */
	    flux_tot_2 = totnsrc * s_logNS->fluxmin * ( (log(frat_max_min)) / (1.0 - (1.0/frat_max_min)) ); 

	    /* estimated unresolved flux = integrated flux below threhsold: */
	    estflux_unr_2 = totnsrc * s_logNS->fluxmin * ( (log(frat_thr_min)) / (1.0 - (1.0/frat_max_min)) ); 

	} else {

	    /* expected number of type-2 sources above threshold: */
	    exp_nsrc_2 = totnsrc * (pow(frat_max_min,omal2) - pow(frat_thr_min,omal2)) / (pow(frat_max_min,omal2) - 1);

	    /* total integrated flux: */
	    flux_tot_2 = totnsrc * s_logNS->fluxmin * (omal2 / tmal2) * ( (pow(frat_max_min,tmal2) - 1) / (pow(frat_max_min,omal2) - 1) );

	    /* estimated unresolved flux = integrated flux below threhsold: */
	    estflux_unr_2 = totnsrc * s_logNS->fluxmin * (omal2 / tmal2) * ( (pow(frat_thr_min,tmal2) - 1) / (pow(frat_max_min,omal2) - 1) );           
	}     
    }

    /* total number of sources from expected number and poisson statistics using poidev function in 
       utils.c [effect of counting statistics (and noise) on the distribution could be accounted for in a 
       more complex way] */

    ns_2 = floor(poidev(exp_nsrc_2, MTstate));
    printf("   ns_2 = %d\n",ns_2);    

    /* draw from cumulative probability distribution to derive "type 2" catalog fluxes (analytic for 
       single point source); assuming use of Mersenne twister random number generator as in heasim */
    flux_res_2 = 0.0; /* actual flux in resolved sources*/

    if (ns_2 == 0){
	scat_2 = calloc(1, sizeof(double));
	flux_res_2 = 0;

    }  else {
	scat_2 = calloc(ns_2, sizeof(double));
	
	for (itype2 = 0; itype2 < ns_2; itype2++) {         
	    xrand = HDmt_drand(MTstate);
	    
	    if (alpha_2 != 1){ 
		scat_2[itype2] = s_logNS->slo * pow( (1.0 + (pow(frat_max_thr,omal2) -1)*xrand),(1/omal2) );
	    } else {
		/* special case: slope=1 */                    
		scat_2[itype2] = s_logNS->slo * pow(frat_max_thr,xrand);
	    }
	    
	    flux_res_2 += scat_2[itype2]; 
	}
    } /* end if (ns_2 == 0) */

    flux_unr_2 = flux_tot_2 - flux_res_2; /* actual "leftover" flux in unresolved sources */

    if (flux_unr_2 < 0.0){
	flux_unr_2 = 0.0;
	flux_tot_2 = flux_res_2;
    }

    printf("   flux_tot_2 =     %e\n",flux_tot_2);
    printf("   flux_res_2 =     %e\n",flux_res_2);
    printf("   estflux_unr_2 =  %e\n",estflux_unr_2);
    printf("   flux_unr_2 =     %e\n",flux_unr_2);
    
    

    /* =================== "type 1" logN-logS (broken) ============================== */
    
    alpha_1b = s_logNS->slopebright1 + 1;
    omal1b = 1 - alpha_1b;
    tmal1b = 2 - alpha_1b;

    alpha_1f = s_logNS->slopefaint1 + 1;
    omal1f = 1 - alpha_1f;
    tmal1f = 2 - alpha_1f;

    ymax = s_logNS->fluxmax / s_logNS->fluxbreak;
    ymin = s_logNS->fluxmin / s_logNS->fluxbreak;
    ylo = s_logNS->slo / s_logNS->fluxbreak;

    /* multiply by detector sq-deg */
    ncon = area * s_logNS->norm1 / eta(omal1f, omal1b, ymax, ymin);
    exp_nsrc_1 = ncon * eta(omal1f, omal1b, ymax, ylo);

    /* total number of sources from expected number and poisson statistics using poidev function */
    ns_1 = (int)poidev(exp_nsrc_1, MTstate);
    printf("   ns_1 = %d\n",ns_1);

    /* total flux */
    fcon = ncon * s_logNS->fluxbreak;
    flux_tot_1 = fcon * eta(tmal1f, tmal1b, ymax, ymin);

    /* estimated flux (in resolved point sources) above threshold: */
    flux_res_1 = fcon * eta(tmal1f,tmal1b,ymax,ylo);
    /* estimated flux (in unresolved point sources) below threshold: */
    estflux_unr_1 = flux_tot_1 - flux_res_1;

    /* make a cumulative probability table of size ntable */
    ntable = 1000;
    cumprob = calloc(ntable, sizeof(double));
    ytable = calloc(ntable, sizeof(double));

    bin = pow(ymax/ylo, 1.0/(ntable-1));
    ytemp = ylo/bin;

    for (itable=0; itable<ntable; itable++){
	ytable[itable] = ytemp * bin;
	cumprob[itable] = ( eta(omal1f, omal1b, ymax, ylo) - eta(omal1f, omal1b, ymax,  ytable[itable]) ) /
	    ( eta(omal1f, omal1b, ymax, ylo) );
	ytemp = ytable[itable];
    }

    if (ns_1 == 0){
	/* just allocate a dummy scat_1 so later calls are valid */
        scat_1 = calloc(1, sizeof(double));
	flux_res_1 = 0.0;
    } else {
	double yy = 0.0;
	scat_1 = calloc(ns_1,sizeof(double));

	/* random draws from this table */
	flux_res_1 = 0.0;
	for (itype1=0; itype1<ns_1; itype1++){
	    xrand = HDmt_drand(MTstate);
	    ii=0;
	    while ( (xrand > cumprob[ii]) && (ii<ntable) ) ii++;
	    if (ii >= ntable){
		yy=ymax;
	    } else if (ii <= 0){
		yy=ylo;
	    } else {
		/* between ytable[ii-1] and ytable[ii] */
		xrand1 = HDmt_drand(MTstate);
		yy = ylo * pow(bin, ( (double)ii-1.0+xrand1) );
		scat_1[itype1] = s_logNS->fluxbreak * yy;
	    }
	    flux_res_1 += scat_1[itype1];
	}
    }
    flux_unr_1 = flux_tot_1 - flux_res_1;

    if (flux_unr_1 < 0.0){
        flux_unr_1 = 0.0;
        flux_tot_1 = flux_res_1;
    }

    printf("   flux_tot_1 =     %e\n",flux_tot_1);
    printf("   flux_res_1 =     %e\n",flux_res_1);
    printf("   estflux_unr_1 =  %e\n",estflux_unr_1);
    printf("   flux_unr_1 =     %e\n",flux_unr_1);

    /* ==================== END GET FLUXES BLOCK =============================*/

    /* GET POSITIONS FOR POINT SOURCES BLOCK */

    /* precalculate sines and cosines */
    sincenterdec = sin(D2R * s_obs->dec);
    coscenterdec = cos(D2R * s_obs->dec);

    /* assign random RAs and DECs within chosen aperture */
    if (ns_1 == 0) {
	ra_cat_1 = calloc(1, sizeof(double));
	dec_cat_1 = calloc(1, sizeof(double));
    }  else {
	errstatus = skyrand(ns_1, s_obs, sincenterdec, coscenterdec, &ra_cat_1, &dec_cat_1, MTstate);
    }

    if (ns_2 == 0) {
	ra_cat_2 = calloc(1, sizeof(double));
	dec_cat_2 = calloc(1, sizeof(double));
    }  else {
	errstatus = skyrand(ns_2, s_obs, sincenterdec, coscenterdec, &ra_cat_2, &dec_cat_2, MTstate);
    }

    /* END GET POSITIONS BLOCK */

    

    /* ============== GET SPECTRA (SPECTRAL MODELS) FOR POINT SOURCES BLOCK =====================*/
    if ( ((s_logNS->spectype1 == 1) || (s_logNS->spectype1 == 2) ) ){

	/* distinct spectral parameters for each point source, else all same */

	/* absorption grid: */
	for (igrid=0; igrid<6; igrid++){
	    abs_grid_lo[igrid] = 20.0 + (double)igrid;
	    abs_grid_hi[igrid] = abs_grid_lo[igrid] + 1.0;
	    abs_grid_mid[igrid] = 0.5 * (abs_grid_hi[igrid] + abs_grid_lo[igrid]);
	}

	abs_grid_hi[5] = 26.0;
	prob_abs[0] = s_logNS->fabs0;
	prob_abs[1] = s_logNS->fabs1;
	prob_abs[2] = s_logNS->fabs2;
	prob_abs[3] = s_logNS->fabs3;
	prob_abs[4] = s_logNS->fabs4;
	prob_abs[5] = s_logNS->fabs5;
	for (igrid=0; igrid<6; igrid++) cprob_abs[igrid] = prob_abs[igrid];
	for (igrid=1; igrid<6; igrid++) cprob_abs[igrid] += cprob_abs[igrid-1];

	/* spectral parameter grid: */
	/*  power-law index for "multi" (spectype1=1) */
	/*  opening angle for "torus" (spectype1=2) */
	if (s_logNS->spectype1 == 1){
	    for (igrid=0; igrid<5; igrid++){
		par_grid_lo[igrid] = 1.5 + 0.2 * (double)igrid;
		par_grid_hi[igrid] = par_grid_lo[igrid] + 0.2;
		par_grid_mid[igrid] = 0.5 * (par_grid_lo[igrid] + par_grid_hi[igrid]);
	    }
	} else {
	    for (igrid=0; igrid<5; igrid++){
		par_grid_lo[igrid] = 15.0 * (double)(igrid+1);
		par_grid_hi[igrid] = par_grid_lo[igrid] + 15.0;
		par_grid_mid[igrid] = 0.5 * (par_grid_lo[igrid] + par_grid_hi[igrid]);
	    }
	    par_grid_lo[0] = 0.0;
	    par_grid_mid[0] = 0.5 * (par_grid_lo[0] + par_grid_hi[0]);
	} /* end if (spectype1 == 1) */

	prob_par[0] = s_logNS->fpar0;
	prob_par[1] = s_logNS->fpar1;
	prob_par[2] = s_logNS->fpar2;
	prob_par[3] = s_logNS->fpar3;
	prob_par[4] = s_logNS->fpar4;
	for (igrid=0; igrid<5; igrid++) cprob_par[igrid] = prob_par[igrid];
	for (igrid=1; igrid<5; igrid++) cprob_par[igrid] += cprob_par[igrid-1];
    }


    errstatus = torus_prep(&s_tpar, &s_tspec, debug);
    gam_tor = 1.8;

    if (s_logNS->samespec == 1){

	/* If we have the same spectype, join the two catalogs together */

	printf("   samespec = 1, merging catalogs.\n");

	if (ns_1 > 0){
	    
	    /* Augment the size of type-1 catalogs, while preserving currently-stored data */

	    double * temp_array1 = 0;
	    double * temp_array2 = 0;
	    double * temp_array3 = 0;

	    temp_array1 = calloc(ns_1, sizeof(double));
	    temp_array2 = calloc(ns_1, sizeof(double));
	    temp_array3 = calloc(ns_1, sizeof(double));
	    
	    for (ii=0; ii<ns_1; ii++){
		temp_array1[ii] = ra_cat_1[ii];
		temp_array2[ii] = dec_cat_1[ii];
		temp_array3[ii] = scat_1[ii];
	    }

	    free(ra_cat_1);
	    free(dec_cat_1);
	    free(scat_1);
            ra_cat_1 = calloc(ns_1+ns_2, sizeof(double));
            dec_cat_1 = calloc(ns_1+ns_2, sizeof(double));
	    scat_1 = calloc(ns_1+ns_2, sizeof(double));
	    
	    for (ii=0; ii<ns_1; ii++){
		ra_cat_1[ii] = temp_array1[ii];
		dec_cat_1[ii] = temp_array2[ii];
		scat_1[ii] = temp_array3[ii];
	    }
	    
	    free(temp_array1);
	    free(temp_array2);
	    free(temp_array3);
	    
	} else {

	    /* Augment the size of type-1 catalogs, which currently store no data */

            free(ra_cat_1);
            free(dec_cat_1);
            free(scat_1);
            ra_cat_1 = calloc(ns_1+ns_2, sizeof(double));
            dec_cat_1 = calloc(ns_1+ns_2, sizeof(double));
            scat_1 = calloc(ns_1+ns_2, sizeof(double));
	}
	
	if (ns_2 > 0){
	    for (itype2=0; itype2<ns_2; itype2++){
		ra_cat_1[ns_1+itype2] = ra_cat_2[itype2];
		dec_cat_1[ns_1+itype2] = dec_cat_2[itype2];
		scat_1[ns_1+itype2] = scat_2[itype2];
	    }
	}
	ns_1 += ns_2;
	ns_2 = 0;
    }

    nh_cat_1 = calloc(ns_1, sizeof(double));
    par_cat_1 = calloc(ns_1, sizeof(double));

    /* total number of point sources: */
    *ncat = ns_1 + ns_2;

    if (*ncat == 0){
        printf("   There are no sources above flux sensitivity.\n");

    } else {
    

	if ( ((s_logNS->spectype1 == 1) || (s_logNS->spectype1 == 2) ) && (ns_1 > 0) ){
	    
	    /* random draws from NH distribution */
	    for (itype1=0; itype1<ns_1; itype1++){
		xrand = HDmt_drand(MTstate);
		ii=0;
		while(xrand > cprob_abs[ii]) ii++;
		xrand1 = HDmt_drand(MTstate);
		nh_cat_1[itype1] = abs_grid_lo[ii] + xrand1*(abs_grid_hi[ii]-abs_grid_lo[ii]);
	    }
	    
	    /* random draws from spectral parameter distribution */
	    for (itype1=0; itype1<ns_1; itype1++){
		xrand = HDmt_drand(MTstate);
		ii=0;
		while(xrand > cprob_par[ii]) ii++;
		xrand1 = HDmt_drand(MTstate);
		par_cat_1[itype1] = par_grid_lo[ii] + xrand1*(par_grid_hi[ii]-par_grid_lo[ii]);
	    }
	    
	} /* end if (spectype1 == 1 || spectype1 == 2) */
	
	
	
	(*ps_cat) = calloc(*ncat, sizeof(double *));
	(*nhcat) = calloc(*ncat, sizeof(double));
	
	for (ii=0; ii<ns_1; ii++){
	    (*ps_cat)[ii] = calloc(6, sizeof(double));
	}
	
	for (itype1=0; itype1<ns_1; itype1++){
	    (*ps_cat)[itype1][0] = ra_cat_1[itype1];
	    (*ps_cat)[itype1][1] = dec_cat_1[itype1];
	    (*ps_cat)[itype1][5] = scat_1[itype1];
	    
	    switch(s_logNS->spectype1){
		double incang_refl = 0.0;
		double openang_refl = 0.0;
		double gam_gmild = 0.0;
		double gam_gthick = 0.0;
		
	    case 0:
		(*ps_cat)[itype1][3] = 0.0;
		(*ps_cat)[itype1][2] = 0.0;
		(*ps_cat)[itype1][4] = 0.0;
		/* the last two are not determined here, overwritten in doWork */
		break;
		
	    case 1:
		if (nh_cat_1[itype1] < abs_grid_lo[4]){  /* "gilli_thin" = a power law */
		    //(*ps_cat)[itype1][2] = pow(10.0, nh_cat_1[itype1]) + s_obs->nh_gal; 
		    (*ps_cat)[itype1][2] = s_obs->nh_gal;
		    (*nhcat)[itype1] = pow(10.0,nh_cat_1[itype1]);
		    (*ps_cat)[itype1][3] = 1.0;
		    (*ps_cat)[itype1][4] = par_cat_1[itype1];
		    
		} else if (nh_cat_1[itype1] < abs_grid_lo[5]){  /* "gilli_mild" */
		    (*ps_cat)[itype1][2] = s_obs->nh_gal;  /* in this case, the intrinsic NH is built into the model */
		    (*nhcat)[itype1] = 0.0;
		    (*ps_cat)[itype1][3] = 4.0;
		    incang_refl = 0.0;  /* face-on for pure reflection, will be set to min available */
		    openang_refl = 90.0; /* 90 deg opening angle for pure reflection, will be set to max available */
		    gam_gmild = par_cat_1[itype1];
		    errstatus = find_torus_index(&s_tpar,gam_gmild,
						 pow(10.0,(nh_cat_1[itype1]-22)),openang_refl,incang_refl,&torus_index);
		    (*ps_cat)[itype1][4] = (double)torus_index;
		    
		} else {  /* "gilli_thick" */
		    (*ps_cat)[itype1][2] = s_obs->nh_gal;   /* in this case, the intrinsic NH is built into the model */
		    (*nhcat)[itype1] = 0.0;
		    (*ps_cat)[itype1][3] = 3.0;
		    incang_refl = 0.0;  /* face-on for pure reflection, will be set to min available */
		    openang_refl = 90.0; /* 90 deg opening angle for pure reflection, will be set to max available */
		    gam_gthick = par_cat_1[itype1];
		    errstatus = find_torus_index(&s_tpar,gam_gthick,
						 pow(10.0,(nh_cat_1[itype1]-22)),openang_refl,incang_refl,&torus_index);
		    (*ps_cat)[itype1][4] = (double)torus_index;
		} /* end if nh_cat_1 */
		break;
		
	    case 2:     /* torus */
		(*ps_cat)[itype1][2] = s_obs->nh_gal;   /* in this case, the intrinsic NH is built into the model */
		(*nhcat)[itype1] = 0.0;
		(*ps_cat)[itype1][3] = 2.0;
		ctheta = -1.0 + 2.0 * HDmt_drand(MTstate);  /* random inclination angle */
		incang = R2D * acos(ctheta);
		if (incang > 90.0) incang = 180.0 - incang;
		errstatus = find_torus_index(&s_tpar,gam_gtor,pow(10.0,(nh_cat_1[itype1]-22)),
					     par_cat_1[itype1],incang,&torus_index);
		(*ps_cat)[itype1][4] = (double)torus_index;
		break;
		
	    default:
		errstatus = -1;
		printf("   ERROR: s_logNS->spectype1 = %d is not supported.\n",s_logNS->spectype1);
		break;
	    }
	    
	} /* end for (itype1=0; itype1<ns_1; itype1++) */
	
	
	if (ns_2 > 0){
	    for (itype2=0; itype2<ns_2; itype2++){
		index = ns_1 + itype2;
		(*ps_cat)[index] = calloc(6, sizeof(double));
		(*ps_cat)[index][0] = ra_cat_2[itype2];
		(*ps_cat)[index][1] = dec_cat_2[itype2];
		(*ps_cat)[index][5] = scat_2[itype2];
		(*ps_cat)[index][3] = 5.0;
		(*ps_cat)[index][2] = 0.0;
		(*ps_cat)[index][4] = 0.0;
		/* the last two are not determined here, overwritten in doWork */
	    }
	}
	
	
	/* ######### GET REDSHIFTS FOR POINT SOURCES BLOCK ################### */
	
	/* type-1 (if spectype1 !=0) and type-2 (if samespec==TRUE) sources will be assigned redshifts for 
	   simple power-law, gilli, or torus model. */
	
	/* initialize redshifts to 0 */
	(*zcat) = calloc(*ncat, sizeof(double));
	
	
	/* ###SUGGEST INITIAL TESTING WITH THE ABOVE### */
	
	if (s_logNS->spectype1 !=0) {
	    
	    /* make a cumulative redshift probability table of size nztable */
	    int nztable = 100;
	    z0 = 1.0;
	    zmax = 10.0;
	    fzcon = 1.0 / (fz(z0,zmax)-fz(z0,0.0));
	    double zbin = pow( (1.0+zmax), (1.0 / ((double)(nztable-1))) );
	    ytemp = 1.0 / zbin;        
	    double zitable = 0.0;
	    double * cprobz = calloc(nztable, sizeof(double));
	    int ii = 0;
	    double yy = 0.0;
	    
	    for (int itable = 0; itable< nztable; itable++) {     
		zitable = ytemp * zbin - 1.0;
		cprobz[itable] = fzcon * (fz(z0,zitable)-fz(z0,0.0));
		ytemp = zitable + 1.0;
	    }
	    
	    for (itype1 = 0; itype1< ns_1; itype1++) {       
		xrand = HDmt_drand(MTstate);
		ii = 0;
		while ( (xrand > cprobz[ii]) && (ii < nztable) ) ii++;
		if (ii >= nztable) {
		    yy = 1.0 + zmax;
		} else if (ii <= 0) {
		    yy = 1.0;
		} else {
		    /* between zbin**ii-1 and zbin**ii */
		    xrand1 = HDmt_drand(MTstate);
		    yy = pow(zbin,( (double)ii-1.0+xrand1) );
		    (*zcat)[itype1] = yy - 1.0;
		}
	    } /* endfor (itype1 = 0,ns_1) */
	    
	    free(cprobz);
	} /* endif (spectype1 !=0) */
	
	/* ######### END GET REDSHIFTS FOR POINT SOURCES BLOCK ############ */
    }  /* end if (*ncat == 0) */
    
    /* ########## GET SPECTRA DIFFUSE SPECTRUM FOR UNRESOLVED SOURCES BLOCK ############## */
    /* average diffuse spectrum - to be returned to doWork where it is combined with "true diffuse spectra */

    xsphab_trans1 = calloc(s_engrid->nebin, sizeof(double));
    xsphab_trans2 = calloc(s_engrid->nebin, sizeof(double));
    
    /* initialize transmissions and spectra */
    
    spec1 = calloc(s_engrid->nebin, sizeof(double));
    spec2 = calloc(s_engrid->nebin, sizeof(double));
    
    for (iebin=0; iebin<s_engrid->nebin; iebin++)
	xsphab_trans1[iebin] = xsphab_trans2[iebin] = 1.0;
    

    if (s_logNS->samespec == 0){
	/* phabs transmission for type-2 sources: */
	errstatus = spectra(0, 0.0, s_logNS->nhmod2, 0.0, 0.0, 0.0, s_engrid->nebin, s_engrid->energ_mid, 
			    s_engrid->xspec_energy, xsphab_trans2, debug);
	/* source spectrum (phtons/cm^2/s/channel) for type-2 sources: */
	spectype = spec_string_to_type(s_logNS->specmod2);
	errstatus = spectra(spectype, 0.0, s_logNS->specpar2, flux_unr_2, s_logNS->bandpasslo, s_logNS->bandpasshi, 
			    s_engrid->nebin, s_engrid->energ_mid, s_engrid->xspec_energy, spec2, debug);

    } else {
	/* catalogs have been merged, use total unresolved flux */
	flux_unr_1 += flux_unr_2;
	flux_unr_2 = 0;
    }

    if (s_logNS->spectype1 !=0) {
	/* calculate the redshifts and weights for the diffuse spectrum */
	nredshifts = 10;
	ztor = calloc(nredshifts, sizeof(double));
	pred = calloc(nredshifts, sizeof(double));
	double zm = 5.0;
	double tpred = 0.0;
	double zfm = 1.0 / (1.0+zm);
	double zpfac = (1.0-zfm) / (double)nredshifts;
	double zold = 0;
	for (int ired = 0; ired < nredshifts; ired++) {
	    double zf = zfm + ((double)ired  * zpfac);
	    ztor[ired] = 1.0/zf - 1.0;
	    if (ired != 0) {
		pred[ired] = - fzcon * (fz(z0,ztor[ired])-fz(z0,zold));  
	    } else {
		pred[ired] = - fzcon * (fz(z0,ztor[ired])-fz(z0,zmax));  
	    }
	    tpred = tpred + pred[ired]; 
	    zold = ztor[ired]; 
	} 

	for (int ired = 0; ired < nredshifts; ired++) {
	    pred[ired] /= tpred;   /* renormalize */
	}
    }
    /* ### SUGGEST INITIAL TESTING WITH ALL ZTOR=0.0, PRED=0.1 ### */


    
    /* spectrum for type-1 sources */
    double * spec1_igrid = calloc(s_engrid->nebin, sizeof(double));

    switch(s_logNS->spectype1){
	double prob_igrid = 0.0;
	double open_tor = 0.0;
	double inc_tor = 0.0;
	int igrid_abs = 0;

    case 0:
	errstatus = spectra(0,0.0,s_logNS->nhmod1,0.0,0.0,0.0,s_engrid->nebin,s_engrid->energ_mid,s_engrid->xspec_energy,xsphab_trans1, debug);
	spectype = spec_string_to_type(s_logNS->specmod1);
	errstatus = spectra(spectype, 0.0, s_logNS->specpar1,flux_unr_1,s_logNS->bandpasslo,s_logNS->bandpasshi,
			    s_engrid->nebin,s_engrid->energ_mid,s_engrid->xspec_energy,spec1, debug);
	/* absorb the diffuse spectrum */
	for (iebin=0; iebin<s_engrid->nebin; iebin++)
	    spec1[iebin] *= xsphab_trans1[iebin];


	break;

    case 1:
	/* "multi" model, weighted sum, using discrete NH and plaw- index distributions and midpoints of each */
	/* face-on, 90-deg opening angle for pure reflection */
	open_tor = 90.0;
	inc_tor = 0.0;

	for (int ired=0; ired<nredshifts; ired++){
	    double redshift_tor = ztor[ired];

	    for (igrid_abs=0; igrid_abs<6; igrid_abs++){
		double nh_multi = 0.0;
		double index_multi = 0.0;
		int igrid_ind = 0;

		nh_multi = abs_grid_mid[igrid_abs];

		/* If compton-thin, calculate transmission (otherwise abs included in model) */
		if (igrid_abs < 4){
		    errstatus = spectra(0,redshift_tor,nh_multi,0.0,0.0,0.0,s_engrid->nebin,s_engrid->energ_mid,
					s_engrid->xspec_energy,xsphab_trans1, debug);
		}
	    
		for (igrid_ind=0; igrid_ind<5; igrid_ind++){
		    index_multi = par_grid_mid[igrid_ind];
		    /* joint probability for this index and abs combo (distributions are independent) */
		    prob_igrid = prob_abs[igrid_abs] * prob_par[igrid_ind];
		    
		    /* if compton-thin, absorbed power law */
		    if (igrid_abs < 4){
			errstatus = spectra(1, redshift_tor, index_multi, flux_unr_1, s_logNS->bandpasslo, s_logNS->bandpasshi,
					    s_engrid->nebin, s_engrid->energ_mid, s_engrid->xspec_energy, spec1_igrid, debug);

			for (iebin=0; iebin<s_engrid->nebin; iebin++)
			    spec1[iebin] += pred[ired] * prob_igrid * xsphab_trans1[iebin] * spec1_igrid[iebin];
				

		    } else if (igrid_abs == 4){
			find_torus_index(&s_tpar, index_multi, pow(10.0,nh_multi-22), open_tor, inc_tor, &torus_index);
			torus_spectra(8, redshift_tor, (double)torus_index, flux_unr_1, s_logNS->bandpasslo, s_logNS->bandpasshi, 
				      s_engrid->nebin, s_engrid->energ_mid, s_engrid->xspec_energy, &s_tpar, &s_tspec, spec1_igrid, debug);
			for (iebin=0; iebin<s_engrid->nebin; iebin++)
			    spec1[iebin] += pred[ired] * prob_igrid * spec1_igrid[iebin];
			
		    } else { /* igrid_abs = 5 */
			find_torus_index(&s_tpar, index_multi, pow(10.0,nh_multi-22), open_tor, inc_tor, &torus_index);
			torus_spectra(7, redshift_tor, (double)torus_index, flux_unr_1, s_logNS->bandpasslo, s_logNS->bandpasshi, 
				      s_engrid->nebin, s_engrid->energ_mid, s_engrid->xspec_energy, &s_tpar, &s_tspec, spec1_igrid, debug);
			for (iebin= 0; iebin < s_engrid->nebin; iebin++)
			    spec1[iebin] += pred[ired] * prob_igrid * spec1_igrid[iebin];
			
		    } /* end igrid_abs conditional */
		} /* end for (igrid_ind=0; igrid_ind<5; igrid_ind++) */
	    } /* end for (igrid_abs=0; igrid_abs<6; igrid_abs++) */
	} /* end for(ired = 0, nredshifts) */
	break;

    case 2:
	/* For "torus" model the parameter is the index of the spectrum library, use the discrete 
	   distributions for NH and openang, a uniform distribution of inclination in 6 bins (to be on the same
	   footing as the others) and fixed power-law index.  One could average over a much finer mesh... */

	for (int ired=0; ired<nredshifts; ired++){

	    double redshift_tor = ztor[ired];

	    for (igrid_abs=0; igrid_abs<6; igrid_abs++){
		int igrid_opn = 0;
		int igrid_inc = 0;
		double nh_tor = 0.0;

		nh_tor = abs_grid_mid[igrid_abs];

		for (igrid_opn=0; igrid_opn<5; igrid_opn++){
		    prob_igrid = prob_abs[igrid_abs] * prob_par[igrid_opn];
		    open_tor = par_grid_mid[igrid_opn];
		    
		    for (igrid_inc=0; igrid_inc<6; igrid_inc++){
			ctheta = 0.2 * (double)(igrid_inc);
			inc_tor = R2D * acos(ctheta);
			if (inc_tor > 90.0) inc_tor = 180.0-inc_tor;
			find_torus_index(&s_tpar, gam_tor, pow(10.0,nh_tor-22), open_tor, inc_tor, &torus_index);
			torus_spectra(6,redshift_tor,(double)torus_index,flux_unr_1,s_logNS->bandpasslo,s_logNS->bandpasshi,
				      s_engrid->nebin,s_engrid->energ_mid,s_engrid->xspec_energy,&s_tpar,&s_tspec,spec1_igrid, debug);

			for (iebin=0; iebin<s_engrid->nebin; iebin++)
			    spec1[iebin] += pred[ired] * prob_igrid * spec1_igrid[iebin] / 6.0;


		    } /* end for (igrid_inc=0; igrid_inc<6; igrid_inc++) */
		} /* end (igrid_opn=0; igrid_opn<5; igrid_opn++) */
	    } /* end (igrid_abs=0; igrid_abs<6; igrid_abs++) */
	} /* end for (ired=0, nredshifts) */
	break;

    default:

	printf("ERROR: s_logNS->spectype1 = %d is not supported.\n",s_logNS->spectype1);
	errstatus = -1;
	return errstatus;
	break;

    } /* end switch */
	
    free(spec1_igrid);


    /* Apply Galactic absorption and combine if there are two spectral types */

    *ps_spec = calloc(s_engrid->nebin, sizeof(double));
    xsphab_trans_gal = calloc(s_engrid->nebin, sizeof(double));

    errstatus = spectra(0, 0.0, s_obs->nh_gal, 0.0, 0.0, 0.0, s_engrid->nebin, s_engrid->energ_mid, s_engrid->xspec_energy, xsphab_trans_gal, debug);
    for (iebin=0; iebin<s_engrid->nebin; iebin++){
	(*ps_spec)[iebin] = spec1[iebin];

	if (s_logNS->samespec == 0)
	    (*ps_spec)[iebin] = xsphab_trans_gal[iebin] * ( (*ps_spec)[iebin] + xsphab_trans2[iebin] * spec2[iebin]);


    }




    if ( (s_logNS->spectype1 == 1) || (s_logNS->spectype1 == 2) ){
	deallocate_torus(&s_tpar, &s_tspec);
	free(ztor);
	free(pred);
    }
   
    free(spec1);
    free(spec2);
    free(cumprob);
    free(scat_2);
    free(scat_1);
    free(ytable);
    free(nh_cat_1);
    free(par_cat_1);
    free(xsphab_trans1);
    free(xsphab_trans2);
    free(ra_cat_1);
    free(ra_cat_2);
    free(dec_cat_1);
    free(dec_cat_2);
    free(xsphab_trans_gal);

    return errstatus;
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
/* CALLED BY: psources                                                      */
/*                                                                          */

int torus_prep(torus_par_struct * s_tpar, torus_spec_struct * s_tspec, int debug){
    
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

    printf("   Populating torus structs...\n");

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
    numvals_arr = calloc(4,sizeof(long));
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
    printf("      Number of energies: %ld\n",s_tspec->num_en);
    
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


    printf("   ...done populating torus structs.\n");


    return fstat;

}


/* FUNCTION NAME: find_torus_index                                          */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Identify the correct index in the sequential list of specra in the     */
/*   library written in torus1006.fits based on a selection of powerlaw     */
/*   index, NH, and opening angle.                                          */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   find_torus_index(&s_tpar, gam_tor, pow(10.0,nh_tor-22), open_tor,      */
/*                    inc_tor, &torus_index);                               */
/*                                                                          */
/* INPUTS: s_tpar, gamma, nh_22, openang, incang                            */
/*                                                                          */
/* OUTPUTS: tor_index                                                       */
/*                                                                          */
/* CALLED BY: psources()                                                    */
/*                                                                          */


int find_torus_index(torus_par_struct * s_tpar,  /* torus parameter structure */
		     double gamma,               /* power-law index */
		     double nh_22,               /* NH in units of 10^22 cm^-2 */
		     double openang,             /* opening angle */
		     double incang,              /* inclination angle */
		     long * tor_index){          /* torus index */
    
    double lognh = 0.0;   /* log of the column density */
    int i_nh = 0;         /* column density index */
    int i_gam = 0;        /* gamma index */
    int i_theta = 0;      /* opening angle index */
    int i_inc = 0;        /* inclination angle index */
    
    lognh = log10(nh_22);

    /* ###### get the nearest NH value ###### */
    if (nh_22 < s_tpar->nhvals[0]){
	/* if nn_22 is off low end of array, set index to zero. */
	i_nh = 0;

    } else if (nh_22 > s_tpar->nhvals[s_tpar->num_nh-1]){
	/* if nh_22 is off high end of array, set index to max. */
	i_nh = s_tpar->num_nh -1;

    } else {
	/* Otherwise, find the highest index i_nh for which nh_22 > nh_vals[i_nh] */
	/* use fsearch() from ahmath */
	/* fsearch(nh_22, s_tpar->nhvals, i_nh); */
	i_nh = find_index(nh_22, s_tpar->num_nh, s_tpar->nhvals) - 1;
	
	/* ...except that we want the nearest index by log. */
	if ( fabs(lognh-log10(s_tpar->nhvals[i_nh+1])) < fabs(lognh-log10(s_tpar->nhvals[i_nh])) ){
	    i_nh++;
	}
    }


    /* ##### get the nearest gamma value ##### */
    if (gamma < s_tpar->gamvals[0]){
	/* if gamma is off low end of array, set to zero. */
	i_gam = 0;

    } else if (gamma > s_tpar->gamvals[s_tpar->num_gam-1]){
	/* if gamma is off high end of array, set to max */
	i_gam = s_tpar->num_gam-1;

    } else {
	/* Otherwise, find highest index i_gam for which gamma > gamvals[i_nh] */
	/* fsearch(gamma, s_tpar->gamvals, i_gam); */
	i_gam = find_index(gamma, s_tpar->num_gam, s_tpar->gamvals) - 1;

	/* Adjust to the nearest linear index. */
	if ( fabs(gamma-s_tpar->gamvals[i_gam+1]) < fabs(gamma-s_tpar->gamvals[i_gam]) ){
	    i_gam++;
	}
    }


    /* ##### get the nearest opening angle ##### */
    if (openang < s_tpar->thvals[0]){
        /* if openang is off low end of array, set to zero. */
        i_theta = 0;

    } else if (openang > s_tpar->thvals[s_tpar->num_theta-1]){
        /* if openang is off high end of array, set to max */
        i_theta = s_tpar->num_theta-1;

    } else {
        /* Otherwise, find highest index i_theta for which openang > thvals[i_nh] */
        /* fsearch(openang, s_tpar->thvals, i_theta); */
	i_theta = find_index(openang, s_tpar->num_theta, s_tpar->thvals) - 1;

        /* Adjust to the nearest linear index. */
        if ( fabs(openang-s_tpar->thvals[i_theta+1]) < fabs(openang-s_tpar->thvals[i_theta]) ){
            i_theta++;
        }
    }


    /* ##### get the nearest inclination angle ##### */
    if (incang < s_tpar->incvals[0]){
        /* if incang is off low end of array, set to zero. */
        i_inc = 0;

    } else if (incang > s_tpar->incvals[s_tpar->num_inc-1]){
        /* if incang is off high end of array, set to max */
        i_inc = s_tpar->num_inc-1;

    } else {
        /* Otherwise, find highest index i_inc for which incang > incvals[i_nh] */
        /* fsearch(incang, s_tpar->incvals, i_inc); */
	i_inc = find_index(incang, s_tpar->num_inc, s_tpar->incvals) - 1;

	/* Adjust to the nearest linear index. */
        if ( fabs(incang-s_tpar->incvals[i_inc+1]) < fabs(incang-s_tpar->incvals[i_inc]) ){
            i_inc++;
        }
    }

    
    /* Compute the torus index */
    *tor_index = i_inc + i_nh * s_tpar->num_gam * s_tpar->num_theta * s_tpar->num_inc +
	i_gam * s_tpar->num_theta * s_tpar->num_inc +
	i_theta * s_tpar->num_inc;

    return 0;

}


/* FUNCTION NAME: write_table_model                                         */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   write the table model fits file for the diffuse spectrum               */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   write_table_model(s_engrid, s_files->difspec_fileroot,                 */
/*                     s_files->difspec_fileroot, tot_spec);                */
/*                                                                          */
/* INPUTS: s_engrid, modfilename, modelname, tot_spec                       */
/*                                                                          */
/* OUTPUTS: FITS file containing final background data                      */
/*                                                                          */
/* CALLED BY: doWork()                                                      */
/*                                                                          */

int write_table_model(Engrid_struct * s_engrid,
		      char * modfilename,
		      char * modelname,
		      double * tot_spec){
    int ii = 0;
    long nebin = 0;
    double * energ_lo = 0;
    double * energ_hi = 0;

    char * base = 0;
    char outfile[STR_MAX_LEN];
    fitsfile * ounit = 0;

    int fstat = 0;
    int nrows = 0;
    int firstelem = 0;
    int firstrow = 0;
    int colnum = 0;

    char * par_ttype[] = {"NAME", "METHOD", "INITIAL", "DELTA", "MINIMUM", "BOTTOM", "MAXIMUM", "TOP", "NUMBVALS", "VALUE", "UNITS"};
    char * par_tform[] = {"12A","1J","1E","1E","1E","1E","1E","1E","1J","2E", "8A"};
    char * par_tunit[]  = {"none","none","none","none","none","none","none","none","none","none","none"};

    char * energ_ttype[] = {"ENERG_LO", "ENERG_HI"};
    char * energ_tform[] = {"1E","1E"};
    char * energ_tunit[]  = {"keV","keV"};

    char * spec_ttype[] = {"PARAMVAL", "INTPSPEC"};
    char * spec_tform[] = {"1E","1E"};
    char * spec_tunit[]  = {"none","photons/cm^2/s"};

    /* Single-element arrays to be written in PARAMETERS hdu */
    char * name_arr[1] = {"scale"};
    int method_arr[1] = {1};
    float initial_arr[1] = {1.0};
    float delta_arr[1] = {0.01};
    float min_arr[1] = {0.0};
    float bottom_arr[1] = {0.0};
    float max_arr[1] = {1.e24};
    float top_arr[1] = {1.e20};
    int numbvals_arr[1] = {1};
    float value_arr[1] = {1.0};
    char * units_arr[1] = {""};
    float paramvals_arr[1] = {1.0};

    /* "unpack" the Engrid_struct: */
    nebin = s_engrid->nebin;

    energ_lo = calloc(nebin,sizeof(double));
    energ_hi = calloc(nebin,sizeof(double));

    for (ii=0; ii<nebin; ii++){
	energ_lo[ii] = s_engrid->energ_lo[ii];
	energ_hi[ii] = s_engrid->energ_hi[ii];
    }

    /* Initialize the output fits file */   
    base = calloc(STR_MAX_LEN, sizeof(char));
    strcpy(base,modfilename);
    remove_substring(base,".fits");
    sprintf(outfile,"%s.fits",base);
    fits_create_file(&ounit, outfile, &fstat);
    free(base);

    /* populate the output event file FITS PRIMARY header */    
    fits_write_imghdr(ounit,16,0,0,&fstat);                        
    /* ...origin/creator/date... */
    fits_write_key_str(ounit,"CONTENT","MODEL","spectrum file contains time intervals and event",&fstat);
    fits_write_key_str(ounit,"FILENAME","IGPLV10","File that FITS was produce from",&fstat);
    fits_write_key_str(ounit,"ORIGIN","heasim/skyback","origin of FITS file",&fstat);
    fits_write_key_str(ounit,"MODLNAME","skyback","model name ",&fstat); 
    fits_write_key_str(ounit,"MODLUNIT","photons/cm^2/s","model units",&fstat); 
    /* REDSHIFT and ADDMODEL need to be logicals */
    fits_write_key_log(ounit,"REDSHIFT",1,"redshift is a parameter",&fstat);
    fits_write_key_log(ounit,"ADDMODEL",1,"If true then this is an additive table model",&fstat);
    fits_write_key_str(ounit,"HDUCLASS","OGIP","format conforms to OGIP standard",&fstat); 
    fits_write_key_str(ounit,"HDUCLAS1","XSPEC TABLE MODEL","model spectra for XSPEC",&fstat);
    fits_write_key_str(ounit,"HDUVERS1","1.0.0","version of format",&fstat);
    fits_write_key_str(ounit,"HDUDOC","OGIP/92-009","document defining format",&fstat); 

    /*  Create binary table extension for PARAMETERS (which will have only one line)*/
    fits_insert_btbl(ounit,0,11,par_ttype,par_tform,par_tunit,"PARAMETERS",0,&fstat);
    fits_write_key_str(ounit,"HDUCLASS","OGIP", "format conforms to OGIP standard",&fstat);
    fits_write_key_str(ounit,"HDUCLAS1","XSPEC TABLE MODEL","model spectra for XSPEC",&fstat);
    fits_write_key_str(ounit,"HDUCLAS2","PARAMETERS","extension containing parameter info",&fstat);
    fits_write_key_str(ounit,"HDUVERS1","1.0.0","version of format",&fstat); 
    fits_write_key_lng(ounit,"NINTPARM",1,"Number of interpolation parameters",&fstat); 
    fits_write_key_lng(ounit,"NADDPARM",0,"Number of additional parameters",&fstat); 

    /* Insert 1 row into PARAMETERS extension */
    fits_movnam_hdu(ounit, BINARY_TBL, "PARAMETERS", 0, &fstat);
    firstrow = 1;
    firstelem = 1;
    nrows = 1;

    colnum = 1;  /* column 1 contains NAME */
    fits_write_col(ounit, TSTRING, colnum, firstrow, firstelem, nrows, name_arr, &fstat);

    colnum = 2;  /* column 2 contains METHOD */
    fits_write_col(ounit, TINT, colnum, firstrow, firstelem, nrows, method_arr, &fstat);

    colnum = 3;  /* column 3 contains INITIAL */
    fits_write_col(ounit, TFLOAT, colnum, firstrow, firstelem, nrows, initial_arr, &fstat);

    colnum = 4;  /* column 4 contains DELTA */
    fits_write_col(ounit, TFLOAT, colnum, firstrow, firstelem, nrows, delta_arr, &fstat);

    colnum = 5;  /* column 5 contains MINIMUM */
    fits_write_col(ounit, TFLOAT, colnum, firstrow, firstelem, nrows, min_arr, &fstat);

    colnum = 6;  /* column 6 contains BOTTOM */
    fits_write_col(ounit, TFLOAT, colnum, firstrow, firstelem, nrows, bottom_arr, &fstat);

    colnum = 7;  /* column 7 contains MAXIMUM */
    fits_write_col(ounit, TFLOAT, colnum, firstrow, firstelem, nrows, max_arr, &fstat);

    colnum = 8;  /* column 8 contains TOP */
    fits_write_col(ounit, TFLOAT, colnum, firstrow, firstelem, nrows, top_arr, &fstat);

    colnum = 9;  /* column 9 contains NUMBVALS */
    fits_write_col(ounit, TINT, colnum, firstrow, firstelem, nrows, numbvals_arr, &fstat);

    colnum = 10;  /* column 10 contains VALUE */
    fits_write_col(ounit, TFLOAT, colnum, firstrow, firstelem, 1, value_arr, &fstat);

    colnum = 11;  /* column 11 contains UNITS */
    fits_write_col(ounit, TSTRING, colnum, firstrow, firstelem, nrows, units_arr, &fstat);
    
    /*  Create binary table extension for ENERGIES */
    fits_insert_btbl(ounit,0,2,energ_ttype,energ_tform,energ_tunit,"ENERGIES",0,&fstat);
    fits_write_key_str(ounit,"HDUCLASS","OGIP","format conforms to OGIP standard",&fstat);
    fits_write_key_str(ounit,"HDUCLAS1","XSPEC TABLE MODEL","model spectra for XSPEC",&fstat);
    fits_write_key_str(ounit,"HDUCLAS2","ENERGIES","extension containing energies info",&fstat);
    fits_write_key_str(ounit,"HDUVERS1","1.0.0","version of format",&fstat); 

    /* Insert nebin rows into ENERGIES extension */
    fits_movnam_hdu(ounit, BINARY_TBL, "ENERGIES", 0, &fstat);
    firstrow = 1;
    firstelem = 1;
    nrows = nebin;

    colnum = 1;  /* column 1 contains ENERG_LO */
    fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, nrows, energ_lo, &fstat);

    colnum = 2;  /* column 2 contains ENERG_HI */
    fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, nrows, energ_hi, &fstat);

    /*  Create binary table extension for SPECTRA */
    spec_tform[1] = calloc(STR_MAX_LEN,sizeof(char));
    sprintf(spec_tform[1],"%ldE",nebin);
    fits_insert_btbl(ounit,0,2,spec_ttype,spec_tform,spec_tunit,"SPECTRA",0,&fstat);
    fits_write_key_str(ounit,"HDUCLASS","OGIP","format conforms to OGIP standard",&fstat);
    fits_write_key_str(ounit,"HDUCLAS1","XSPEC TABLE MODEL","model spectra for XSPEC",&fstat);
    fits_write_key_str(ounit,"HDUCLAS2","SPECTRA","model spectra for xspec",&fstat);
    fits_write_key_str(ounit,"HDUVERS1","1.0.0","version of format",&fstat); 
    free(spec_tform[1]);

    /* Insert 1 row into SPECTRA extension */
    fits_movnam_hdu(ounit, BINARY_TBL, "SPECTRA", 0, &fstat);
    firstrow = 1;
    firstelem = 1;
    nrows = 1;
    colnum = 1;  /* column 1 contains PARAMVALS */
    fits_write_col(ounit, TFLOAT, colnum, firstrow, firstelem, 1, paramvals_arr, &fstat);
    colnum = 2;  /* column 2 contains INTSPEC */
    fits_write_col(ounit, TDOUBLE, colnum, firstrow, firstelem, nebin, tot_spec, &fstat);

    fits_close_file(ounit,&fstat);

    free(energ_hi);
    free(energ_lo);

    return fstat;
}



/* FUNCTION NAME: spectra                                                                                      */
/*                                                                                                             */
/* CALLING SEQUENCE: spectra()                                                                                 */
/*                                                                                                             */
/* PURPOSE: calculate the input spectrum in photons/cm2/sec/channel from one of the supported standard models, */
/*          now supplemented with Brightman/Nandra torus and Gilli models w/ reflection. The spectral type,    */
/*          spectral parameters, and input energy scale are used to produce the output spectrum in             */
/*          photons/cm^2/s/channel vs keV in each bin on the input energy grid. The input flux and bandpass    */
/*          determine the model normalization.                                                                 */
/*                                                                                                             */
/* INPUT: spectype, specpar, zpar, fluxpar, norm_lower, norm_upper, nebin, energ_mid, xspec_energy             */
/*                                                                                                             */
/* OUTPUT: spec                                                                                                */
/*                                                                                                             */
/* CALLED BY: psources()                                                                                       */
/*                                                                                                             */

int spectra(int spectype,           /* 0:phabs,1:power, 2:rs, 3:bb, 4:brem, 5:mono, 6:torus, 7: gilli_thick, 8:gilli_mild */
	    double zpar,            /* redshift parameter */
	    double specpar,         /* see comment below */
	    double fluxpar,         /* over the input bandpass in erg/cm2/sec */
	    double norm_lower,      /* lower limit of bandpass in keV for flux */
	    double norm_upper,      /* upper limit of bandpass in keV for flux */
	    long nebin,             /* number input energy bins */
            double * energ_mid,
            double * xspec_energy,
	    double * spec,         /* spectrum in photons/cm2/sec/channel on input energy grid */
	    int debug){

    /* specpar = NH for phabs */
    /*           index for powerlaw */
    /*           kT in keV for rs, bb, or brem */
    /*           line energy in keV for mono */
    /*           torus spectral model index for torus */
    /*           Gilli very-Compton thick for gilli_thick */
    /*           Gilli mildly-Compton thick for gilli_mild */
    
    int errstatus = 0;
    int ii = 0;
    double onekeVphoton2ergs = 1.60218e-09;
    double xspwlw_param[1]; /* Xspec powerlaw params - ONLY ONE THAT SHOULD BE DOUBLE!*/
    float xsrays_param[3];  /* Xspec xsrays params */
    float xsblbd_param[1];  /* Xspec xsblbd params */
    float xsbrms_param[1];  /* Xspec xsbrms params */
    float xsgaul_param[2];  /* Xspec xsgaul params */
    float xsphab_param[2];  /* Xspec xsphab params */
    int ifln = 1; /* the spectrum number being calculated, always 1; required for Xspec model code */

    double * specerr = 0;                 /* uncertainty on spec...needed by Xspec but we never use it */
    float * float_xspec_energy = 0;       /* float cast of xspec_energy */
    float * float_spec = 0;               /* float cast of spec */
    float * float_specerr = 0;            /* float cast of specerr */
    float * float_xsphab_trans = 0;       /* transmitted spectrum - output from Xsphab.  We cast as double and copy to spec. */
    float * float_xsphab_trans_err = 0;   /* uncertainty on float_xsphab_trans */

    double t=0.0;

    /* Allocate these here, deallocate at end of spectra */
    specerr = calloc(nebin, sizeof(double));  /* this is never used for anything... */
    float_xspec_energy = calloc( (nebin+1), sizeof(float));
    float_spec = calloc(nebin, sizeof(float)); /* will be converted to double later. */
    float_specerr = calloc(nebin, sizeof(float));
    float_xsphab_trans = calloc(nebin, sizeof(float));
    float_xsphab_trans_err = calloc(nebin, sizeof(float));
  
    for (int iebin=0; iebin<nebin+1; iebin++)
	float_xspec_energy[iebin] = (float)xspec_energy[iebin];

    /* Get the spectrum */
    /* begin spectype conditional */

    switch(spectype){

    case 0:
	if (debug > 0) printf("   Spectra: xszphb (Redshifted absorption)\n");
	xsphab_param[0] = (float) (specpar/ 1.0e+22);
	xsphab_param[1] = (float) zpar;
	Xszphb(float_xspec_energy, nebin, xsphab_param, ifln, float_xsphab_trans, float_xsphab_trans_err);
	break;

    case 1:
	if (debug > 0) printf("   Spectra: Powerlaw\n");
	xspwlw_param[0] = specpar;
	C_powerLaw(xspec_energy, (int)nebin, xspwlw_param, ifln, spec, specerr, "pl");
	//Cpowtest(xspec_energy, (int)nebin, xspwlw_param, ifln, wtf, specerr, "pl");
	/* specerr never used; C_powerLaw replaced the xspec pow model (xspwls) because of
	   precision issues. Need to confirm this (i.e. why not an issue for other models?).*/
	break;

    case 2:
	if (debug > 0) printf("   Spectra: xsrays (Raymond-Smith)\n");
        xsrays_param[0] = (float)specpar;
	xsrays_param[1] = 1.0;
	xsrays_param[2] = 0.0;
	/* abundance here set to 1, redshift to 0 */
	Xsrays(float_xspec_energy, nebin, xsrays_param, ifln, float_spec, float_specerr);
	/* float conversion here and below; spec will be re-converted to double (a recent revision) */
	break;

    case 3:
	if (debug > 0) printf("   Spectra: xsblbd (blackbody)\n");
	xsblbd_param[0]= (float)specpar;
        Xsblbd(float_xspec_energy, nebin, xsblbd_param, ifln, float_spec, float_specerr);
	break;

    case 4:
	if (debug > 0) printf("   Spectra: xsbrms (Bremsstrahlung)\n");
        xsbrms_param[0] = (float)specpar;
	Xsbrms(float_xspec_energy, nebin, xsbrms_param, ifln, float_spec, float_specerr);
	break;

    case 5:
	if (debug > 0) printf("   Spectra: xsgaul (mono-energetic)\n");
	xsgaul_param[0] = (float)specpar;
	xsgaul_param[1] = -0.1;
	/* zero intrinsic width */
	Xsgaul(float_xspec_energy, nebin, xsgaul_param, ifln, float_spec, float_specerr);
	if (debug == 1){
	    for (int ii=0; ii<nebin; ii++)
		if (float_spec[ii] != 0)
		    printf("   mono spec[%d] = %e\n",ii,float_spec[ii]);
	}
	break;

    default:
	printf("   ERROR: spectype %d is not a supported type.\n",spectype);
    }/* end spectype conditional*/


    if (spectype == 0){
	for (ii=0; ii<nebin; ii++)
	    spec[ii] = (double)float_xsphab_trans[ii];

    } else {

	if (spectype != 1){
	    /* re-cast float spectra as double spectra */
	    for (ii=0; ii<nebin; ii++)
		spec[ii] = (double)float_spec[ii];
	}

	/* Calculate the flux in erg/cm2/sec in spec between norm_lower_limit , norm_upper_limit; 
	   the spectrum must be renormalized. These are unabsorbed fluxes.  Calculate the flux "t" in keV/cm2/sec: */
	errstatus = spec_scale(energ_mid, spec, nebin, norm_lower, norm_upper, &t);
	if (debug > 0){
	    printf("   fluxpar = %e\n",fluxpar);
	    printf("   t = %e\n",t);
	}

	/* Convert to cgs: */
	t *= onekeVphoton2ergs;        
	for (ii=0; ii<nebin; ii++) {
	    if (t > 0){
		spec[ii] *= fluxpar / t;
	    } else {
		spec[ii] = 0.0;
	    }
	}

    } /* end if spectype == 0 */


    free(specerr);
    free(float_xspec_energy);
    free(float_spec);
    free(float_specerr);
    free(float_xsphab_trans);
    free(float_xsphab_trans_err);

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

int spec_scale(double *en,          /* spectrum energy grid midpoints in keV */
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



/* FUNCTION NAME: torus_spectra                                             */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Calculate the input spectraum in photons/cm^2/sec/channel from         */
/*   Brightman/Nandra torus and Gilli models with reflection.               */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   torus_spectra(6,(double)torus_index,flux_unr_1,s_logNS->bandpasslo,   */
/*                 s_logNS->bandpasshi,s_engrid->nebin, energ_mid,         */
/*                 xspec_energy,&s_tpar,&s_tspec,spec1_igrid);              */
/*                                                                          */
/* INPUTS: spectype, zpar, specpar, fluxpar, norm_upper, norm_lower, nebin, */
/*          energ_mid, xspec_energy, s_tpar, s_tspec                        */
/*                                                                          */
/* OUTPUTS: spec                                                            */
/*                                                                          */
/* CALLED BY: psources()                                                    */
/*                                                                          */

int torus_spectra(int spectype,           /* 6:torus, 7: gilli_thick, 8:gilli_mild */
		  double zpar,            /* redshift parameter */
		  double specpar,         /* see comment below */
		  double fluxpar,         /* over the input bandpass in erg/cm2/sec */
		  double norm_lower,      /* lower limit of bandpass in keV for flux */
		  double norm_upper,      /* upper limit of bandpass in keV for flux */
		  long nebin,             /* number input energy bins */
		  double * energ_mid,      /* midpoint of energy grid bins */
		  double * xspec_energy,  /* vector of energy grid boundaries for Xspec model input */
		  torus_par_struct * s_tpar, /* torus structure containing parameters */
		  torus_spec_struct *s_tspec, /* torus structure containing spectra */
		  double * spec,         /* spectrum in photons/cm2/sec/channel on input energy grid */
		  int debug){

    double onekeVphoton2ergs = 1.60218e-9;
    int index = 0;
    int itheta = 0;
    int i1b = 0;
    int iangle = 0;
    int i2b = 0;
    int igamma = 0;
    int i3b = 0;
    int iabs = 0;
    double nh=0.0, gamma=0.0, opnang=0.0, theta=0.0;
    int errstatus = 0;
    double * xsphab_tor = 0;
    double * inspec = 0;
    double * int_spec = 0;
    int ii = 0;
    double * binlo_out = 0;
    double * binhi_out = 0;
    double * tor_rshift_xspec_energy = 0;
    double zfac = 0;

    /* TRF says to unpack torus structure here...actually necessary? */

    /* printf("Calculating torus spectra...\n"); */

    index = (int)specpar;

    if ( (spectype == 7) || (spectype == 8) ){
	itheta = index % s_tpar->num_theta;
	i1b = index / s_tpar->num_theta;

	iangle = i1b % s_tpar->num_inc;
	i2b = i1b / s_tpar->num_inc;

	igamma = i2b % s_tpar->num_gam;
	i3b = i2b / s_tpar->num_gam;

	iabs = i3b % s_tpar->num_nh;

	nh = s_tpar->nhvals[iabs];
	gamma = s_tpar->gamvals[igamma];
	opnang = s_tpar->thvals[iangle];
	theta = s_tpar->incvals[itheta];
				
    } else {
	if (spectype != 6){
	    printf("ERROR: Type %d is not a supported torus spectype, returning.\n",spectype);
	    for (int ii=0; ii<nebin; ii++)
		spec[ii] = 0.0;
	    return -1;  /* Premature return here is ok, nothing has been allocated yet, no memory leaks here. */
	}
    }

    if (spectype == 8){
	xsphab_tor = calloc(s_tspec->num_en, sizeof(double));
	errstatus = spectra(0, zpar, nh, 0.0, 0.0, 0.0, s_tspec->num_en, s_tspec->tor_energ_mid, s_tspec->tor_xspec_energy, xsphab_tor, debug);
    }

    inspec = calloc(s_tspec->num_en, sizeof(double));
    int_spec = calloc(s_tspec->num_en+1, sizeof(double));
    
    for (int ien=0; ien<s_tspec->num_en; ien++){
	/* spectrum over torus energy grid for these parameters: */
	double pl_spec = 0.0;
	double refl_spec = 0.0;
	double inspec_ien = s_tspec->tor_spec[index][ien];
	double ewid = s_tspec->tor_energ_wid[ien];

	if ( (spectype == 7) || (spectype == 8) ){
	    /* torus reprocessed - reflection only for these cases (per keV) */
	    pl_spec = pow(s_tspec->tor_energ_mid[ien], -gamma);
	    refl_spec = inspec_ien / ewid - pl_spec;
	    
	    if (spectype == 7){
		/* reprocessed (reflection) only */
		inspec_ien = ewid * refl_spec;
	    } else {
		/* plaw plus some reprocessed (reflection) */
		inspec_ien = ewid * (0.37 * refl_spec + xsphab_tor[ien] * pl_spec);
	    }
	}
	inspec[ien] = inspec_ien;
    }

    /* to redshift the torus spectrum diminish the flux and energy by 1+z */
    zfac = 1.0 + zpar;
    tor_rshift_xspec_energy = calloc(s_tspec->num_en+1, sizeof(double));
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


    if (spectype != 0) {
	double t = 0.0;
	
	/* calculate the flux in erg/cm2/sec in spec between norm_lower_limit , norm_upper_limit; */
	/* the spectrum must be renormalized. */
	/* These are unabsorbed fluxes:*/
	errstatus = spec_scale(energ_mid, spec, nebin, norm_lower, norm_upper, &t);
	/* this is the subroutine that calculates the flux "t" in keV/cm2/sec; convert to cgs: */
	t = t * onekeVphoton2ergs;        
	for (ii = 0; ii < nebin; ii++) {
	    if (t > 0){
		spec[ii] *= fluxpar / t;
	    } else {
		spec[ii] = 0.0;
	    }
	}
    }

    if (spectype == 8)
	free(xsphab_tor);

    free(inspec);
    free(int_spec);
    free(tor_rshift_xspec_energy);

    /* printf("...done.\n"); */

    return 0;
}



/* FUNCTION NAME: swcx                                                      */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Compute background contribution from Solar Wind Charge Exchange (SWCX) */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   swcx(s_obs, s_engrid, s_flux->swcxOVII, &swcx_spec);             */
/*                                                                          */
/* INPUTS: s_obs, s_engrid, swcxOVII                                  */
/*                                                                          */
/* OUTPUTS: swcx_spec                                                       */
/*                                                                          */
/* CALLED BY: doWork()                                                      */
/*                                                                          */

int swcx(Obs_params_struct * s_obs,
	 Engrid_struct * s_engrid,
	 Flux_struct * s_flux,
	 double ** swcx_spec,
	 int debug){

    /* Currently the only input is the total flux Finput (in LU;  1 LU == 1 photon s-1 cm-2 str-1) in the 
       brightest (0.57 OVIII) feature. Additional lines are included with fluxes based on fixed ratios 
       as follows: 
       
       F(OVIII L# 0.653) = 0.25 * Finput, 
       F(OVIII K#) = 0.083 *F(OVIII L# 0.653); 
       F(OVII forbidden 0.561)=2/3*Finput, 
       F(OVII  resonance 0.574)=1/6*Finput, 
       F(OVII intercombination 0.568)=1/6*Finput. 

       (Ratios from Yoshitake thesis, Koutroumpa). 
       One can add other lines , e.g. NVII (0.500), CVI (0.367), SiL, MgL... or a real model (Galleazzi,Snowden/Kuntz?).
     */

    double pi = 3.141592741012573;
    double arcmin_to_str = 8.461594994e-8;
    double onekeVphoton2ergs = 1.60218e-09;
    int num_lines = 5;
    double area_dif = 0.0;
    double line_en_vec[] = {0.5609,0.5740,0.5685,0.6536,0.6657};
    double line_rat_vec[] = {0.666,0.167,0.167,0.25,0.021};
    double elo=0.0, ehi=0.0;
    int errstatus = 0;
    double fluxcon = 0.0;
    double * lflux = 0;
    double * xsphab_trans = 0;
    double * spec_line = 0;
    double * spec_cont = 0;
    double cont_flux = 0.0;

    area_dif = pi * s_obs->radius * s_obs->radius;

    /* line fluxes in erg/cm^2/sec: */
    fluxcon = s_flux->swcxOVII * arcmin_to_str * area_dif * onekeVphoton2ergs;
    lflux = calloc(num_lines,sizeof(double));
    for (int iline= 0; iline < num_lines ; iline++) {
	lflux[iline] = fluxcon * line_en_vec[iline] * line_rat_vec[iline];
    }

    elo = s_engrid->energ_lo[0];
    ehi = s_engrid->energ_hi[s_engrid->nebin-1];

    *swcx_spec = calloc(s_engrid->nebin, sizeof(double)); /* allocate here, remember to free in doWork */

    spec_line = calloc(s_engrid->nebin, sizeof(double));
    spec_cont = calloc(s_engrid->nebin, sizeof(double));
    
    /* Mono-energetic spectrum for each line: */
    for (int iline= 0; iline < num_lines ; iline++) {
	double line_en = line_en_vec[iline];
	double line_flx = lflux[iline];

	/* Does line_en fall between elo and ehi? */
	if ( (line_en < elo) || (line_en > ehi) ){
	    printf("   WARNING: line energy outside of specified bandpass, excluding.\n");
	    printf("   elo = %f   line_en = %f   ehi = %f\n",elo,line_en,ehi);
	} else {
	    /* spectype 5 = mono */
	    errstatus = spectra(5, 0.0, line_en, line_flx, elo, ehi, s_engrid->nebin, s_engrid->energ_mid, s_engrid->xspec_energy, spec_line, debug);
	    for (int iebin= 0; iebin < s_engrid->nebin ; iebin++) {
		(*swcx_spec)[iebin] += spec_line[iebin];
	    }
	}
    }//end for loop iline

    /* spectype 2 = rs */
    cont_flux = s_flux->swcxcont * area_dif;
    errstatus = spectra(2, 0.0, 0.102, cont_flux, 0.2, 2.0, s_engrid->nebin, s_engrid->energ_mid, s_engrid->xspec_energy, spec_cont, debug);
    for (int iebin= 0; iebin < s_engrid->nebin ; iebin++) {
	(*swcx_spec)[iebin] += spec_cont[iebin];
    }


    free(spec_line);
    free(spec_cont);
    free(xsphab_trans);
    free(lflux);

    return errstatus;

}



/* FUNCTION NAME: galactic                                                  */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Compute background contribution from galactic sources                  */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   galactic(s_obs, s_engrid, &gal_spec);                                  */
/*                                                                          */
/* INPUTS: s_obs, s_engrid                                                  */
/*                                                                          */
/* OUTPUTS: gal_spec                                                        */
/*                                                                          */
/* CALLED BY: doWork()                                                      */
/*                                                                          */

int galactic(Obs_params_struct * s_obs,
	     Engrid_struct * s_engrid,
	     Flux_struct * s_flux,
	     double ** gal_spec,
	     int debug){

    double pi = 3.141592741012573;
    int errstatus = 0;
    double nebin = s_engrid->nebin;
    
    /* image_to_array parameters */
    long nx_c=0, ny_c=0, nx_m=0, ny_m=0, nx_ij=0, ny_ij=0;
    double ** c_arr=0;
    double ** m_arr=0;
    double ** ij_arr=0;
    int ira=0, idec=0;
    double cr=0.0, mr=0.0, ijr=0.0;

    double mr_gal=0.0;
    double ijr_gal=0.0;
    double cr_gal=0.0;

    double taum_max=0.0;
    double taum_gal=0.0;
    double taum_tot=0.0;
    double tauc_gal=0.0;
    double tauc_tot=0.0;
    double tauij_gal=0.0;
    double nh_max=0.0;
    double nh_galtot=0.0; 
    double nh_galpart=0.0;

    double mr_gal_da=0.0;
    double ijr_gal_da=0.0;

    double hrat = 0.0;

    double thalo_keV=0.0;
    double tlhb_keV=0.1085;


    double fluxm=0.0;
    double fluxc=0.0;

    double * xsphab_trans = 0;
    double * spec_halo = 0;
    double * spec_lhb = 0;
    double area_dif = 0.0;

    char * headas = 0;
    char rac[STR_MAX_LEN];
    char ram[STR_MAX_LEN];
    char raij[STR_MAX_LEN];

    headas = getenv("HEADAS");
    sprintf(rac,"%s%s",headas,"/refdata/rac.fits");
    sprintf(ram,"%s%s",headas,"/refdata/ram.fits");
    sprintf(raij,"%s%s",headas,"/refdata/raij.fits");

    errstatus = image_to_array(rac, &nx_c, &ny_c, &c_arr, debug);
    errstatus = image_to_array(ram, &nx_m, &ny_m, &m_arr, debug);
    errstatus = image_to_array(raij, &nx_ij, &ny_ij, &ij_arr, debug);

    /* Identify map locations.  Should really use header info, but some of these are wrong. */
    /* (this is the same as "cosmicbackground" from quicksim) */
    ira = floor(s_obs->ra) + 1;
    if (ira < 1) ira = 1;
    if (ira > 360) ira = 360;
    idec = 91 + floor(s_obs->dec);
    if(idec<1) idec = 1;
    if (idec > 180) idec = 180;

    cr = c_arr[idec-1][ira-1];
    mr = m_arr[idec-1][ira-1];
    ijr = ij_arr[idec-1][ira-1];
    nh_galtot = s_obs->nh_gal;

    /* average for low rates (necessary since there are gaps in the maps); see below for function */
    /* count_avg */
    if (cr < 100) 
	cr = count_avg(c_arr, 360, 180, ira-1, idec-1, 1);
    if (mr < 30) 
	mr = count_avg(m_arr, 360, 180, ira-1, idec-1, 1);
    if (ijr < 30) 
	ijr = count_avg(ij_arr, 360, 180, ira-1, idec-1, 1);

    /* excess in IJ and M bands over the extra-galactic part (correcting for optical depth): */
    ijr_gal = ijr - 107.5 * exp(-nh_galtot * 1.474e-22);
    if (ijr_gal < 5.0) 
	ijr_gal = 5.0;
    mr_gal = mr - 60.4 * exp(-nh_galtot * 4.34e-22);

    /* for the galactic components emission and absorption are actually intermixed in the plane; so the 
       effective absorption there is less than the total Galactic. the code handles this by imposing a maximum 
       M-band optical depth of 0.5 as follows. nh_gal will be applied to galactic components, nh_tot to extra-galactic */

    taum_max = 0.5;
    nh_max = taum_max/4.46e-22;
    if (nh_galtot > nh_max) {
	taum_gal = taum_max;
	nh_galpart = 0.5/4.46e-22;
    }  else {
	nh_galpart = nh_galtot;
	taum_gal = 4.46e-22*nh_galpart;
    }

    /* de-absorbed m-band excess: */
    mr_gal_da = mr_gal*exp(taum_gal);
    
    /* c-band optical depth assuming logThalo=6.6*/
    tauc_gal = dblmin(10.0,nh_galpart*0.775e-20);
    tauc_tot= dblmin(10.0,nh_galtot*0.743e-20);

    /* c-band excess assuming logThalo=6.6: */
    cr_gal = cr - 238.8*exp(-tauc_tot) - 1.230*mr_gal_da*exp(-tauc_gal);

    /* corrected absorbed and de-absorbed m-band excess, assuming logTLHB=6.1: */
    mr_gal = mr_gal*exp(-taum_tot) - cr_gal/59.5;
    mr_gal_da = mr_gal*exp(taum_tot);
    if (mr_gal_da < 5) 
	mr_gal_da = 5;

    /* ij-band optical depth and de-absorbed rate: */
    tauij_gal = nh_galpart*3.0e-22;
    ijr_gal_da = ijr_gal*exp(tauij_gal);

    /* from the hardness ratio of intrinsic (de-absorbed) excess ij- and m-band rates, get a better 
       estimate of Thalo, and adjust de-absorbed rates for self-consistency. 6.3<=logThalo<= 6.8 is 
       assumed/imposed. */

    hrat = ijr_gal_da/mr_gal_da;

    if (hrat < 0.167) { /* Thalo  = 10^6.3 */
	mr_gal_da = mr_gal*exp(nh_galpart*6.7961521e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*3.6896383e-22);

    } else if (hrat < 0.214) {	/* Thalo  = 10^6.35 */
	mr_gal_da = mr_gal*exp(nh_galpart*6.1659590e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*3.5355703e-22);

    } else if (hrat < 0.263) {	/* Thalo  = 10^6.4 */
	mr_gal_da = mr_gal*exp(nh_galpart*5.6704685e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*3.4076196e-22);

    } else if (hrat < 0.314) { /*Thalo  = 10^6.45 */
	mr_gal_da = mr_gal*exp(nh_galpart*5.2751283e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*3.2896391e-22);

    } else if (hrat < 0.367) { /* Thalo  = 10^6.5 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.9492623e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*3.1864584e-22);

    } else if (hrat < 0.419) { /* Thalo  = 10^6.55 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.6791972e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*3.0998715e-22);

    } else if (hrat < 0.469) { /* Thalo  = 10^6.6 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.4587872e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*3.0284875e-22);

    } else if (hrat < 0.516) { /* Thalo  = 10^6.65 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.2824252e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*2.9688256e-22);

    } else if (hrat < 0.565) { /* Thalo  = 10^6.7 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.1421945e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*2.9153916e-22);

    } else if (hrat < 0.625) { /* Thalo  = 10^6.75 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.0256746e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*2.8583260e-22);

    } else { /* Thalo  = 10^6.8 */
	mr_gal_da = mr_gal*exp(nh_galpart*3.9222212e-22);
	ijr_gal_da = ijr_gal*exp(nh_galpart*2.7890494e-22);
    } 

    /* iterate one more time with adjusted m- and ij-band rates, adjusting the c-band rate, and 
       deriving the final m- and c-band fluxes (in erg/sec/cm^2/arcmin^2) and Thalo (in keV) */

    hrat = ijr_gal_da/mr_gal_da;

    if (hrat < 0.167) {
	/* Thalo  = 10^6.3 */
	mr_gal_da = mr_gal*exp(nh_galpart*6.7961521e-22);
	fluxm = 5.32e-17*mr_gal_da;
	thalo_keV = 0.1719;
	taum_gal = nh_galpart*7.4305473e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 8.228*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.214) {	/* Thalo  = 10^6.35 */
	mr_gal_da = mr_gal*exp(nh_galpart*6.1659590e-22);
	fluxm = 4.11e-17*mr_gal_da;
	thalo_keV = 0.1929;
	taum_gal = nh_galpart*7.5221802e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 5.331*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.263) {  /* Thalo  = 10^6.4 */
	mr_gal_da = mr_gal*exp(nh_galpart*5.6704685e-22);
	fluxm = 3.28e-17*mr_gal_da;
	thalo_keV = 0.2165;
	taum_gal = nh_galpart*7.4938726e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 3.622*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.314) {  /* Thalo  = 10^6.45 */
	mr_gal_da = mr_gal*exp(nh_galpart*5.2751283e-22);
	fluxm = 2.74e-17*mr_gal_da;
	thalo_keV = 0.2429;
	taum_gal = nh_galpart*7.4300901e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 2.602*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.367) {  /* Thalo  = 10^6.5 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.9492623e-22);
	fluxm = 2.39e-17*mr_gal_da;
	thalo_keV = 0.2725;
	taum_gal = nh_galpart*7.3893951e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 1.967*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.419) {  /* Thalo  = 10^6.55 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.6791972e-22);
	fluxm = 2.16e-17*mr_gal_da;
	thalo_keV = 0.3057;
	taum_gal = nh_galpart*7.3849013e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 1.539*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.469) {  /* Thalo  = 10^6.6 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.4587872e-22);
	fluxm = 2.00e-17*mr_gal_da;
	thalo_keV = 0.3431;
	taum_gal = nh_galpart*7.4302016e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 1.230*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.516) {  /* Thalo  = 10^6.65 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.2824252e-22);
	fluxm = 1.90e-17*mr_gal_da;
	thalo_keV = 0.3489;
	taum_gal = nh_galpart*7.520915e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 1.004*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.565) {  /* Thalo  = 10^6.7 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.1421945e-22);
	fluxm = 1.85e-17*mr_gal_da;
	thalo_keV = 0.4319;
	taum_gal = nh_galpart*7.6912110e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 0.840*mr_gal*exp(-taum_gal);

    } else if (hrat < 0.625) {  /* Thalo  = 10^6.75 */
	mr_gal_da = mr_gal*exp(nh_galpart*4.0256746e-22);
	fluxm = 1.85e-17*mr_gal_da;
	thalo_keV = 0.4846;
	taum_gal = nh_galpart*7.9517284e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 0.726*mr_gal*exp(-taum_gal);

    } else { /* Thalo  = 10^6.8 */
	mr_gal_da = mr_gal*exp(nh_galpart*3.9222212e-22);
	fluxm = 1.90e-17*mr_gal_da;
	thalo_keV = 0.5437;
	taum_gal = nh_galpart*8.3102749e-21;
	cr_gal = cr - 238.8*exp(-tauc_gal) - 0.66*mr_gal*exp(-taum_gal);

    } /* endif hrat */
						  
    area_dif=pi*s_obs->radius*s_obs->radius;
    fluxm=fluxm*area_dif;

    /* c-band flux, assuming unabsorbed 10^6.1 plasma */

    fluxc = 3.497e-18 * cr_gal * area_dif;

    /* Since the above process ends up over predicting the C (M) band by 5 (4)%
       on average, reduce the fluxes accordingly: */
    fluxc /= 1.05;
    fluxm /= 1.04;

    fluxc -= 0.6342 * s_flux->swcxcont * area_dif;
    if (fluxc < 0.0) fluxc = 0;

    xsphab_trans = calloc(nebin, sizeof(double));
    spec_halo = calloc(nebin, sizeof(double));
    spec_lhb = calloc(nebin, sizeof(double));
    (*gal_spec) = calloc(nebin, sizeof(double));

    /* Transmission for galactic absorption: */
    errstatus = spectra(0, 0.0, nh_galpart, 0.0, 0.0, 0.0, nebin, s_engrid->energ_mid, s_engrid->xspec_energy, xsphab_trans, debug);

    /* Galactic  (Raymond-Smith) halo spectrum: */
    errstatus = spectra(2, 0.0, thalo_keV, fluxm, 0.47, 1.21, nebin, s_engrid->energ_mid,  s_engrid->xspec_energy, spec_halo, debug);

    /* LHB  (Raymond-Smith) halo spectrum: */
    errstatus = spectra(2, 0.0, tlhb_keV, fluxc, 0.12, 0.284, nebin, s_engrid->energ_mid,  s_engrid->xspec_energy, spec_lhb, debug);

    /* Print out some values, for checking: */
    printf("   thalo_keV = %e\n",thalo_keV);
    printf("   fluxm = %e\n",fluxm);
    printf("   tlhb_keV = %e\n",tlhb_keV);
    printf("   fluxc = %e\n",fluxc);

    /* Add these, absorbing the halo only: */
    for (int iebin= 0; iebin < nebin; iebin++) {
	(*gal_spec)[iebin] = spec_lhb[iebin] + xsphab_trans[iebin]*spec_halo[iebin]; 
	//printf("%f  %f  %f  %f\n",xsphab_trans[iebin],spec_halo[iebin],spec_lhb[iebin],gal_spec[iebin]);
    }						  

    free(xsphab_trans);
    free(spec_halo);
    free(spec_lhb);
    

    for (int ii=0; ii<nx_c+1; ii++)
	free(c_arr[ii]);
    free(c_arr);

    for (int ii=0; ii<nx_m+1; ii++)
	free(m_arr[ii]);
    free(m_arr);

    for (int ii=0; ii<nx_ij+1; ii++)
	free(ij_arr[ii]);
    free(ij_arr);


    return errstatus;
}


/* FUNCTION NAME: dgrb                                                      */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Compute background contribution from diffuse gamma ray background      */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   dgrb(s_obs, s_engrid, &dgrb_spec);                                     */
/*                                                                          */
/* INPUTS: s_obs, s_engrid                                                  */
/*                                                                          */
/* OUTPUTS: dgrb_spec                                                       */
/*                                                                          */
/* CALLED BY: doWork()                                                      */
/*                                                                          */

int dgrb(Obs_params_struct * s_obs,
         Engrid_struct * s_engrid,
         double ** dgrb_spec,
	 int debug){

    (*dgrb_spec) = calloc(s_engrid->nebin, sizeof(double));
    printf("   Contributions from dgrb are not yet supported.  Continuing...\n");

    return 0;
}




/* FUNCTION NAME: grxe                                                      */
/*                                                                          */
/* PURPOSE:                                                                 */
/*   Compute background contribution from galactic ridge                    */
/*                                                                          */
/* CALLING SEQUENCE:                                                        */
/*   grxe(s_obs, s_engrid, &grxe_spec);                                     */
/*                                                                          */
/* INPUTS: s_obs, s_engrid                                                  */
/*                                                                          */
/* OUTPUTS: grxe_spec                                                       */
/*                                                                          */
/* CALLED BY: doWork()                                                      */
/*                                                                          */


int grxe(Obs_params_struct * s_obs,
         Engrid_struct * s_engrid,
         double ** grxe_spec,
	 int debug){

    (*grxe_spec) = calloc(s_engrid->nebin, sizeof(double));
    printf("   Contributions from grxe are not yet supported.  Continuing...\n");

    return 0;
}



/*
 ! $Log: doWork.c,v $
 ! Revision 1.35  2015/10/06 14:54:46  driethmi
 ! More provisions to handle case with zero point sources.  Needed to move call
 ! to torus_prep() outside the loop, so it is executed even with zero point
 ! sources.
 !
 ! Revision 1.34  2015/10/02 18:36:47  driethmi
 ! Changes to handle case where zero sources are present.  Probably was already ok,
 ! but we now handle this explicitly.
 !
 ! Revision 1.33  2015/09/10 19:52:36  driethmi
 ! Now initialize value of flux_res_1 = 0.0, even if ns_1 == 0.  Also, changed
 ! print statement reporting the case of no sources given above the specified
 ! flux sensitivity.
 !
 ! Revision 1.32  2015/05/19 17:12:16  driethmi
 ! Added CVS macro to print file revision history at end of file.
 !
*/
