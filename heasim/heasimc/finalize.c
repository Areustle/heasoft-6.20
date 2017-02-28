/**
 * \file finalize.c
 * \brief Contains routines finalize code, clean up memory allocations
 * \author David Riethmiller
 * \date $Date: 2016/03/31 21:41:40 $
 */

#include "heasim.h"
#include <stdio.h>
#include <stdlib.h>
#ifndef use_legacy_heasp
#include "Cheasp.h"       /* C-interface to C++ HEASP library */
#endif


/* FUNCTION NAME: deallocate_source_data                                                                       */
/*                                                                                                             */
/* CALLING SEQUENCE:                                                                                           */
/*       deallocate_source_data(nsource,  &ras, &decs, &colden, &spectype, &specpar, &fluxpar,                 */
/*                           &band_lo, &band_hi, &sfilename, &sformat, &sunits, &period, &pulse_fraction,      */
/*                         &ifilename, sd_matrix_size, &sd_param_matrix);                                      */
/*                                                                                                             */
/* PURPOSE: Free variables from memory heap                                                                    */
/*                                                                                                             */
/* INPUTS: nsource, ras, decs, colden, spectype, specpar, fluxpar, band_lo, band_hi, sfilename, sformat        */
/*               sunits, period, pulse_fraction, ifilename, sd_matrix_size, sd_param_matrix                    */
/*                                                                                                             */
/* OUTPUTS: none                                                                                               */
/*                                                                                                             */
/* CALLED BY:                                                                                                  */
/*   main()                                                                                                    */
/*                                                                                                             */

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
                            double *** sd_param_matrix){   /* matrix of spatial distribution quantities needed to apply source distribution */

    int i;  /* generic loop index */

    printf("Deallocating source data...\n");
    free(*ras);
    free(*decs);
    free(*colden);
    free(*spectype);
    free(*specpar);
    free(*fluxpar);
    free(*band_lo);
    free(*band_hi);
    free(*sformat);
    free(*sunits);
    free(*period);
    free(*pulse_fraction);
    free(*tburst);
    free(*trise);
    free(*tdecay);
    free(*burst_rat);
    free(*burst_tabidx);

    for (i=0; i<nsource; i++){

	free( (*sd_param_matrix)[i] );
	free( sfilename[i] );
	free( ifilename[i] );

    }

    free(*sd_param_matrix);
    free(sfilename);
    free(ifilename);
    printf("...done.\n");
}


/* FUNCTION NAME: deallocate_rmf_data               */
/*                                                  */
/* PURPOSE: Free rmf_struct from memory heap        */
/*                                                  */
/* INPUTS: rmf_struct                               */
/*                                                  */
/* CALLED BY: main()                                */
/*                                                  */

void deallocate_rmf_data(RMF * rmf_struct){
    
    printf("Deallocating RMF data...\n");
    free(rmf_struct->NumberGroups);
    free(rmf_struct->FirstGroup);
    free(rmf_struct->FirstChannelGroup);
    free(rmf_struct->NumberChannelGroups);
    free(rmf_struct->FirstElement);
    if (rmf_struct->isOrder == 1)
	free(rmf_struct->OrderGroup);
    free(rmf_struct->LowEnergy);
    free(rmf_struct->HighEnergy);
    free(rmf_struct->Matrix);
    free(rmf_struct->ChannelLowEnergy);
    free(rmf_struct->ChannelHighEnergy);
    printf("...done.\n");
}



/* FUNCTION NAME: deallocate_arf_data               */
/*                                                  */
/* PURPOSE: Free arf_struct from memory heap        */
/*                                                  */
/* INPUTS: arf_struct                               */
/*                                                  */
/* CALLED BY: main()                                */
/*                                                  */

void deallocate_arf_data(ARF * arf_struct){

    printf("Deallocating ARF data...\n");
    free(arf_struct->LowEnergy);
    free(arf_struct->HighEnergy);
    free(arf_struct->EffArea);
    printf("...done.\n");

}

/* FUNCTION NAME: deallocate_burst_data             */
/*                                                  */
/* PURPOSE: Free btab from memory heap              */
/*                                                  */
/* INPUTS: btab                                     */
/*                                                  */
/* CALLED BY: main()                                */
/*                                                  */


void deallocate_burst_data(burst_table_struct * btab){

    printf("Deallocating burst data...\n");

    for (int ii=0; ii<btab->xdim; ii++){
        for (int jj=0; jj<btab->ydim; jj++){
            free(btab->burst_table[ii][jj]);
        }
        free(btab->burst_table[ii]);
    }
    free(btab->burst_table);
    free(btab->burst_tabidx);

    printf("...done.\n");
}



/* FUNCTION NAME: deallocate_bin_data                                   */
/*                                                                      */
/* PURPOSE: Free data allocated during check_calfiles_exist from memory */
/*                                                                      */
/* INPUTS: ebin_lo, ebin_hi, ebin_mid, ebin_delt, area, ebin_lo_rmf,    */
/*          ebin_hi_rmf, emin, emax, ecen, edelt                        */
/*                                                                      */
/* CALLED BY: main()                                                    */
/*                                                                      */

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
                         double ** edelt){        /* grid spacing output energy grid bins */

    printf("Deallocating bin data...\n");
    free(*ebin_lo);
    free(*ebin_hi);
    free(*ebin_mid);
    free(*ebin_del);
    free(*area);
    free(*ebin_lo_rmf);
    free(*ebin_hi_rmf);
    free(*emin);
    free(*emax);
    free(*ecen);
    free(*edelt);
    printf("...done.\n");
}



/* FUNCTION NAME: deallocate_imagedis_data                              */
/*                                                                      */
/* PURPOSE: Free data allocated during imagedis routine                 */
/*                                                                      */
/* INPUTS: cprob, nel_subimg, ra_img, dec_img                           */
/*                                                                      */
/* CALLED BY: main()                                                    */
/*                                                                      */

void deallocate_imagedis_data(double *** cprob, double ** cprob_vec, int nel_subimg){
    int i=0;
    
    //printf("Deallocating imagedis data...\n");
    for (i=0; i<nel_subimg; i++){
	free( (*cprob)[i] );
    }
    free(*cprob);
    free(*cprob_vec);

    //printf("...done.\n");
}



/* FUNCTION NAME: deallocate_sourcedis_data                             */
/*                                                                      */
/* PURPOSE: Free data allocated during sourcedis routine                */
/*                                                                      */
/* INPUTS: dx_arcmin, dy_arcmin                                         */
/*                                                                      */
/* CALLED BY: main()                                                    */
/*                                                                      */

void deallocate_sourcedis_data(double ** dx_arcmin, double ** dy_arcmin){
    
    printf("Deallocating sourcedis data...\n");
    free(*dx_arcmin);
    free(*dy_arcmin);
    printf("...done.\n");
}



/* FUNCTION NAME: deallocate_pha_data                         */
/*                                                            */
/* PURPOSE: Free background structure from memory heap        */
/*                                                            */
/* INPUTS: iback_pha_struct                                   */
/*                                                            */
/* CALLED BY: read_internal_background()                      */
/*                                                            */

void deallocate_pha_data(PHA * iback_pha_struct){

    printf("Deallocating PHA data...\n");
    free(iback_pha_struct->Pha);
    free(iback_pha_struct->StatError);
    free(iback_pha_struct->SysError);
    free(iback_pha_struct->Quality);
    free(iback_pha_struct->Grouping);
    free(iback_pha_struct->Channel);
    free(iback_pha_struct->AreaScaling);
    free(iback_pha_struct->BackScaling);
    printf("...done.\n\n");

}



/* FUNCTION NAME: deallocate_vignette_data                    */
/*                                                            */
/* PURPOSE: Free vig structure from memory heap               */
/*                                                            */
/* INPUTS: s_vig                                              */
/*                                                            */
/* CALLED BY: main()                                          */
/*                                                            */

void deallocate_vignette_data(Vig_struct * s_vig){
    int ii=0;

    printf("Deallocating vignette data...\n");

    if (s_vig->vigtype != 3){ /* if not an image */
	for (ii=0; ii<s_vig->ncol-1; ii++)
	    free(s_vig->vig_matrix[ii]);
	free(s_vig->vig_matrix);

    } else { /* if an image */
	for (ii=0; ii<s_vig->n_energy; ii++)
	    free(s_vig->vig_immat[ii]);
	free(s_vig->vig_immat);
    }


    free(s_vig->energy_vec);
    free(s_vig->angle_vec);

    printf("...done.\n");
    
}

/* FUNCTION NAME: deallocate_psf_data                         */
/*                                                            */
/* PURPOSE: Free psf structure from memory heap               */
/*                                                            */
/* INPUTS: s_psf                                              */
/*                                                            */
/* CALLED BY: main()                                          */
/*                                                            */

void deallocate_psf_data(PSF_struct * s_psf, PSF_image * s_psf_image){
    int ii=0;

    printf("Deallocating psf data...\n");

    if (s_psf->psftype != 3){ /* if not an image */
        for (ii=0; ii<s_psf->n_eef; ii++)
            free(s_psf->eef_matrix[ii]);
        free(s_psf->eef_matrix);
	free(s_psf->radii);
	free(s_psf->energy_vec);
	free(s_psf->angle_vec);


    } else { /* if an image */
	for (ii=0; ii<s_psf_image->nimages; ii++){
	    free(s_psf_image->prob_array[ii]);
	}

	free(s_psf_image->xdim);
	free(s_psf_image->ydim);
	free(s_psf_image->crvl1);
	free(s_psf_image->crvl2);
	free(s_psf_image->crpx1);
	free(s_psf_image->crpx2);
	free(s_psf_image->cdlt1);
	free(s_psf_image->cdlt2);
	free(s_psf_image->azim_vec);
        free(s_psf_image->energy_vec);
        free(s_psf_image->angle_vec);
	free(s_psf_image->prob_array);

    }

    printf("...done.\n");
}



/* FUNCTION NAME: deallocate_torus                            */
/*                                                            */
/* PURPOSE:                                                   */
/*  Free variables contained in torus structs                 */
/*                                                            */
/* CALLING SEQUENCE:  deallocate_torus(&s_tpar,&s_tspec);     */
/*                                                            */
/* INPUTS: s_tpar, s_tspec                                    */
/*                                                            */
/* OUTPUTS: none                                              */
/*                                                            */

void deallocate_torus(torus_par_struct * s_tpar, torus_spec_struct *s_tspec){

    int ii = 0;

    free(s_tpar->nhvals);
    free(s_tpar->gamvals);
    free(s_tpar->thvals);
    free(s_tpar->incvals);

    free(s_tspec->tor_energ_lo);
    free(s_tspec->tor_energ_hi);
    free(s_tspec->tor_energ_mid);
    free(s_tspec->tor_energ_wid);
    free(s_tspec->tor_xspec_energy);

    for (ii=0; ii<s_tspec->num_spec; ii++)
        free(s_tspec->tor_spec[ii]);
    free(s_tspec->tor_spec);

}

/* FUNCTION NAME: update_checksums                            */
/*                                                            */
/* PURPOSE:                                                   */
/*  Update all checksums in the fits file                     */
/*                                                            */
/* CALLING SEQUENCE:  update_checksums(ounit)                 */
/*                                                            */
/* INPUTS: ounit                                              */
/*                                                            */
/* OUTPUTS: none                                              */
/*                                                            */


void update_checksums(fitsfile * ounit){
    int fstat = 0;

    fits_movnam_hdu(ounit, IMAGE_HDU, "Primary", 0, &fstat);
    fits_write_chksum(ounit, &fstat);

    fits_movnam_hdu(ounit, BINARY_TBL, "EVENTS", 0, &fstat);
    fits_write_chksum(ounit, &fstat);

    fits_movnam_hdu(ounit, BINARY_TBL, "GTI", 0, &fstat);
    fits_write_chksum(ounit, &fstat);

}


/*
  ! $Log: finalize.c,v $
  ! Revision 1.25  2016/03/31 21:41:40  driethmi
  ! Impemented suite of changes to handle burst time assignment.  (There will
  ! likely be more bug fixes to this implementation soon.)
  !
  ! Revision 1.24  2015/07/01 18:46:34  driethmi
  ! Cleaned up checksum writing, so this happens at the end of the code.  Heasim
  ! output now passes ftverify check.
  !
  ! Revision 1.23  2015/05/29 17:49:04  driethmi
  ! Removed function clean_output_fits().  This functionality is now done inline,
  ! rather than at the end of heasim, making it obsolete.
  !
  ! Revision 1.22  2015/05/04 16:16:33  driethmi
  ! Implemented better variable initializations.
  !
  ! Revision 1.21  2015/03/19 20:35:30  driethmi
  ! Added burst capability, and updated function comment blocks.
  !
  ! Revision 1.20  2015/02/18 21:34:07  driethmi
  ! Corrected bug in process_image, needed to use fits_read_key_dbl instead of
  ! fits_read_key_flt - lack of precision was causing zeroes to be read.  Also
  ! improved verbosity of debug statements.
  !
  ! Revision 1.19  2015/02/18 18:52:51  driethmi
  ! Improved diagnostic output, restricted more output to debug == 1.
  !
  ! Revision 1.18  2015/02/18 15:53:41  driethmi
  ! Preliminary redshift changes, and cleaned up output chatter so debug = 1
  ! is more useful.
  !
  ! Revision 1.17  2015/01/28 21:32:20  driethmi
  ! Enabled torus spectral model for background point sources.
  !
  ! Revision 1.16  2015/01/21 17:25:02  driethmi
  ! Modified heasim to accept data files from sky background tool.
  !
  ! Revision 1.15  2014/12/02 19:57:40  driethmi
  ! Changed floats to doubles for consistency, except where float is required.
  !
  ! Revision 1.14  2014/08/19 18:30:40  driethmi
  ! Added initial capability to read PSF images correctly.
  !
  ! Revision 1.13  2014/07/07 15:39:34  driethmi
  ! Adjusted call to deallocate imagedis data.
  !
  ! Revision 1.12  2014/07/01 13:47:15  driethmi
  ! Current state - added roll angle and image HDU capability.  Still some
  ! issues to work out with response file and background read.  Also, imagedis
  ! seems to have execution bottleneck.
  !
  ! Revision 1.11  2014/06/02 18:54:41  driethmi
  ! Added switch to use legacy C heasp library instead of active C++ library
  ! from heacore/heasp.
  !
  ! Revision 1.10  2014/04/29 21:11:38  driethmi
  ! Added function to remove lines from output fits file with PI channel less
  ! than or equal to zero.  Called during the "finalize" phase.
  !
  ! Revision 1.9  2014/04/03 20:41:06  driethmi
  ! Updated commenting and minor changes to attempt to comply more with coding
  ! standards.  Added doxygen tags.
  !
*/
