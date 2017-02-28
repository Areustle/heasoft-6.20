/**                                                                                                                                              
 * \file finalize.c                                                                                                                              
 * \brief Contains routines finalize code, clean up memory allocations                                                                           
 * \author David Riethmiller                                                                                                                     
 * \date $Date: 2015/05/19 17:12:16 $                                                                                                            
 */

#include "skyback.h"
#include <stdio.h>
#include <stdlib.h>


/* FUNCTION NAME: deallocate_energ_data                       */
/*                                                            */
/* PURPOSE:                                                   */
/*  Free variables contained in s_engrid struct               */
/*                                                            */
/* CALLING SEQUENCE:  deallocate_energ_data(&s_engrid);       */
/*                                                            */
/* INPUTS: s_engrid                                           */
/*                                                            */
/* OUTPUTS: none                                              */
/*                                                            */




void deallocate_energ_data(Engrid_struct * s_engrid){
    free(s_engrid->energ_hi);
    free(s_engrid->energ_lo);
    free(s_engrid->energ_mid);
    free(s_engrid->xspec_energy);
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


/*
 ! $Log: finalize.c,v $
 ! Revision 1.5  2015/05/19 17:12:16  driethmi
 ! Added CVS macro to print file revision history at end of file.
 !
*/

