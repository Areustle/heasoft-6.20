/*
 *	XrtPsfCalc: --- XRT PSF Computation
 *
 *
 *	DESCRIPTION:
 *         Functions to compute the PSF. The model is a GAUSSIAN+KING model.
 *         The Radial PSF has been extracted at different energies for
 *         the FM2, using the Q software and subtracting background
 *         with BEAM/ABEAM, The derived PSF has been then fitted
 *         using QDP/PGPLOT (Sergio Campana) 
 *
 *
 *	DOCUMENTATION:
 *         The Algoritm used to calculate the PSF has been taken
 *         from Sergio Campana PSF Report. 
 *
 *
 *      FUNCTION CALL
 *          int CalReadPsfFile(BOOL FromCaldb,
 *			       const char *CalFileName,
 *                             PsfPar_t *PsfParm)
 *
 *	CHANGE HISTORY:
 *        0.1.0: - FT 06/03/2002 - First version
 *
 *	AUTHORS:
 *        Italian Swift Archive Center (FRASCATI)
 */

#define XRTPSFCALC_C
#define XRTPSFCALC_VERSION			"0.1.0"

		/********************************/
		/*        header files          */
		/********************************/

#include "XrtPsfCalc.h"
#include <math.h>

/********************************/
/*         definitions          */
/********************************/

#define DOUBLESAME 0.0000000001

/********************************/
/*           typedefs           */
/********************************/

/********************************/
/*           globals            */
/********************************/

/********************************/
/*          functions           */
/********************************/



/*
 *	ComputeEnDepParams: --- Compute the GAUSSIAN-KING Energy
 *                              dependent parameters  ---
 *
 *
 *	DOCUMENTATION:
 *         The Algoritm used to calculate the PSF has been taken
 *         from Sergio Campana PSF Report.
 *
 *
 *      FUNCTION CALL
 *         int ComputeEnDepParams(double Energy, 
 *                                 PsfPar_t *PsfParm)
 *
 *      INPUT PARAMETERS:
 *         Energy - Energy value of the event
 *         PsfParm   - output structure array that will be fill by computed 
 *      	    Enrgy Dependent Parameters (see xrtcaldb.h for structure
 *      	    description). The structure must be previously filled by 
 *      	    a call to CalReadPsfFile function.
 *      OUTPUT PARAMETERS:
 *         PsfParm   - structure fillwith Enrgy Dependent Parameters 
 *      	    (see xrtcaldb.h for structure description)
 *
 *	CHANGE HISTORY:
 *        0.1.0: - FT 06/03/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 *
 */

int ComputeEnDepParams(double Energy, PsfPar_t *PsfParm )
{


  int i,ilast;
  double sigma, r_l, m_e, r_e, d2;
  double plate_scale;
  int offidx;

  plate_scale = 1./3.; /* ????? */
  

  /*
   * If the Energy is the same of the last computation do not
   * compute again the Energy dependent parameters
   */
  if ( fabs(Energy - PsfParm->currEn) < DOUBLESAME ) 
    return OK;

  headas_chat(CHATTY, 
	      "ComputeEnDepParams_%s: Computing Parameters for Energy %lfKeV\n",
	      XRTPSFCALC_VERSION,Energy);

  if (  PsfParm->numb_off_axis == 0 ) {
    headas_chat(MUTE, 
		"ComputeEnDepParams_%s: Error reading parameters from PsfParm"
		" the structure could be empty or problems occurred reading" 
                " XRT PSF calibration File",
		XRTPSFCALC_VERSION);
    return NOT_OK;
  }

  ilast = PsfParm->numb_off_axis - 1 ;

  /* 
   * Comute Energy Dependent Parameters for all ON/OFF-AXIS parameters
   */
  for ( i=0; i<=ilast; i++ ) {
    sigma = ( PsfParm->params[i].coefs[sigma_p][0] +
	      PsfParm->params[i].coefs[sigma_p][1]*Energy +
	      PsfParm->params[i].coefs[sigma_p][2]*Energy*Energy );

    r_l = ( PsfParm->params[i].coefs[r_l_p][0] +
	    PsfParm->params[i].coefs[r_l_p][1]*Energy +
	    PsfParm->params[i].coefs[r_l_p][2]*Energy*Energy );

    m_e = ( PsfParm->params[i].coefs[m_e_p][0] +
	    PsfParm->params[i].coefs[m_e_p][1]*Energy +
	    PsfParm->params[i].coefs[m_e_p][2]*Energy*Energy );

    r_e = ( PsfParm->params[i].coefs[r_e_p][0] +
	    PsfParm->params[i].coefs[r_e_p][1]*Energy +
	    PsfParm->params[i].coefs[r_e_p][2]*Energy*Energy );

    d2 = r_l*r_l/(2.*(m_e-1.));

    PsfParm->params[i].sigma = sigma;
    PsfParm->params[i].r_l = r_l;
    PsfParm->params[i].m_e = m_e;
    PsfParm->params[i].r_e = r_e;
    PsfParm->params[i].d2 = d2;
  }

  for( offidx=0; offidx<PsfParm->numb_off_axis;offidx++) {
    headas_chat(CHATTY,"\toffset %lf\n",PsfParm->params[offidx].offset );
    headas_chat(CHATTY,"\t\tsigma  %lf\n",PsfParm->params[offidx].sigma );
    headas_chat(CHATTY,"\t\tr_l    %lf\n",PsfParm->params[offidx].r_l );
    headas_chat(CHATTY,"\t\tm_e    %lf\n",PsfParm->params[offidx].m_e );
    headas_chat(CHATTY,"\t\tr_e    %lf\n",PsfParm->params[offidx].r_e );

    if ( PsfParm->params[offidx].sigma<0. ||
	 PsfParm->params[offidx].r_l<0. ||
	 PsfParm->params[offidx].m_e<0. ||
	 PsfParm->params[offidx].r_e<0. ) {

      headas_chat(NORMAL, 
	    "ComputeEnDepParams_%s: Error one negative quantitaty computed\n",
	     XRTPSFCALC_VERSION);

      headas_chat(CHATTY,"\tenergy %lf\n",Energy );
      headas_chat(CHATTY,"\toffset %lf\n",PsfParm->params[offidx].offset );
      headas_chat(CHATTY,"\t\tsigma  %lf\n",PsfParm->params[offidx].sigma );
      headas_chat(CHATTY,"\t\tr_l    %lf\n",PsfParm->params[offidx].r_l );
      headas_chat(CHATTY,"\t\tm_e    %lf\n",PsfParm->params[offidx].m_e );
      headas_chat(CHATTY,"\t\tr_e    %lf\n",PsfParm->params[offidx].r_e );
      return NOT_OK;

    }
  }

  /*
   *    reset current energy
   */
  PsfParm->currEn = Energy;

  return OK;

}

/*
 *
 *
 *	PsfFunction: --- Compute the psf at a given Energy
 *                       and distance to the FOV (or offset) ---
 *
 *
 *	DOCUMENTATION:
 *         The Algoritm used to calculate the PSF has been taken
 *         from Sergio Campana PSF Report.
 *
 *
 *      FUNCTION CALL
 *        PsfFunction(double dist,   
 *		      double energy, 
 *		      double offset, 
 *		      PsfPar_t *PsfParm )
 *
 *      INPUT PARAMETERS
 *        dist       - distance (arcmin) from the FOV (offset = 0 )
 *                     or from the offset point (offset > 0)
 *        energy     - energy (KeV) fot PSF Computation
 *        offset     - offset from the FOV
 *        PsfParm    - psf parameters read from PSF Calibration
 *                     File (CalReadPsfFils call) and Energy dependent
 *                     parameters
 *      OUTPUT PARAMETERS
 *        PsfParm    - Energy dependent parameters filled                   
 *
 *      RERURN VALUES:
 *        psfval     - computed value of psf
 *        NULL_VALUE - error on psf computation
 *        
 *	CHANGE HISTORY:
 *        0.1.0: - FT 06/03/2002 - First version
 *
 *	AUTHORS:
 *
 *        Italian Swift Archive Center (FRASCATI)
 *
 */

double PsfFunction(double dist,   /* arcmin */
		   double energy, /* KeV    */ 
		   double offset, /* arcmin */
		   PsfPar_t *PsfParm )
{

  double sigma, r_l, m_e, r_e, d2;
  double pixel_size;
  double twopi = 6.283185; 
  double psfval;
  double esponenz;
  int    offindex = 0;

  /*
   *    Compute the Energy Dependent Parameters
   */
  if ( ComputeEnDepParams(energy, PsfParm) ) {
    headas_chat(MUTE, "PsfFunction_%s: Fatal Error ComputeEnepParams\n",
		XRTPSFCALC_VERSION);					      
    return NULL_VALUE;
  }

  /*
   * Select index for Off-Axis PSF Parameters
   */
  offindex = GetOffAxisIdx( PsfParm, offset);

  if ( offindex >= PsfParm->numb_off_axis ) {

    headas_chat(MUTE, "PsfFunction_%s: Fatal Error scanning PsfParm_t for PSF OFF-Axis computation",
		XRTPSFCALC_VERSION);					      

    return NOT_OK;
  }

  sigma = PsfParm->params[offindex].sigma;
  r_l = PsfParm->params[offindex].r_l;
  m_e = PsfParm->params[offindex].m_e;
  r_e = PsfParm->params[offindex].r_e;
  d2 = PsfParm->params[offindex].d2;

  /*
  psfval = 1./twopi/(r_e*sigma*sigma+d2)*
	  ( r_e*exp(-(dist*dist)/(2.*sigma*sigma))+
		pow((1.+(dist/r_l)*(dist/r_l)), -m_e ) ));
  */

  if ( r_l != 0. && (r_e*sigma*sigma+d2) != 0. && (2.*sigma*sigma) != 0.) {
    psfval = 1./twopi/(r_e*sigma*sigma+d2)*
            ( r_e*exp(-(dist*dist)/(2.*sigma*sigma) )+
            pow( (1.+(dist/r_l)*(dist/r_l)), -m_e ) );
    return psfval;
  }
  else {
    headas_chat(MUTE, "PsfFunction_%s: Fatal Error Computing Psf Value\n",
		XRTPSFCALC_VERSION);					      
    return NULL_VALUE ;
  }

}/* PsfFunction */


int  GetOffAxisIdx( PsfPar_t *Parm, double offset ) 
{                                            

  /*									      
   * Parm - Pointer to a PsfPar_t structure (xrtcaldb.h)
   * offset - current offset
   */									      
    int ilast;								      
    int idx;	
							      
   /*									      
    * Set the end of the loop on OFF-Axis values			      
    */ 									      
    ilast = Parm->numb_off_axis - 1;					      
									      
    if ( ilast == 0 && offset > 0 ) {
    									      
      headas_chat(CHATTY, "PsfFunction_%s: Warning Parameters for OFF-AXIS "  
		  " PSF computation not available\n",XRTPSFCALC_VERSION);     
      headas_chat(CHATTY, "PsfFunction_%s: Warning PSF computed on Axis \n",  
		  XRTPSFCALC_VERSION);					      
      return 0;								      
    }                                                                         
    else {								      
      int found = 0;							      
      idx = 0;								      
      while( idx<=ilast-1 || found ) {					      
	if ( offset >= Parm->params[idx].offset &&                        
             offset <= Parm->params[idx+1].offset ) {	              
	  return ((( offset - Parm->params[idx].offset ) < 
	   ( Parm->params[idx+1].offset - offset )) ?	      
	    (idx) : (idx-1)) ;					      
	}	
	idx++;
      }									      
      return (ilast);
    }                                                                         
}
